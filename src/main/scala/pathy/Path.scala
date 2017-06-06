/*
 * Copyright 2014–2017 SlamData Inc.
 * Copyright      2017 @alexknvl
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.alexknvl
package pathy

import java.nio.file.FileSystems
import java.nio.file.{ Path => NioPath }

import cats.data.NonEmptyList
import cats.{Order, Show}
import cats.syntax.either._
import cats.syntax.order._
import cats.syntax.show._
import cats.instances.all._

import scala.annotation.tailrec

object `package` {
  type RelFile     = Path[Rel, File]
  type AbsFile     = Path[Abs, File]
  type RelDir      = Path[Rel,  Dir]
  type AbsDir      = Path[Abs,  Dir]
  type AnyPath     = Path[_,      _]
  type RelPath     = Path[Rel,    _]
  type AbsPath     = Path[Abs,    _]
}

final class Rel private ()
final class Abs private ()

final class File private ()
final class Dir private ()

sealed abstract class IsFile[-X] {
  def subst[B](p: Path[B, X]): Path[B, File]
}
object IsFile {
  implicit val instance: IsFile[File] = new IsFile[File] {
    def subst[B](p: Path[B, File]): Path[B, File] = p
  }
}

sealed abstract class IsDir[-X] {
  def subst[B](p: Path[B, X]): Path[B, Dir]
}
object IsDir {
  implicit val instance: IsDir[Dir] = new IsDir[Dir] {
    def subst[B](p: Path[B, Dir]): Path[B, Dir] = p
  }
}

final case class FileName(value: String) extends AnyVal {
  def extension: String = {
    val idx = value.lastIndexOf(".")
    if (idx == -1) "" else value.substring(idx + 1)
  }

  def dropExtension: FileName = {
    val idx = value.lastIndexOf(".")
    if (idx == -1) this else FileName(value.substring(0, idx))
  }

  def changeExtension(f: String => String): FileName =
    FileName(dropExtension.value + "." + f(extension))
}
object FileName {
  implicit val order: Order[FileName] = Order.by(_.value)
  implicit val show: Show[FileName] = Show.show(_.value)
}

final case class DirName(value: String) extends AnyVal
object DirName {
  implicit val order: Order[DirName] = Order.by(_.value)
  implicit val show: Show[DirName] = Show.show(_.value)
}


sealed trait Path[+B, +T] {
  import Path._

  def isAbsolute: Boolean
  def isRelative: Boolean = !isAbsolute

  def identicalPathWith(p2: AnyPath): Boolean =
    this.show == p2.show

  final def relativeTo[BB >: B](dir: Path[BB, Dir]): Option[Path[Rel, T]] = {
    def go[TT](p1: Path[BB, TT], p2: Path[BB, Dir]): Option[Path[Rel, TT]] =
      if (p1.identicalPathWith(p2)) Some(Current)
      else p1.peel match {
        case None => (p1, p2) match {
          case (Root, Root)       => Some(Current)
          case (Current, Current) => Some(Current)
          case _                  => None
        }
        case Some((p1p, v)) =>
          go(p1p, p2).map(p => p </> v.fold[Path[Rel, TT]](DirIn(Current, _), FileIn(Current, _)))
      }
    go(this.canonicalize, dir.canonicalize)
  }

  final def </>[TT](rel: Path[Rel, TT])(implicit ev: IsDir[T]): Path[B, TT] =
    (this, rel) match {
      case (Current, Current)        => Current
      case (Root, Current)           => Root
      case (ParentIn(p1), Current)   => ParentIn(p1 </> Current)
      case (FileIn(p1, f1), Current) => FileIn(p1 </> Current, f1)
      case (DirIn(p1, d1), Current)  => DirIn(p1 </> Current, d1)

      // these don't make sense, but cannot exist anyway
      case (Current, Root)        => Current
      case (Root, Root)           => Root
      case (ParentIn(p1), Root)   => ParentIn(p1 </> Current)
      case (FileIn(p1, f1), Root) => FileIn(p1 </> Current, f1)
      case (DirIn(p1, d1), Root)  => DirIn(p1 </> Current, d1)

      case (p1, ParentIn(p2))   => ParentIn(p1 </> p2)
      case (p1, FileIn(p2, f2)) => FileIn(p1 </> p2, f2)
      case (p1, DirIn(p2, d2))  => DirIn(p1 </> p2, d2)
    }

  // NB: scala doesn't cotton to `<..>`
  final def <::>[BB >: B, TT](rel: Path[Rel, TT])(implicit ev: IsDir[T]): Path[BB, TT] =
    this.parentDir1 </> rel

  // NB: scala doesn't cotton to `<.>`
  final def <:>(ext: String)(implicit ev: IsFile[T]): Path[B, File] =
    renameFile(name => name.changeExtension(_ => ext))(ev)

  final def fileName(implicit ev: IsFile[T]): FileName = this match {
    case FileIn(_, name) => name
    case _ => sys.error("impossible!")
  }

  final def dirName(implicit ev: IsDir[T]): Option[DirName] = this match {
    case DirIn(_, name) => Some(name)
    case _ => None
  }

  final def refineType: Path[B, Dir] Either Path[B, File] = this match {
    case Current      => Left(Current)
    case Root         => Left(Root)
    case ParentIn(p)  => Left(ParentIn(p.unsafeCoerceType))
    case FileIn(p, f) => Right(FileIn(p.unsafeCoerceType, f))
    case DirIn(p, d)  => Left(DirIn(p.unsafeCoerceType, d))
  }

  final def maybeDir: Option[Path[B, Dir]] =
    this.refineType.swap.toOption

  final def maybeFile: Option[Path[B, File]] =
    this.refineType.toOption

  final def peel: Option[(Path[B, Dir], DirName Either FileName)] = this match {
    case Current       => None
    case Root          => None
    case p@ParentIn(_) =>
      val (c, p1) = p.canonicalize1
      if (c) p1.peel else None
    case DirIn(p, d)  => Some(p.unsafeCoerceType -> Left(d))
    case FileIn(p, f) => Some(p.unsafeCoerceType -> Right(f))
  }

  final def depth: Int = this match {
    case Current      => 0
    case Root         => 0
    case ParentIn(p)  => p.depth - 1
    case FileIn(p, _) => p.depth + 1
    case DirIn(p, _)  => p.depth + 1
  }

  final def parentDir: Option[Path[B, Dir]] =
    peel.map(_._1)

  final def fileParent(implicit ev: IsFile[T]): Path[B, Dir] = this match {
    case FileIn(p, _) => p.unsafeCoerceType
    case _ => sys.error("impossible!")
  }

  private[pathy] final def unsafeCoerceType[TT]: Path[B, TT] = this match {
    case Current      => Current
    case Root         => Root
    case ParentIn(p)  => ParentIn(p.unsafeCoerceType)
    case DirIn(p, d)  => DirIn(p.unsafeCoerceType, d)
    case FileIn(p, f) => FileIn(p.unsafeCoerceType, f)
  }

  final def renameFile(f: FileName => FileName)(implicit ev: IsFile[T]): Path[B, File] =
    ev.subst(this) match {
      case FileIn(p, f0) => FileIn(p, f(f0))
      case p => p
    }

  final def renameDir(f: DirName => DirName)(implicit ev: IsDir[T]): Path[B, Dir] =
    ev.subst(this) match {
      case DirIn(p, d) => DirIn(p, f(d))
      case p => p
    }

  final def parentDir1: Path[B, Dir] =
    ParentIn(this.unsafeCoerceType)

  final def canonicalize: Path[B, T] =
    this.canonicalize1._2

  private[pathy] final def canonicalize1: (Boolean, Path[B, T]) =
    this match {
      case Current                => false -> Current
      case Root                   => false -> Root
      case ParentIn(FileIn(p, f)) => true -> p.canonicalize1._2
      case ParentIn(DirIn(p, f))  => true -> p.canonicalize1._2
      case ParentIn(p)            =>
        val (ch, p1) = p.canonicalize1
        val p2 = ParentIn(p1)
        if (ch) p2.canonicalize1 else ch -> p2  // ???
      case FileIn(p, f)           =>
        val (ch, p1) = p.canonicalize1
        ch -> FileIn(p1, f)
      case DirIn(p, d)            =>
        val (ch, p1) = p.canonicalize1
        ch -> DirIn(p1, d)
    }

  final def flatten[X]
  (root: => X, currentDir: => X, parentDir: => X,
   dirName: String => X, fileName: String => X): NonEmptyList[X] =
  {
    @tailrec def go(xs: NonEmptyList[X], at: AnyPath): NonEmptyList[X] = {
      val tl = xs.head :: xs.tail

      at match {
        case Current      => NonEmptyList(currentDir, tl)
        case Root         => NonEmptyList(root, tl)
        case ParentIn(p)  => go(NonEmptyList(parentDir, tl), p)
        case DirIn(p, d)  => go(NonEmptyList(dirName(d.value), tl), p)
        case FileIn(p, f) => go(NonEmptyList(fileName(f.value), tl), p)
      }
    }

    this match {
      case Current      => NonEmptyList(currentDir, List.empty)
      case Root         => NonEmptyList(root, List.empty)
      case ParentIn(p)  => go(NonEmptyList(parentDir, List.empty), p)
      case DirIn(p, d)  => go(NonEmptyList(dirName(d.value), List.empty), p)
      case FileIn(p, f) => go(NonEmptyList(fileName(f.value), List.empty), p)
    }
  }

  final def toNioPath: NioPath =
    FileSystems.getDefault.getPath(PathCodec.posix.printPath(this))
}
object Path {
  // Note: this ADT allows invalid paths, but the exposed functions
  // of the package do not.
  private[pathy] case object Current extends Path[Nothing, Nothing] {
    def isAbsolute: Boolean = false
  }
  private[pathy] case object Root extends Path[Nothing, Nothing] {
    def isAbsolute: Boolean = true
  }
  private[pathy] final case class ParentIn[B, T](parent: Path[B, T]) extends Path[B, T] {
    def isAbsolute: Boolean = parent.isAbsolute
  }
  private[pathy] final case class DirIn[B, T](parent: Path[B, T], name: DirName) extends Path[B, T] {
    def isAbsolute: Boolean = parent.isAbsolute
  }
  private[pathy] final case class FileIn[B, T](parent: Path[B, T], name: FileName) extends Path[B, T] {
    def isAbsolute: Boolean = parent.isAbsolute
  }

  def currentDir: RelDir = Current
  def rootDir:    AbsDir = Root

  def file(name: String): RelFile = file1(FileName(name))
  def file1(name: FileName): RelFile = FileIn(Current, name)

  def dir(name: String): RelDir = dir1(DirName(name))
  def dir1(name: DirName): RelDir = DirIn(Current, name)

  implicit final def showInterpolator(sc: StringContext): Show.ShowInterpolator =
    Show.ShowInterpolator(sc)

  implicit def pathShow[B, T]: Show[Path[B, T]] = {
    case Current                => "currentDir"
    case Root                   => "rootDir"
    case ParentIn(p)            => show"parentDir($p)"
    case FileIn(p, FileName(f)) => show"$p </> file($f)"
    case DirIn(p, DirName(d))   => show"$p </> dir($d)"
  }

  implicit def pathOrder[B, T]: Order[Path[B, T]] =
    Order.by(p =>
      p.flatten[(Option[Int], Option[String Either String])](
        root       =      (Some(0),          None),
        parentDir  =      (Some(1),          None),
        currentDir =      (Some(2),          None),
        dirName    = s => (None   , Some( Left(s))),
        fileName   = s => (None   , Some(Right(s)))))
}

final case class PathCodec(separator: Char, escape: String => String, unescape: String => String) {
  import Path._

  def unsafePrintPath(path: AnyPath): String = {
    val s = path.flatten("", ".", "..", escape, escape)
      .toList.mkString(separator.toString)
    path.maybeDir.fold(s + separator)(_ => s)
  }

  def printPath(path: AnyPath): String =
    unsafePrintPath(path)

  def parsePath[Z](rf: RelFile => Z, af: AbsFile => Z, rd: RelDir => Z, ad: AbsDir => Z)(str: String): Z = {
    val segs = str.split(separator)
    val isAbs = str.startsWith(separator.toString)
    val isDir = List(separator.toString, s"$separator.", s"$separator..").exists(str.endsWith) ||
      str == "." || str == ".."

    def folder[B](base: Path[B, Dir], segments: String): Path[B, Dir] = segments match {
      case ""    => base
      case "."   => base
      case ".."  => ParentIn(base)
      case seg   => base </> dir(unescape(seg))
    }

    if (str == "")
      rd(Current)
    else if (isAbs && !isDir)
      af(segs.init.foldLeft[AbsDir](rootDir)(folder) </> file(unescape(segs.last)))
    else if (isAbs && isDir)
      ad(segs.foldLeft[AbsDir](rootDir)(folder))
    else if (!isAbs && !isDir)
      rf(segs.init.foldLeft[RelDir](Current)(folder) </> file(unescape(segs.last)))
    else
      rd(segs.foldLeft[RelDir](Current)(folder))
  }

  val parseRelFile: String => Option[RelFile] =
    parsePath[Option[RelFile]](Some(_), _ => None, _ => None, _ => None)

  val parseAbsFile: String => Option[AbsFile] =
    parsePath[Option[AbsFile]](_ => None, Some(_), _ => None, _ => None)

  val parseRelDir: String => Option[RelDir] =
    parsePath[Option[RelDir]](_ => None, _ => None, Some(_), _ => None)

  val parseAbsDir: String => Option[AbsDir] =
    parsePath[Option[AbsDir]](_ => None, _ => None, _ => None, Some(_))

  def parseDir(s: String): Option[Path[_, Dir]] =
    parsePath[Option[Path[_, Dir]]](_ => None, _ => None, Some(_), Some(_))(s)
  def parseFile(s: String): Option[Path[_, File]] =
    parsePath[Option[Path[_, File]]](Some(_), Some(_), _ => None, _ => None)(s)

  private def asDir[B](path: Path[B, File]): Path[B, Dir] = path match {
    case FileIn(p, FileName(n)) => DirIn(p.unsafeCoerceType, DirName(n))
    case _ => sys.error("impossible!")
  }

  val parseRelAsDir: String => Option[RelDir] =
    parsePath[Option[RelDir]](p => Some(asDir(p)), _ => None, Some(_), _ => None)

  val parseAbsAsDir: String => Option[AbsDir] =
    parsePath[Option[AbsDir]](_ => None, p => Some(asDir(p)), _ => None, Some(_))

}

object PathCodec {
  lazy val posix: PathCodec = PathCodec placeholder '/'
  lazy val windows: PathCodec = PathCodec placeholder '\\'

  /**
    * The placeholder codec, replaces literal instances of the separator
    * in segments with a placeholder as well as segments equal to either of the
    * relative dir literals, "." and "..".
    */
  def placeholder(sep: Char): PathCodec = {
    val escapeSep = (_: String).replaceAllLiterally(sep.toString, $sep$)
    val unescapeSep = (_: String).replaceAllLiterally($sep$, sep.toString)

    PathCodec(sep, escapeRel compose escapeSep, unescapeSep compose unescapeRel)
  }

  private val escapeRel = (s: String) =>
    if (s == "..") $dotdot$ else if (s == ".") $dot$ else s

  private val unescapeRel = (s: String) =>
    if (s == $dotdot$) ".." else if (s == $dot$) "." else s

  private val $sep$ = "$sep$"
  private val $dot$ = "$dot$"
  private val $dotdot$ = "$dotdot$"
}
