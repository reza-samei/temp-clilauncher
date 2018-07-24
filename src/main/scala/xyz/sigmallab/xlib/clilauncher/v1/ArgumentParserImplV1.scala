package xyz.sigmallab.xlib.clilauncher.v1

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  *
  * https://en.wikipedia.org/wiki/Command-line_interface#Option_conventions_in_Unix-like_systems
  * https://stackoverflow.com/questions/9725675/is-there-a-standard-format-for-command-line-shell-help-text
  * https://stackoverflow.com/questions/10866443/what-is-the-posix-defined-format-for-command-line-error-messages-which-standa
  * http://pubs.opengroup.org/onlinepubs/009695399/functions/xsh_chap02_03.html
  * http://pubs.opengroup.org/onlinepubs/9699919799/
  * https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html
  *
  * Space between key and values is mandatory
  * Invalids: --name=family -n100
  * Valid   : --name family -n 100
  * So in -abc or -n1 all are keys: -a -b -c -n -1
  *
  * @param args
  */
class ArgumentParserImplV1(args: Seq[String]) extends ArgumentParser {

    protected sealed trait Arg
    protected case class AloneValue(vs: List[String]) extends Arg
    protected case class KeyValue(key: String, vs: List[String]) extends Arg
    protected case class Extra(vs: List[String]) extends Arg

    val normalized = args.flatMap{
        case i if i == "--" => "--" :: Nil
        case i if i.startsWith("--") => i :: Nil
        case i if i.startsWith("-") => i.toList.drop(1).map{ i => s"-${i}"}
        case i => i :: Nil
    }

    def normalizeKey(i: String) = i match {
        case i if i.startsWith("--") => i.drop(2)
        case i if i.startsWith("-") => i.drop(1)
    }

    val proc = normalized.foldLeft(Nil: List[Arg]){

        case (Nil, i) if i == "--" => Extra(Nil) :: Nil
        case (Nil, i) if i.startsWith("-") => KeyValue(normalizeKey(i), Nil) :: Nil
        case (Nil, i) => AloneValue(i :: Nil) :: Nil

        case (Extra(vs) :: tail, i) => Extra(i :: vs) :: tail

        case (st, i) if i == "--" => Extra(Nil) :: st
        case (st, i) if i.startsWith("-") => KeyValue(normalizeKey(i), Nil) :: st

        case (KeyValue(k,vs) :: tail, i) => KeyValue(k, i :: vs) :: tail
        case (AloneValue(vs) :: tail, i) => AloneValue(i :: vs) :: tail
    }.reverse

    // before any key
    val alones =
        proc.filter{_.isInstanceOf[AloneValue]}
            .flatMap{_.asInstanceOf[AloneValue].vs.reverse}


    // after `--`
    val extras =
        proc.filter{_.isInstanceOf[Extra]}
            .flatMap{_.asInstanceOf[Extra].vs.reverse}

    // key and values
    val options =
        proc.filter{_.isInstanceOf[KeyValue]}
        .map{_.asInstanceOf[KeyValue]}
        .map{i => i.key -> i.vs.reverse }
        .toMap

    import ArgumentParserResult._

    override def extract (key : String) : ArgumentParserResult = {
        options.get(key) match {
            case None => NotFoundKey
            case Some(Nil) => JustKey
            case Some(list) => Values(list)
        }
    }

    override def first : Option[String] = alones.headOption
}
