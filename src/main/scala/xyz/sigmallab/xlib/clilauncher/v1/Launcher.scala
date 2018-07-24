package xyz.sigmallab.xlib.clilauncher.v1

import scalaz.Validation
import xyz.sigmallab.xlib.clilauncher.v1.ArgumentParserResult

class Launcher {

    type ArgError = RuntimeException

    type Type[T] = Validation[Seq[ArgError], T]

    import ArgumentParserResult._

    def checkProblems(
        opt: CliOption,
        args: ArgumentParser
    ): Seq[ArgError] = opt match {
        case flag:CliOption.Flag => checkFlag(flag, args)
        case required: CliOption.Required[_] => ???
        case optional: CliOption.Optional[_] => ???
        case repeated: CliOption.Repeated[_] => ???

    }

    def required(opt: CliOption.Required[_], args: ArgumentParser): Seq[ArgError] = {
        // flag.key
        val keys : List[String] = ???
        val temp = keys.map{ i => args.extract(i) }.flatMap(removeNotFounds)
        require(temp.size == 1)
        require(temp.head.isInstanceOf[Values]) // Not JustKey

        val value = temp.head.asInstanceOf[Values]
        require(value.list.size == 1)

        require(opt.parser(value.list.head).isRight)

        Nil
    }

    def checkFlag(flag: CliOption.Flag, args: ArgumentParser): Seq[ArgError] = {
        // flag.key
        val keys : List[String] = ???

        val rsl = keys.map{ i => args.extract(i) }.flatMap(removeNotFounds)

        if (rsl == List(JustKey)) Nil else ???
    }

    def removeNotFounds(v: ArgumentParserResult) = v match {
        case NotFoundKey => None
        case JustKey => Some(JustKey)
        case v: Values => Some(v)
    }

}
