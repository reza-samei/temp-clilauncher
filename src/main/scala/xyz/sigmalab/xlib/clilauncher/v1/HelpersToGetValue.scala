package xyz.sigmalab.xlib.clilauncher.v1

import java.sql.ClientInfoStatus

import scalaz.Validation

import scala.util.{ Either, Left, Right }

trait HelpersToGetValue {

    import ArgumentParserResult._

    def getValue(
        opt: CliOption,
        args: ArgumentParser
    ): Either[Error, _] = opt match {
        // case flag:CliOption.Flag => getFlag(flag, args)
        case required: CliOption.Required[_] => getRequired(required, args)
        case optional: CliOption.Optional[_] => grtOptional(optional, args)
        case repeated: CliOption.Repeated[_] => getRepeated(repeated, args)
    }

    def getRepeated[T](opt: CliOption.Repeated[T], args: ArgumentParser): Either[Error, Seq[T]] = {
        for {
            a <- posibilities(keysOf(opt), args)
            b <- removeNotFounds(a)
            c <- justValues(opt, b)
            d <- forceOne(opt, c)
            e <- readRaws(opt, d)
            f <- parseValues(opt, e)
        } yield f
    }

    def grtOptional[T](opt: CliOption.Optional[T], args: ArgumentParser): Either[Error, Option[T]] = {
        for {
            a <- posibilities(keysOf(opt), args)
            b <- removeNotFounds(a)
            c <- justValues(opt, b)
            d <- forceOne(opt, c)
            e <- readRaws(opt, d)
            f <- parseValues(opt, e)
            g <- optionalValue(opt, f)
        } yield g
    }

    def getRequired[T](opt: CliOption.Required[T], args: ArgumentParser): Either[Error, T] = {
        for {
            a <- posibilities(keysOf(opt), args)
            b <- removeNotFounds(a)
            c <- justValues(opt, b)
            d <- forceOne(opt, c)
            e <- readRaws(opt, d)
            f <- parseValues(opt, e)
            g <- forceOneValue(opt, f)
        } yield g
    }

    /*def getFlag(opt: CliOption.Flag, args: ArgumentParser): Either[Error, Boolean] = {

        for {
            a <- posibilities(keysOf(opt), args)
            aa = { println(a)}
            b <- removeNotFounds(a)
            bb = { println(b)}
            c <- justKeys(opt, b)
            cc = { println(c)}
            d <- headOption(opt, c)
            dd = { println(d)}
        } yield d.isDefined
    }*/

    def optionalValue[T](
        opt: CliOptionWithValue[T],
        vs: Seq[T]
    ): Either[Error, Option[T]] = vs match {
        case xs if xs.size == 1 || xs.size == 0 => Right(xs.headOption)
        case xs => Left(Error(s"Invlaid usage for option: ${opt} / ${vs}"))
    }

    def forceOneValue[T](
        opt: CliOptionWithValue[T],
        vs: Seq[T]
    ): Either[Error, T] = vs match {
        case xs if xs.size == 1 => Right(xs.head)
        case xs => Left(Error(s"Invlaid usage for option: ${opt} / ${vs}"))
    }

    def parseValues[T](
        opt: CliOptionWithValue[T],
        vs : Seq[String]
    ): Either[Error, Seq[T]] = {
        val rsl = vs.map { i => opt.parser(i) }
        rsl.filter{_.isLeft}
            .headOption
            .map { i =>
                val err = i.left.get
                Left(Error(s"Invalid value for option: ${opt} / ${i}", err))
            }.getOrElse { Right(rsl.map(_.right.get)) }
    }

    def readRaws(
        opt: CliOptionWithValue[_],
        v: ArgumentParserResult
    ): Either[Error, Seq[String]] = v match {
        case Values(vs) =>
            Right(vs)
        case _ =>
            Left(Error(s"Invalid option usage: ${opt} / ${v}; this is a flag!"))
    }

    def justValues(
        opt: CliOption,
        vs: Seq[ArgumentParserResult]
    ): Either[Error, Seq[ArgumentParserResult]] = vs.filter(_.isInstanceOf[Values]) match {
        case xs if xs.size != vs.size =>
            Left(Error(s"Invalid option usage: ${opt} / ${vs}; this is a flag!"))
        case xs =>
            Right(xs)
    }

    def justKeys(
        opt: CliOption,
        vs: Seq[ArgumentParserResult]
    ): Either[Error, Seq[ArgumentParserResult]] = vs.filter(_ == JustKey) match {
        case xs if xs.size != vs.size =>
            Left(Error(s"Invalid option usage: ${opt} / ${vs}; this is a flag!"))
        case xs =>
            Right(xs)
    }

    def headOption(
        opt: CliOption,
        vs: Seq[ArgumentParserResult]
    ): Either[Error, Option[ArgumentParserResult]] = {
        Right(vs.headOption)
    }

    def forceOne(
        opt: CliOption,
        vs: Seq[ArgumentParserResult]
    ): Either[Error, ArgumentParserResult] = vs.size match {
        case 1 =>
            Right(vs.head)
        case _ =>
            Left(Error(s"Invalid option usage: ${opt} / ${vs}"))
    }

    def removeNotFounds(
        vs: Seq[ArgumentParserResult]
    ) : Either[Error, Seq[ArgumentParserResult]] = Right {
        vs.flatMap(removeNotFound)
    }

    def removeNotFound(v: ArgumentParserResult) = v match {
        case NotFoundKey => None
        case JustKey => Some(JustKey)
        case v: Values => Some(v)
    }

    def posibilities(
        keys: Seq[String], args: ArgumentParser
    ): Either[Error, Seq[ArgumentParserResult]] = Right {
        keys.map { i => args.extract(i) }
    }

    def format(opt: CliOption): String =
        s"${opt.getClass.getName}(key: ${opt.key}, help: ${opt.help}, type: ???)"

    def keysOf(opt: CliOption) : Seq[String] =
        opt.key :: Nil

}

object HelpersToGetValue extends HelpersToGetValue

