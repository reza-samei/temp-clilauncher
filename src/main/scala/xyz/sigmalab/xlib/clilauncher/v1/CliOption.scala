package xyz.sigmalab.xlib.clilauncher.v1

import scalaz.Validation

sealed trait CliOption {
    // def keys: Seq[String]
    def key: String
    def help: String
}

sealed trait CliOptionWithValue[T] extends CliOption {
    def parser: FromString[T]
}

object CliOption {

    /*final case class Flag(
        key: String,
        help: String
    ) extends CliOption {
        def apply(args: Arguments): Boolean = ???
    }*/

    final case class Required[T](
        key: String,
        help: String,
        parser: FromString[T]
    ) extends CliOptionWithValue[T] {
        def apply(args: Arguments): T = ???
    }

    final case class Optional[T](
        key: String,
        help: String,
        parser: FromString[T]
    ) extends CliOptionWithValue[T] {
        def apply(args: Arguments): Option[T] = ???
    }


    final case class Repeated[T](
        key: String,
        help: String,
        parser: FromString[T]
    ) extends CliOptionWithValue[T] {
        def apply(args: Arguments): Seq[T] = ???
    }

}


trait CliOptionHelpers {

    import CliOption._

    def required[T](
        key: String, help: String
    )(implicit parser: FromString[T]): Required[T] = {
        Required[T](key, help, parser)
    }

    def optional[T](
        key: String, help: String
    )(implicit parser: FromString[T]): Optional[T] = {
        Optional(key, help, parser)
    }

    def repeated[T](
        key: String, help: String
    )(implicit parser: FromString[T]): Repeated[T] = {
        Repeated(key, help, parser)
    }

}