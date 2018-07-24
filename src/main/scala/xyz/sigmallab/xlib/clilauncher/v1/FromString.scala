package xyz.sigmallab.xlib.clilauncher.v1

import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{ Either, Left, Right }

object FromString {

    class Error private[clilauncher](
        desc: String,
        cause: Throwable
    ) extends RuntimeException(desc, cause)

    def error(desc: String, cause: Throwable = null) =
        new Error(desc, cause)

    def from[T: ClassTag](fn: String => T): FromString[T] = new FromString[T] {
        override def apply (v : String) : Either[Error, T] = {
            try { Right(fn(v)) } catch {
                case NonFatal(cause) =>
                    val msg =
                        s"Can't Convert ${v}' to ${implicitly[ClassTag[T]].runtimeClass.getName}"
                    Left(error(msg, cause))
            }
        }
    }


    implicit val string = from { i => i }
    implicit val long = from { _.toLong }
    implicit val int = from { _.toInt }
}

trait FromString[T] extends (String => Either[FromString.Error, T])


