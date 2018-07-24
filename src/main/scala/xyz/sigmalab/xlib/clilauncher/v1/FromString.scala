package xyz.sigmalab.xlib.clilauncher.v1

import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{ Either, Left, Right }

object FromString {

    def from[T: ClassTag](fn: String => T): FromString[T] = new FromString[T] {
        val classT = implicitly[ClassTag[T]].runtimeClass
        override def name: String = s"String => ${classT.getName}"
        override def apply (v : String) : Either[Error, T] = {
            try { Right(fn(v)) } catch {
                case NonFatal(cause) =>
                    Left(Error(s"Can't Convert ${v}' to ${classT.getName}", cause))
            }
        }
        override def toString = s"FromString(${name})"
    }


    implicit val string = from { i => i }
    implicit val long = from { _.toLong }
    implicit val int = from { _.toInt }
    implicit val bool = from { _.toLowerCase match {
        case "false" | "no" => false
        case "true" | "yes" => true
    }}
}

trait FromString[T] extends (String => Either[Error, T]) {
    def name: String
}


