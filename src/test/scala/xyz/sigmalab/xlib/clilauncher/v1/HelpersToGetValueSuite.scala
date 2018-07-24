package xyz.sigmalab.xlib.clilauncher.v1

import org.scalatest._
import scala.util.{Either, Left, Right}
import FromString._

class HelpersToGetValueSuite
    extends FlatSpec
        with MustMatchers
        with CliOptionHelpers {

    def logE[T](e: Either[Error, T]) = e match {
        case Right(v) => info(s"Value: ${v}")
        case Left(err) => info(s"Error: ${err.getMessage}")
    }


    /*it must "[Flag] return false for unset flag" in {
        val flag = CliOption.Flag("v", "Log details of progress in stdout!")
        val parser = new ArgumentParserImplV1("salam" :: "-f" :: "-n" :: Nil)
        HelpersToGetValue.getFlag(flag, parser) mustEqual Right(false)
    }

    it must "[Flag]  return true for setted flag" in {
        val flag = CliOption.Flag("v", "Log details of progress in stdout!")
        val parser = new ArgumentParserImplV1("salam" :: "-vn" :: "data" :: Nil)
        HelpersToGetValue.getFlag(flag, parser) mustEqual Right(true)
    }

    it must "[Flag] return error for flag with data (like option)" in {
        // in the case i should consider that data/value as alonevalues!
        // So I'm going to remove flag cli-option
    }*/

    it must "[required] return error for unset value" in {
        // val opt = CliOption.Required("debug", "Log details of progress in stdout!", FromString.bool)
        val opt = required[Boolean]("debug", "Log details of progress in stdout!")
        val parser = new ArgumentParserImplV1("salam" :: "-f" :: "-n" :: Nil)
        val rsl = HelpersToGetValue.getRequired(opt, parser)
        logE(rsl)
        rsl mustBe a[Left[Error,_]]
        val err = rsl.left.get
        err.getMessage.contains("Missing option") mustBe true
    }

    it must "[required] return error for justkey state" in {
        val opt = required[Boolean]("debug", "Log details of progress in stdout!")
        val parser = new ArgumentParserImplV1("salam" :: "--debug" :: Nil)
        val rsl = HelpersToGetValue.getRequired(opt, parser)
        logE(rsl)
        rsl mustBe a[Left[Error,_]]
        val err = rsl.left.get
        err.getMessage.contains("Missed value") mustBe true
    }

    it must "[required] return error for error-in-convert" in {
        val opt = required[Boolean]("debug", "Log details of progress in stdout!")
        val parser = new ArgumentParserImplV1("salam" :: "--debug" :: "xbad" :: Nil)
        val rsl = HelpersToGetValue.getRequired(opt, parser)
        logE(rsl)
        rsl mustBe a[Left[Error,_]]
        val err = rsl.left.get
        err.getMessage.contains("Convert") mustBe true
    }

    it must "[required] return value" in {
        val opt = required[Boolean]("debug", "Log details of progress in stdout!")
        val parser = new ArgumentParserImplV1("salam" :: "--debug" :: "yes" :: Nil)
        val rsl = HelpersToGetValue.getRequired(opt, parser)
        logE(rsl)
        rsl mustEqual Right(true)
    }
}
