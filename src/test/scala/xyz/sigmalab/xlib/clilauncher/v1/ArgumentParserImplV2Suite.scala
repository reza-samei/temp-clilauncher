package xyz.sigmalab.xlib.clilauncher.v1

import org.scalatest._

import scala.collection.immutable

class ArgumentParserImplV2Suite extends FlatSpec with MustMatchers {

    def debug(p: ArgumentParserImplV2) = {
        info(s"Normalized: ${p.normalized}")
        info(s"Proc: ${p.proc}")
        info(s"Alones: ${p.alones}")
        info(s"Extra: ${p.extras}")
        info(s"Options: ${p.options}")
    }

    it must "pasre" in {

        val args =
            "salam" :: "-fn" :: "happy" :: "dance" :: "-n" :: "topol" :: Nil

        val parser = new ArgumentParserImplV2(args)

        debug(parser)

        parser.alones mustEqual "salam" :: "dance" :: Nil
        parser.extras mustEqual Nil
        parser.options mustEqual Map(
            "f" -> Nil,
            "n" -> { "happy" :: "topol" :: Nil }
        )
    }

}
