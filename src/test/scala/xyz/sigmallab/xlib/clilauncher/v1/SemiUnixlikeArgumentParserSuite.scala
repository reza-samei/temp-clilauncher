package xyz.sigmallab.xlib.clilauncher.v1

import org.scalatest._

class SemiUnixlikeArgumentParserSuite extends FlatSpec  with MustMatchers {

    def debug(p: SemiUnixlikeArgumentParser) = {
        info(s"Normalized: ${p.normalized}")
        info(s"Proc: ${p.proc}")
        info(s"Alones: ${p.alones}")
        info(s"Extra: ${p.extras}")
        info(s"Options: ${p.options}")
    }

    it must "parse `salam` as a single alone-value" in {
        val args = "salam" :: Nil
        val parser = new SemiUnixlikeArgumentParser(args)
        debug(parser)
        parser.alones mustEqual "salam" :: Nil
    }

    it must "parse `salam -fn` as `salam -f -n`" in {
        val args = "salam" :: "-fn" :: Nil
        val parser = new SemiUnixlikeArgumentParser(args)
        debug(parser)
        parser.alones mustEqual "salam" :: Nil
        parser.options mustEqual Map("f" -> Nil, "n" -> Nil)
    }

    it must "parse `salam -fn 1000` as `salam -f -n 1000`" in {
        val args = "salam" :: "-fn" :: "1000" :: Nil
        val parser = new SemiUnixlikeArgumentParser(args)
        debug(parser)
        parser.alones mustEqual "salam" :: Nil
        parser.options mustEqual Map(
            "f" -> Nil,
            "n" -> { "1000" :: Nil}
        )
    }

    it must "parse `salam -fn 1000 2000 3000` as `salam -f -n 1000 2000 3000`" in {
        val args = "salam" :: "-fn" :: "1000"  :: "2000" :: "3000" :: Nil
        val parser = new SemiUnixlikeArgumentParser(args)
        debug(parser)
        parser.alones mustEqual "salam" :: Nil
        parser.options mustEqual Map(
            "f" -> Nil,
            "n" -> { "1000"  :: "2000" :: "3000" :: Nil }
        )
    }

    it must "parse `salam be to -fn 1000` as `salam be to -f -n 1000`" in {
        val args = "salam" :: "be" :: "to" :: "-fn" :: "1000"  :: Nil
        val parser = new SemiUnixlikeArgumentParser(args)
        debug(parser)
        parser.alones mustEqual "salam" :: "be" :: "to" :: Nil
        parser.options mustEqual Map(
            "f" -> Nil,
            "n" -> { "1000" :: Nil }
        )
    }

    it must "parse `salam be to -fn 1000 -- delbar -f -k --name bama -- dobare`; extra => `delbar -f -k --name bama -- dobare`" in {
        val args =
            "salam" :: "be" :: "to" :: "-fn" :: "1000" :: "--" ::
            "delbar" :: "-f" :: "-k" :: "--name" :: "bama" :: "--" :: "dobare" :: Nil
        val parser = new SemiUnixlikeArgumentParser(args)
        debug(parser)
        parser.extras mustEqual { "delbar" :: "-f" :: "-k" :: "--name" :: "bama" :: "--" :: "dobare" :: Nil }
        parser.alones mustEqual "salam" :: "be" :: "to" :: Nil
        parser.options mustEqual Map(
            "f" -> Nil,
            "n" -> { "1000" :: Nil }
        )
    }

}
