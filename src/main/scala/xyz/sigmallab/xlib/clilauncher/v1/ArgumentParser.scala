package xyz.sigmallab.xlib.clilauncher.v1

object ArgumentParser {

}

trait ArgumentParser {
    def extract(key: String): ArgumentParserResult
    def first: Option[String]
}
