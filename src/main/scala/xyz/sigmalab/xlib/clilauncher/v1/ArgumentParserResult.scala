package xyz.sigmalab.xlib.clilauncher.v1

sealed trait ArgumentParserResult
object ArgumentParserResult {
    case object NotFoundKey extends ArgumentParserResult
    case object JustKey extends ArgumentParserResult
    case class Values(list: List[String]) extends ArgumentParserResult
}
