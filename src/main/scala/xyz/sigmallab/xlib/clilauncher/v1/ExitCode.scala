package xyz.sigmallab.xlib.clilauncher.v1

import scala.concurrent.Future

trait ExitCode
object ExitCode extends ExitCodeHelpers {

    case object Successful extends ExitCode
    case class Failed(code: Int, desc: String) extends ExitCode

}

trait ExitCodeHelpers {

    import ExitCode._

    def successful() : ExitCode = Successful

    def asyncSuccessful(): Future[ExitCode] =
        Future.successful(successful())

    def error(code : Int = 1, desc: String = "Error!"): ExitCode =
        new Failed(code, desc)

    def asyncError(code: Int = 1, desc: String = "Error!"): Future[ExitCode] =
        Future.successful(error(code, desc))

    def configurationError(desc: String = "Configuration Error"): ExitCode =
        error(76, desc)
}
