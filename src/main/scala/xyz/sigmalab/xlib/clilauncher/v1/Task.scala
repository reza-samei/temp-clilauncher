package xyz.sigmalab.xlib.clilauncher.v1

import scala.concurrent.Future

trait Task extends AnyRef with ExitCodeHelpers with  CliOptionHelpers {
    def name: String
    def options: Seq[CliOption]
    def run(args: Arguments) : Future[ExitCode]
}


class Example extends Task {

    val fistName: CliOption.Required[String] = required[String]("name", "User first-name")
    val sureName: CliOption.Optional[String] = optional[String]("family", "User family-name")
    val age: CliOption.Optional[Int] = optional[Int]("age", "User age")

    override def name = "callme"

    override def options  =
        fistName :: sureName :: age :: Nil

    override def run(args: Arguments): Future[ExitCode] = {

        val (name, family, ag) = (
            fistName(args),
            sureName(args),
            age(args)
        )

        println(s"FirstName: ${name}, Family: ${family}, Age: ${age}")

        asyncSuccessful()
    }

}