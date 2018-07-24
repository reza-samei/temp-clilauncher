package xyz.sigmallab.xlib.clilauncher.v1

class Error (
    desc: String,
    cause: Throwable
) extends RuntimeException(desc, cause)

object Error {

    def apply(desc: String, cause: Throwable = null) =
        new Error(desc, cause)
}

