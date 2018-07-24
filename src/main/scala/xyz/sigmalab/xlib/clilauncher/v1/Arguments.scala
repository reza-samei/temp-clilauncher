package xyz.sigmalab.xlib.clilauncher.v1

trait Arguments {
    def values[T](opt: CliOption): Seq[T]
}
