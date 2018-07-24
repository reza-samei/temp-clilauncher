package xyz.sigmallab.xlib.clilauncher.v1

class ArgumentParserImplV2(args: Seq[String]) extends ArgumentParser {

    protected sealed trait Arg
    protected case class AloneValue(vs: List[String]) extends Arg
    protected case class KeyValue(key: String, vs: Option[String]) extends Arg
    protected case class Extra(vs: List[String]) extends Arg

    val normalized = args.flatMap{
        case i if i == "--" => "--" :: Nil
        case i if i.startsWith("--") => i :: Nil
        case i if i.startsWith("-") => i.toList.drop(1).map{ i => s"-${i}"}
        case i => i :: Nil
    }

    def normalizeKey(i: String) = i match {
        case i if i.startsWith("--") => i.drop(2)
        case i if i.startsWith("-") => i.drop(1)
    }

    val proc = normalized.foldLeft(Nil: List[Arg]){

        case (Nil, i) if i == "--" => Extra(Nil) :: Nil
        case (Nil, i) if i.startsWith("-") => KeyValue(normalizeKey(i), None) :: Nil
        case (Nil, i) => AloneValue(i :: Nil) :: Nil

        case (Extra(vs) :: tail, i) => Extra(i :: vs) :: tail

        case (st, i) if i == "--" => Extra(Nil) :: st
        case (st, i) if i.startsWith("-") => KeyValue(normalizeKey(i), None) :: st

        case (KeyValue(k,None) :: tail, i) => KeyValue(k, Some(i)) :: tail
        case (AloneValue(vs) :: tail, i) => AloneValue(i :: vs) :: tail
        // case (kv @ KeyValue(k,Some(v)) :: tail, i) => Aone:: kv tail
        case (st, i) => AloneValue(i :: Nil) :: st
    }.reverse

    // before any key
    val alones =
        proc.filter{_.isInstanceOf[AloneValue]}
            .flatMap{_.asInstanceOf[AloneValue].vs.reverse}


    // after `--`
    val extras =
        proc.filter{_.isInstanceOf[Extra]}
            .flatMap{_.asInstanceOf[Extra].vs.reverse}

    // key and values
    val options =
        proc.filter{_.isInstanceOf[KeyValue]}
            .map{_.asInstanceOf[KeyValue]}
            .groupBy{_.key}
            .mapValues {_.flatMap{_.vs}} // Missing a JustKey !!!
            .toMap

    import ArgumentParserResult._

    override def extract (key : String) : ArgumentParserResult = {
        println(key, options.get(key))
        options.get(key) match {
            case None => NotFoundKey
            case Some(Nil) => JustKey
            case Some(list) => Values(list)
        }
    }

    override def first : Option[String] = alones.headOption

}
