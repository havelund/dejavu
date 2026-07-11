package dejavu

import scala.util.parsing.combinator._
import java.io._


class Parser extends JavaTokenParsers with PackratParsers {

  protected override val whiteSpace = """(\s|/\*(.|\n|\r)*?\*/|//.*\n)+""".r

  lazy val reserved: PackratParser[String] =
    "prop\\b".r |
      "where\\b".r |
      "pred\\b".r |
      "preds\\b".r |
      "event\\b".r |
      "events\\b".r |
      "H\\b".r |
      "P\\b".r |
      "S\\b".r |
      "Forall\\b".r |
      "Exists\\b".r |
      "forall\\b".r |
      "exists\\b".r |
      "true\\b".r |
      "false\\b".r

  lazy val le = "<=" ^^ (_ => LEOP)

  lazy val lt = "<" ^^ (_ => LTOP)

  lazy val ge = ">=" ^^ (_ => GEOP)

  lazy val gt = ">" ^^ (_ => GTOP)

  lazy val eq = "=" ^^ (_ => EQOP)

  lazy val oper = le | lt | ge | gt | eq

  val name: Parser[String] = not(reserved) ~> ident

  lazy val document: PackratParser[Document] = rep(definition) ^^ {
    case definitions => Document(definitions.flatten)
  }

  def definition: Parser[List[Definition]] =
    macrodef ^^ { case m => List(m) } |
      propertydef ^^ { case p => List(p) } |
      eventdef

  lazy val macrodef: PackratParser[Macro] =
    "pred" ~ name ~ opt("(" ~> repsep(name, ",") <~ ")") ~ "=" ~ ltl ^^ {
      case _ ~ name ~ optargs ~ _ ~ ltl =>
        val argList: List[String] = optargs match {
          case None => Nil
          case Some(args) => args
        }
        Macro(name, argList, ltl)
    }

  lazy val propertydef: PackratParser[Property] =
    "prop" ~ name ~ ":" ~ ltl ~ opt("where" ~> repsep(relationdef, ",")) ^^ {
      case _ ~ id ~ _ ~ ltl ~ optreldefs =>
        val relationList =
          optreldefs match {
            case None => Nil
            case Some(list) => list
          }
        Property(id, ltl, relationList)
    }

  lazy val relationdef: PackratParser[Rule] =
    name ~ opt("(" ~> repsep(name, ",") <~ ")") ~ ":=" ~ ltl ^^ {
      case name ~ optargs ~ _ ~ ltl =>
        val argList: List[String] = optargs match {
          case None => Nil
          case Some(args) => args
        }
        ltl.isRuleBodyTop = true
        Rule(name, argList, ltl)
    }

  def eventdef: Parser[List[Event]] =
    ("preds" | "pred" | "events" | "event") ~ repsep(name ~ opt("(" ~> repsep(name, ",") <~ ")"), ",") ^^ {
      case _ ~ events =>
        for ((id ~ optArgs) <- events) yield {
          optArgs match {
            case None => Event(id, Nil)
            case Some(args) => Event(id, args)
          }
        }
    }

  lazy val ltl: PackratParser[LTL] =
    ltl1 ~ rep(("->" | "<->") ~ ltl1) ^^ {
      case ltl ~ ltls => mkBinary(ltl, ltls)
    }

  lazy val ltl1: PackratParser[LTL] =
    ltl2 ~ rep("|" ~ ltl2) ^^ {
      case ltl ~ ltls => mkBinary(ltl, ltls)
    }

  lazy val ltl2: PackratParser[LTL] =
    ltl3 ~ rep("&" ~ ltl3) ^^ {
      case ltl ~ ltls => mkBinary(ltl, ltls)
    }

  lazy val ltl3: PackratParser[LTL] =
    ltlLeaf ~ "S" ~ ltTime ~ ltlLeaf ^^ {
      case ltl1 ~ _ ~ time ~ ltl2 => ExistsTime(SinceTimeLE(ltl1, time, ltl2))
    } |
      ltlLeaf ~ "S" ~ gtTime ~ ltlLeaf ^^ {
        case ltl1 ~ _ ~ time ~ ltl2 => ExistsTimeGT(SinceTimeGT(ltl1, time, ltl2), time)
      } |
      ltlLeaf ~ "Z" ~ ltTime ~ ltlLeaf ^^ {
        case ltl1 ~ _ ~ time ~ ltl2 => ExistsTime(ZinceTimeLE(ltl1, time, ltl2))
      } |
      ltlLeaf ~ "S" ~ ltlLeaf ^^ {
        case ltl1 ~ _ ~ ltl2 => Since(ltl1, ltl2)
      } |
      ltlLeaf

  lazy val ltTime: PackratParser[Int] =
    "[<=" ~ wholeNumber ~ "]" ^^ {
      case _ ~ n ~ _ => n.toInt
    }

  lazy val gtTime: PackratParser[Int] =
    "[>" ~ wholeNumber ~ "]" ^^ {
      case _ ~ n ~ _ => n.toInt
    }

  lazy val ltlLeaf: PackratParser[LTL] =
    "true" ^^ {
      case _ => True
    } |
      "false" ^^ {
        case _ => False
      } |
      name ~ oper ~ name ^^ {
        case id1 ~ op ~ id2 => Rel(id1, op, id2)
      } |
      name ~ oper ~ const ^^ {
        case id ~ op ~ co => RelConst(id, op, co)
      } |
      name ~ opt("(" ~ repsep(constOrVar, ",") ~ ")") ^^ {
        case id ~ optConstOrVars =>
          val constOrVars = optConstOrVars match {
            case None => Nil
            case Some(_ ~ as ~ _) => as
          }
          Pred(id, constOrVars)
      } |
      "!" ~ ltlLeaf ^^ {
        case _ ~ ltl => Not(ltl)
      } |
      "@" ~ ltlLeaf ^^ {
        case _ ~ ltl => Previous(ltl.setBelowPrevious())
      } |
      "P" ~ ltTime ~ ltlLeaf ^^ {
        case _ ~ time ~ ltl => SometimeLE(ltl, time)
      } |
      "P" ~ gtTime ~ ltlLeaf ^^ {
        case _ ~ time ~ ltl => SometimeGT(ltl, time)
      } |
      "P" ~ ltlLeaf ^^ {
        case _ ~ ltl => Sometime(ltl)
      } |
      "H" ~ ltTime ~ ltlLeaf ^^ {
        case _ ~ time ~ ltl => HistoryLE(ltl, time)
      } |
      "H" ~ gtTime ~ ltlLeaf ^^ {
        case _ ~ time ~ ltl => HistoryGT(ltl, time)
      } |
      "H" ~ ltlLeaf ^^ {
        case _ ~ ltl => History(ltl)
      } |
      "[" ~ ltl ~ "," ~ ltl ~ ")" ^^ {
        case _ ~ ltl1 ~ _ ~ ltl2 ~ _ => Interval(ltl1, ltl2)
       } |
      "Exists" ~ name ~ "." ~ ltl ^^ {
        case _ ~ id ~ _ ~ ltl => Exists(id, ltl)
      } |
      "Forall" ~ name ~ "." ~ ltl ^^ {
        case _ ~ id ~ _ ~ ltl => Forall(id, ltl)
      } |
      "exists" ~ name ~ "." ~ ltl ^^ {
        case _ ~ id ~ _ ~ ltl => ExistsSeen(id, ltl)
      } |
      "forall" ~ name ~ "." ~ ltl ^^ {
        case _ ~ id ~ _ ~ ltl => ForallSeen(id, ltl)
      } |
      "(" ~ ltl ~ ")" ^^ {
        case _ ~ ltl ~ _ => Paren(ltl)
      }

  lazy val const: PackratParser[Any] = stringLiteral | wholeNumber

  lazy val constOrVar: PackratParser[ConstOrVar] =
    name ^^ {
      case id => VPat(id)
    } |
      const ^^ {
        case v => CPat(v)
      }

  // --------------- AUXILIARY -----------------

  def mkBinary(t: LTL, ts: List[String ~ LTL]): LTL =
    ts match {
      case Nil => t
      case (op ~ t_) :: ts_ =>
        val binary =
          op match {
            case "<->" => BiImpl(t, t_)
            case "->" => Implies(t, t_)
            case "|" => Or(t, t_)
            case "&" => And(t, t_)
            case "S" => Since(t, t_)
          }
        mkBinary(binary, ts_)
    }

  // --------------- PARSING -------------------

  // generic:

  def readerFromFile(file: String): java.io.Reader =
    new FileReader(file)

  def readerFromString(str: String): java.io.Reader =
    new StringReader(str)

  // parsing any NT:

  def stopProgram(): Spec = {
    if (Options.UNIT_TEST) {
      (throw new Throwable("WF_ERROR")).asInstanceOf[Spec]
    } else {
      System.exit(1).asInstanceOf[Spec]
    }
  }

  def parseReader(reader: java.io.Reader): Spec =
    parseAll(document, reader) match {
      case Success(doc, _) =>
        if (doc.isWellformed) {
          val specWithMacrosExpanded = doc.expandMacroCalls
          val specFinal = specWithMacrosExpanded.duplicateRules
          if (SymbolTable.errors) stopProgram() else specFinal
        } else {
          println("\n*** The specification document is not well-formed!\n")
          stopProgram()
        }

      case fail@NoSuccess(_, _) =>
        println(fail)
        println("\n*** The specification document contains syntax errors!\n")
        stopProgram()
    }

  def parseFile(file: String): Spec =
    parseReader(readerFromFile(file))

  def parseString(str: String): Spec =
    parseReader(readerFromString(str))
}


