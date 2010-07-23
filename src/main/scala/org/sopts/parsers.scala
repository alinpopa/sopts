package org.sopts

import scala.annotation.tailrec
import utils._

sealed trait Parser {def parse(opts: List[String]): Unit}
sealed trait PerfParser extends Parser {
  abstract override def parse(opts: List[String]): Unit = {
    time("parse",super.parse(opts))
  }
}

object OptionsParser {
  
  def apply(optActions: Map[Opt, OptAction])(opts: List[String]): Unit = {
    (new OptionsParser(optActions)).parse(opts)
  }

  class OptionsParser(optActions: Map[Opt, OptAction]) extends Parser {
    def parse(opts: List[String]) = {
      val parsedOptions = parseOptions(opts, List[(Opt,Option[String],OptAction)]())
      for(opt <- optActions) {
        val (o,a) = opt
        if(isMandatory(a) && !contains(parsedOptions,o)) throw new MandatoryOptionException(o.get)
      }
      execActions(parsedOptions.reverse)
    }

    @tailrec
    private def execActions(list: List[(Opt,Option[String],OptAction)]): Unit = list match {
      case (o:Opt,Some(v),a:ValueAction) :: tail => {a.exec(v); execActions(tail)}
      case (o:Opt,None,a:NoValueAction) :: tail => {a.exec; execActions(tail)}
      case Nil => ()
    }

    @tailrec
    private def parseOptions(list: List[String],resActs: List[(Opt,Option[String],OptAction)]): List[(Opt,Option[String],OptAction)] = list match {
      case opt :: value :: tail if(isValidOpt(Opt(opt)) && needValue(Opt(opt))) => {
        if(!isValidOpt(Opt(value))) parseOptions(tail, (Opt(opt),Some(value),get(Opt(opt))) :: resActs)
        else throw new MandatoryValueException("No value specified for option " + opt)
      }
      case opt :: value :: tail if(isValidOpt(Opt(opt))) => parseOptions(value :: tail, (Opt(opt),None,get(Opt(opt))) :: resActs)
      case opt :: value :: tail => parseOptions(value :: tail, resActs)
      case opt :: tail if(isValidOpt(Opt(opt)) && needValue(Opt(opt))) => throw new MandatoryValueException("No value specified for option " + opt)
      case opt :: tail if(isValidOpt(Opt(opt))) => parseOptions(tail, (Opt(opt),None,get(Opt(opt))) :: resActs)
      case opt :: tail => parseOptions(tail, resActs)
      case Nil => resActs
    }

    private def isMandatory(action: OptAction): Boolean = action match {
      case (_:MandatoryAction) => true
      case _ => false
    }

    private def isValidOpt(opt: Opt): Boolean = optActions.contains(opt)
    private def get(opt: Opt): OptAction = optActions(opt)
    private def needValue(opt: Opt): Boolean = get(opt).isInstanceOf[ValueAction]

    @tailrec
    private def contains(parsedOptions: List[(Opt,Option[String],OptAction)], opt: Opt): Boolean = parsedOptions match {
      case (o:Opt,_:Option[_],_:OptAction) :: tail if(o == opt) => true
      case _ :: tail => contains(tail,opt)
      case _ => false
    }
  }
}

// vim: set ts=2 sw=2 et:
