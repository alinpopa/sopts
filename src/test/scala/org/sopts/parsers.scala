package org.sopts

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar

class OptionsParserSpec extends Spec with ShouldMatchers with MockitoSugar {
  describe("OptionsParser"){
    it("should throw exception when mandatory param is not present and no other params are provided"){
      val optActions = Map[Opt,OptAction](
        Opt("-m") -> new ValueAction with MandatoryAction {
          override def exec(value: String) = ()
        }
      )
      evaluating{
        OptionsParser(optActions){List[String]()}
      } should produce [MandatoryOptionException]
    }

    it("should throw exception when mandatory param is not present and other params are provided"){
      val optActions = Map[Opt,OptAction](
        Opt("-m") -> new ValueAction with MandatoryAction {
          override def exec(value: String) = ()
        }
      )
      evaluating{
        OptionsParser(optActions){List[String]("-a")}
      } should produce [MandatoryOptionException]
    }
    
    it("should throw exception when value action is not found and other params are provided"){
      val optActions = Map[Opt,OptAction](
        Opt("-a") -> new ValueAction {def exec(value:String) = ()},
        Opt("-b") -> new NoValueAction {def exec = ()}
      )
      evaluating{
        OptionsParser(optActions){List[String]("-a","-b")}
      } should produce [MandatoryValueException]
    }
    
    it("should throw exception when value action is not found"){
      val optActions = Map[Opt,OptAction](Opt("-a") -> new ValueAction {def exec(value:String) = ()})
      evaluating{
        OptionsParser(optActions){List[String]("-a")}
      } should produce [MandatoryValueException]
    }

    it("should call execute on the provided value action"){
      val valueAction = mock[ValueAction]
      val optActions = Map[Opt,OptAction](Opt("-a") -> valueAction)

      OptionsParser(optActions){List("-a","test value")}

      verify(valueAction).exec("test value")
    }

    it("should call execute on the provided novalue action"){
      val noValueActionA = mock[NoValueAction]
      val optActions = Map(Opt("-a") -> noValueActionA)

      OptionsParser(optActions){List("-a")}

      verify(noValueActionA).exec
    }

    it("should not call execute on value action at all when no parameters are provided"){
      val valueAction = mock[ValueAction]
      val optActions = Map(Opt("-a") -> valueAction)

      OptionsParser(optActions){List[String]()}

      verify(valueAction, never()).exec(any())
    }
    
    it("should not call execute on novalue action at all when no parameters are provided"){
      val noValueAction = mock[NoValueAction]
      val optActions = Map(Opt("-a") -> noValueAction)

      OptionsParser(optActions){List[String]()}

      verify(noValueAction, never()).exec
    }
  }
}
