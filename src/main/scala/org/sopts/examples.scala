package org.sopts

object opts {
  import scala.annotation.tailrec

  case class ID(id: Int)
  case class Service(name: String){
    var items = List[String]()
    def add(item: String) = {
      items = item :: items
      println(" > Add item to service: "+item)
    }
    def delete(item: String):List[String] = {
      val currentItems = items
      println(" < Delete item from service: " + item)
      items = delete(currentItems,item,List[String]())
      items
    }

    @tailrec
    private def delete(items: List[String], item: String, newItems: List[String]): List[String] = items match {
      case headItem :: tail if(headItem == item) => delete(tail,item,newItems)
      case headItem :: tail => delete(tail,item,headItem :: newItems)
      case Nil => newItems.reverse
    }
    def getItems: List[String] = items.reverse
  }

  object Repository {
    val services = List[(ID,Service)]((ID(100),Service("hundred")), (ID(200),Service("two hundred")))

    def get(id: ID):Option[Service] = get(id,services)

    @tailrec
    private def get(id: ID, services: List[(ID,Service)]): Option[Service] = services match {
      case (sid:ID,s:Service) :: tail if(sid == id) => Some(s)
      case (sid:ID,s:Service) :: tail => get(id,tail)
      case Nil => None
    }
  }

  case class AddAction(service: Option[Service]) extends ValueAction {
    def exec(value: String) = {
      service match {
        case Some(s) => {
          s.add(value); println(" STATUS: Values after add: " + s.getItems)
        }
        case None => ()
      }
    }
  }

  case class DeleteAction(service: Option[Service]) extends ValueAction {
    def exec(value: String) = {
      service match {
        case Some(s) => {
          s.delete(value); println(" STATUS: Values after delete: " + s.getItems)
        }
        case None => ()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val service = Repository.get(ID(200))
    val optActions = Map[Opt,OptAction](
      Opt("-a") -> AddAction(service),
      Opt("-d") -> DeleteAction(service)
    )
    OptionsParser(optActions){args.toList}
  }
}

