package org.sopts

object utils{
  def time[T](methodName: String, body: => T) = {
    println(">>> Running: " + methodName)
    val start = System.currentTimeMillis
    try{
      val resp = body
      resp
    }catch {
      case oe: OptionsException => throw oe
    }finally {
      println(">>> Execution (" + methodName + ") took: " + (System.currentTimeMillis - start) + " ms")
    }
  }
}

