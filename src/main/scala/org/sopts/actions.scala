package org.sopts

sealed trait OptAction
trait ValueAction extends OptAction {
  def exec(value: String): Unit
}
trait NoValueAction extends OptAction {
  def exec: Unit
}
trait MandatoryAction

