package org.sopts

sealed class OptionsException(msg: String) extends RuntimeException(msg)
class MandatoryValueException(msg: String) extends OptionsException(msg)
class MandatoryOptionException(msg: String) extends OptionsException(msg)

