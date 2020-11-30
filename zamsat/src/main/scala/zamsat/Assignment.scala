package zamsat

object Assignment {
  final val UNASSIGNED = 0
  final val TRUE = 1
  final val FALSE = 2

  def satAssignment(literal: Int): Int = if (literal > 0) Assignment.TRUE else Assignment.FALSE
}
