package skulks.error

sealed class SkulksError(msg: String) extends Throwable
object SkulksErrorSegNoRoom extends SkulksError("data too big for Segment")
