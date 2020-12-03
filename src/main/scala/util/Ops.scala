package util

object Ops {

  implicit class EitherOps[A, B](x: Either[A, B]) {
    def yolo: B = x.getOrElse(throw new RuntimeException("yolo"))
  }

}
