package akkete


abstract class Ball {
  val x: Int; val y: Int; val dx: Int; val dy: Int
}

case class Player(x: Int, y: Int, dx: Int, dy: Int) extends Ball
case class CannonBall(x: Int, y: Int, dx: Int, dy: Int) extends Ball
case class Chaser(x: Int, y: Int, dx: Int, dy: Int) extends Ball