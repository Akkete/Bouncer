package akkete

import scala.math.abs

abstract class Ball {
  val x: Int; val y: Int; val dx: Int; val dy: Int
  def update(model: Model, input: Direction = NoDirection): Ball
}

case class Player(x: Int, y: Int, dx: Int, dy: Int) extends Ball {
  def update(model: Model, input: Direction): Player = {
    val floor = model.floor
    val landingEffect = 
        model.floor.getOrElse((x, y), Fall).landingEffect(this)
    val ndx = dx + input.dx + landingEffect.dx
    val ndy = dy + input.dy + landingEffect.dy
    val nx  = x + ndx
    val ny  = y + ndy
    Player(nx, ny, ndx, ndy)
  }
}

case class CannonBall(x: Int, y: Int, dx: Int, dy: Int) extends Ball {
  def update(model: Model, input: Direction): CannonBall = {
    val floor = model.floor
    val landingEffect = 
        model.floor.getOrElse((x, y), Fall).landingEffect(this)
    val ndx = dx + landingEffect.dx
    val ndy = dy + landingEffect.dy
    val nx  = x + ndx
    val ny  = y + ndy
    CannonBall(nx, ny, ndx, ndy)
  }
}

case class Chaser(x: Int, y: Int, dx: Int, dy: Int) extends Ball {
  def update(model: Model, input: Direction): Chaser = {
    val floor = model.floor
    val landingEffect = 
      model.floor.getOrElse((x, y), Fall).landingEffect(this)
    val direction: Direction = (model.balls(0) map { player =>
      val xdiff = player.x - (x + dx)
      val ydiff = player.y - (y + dy)
      if abs((xdiff)) >= abs((ydiff)) then
          if xdiff > 0 then Right else Left
      else
          if ydiff > 0 then Down else Up
      }).getOrElse(NoDirection)
    val ndx = dx + direction.dx + landingEffect.dx
    val ndy = dy + direction.dy + landingEffect.dy
    val nx  = x + ndx
    val ny  = y + ndy
    Chaser(nx, ny, ndx, ndy)
  }
}

case class CarefulChaser(x: Int, y: Int, dx: Int, dy: Int) extends Ball {
  def update(model: Model, input: Direction): CarefulChaser = {
    val floor = model.floor
    val landingEffect = 
      model.floor.getOrElse((x, y), Fall).landingEffect(this)
    val defaultdx = dx + landingEffect.dx
    val defaultdy = dy + landingEffect.dy
    val defaultx  = x + defaultdx
    val defaulty  = y + defaultdy
    val options = Direction.allDirections map {direction =>
      (direction, (defaultx + direction.dx, defaulty + direction.dy))  
      }
    val chosenDirection = options filterNot {(direction, point) => 
      val (x, y) = point
      model.floor.getOrElse((x, y), Fall).landingEffect(this).deadly
      } maxByOption {(direction, point) =>
      val (x, y) = point
      val (playerx, playery) = model.balls(0) map {player => 
          (player.x, player.y)
          } getOrElse (defaultx, defaulty)
      val distance = abs(x - playerx) + abs(y - playery)
      -distance
      } map {_._1} getOrElse NoDirection
    val ndx = dx + chosenDirection.dx + landingEffect.dx
    val ndy = dy + chosenDirection.dy + landingEffect.dy
    val nx  = x + ndx
    val ny  = y + ndy
    CarefulChaser(nx, ny, ndx, ndy)
  }
}