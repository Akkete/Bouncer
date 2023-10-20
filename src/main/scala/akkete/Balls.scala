package akkete


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
            if scala.math.abs((xdiff)) >= scala.math.abs((ydiff)) then
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