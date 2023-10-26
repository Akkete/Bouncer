package akkete

import indigo.*
import indigo.scenes.*
import scala.scalajs.js.annotation.JSExportTopLevel

abstract class Tile {
  def landingEffect(ball: Ball): LandingEffect = LandingEffect(None)
  def crumbleEffect: CrumbleEffect = CrumbleEffect(this)
}

case object Solid extends Tile

case object Fall extends Tile {
  override def landingEffect(ball: Ball): LandingEffect = 
    LandingEffect(None, deadly = true)
}

case class Crackable(cracks: Int) extends Tile {
  override def landingEffect(ball: Ball): LandingEffect = 
    if (cracks == 0) then
      LandingEffect(Some(Crackable(1))) 
    else if (cracks == 1) then
      LandingEffect(Some(Crackable(2)), crumble = Set(Crackable(1))) 
    else 
      LandingEffect(Some(Fall), crumble = Set(Crackable(2)))
  override def crumbleEffect: CrumbleEffect =
    if (cracks == 0) then
      CrumbleEffect(Crackable(0))
    else if (cracks == 1) then
      CrumbleEffect(Crackable(2), crumble = Set(Crackable(1))) 
    else 
      CrumbleEffect(Fall, crumble = Set(Crackable(2)))
}

case object Sand extends Tile {
  override def landingEffect(ball: Ball): LandingEffect =
    LandingEffect(None, 
    -(ball.dx+ball.dx.sign)/2, 
    -(ball.dy+ball.dy.sign)/2
    )
}

case class Booster(direction: Direction, boost: Int) extends Tile {
  override def landingEffect(ball: Ball): LandingEffect =
    LandingEffect(None, direction.dx * boost, direction.dy * boost)
}

case class Goal(goalAreaId: Int) extends Tile {
  override def landingEffect(ball: Ball): LandingEffect =
    LandingEffect(None, goal = Some(goalAreaId))
}