package akkete

import indigo.*
import indigo.scenes.*

import scala.scalajs.js.annotation.JSExportTopLevel

abstract class Tile {
  def landingEffect(ball: Ball): LandingEffect = LandingEffect(None)
  def crumbleEffect: CrumbleEffect = CrumbleEffect(this)
}

case class LandingEffect(
  tileChange: Option[Tile] = None, 
  dx: Int = 0, 
  dy: Int = 0, 
  deadly: Boolean = false,
  goal: Option[Int] = None,
  crumble: Set[Tile] = Set.empty,
  meter: Int = 0
  )

case class CrumbleEffect(
  tile: Tile,
  crumble: Set[Tile] = Set.empty,
)

case object Solid extends Tile {
  override def landingEffect(ball: Ball): LandingEffect =
    LandingEffect(
      meter = ball match {case Player(_, _, _, _) => 4 case _ => 0})
}

case object Fall extends Tile {
  override def landingEffect(ball: Ball): LandingEffect = 
    LandingEffect(None, deadly = true)
}

case class Crackable(cracks: Int) extends Tile {
  override def landingEffect(ball: Ball): LandingEffect = 
    if (cracks == 0) then
      LandingEffect(
        Some(Crackable(1)), 
        meter = ball match {case Player(_, _, _, _) => 1 case _ => 0})
    else if (cracks == 1) then
      LandingEffect(
        Some(Crackable(2)), 
        crumble = Set(Crackable(1)),
        meter = ball match {case Player(_, _, _, _) => 1 case _ => 0}) 
    else 
      LandingEffect(Some(Fall), 
      crumble = Set(Crackable(2)),
      meter = ball match {case Player(_, _, _, _) => 1 case _ => 0})
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
      -(ball.dy+ball.dy.sign)/2,
      meter = ball match {case Player(_, _, _, _) => 4 case _ => 0}
    )
}

case class Booster(direction: Direction, boost: Int) extends Tile {
  override def landingEffect(ball: Ball): LandingEffect =
    LandingEffect(None, direction.dx * boost, direction.dy * boost,
      meter = ball match {case Player(_, _, _, _) => 4 case _ => 0})
}

case class Goal(goalAreaId: Int) extends Tile {
  override def landingEffect(ball: Ball): LandingEffect =
    LandingEffect(None, goal = Some(goalAreaId),
      meter = ball match {case Player(_, _, _, _) => 4 case _ => 0})
}