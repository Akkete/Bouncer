package akkete

import indigo.*
import indigo.scenes.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object Bouncer extends IndigoGame[Unit, Dice, Model, ViewModel]:

  def initialScene(bootData: Unit): Option[SceneName] =
    None

  def scenes(bootData: Unit): NonEmptyList[Scene[Dice, Model, ViewModel]] =
    NonEmptyList(GameScene)

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(
      BootResult.noData(
        GameConfig.default
          .withViewport(1100, 800)
      )
      .withAssets(
        AssetType.Image(AssetName("tiles"), AssetPath("assets/TileMap.png")),
      )
    )

  def initialModel(startupData: Dice): Outcome[Model] =
    Outcome(Model.test(seconds = Seconds(0), dice = startupData))

  def initialViewModel(startupData: Dice, model: Model): Outcome[ViewModel] =
    Outcome(ViewModel(NoDirection, GameViewport(1100, 800)))

  def setup(
      bootData: Unit,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[Dice]] =
    Outcome(Startup.Success((dice)))

  def updateModel(
      context: FrameContext[Dice],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Dice],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Dice],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)


case class Model(
  seconds: Seconds,
  part: Int,
  dice: Dice,
  balls: Vector[Option[Ball]],
  crumbles: Set[(Int, Int)] = Set.empty,
  score: Int,
  scoreGoal: Int,
  floor: Map[(Int, Int), Tile],
  width: Int,
  height: Int,
  goalAreas: Vector[GoalArea],
  enemyCounter: Int,
  ) {
    def turn(input: Direction, seconds: Seconds): Model = 
      val ballOption = balls(part)
      val landingEffect = ballOption map { ball => 
          floor.getOrElse((ball.x, ball.y), Fall).landingEffect(ball)
      } getOrElse LandingEffect(None)
      val crumbleEffects = 
        crumbles.map(p => p -> floor.getOrElse(p, Fall).crumbleEffect).toMap
      val updatedBall = if landingEffect.deadly then None else 
        ballOption.map(_.update(this, input))
      val updatedGoals = 
        landingEffect.goal.filter(goalAreas(_).active) 
          .map {scoredGoal => 
            val gas = goalAreas.zipWithIndex.map((ga, id) =>
              if (id == scoredGoal) {ga.deactivate} else {ga.reduceCooldow}
            )
            val ready = 
              gas.zipWithIndex.filter {(ga, id) =>
                ga.cooldown == 0 && !ga.active
              }
            val next = 
              if ready.isEmpty then None else 
                Some(ready(dice.rollFromZero(ready.length)))
            next map {
              (nx, id) => gas.updated(id, nx.copy(active = true))
            } getOrElse {gas}
          } getOrElse {
            if (goalAreas.exists(_.active)) {
              goalAreas
            } else {
              goalAreas.map(_.reduceCooldow)
            }
          }
      val tileChangeByLanding = ballOption flatMap { ball => 
        landingEffect.tileChange map { tile =>
          ((ball.x, ball.y), tile)
        }
      }
      val updatedTiles = 
        crumbleEffects.view.mapValues(_.tile) ++ tileChangeByLanding
      val landingCrumbles = ballOption map { ball => 
        neighbourhood(ball.x, ball.y)
        .filter(landingEffect.crumble contains floor.getOrElse(_, Fall)) 
      } getOrElse Set()
      val crumbleCrumbles = crumbleEffects.map((p, c) => 
        neighbourhood(p._1, p._2)
        .filter(c.crumble contains floor.getOrElse(_, Fall))
      ).flatten.toSet
      val updatedCrumbles = landingCrumbles ++ crumbleCrumbles
      val enemyAppears: (Int, Option[Ball]) = 
        if enemyCounter == 99 && part == 0 then
          val i = balls.indexOf(None)
          val dx = dice.roll(5) - 3
          val dy = dice.roll(5) - 3
          val possibleSpots = (floor filter { 
            (xy: (Int, Int), tile: Tile) => 
              val (x, y) = xy
              val landingEffect = tile.landingEffect(Chaser(x, y, dx, dy))
              !landingEffect.deadly && landingEffect.goal.isEmpty
          }).keySet
          val landingSpot = if possibleSpots.isEmpty then (0, 0) else
            possibleSpots.toVector(dice.rollFromZero(possibleSpots.size))
          val (x, y) = landingSpot
          val enemy = 
            if i > -1 then Some(Chaser(x, y, dx, dy)) else None
          (i, enemy)
        else
          (-1, None)
            
      this.copy(
        seconds, 
        part = (part + 1) % 8,
        balls = 
          if enemyAppears._1 > -1 then 
            balls.updated(part, updatedBall)
                 .updated(enemyAppears._1, enemyAppears._2)
          else
            balls.updated(part, updatedBall), 
        score = 
          if landingEffect.goal.map(goalAreas(_).active).getOrElse(false) then 
            score + 1 
          else 
            score,
        floor = floor ++ updatedTiles, 
        goalAreas = updatedGoals,
        crumbles = updatedCrumbles,
        enemyCounter = (enemyCounter + {if(part == 0) 1 else 0}) % 100,
      )
  }

object Model {
  def test(seconds: Seconds, dice: Dice): Model =
    val width = 36
    val height = 28
    Model(
      seconds = seconds,
      part = 0,
      dice = dice,
      balls = Vector(
        Some(Player(9, 9, 0, 0)),
        Some(CannonBall(21, 5, 1, 1)),
        Some(Chaser(15, 15, 0, 0)),
        None,
        None,
        None,
        None,
        None
        ),
      score = 0,
      scoreGoal = 12,
      width = width,
      height = height,
      floor = (
          for i <- 0 until width; j <- 0 until height yield
            if (i < 1 || i >= width-1 || j < 1 || j >= height-1) {
              (i, j) -> Crackable(2)
            } else if (i < 2 || i >= width-2 || j < 2 || j >= height-2) {
              (i, j) -> Crackable(1)
            } else if (i < 4 || i >= width-4 || j < 4 || j >= height-4) {
              (i, j) -> Crackable(0)
            } else if ((i < 7) && (j < 7)) {
              (i, j) -> Goal(0)
            } else if ((i < 7) && (j >= height-7)) {
              (i, j) -> Goal(1)
            } else if ((i >= width-7) && (j >= height-7)) {
              (i, j) -> Goal(2)
            } else if ((i >= width-7) && (j < 7)) {
              (i, j) -> Goal(3)
            } else if (i < 8 || i >= width-8 || j < 8 || j >= height-8) {
              (i, j) -> Crackable(0)
            } else if (i > 12 && i <= 16) {
              (i, j) -> Sand
            } else if (i==18) {
              (i, j) -> Booster(Right, 4)
            } else if (i==19) {
              (i, j) -> Booster(Right, 3)
            } else if (i==20) {
              (i, j) -> Booster(Right, 2)
            } else if (i==21) {
              (i, j) -> Booster(Right, 1)
            } else if (i==23) {
              (i, j) -> Booster(Left, 1)
            } else if (i==24) {
              (i, j) -> Booster(Left, 2)
            } else if (i==25) {
              (i, j) -> Booster(Left, 3)
            } else if (i==26) {
              (i, j) -> Booster(Left, 4)
            } else {
              (i, j) -> Solid
            }
        ).toMap,
    goalAreas = Vector(
        GoalArea(false, 1, 2),
        GoalArea(true,  0, 2),
        GoalArea(false, 1, 2),
        GoalArea(false, 1, 2),
      ),
    enemyCounter = 90,
    )
  def arena1(seconds: Seconds, dice: Dice): Model = 
    val width = 36
    val height = 28
    val goalPositions = Vector(
      (4 + dice.rollFromZero(3), 4 + dice.rollFromZero(3)),
      (6 + dice.rollFromZero(3), height-1-10 + dice.rollFromZero(3)),
      (width/2-1-1 + dice.rollFromZero(2), 
       height/2-1-1 + dice.rollFromZero(2)),
      (width-1-10 + dice.rollFromZero(3), 6 + dice.rollFromZero(3)),
      (width-1-4-4 + dice.rollFromZero(3), 
       height-1-4-4 + dice.rollFromZero(3)),
    )
    val holePositions = Vector(
        (goalPositions(0)._1+3 max 8, 
         goalPositions(0)._2+3 max 8), 
        (goalPositions(4)._1-6 min width-1-13, 
         goalPositions(4)._2-6 min height-1-13), 
      )
    Model(
      seconds = seconds,
      part = 0,
      dice = dice,
      balls = Vector(
        Some(Player(goalPositions(2)._1+1, goalPositions(2)._2+1, 0, 0)),
        None,
        None,
        None,
        None,
        None,
        None,
        None
        ),
      score = 0,
      scoreGoal = 50,
      width = width,
      height = height,
      floor = 
        (
          for i <- 0 until width; j <- 0 until height yield
            if (j < 3 && i >= 4 && i < width-4) {
              (i, j) -> Booster(Down, 3-j)
            } else if (j >= height-3 && i >= 4 && i < width-4) {
              (i, j) -> Booster(Up, 3-height+j+1)
            } else if (i < 3 && j >= 4 && j < height-4) {
              (i, j) -> Booster(Right, 3-i)
            } else if (i >= width-3 && j >= 4 && j < height-4) {
              (i, j) -> Booster(Left, 3-width+i+1)
            } else if (i >= 3 && i < width-3 && j >= 3 && j < height-3) {
              (i, j) -> Crackable(0)
            } else {
              (i, j) -> Fall
            }
        ).toMap 
        ++
        Map(
          (1, 2) -> Booster(Right, 3),
          (1, 3) -> Booster(Right, 3),
          (1, height-1-2) -> Booster(Right, 3),
          (1, height-1-3) -> Booster(Right, 3),
          (2, 3) -> Booster(Right, 2),
          (2, height-1-2) -> Booster(Right, 2),
          (2, height-1-3) -> Booster(Right, 2),
          (3, height-1-3) -> Booster(Right, 1),
          (width-1-1, 2) -> Booster(Left, 3),
          (width-1-1, 3) -> Booster(Left, 3),
          (width-1-1, height-1-2) -> Booster(Left, 3),
          (width-1-1, height-1-3) -> Booster(Left, 3),
          (width-1-2, 3) -> Booster(Left, 2),
          (width-1-2, 2) -> Booster(Left, 2),
          (width-1-2, height-1-3) -> Booster(Left, 2),
          (width-1-3, 3) -> Booster(Left, 1),
          (2, 1) -> Booster(Down, 3),
          (3, 1) -> Booster(Down, 3),
          (width-1-2, 1) -> Booster(Down, 3),
          (width-1-3, 1) -> Booster(Down, 3),
          (3, 2) -> Booster(Down, 2),
          (2, 2) -> Booster(Down, 2),
          (width-1-3, 2) -> Booster(Down, 2),
          (3, 3) -> Booster(Down, 1),
          (2, height-1-1) -> Booster(Up, 3),
          (3, height-1-1) -> Booster(Up, 3),
          (width-1-2, height-1-1) -> Booster(Up, 3),
          (width-1-3, height-1-1) -> Booster(Up, 3),
          (3, height-1-2) -> Booster(Up, 2),
          (width-1-2, height-1-2) -> Booster(Up, 2),
          (width-1-3, height-1-2) -> Booster(Up, 2),
          (width-1-3, height-1-3) -> Booster(Up, 1),
        )
        ++ 
        (
          for (x, y) <- holePositions; 
            i <- 0 until 6; j <- 0 until 6 yield
              if (i < 2 || i >= 6-2 || j < 2 || j >= 6-2) {
                (i+x, j+y) -> Sand
              } else {
                (i+x, j+y) -> Fall
              }
        ).toMap
        ++ 
        (
          for ((x, y), id) <- goalPositions.zipWithIndex; 
            i <- 0 until 3; j <- 0 until 3 yield
            (i+x, j+y) -> Goal(id)
        ).toMap
      ,
    goalAreas = Vector(
        GoalArea(true,  0, 5),
        GoalArea(false, 1, 5),
        GoalArea(false, 2, 2),
        GoalArea(false, 3, 5),
        GoalArea(false, 4, 5),
      ),
    enemyCounter = 60,
    )
}

case class LandingEffect(
  tileChange: Option[Tile] = None, 
  dx: Int = 0, 
  dy: Int = 0, 
  deadly: Boolean = false,
  goal: Option[Int] = None,
  crumble: Set[Tile] = Set.empty,
  )

case class CrumbleEffect(
  tile: Tile,
  crumble: Set[Tile] = Set.empty,
)
  
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

case class GoalArea(
    active: Boolean,
    cooldown: Int,
    maxCooldown: Int
  ) {
    def reduceCooldow: GoalArea = 
      this.copy(cooldown = cooldown - 1 max 0)
    def deactivate: GoalArea = 
      this.copy(active = false, cooldown = maxCooldown)
  }

case class ViewModel(
  currentInput: Direction,
  viewport: GameViewport
)

abstract class Direction{
  def dx: Int = 0
  def dy: Int = 0
}

case object NoDirection extends Direction
case object Up extends Direction {
  override def dy = -1
}
case object Down extends Direction {
  override def dy = 1
}
case object Left extends Direction {
  override def dx = -1
}
case object Right extends Direction {
  override def dx = 1
}

def neighbourhood(x: Int, y: Int): Set[(Int, Int)] =
  Set((x, y+1), (x, y-1), (x-1, y), (x+1, y))