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
  dice: Dice,
  player: Player,
  crumbles: Set[(Int, Int)] = Set.empty,
  score: Int,
  scoreGoal: Int,
  floor: Map[(Int, Int), Tile],
  width: Int,
  height: Int,
  goalAreas: Vector[GoalArea]
  ) {
    def turn(input: Direction, seconds: Seconds): Model = 
      val landingEffect = 
        floor.getOrElse((player.x, player.y), Fall).landingEffect(player)
      val dx = player.dx + input.dx + landingEffect.dx
      val dy = player.dy + input.dy + landingEffect.dy
      val x  = player.x + dx
      val y  = player.y + dy
      val updatedPlayer = 
        Player(
          x, y, dx, dy, 
          dead = player.dead || landingEffect.deadly
        )
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
      val updatedFloor =  
        floor.updated((player.x, player.y), landingEffect.tile)
      this.copy(
        seconds, 
        player = updatedPlayer, 
        score = 
          if landingEffect.goal.map(goalAreas(_).active).getOrElse(false) then 
            score + 1 
          else 
            score,
        floor = updatedFloor, 
        goalAreas = updatedGoals,
        crumbles = crumbles ++ (
            neighbourhood(player.x, player.y)
            .filter(landingEffect.crumble contains floor.getOrElse(_, Fall))
          )
      )
    
    def crumble: Model =
      val crumbleEffects = crumbles.map(p => p -> floor.getOrElse(p, Fall).crumbleEffect).toMap
      val updatedTiles = crumbleEffects.view.mapValues(_.tile)
      val updatedCrumbles = crumbleEffects.map((p, c) => 
        neighbourhood(p._1, p._2)
        .filter(c.crumble contains floor.getOrElse(_, Fall))
      ).flatten.toSet
      this.copy(
        floor = floor ++ updatedTiles,
        crumbles = updatedCrumbles
      )
  }

object Model {
  def test(seconds: Seconds, dice: Dice): Model =
    val width = 36
    val height = 28
    val goalAreas = Vector(
      GoalArea(false, 1, 2),
      GoalArea(true,  0, 2),
      GoalArea(false, 1, 2),
      GoalArea(false, 1, 2),
    )
    Model(
      seconds = seconds,
      dice = dice,
      player = Player(9, 9, 0, 0),
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
    goalAreas = goalAreas
    )
}

case class Player(x: Int, y: Int, dx: Int, dy: Int, dead: Boolean = false)

case class LandingEffect(
  tile: Tile, 
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
  def landingEffect(player: Player): LandingEffect = LandingEffect(this)
  def crumbleEffect: CrumbleEffect = CrumbleEffect(this)
}

case object Solid extends Tile
case object Fall extends Tile {
  override def landingEffect(player: Player): LandingEffect = 
    LandingEffect(this, deadly = true)
}
case class Crackable(cracks: Int) extends Tile {
  override def landingEffect(player: Player): LandingEffect = 
    if (cracks == 0) then
      LandingEffect(Crackable(1)) 
    else if (cracks == 1) then
      LandingEffect(Crackable(2), crumble = Set(Crackable(1))) 
    else 
      LandingEffect(Fall, crumble = Set(Crackable(2)))
  override def crumbleEffect: CrumbleEffect =
    if (cracks == 0) then
      CrumbleEffect(Crackable(0))
    else if (cracks == 1) then
      CrumbleEffect(Crackable(2), crumble = Set(Crackable(1))) 
    else 
      CrumbleEffect(Fall, crumble = Set(Crackable(2)))
}
case object Sand extends Tile {
  override def landingEffect(player: Player): LandingEffect =
    LandingEffect(this, 
    -(player.dx+player.dx.sign)/2, 
    -(player.dy+player.dy.sign)/2
    )
}
case class Booster(direction: Direction, boost: Int) extends Tile {
  override def landingEffect(player: Player): LandingEffect =
    LandingEffect(this, direction.dx * boost, direction.dy * boost)
}
case class Goal(goalAreaId: Int) extends Tile {
  override def landingEffect(player: Player): LandingEffect =
    LandingEffect(this, goal = Some(goalAreaId))
}

case class GoalArea(
    active: Boolean,
    cooldown: Int,
    maxCooldown: Int
  ) {
    def reduceCooldow: GoalArea = this.copy(cooldown = cooldown - 1 max 0)
    def deactivate: GoalArea = this.copy(active = false, cooldown = maxCooldown)
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