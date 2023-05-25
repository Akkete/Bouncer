package akkete

import indigo.*
import indigo.scenes.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object Bouncer extends IndigoGame[Unit, Unit, Model, ViewModel]:

  def initialScene(bootData: Unit): Option[SceneName] =
    None

  def scenes(bootData: Unit): NonEmptyList[Scene[Unit, Model, ViewModel]] =
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

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(Model.test)

  def initialViewModel(startupData: Unit, model: Model): Outcome[ViewModel] =
    Outcome(ViewModel(NoInput))

  def setup(
      bootData: Unit,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def updateModel(
      context: FrameContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)


case class Model(
  seconds: Seconds,
  player: Player,
  floor: Map[(Int, Int), Tile]
  ) {
    def turn(input: Input, seconds: Seconds): Model = 
      val dx = player.dx + input.dx
      val dy = player.dy + input.dy 
      val x  = player.x + dx
      val y  = player.y + dy
      val updatedFloor = 
        floor.updatedWith((player.x, player.y))(x => 
          x.map(_.landingEffect)
        )
      Model(seconds, Player(x, y, dx, dy), updatedFloor)
  }

object Model {
  def test: Model =
    val width = 50
    val height = 30
    Model(
      seconds = Seconds(0),
      player = Player(9, 9, 0, 0),
      floor = (
          for i <- 0 until width; j <- 0 until height yield
            if (i < 1 || i >= width-1 || j < 1 || j >= height-1) {
              (i, j) -> Crackable(2)
            } else if (i < 2 || i >= width-2 || j < 2 || j >= height-2) {
              (i, j) -> Crackable(1)
            } else if (i < 8 || i >= width-8 || j < 8 || j >= height-8) {
              (i, j) -> Crackable(0)
            } else {
              (i, j) -> Solid
            }
        ).toMap
    )
}

case class Player(x: Int, y: Int, dx: Int, dy: Int)

abstract class Tile {
  def landingEffect: Tile = this
}

case object Fall extends Tile
case object Solid extends Tile
case class Crackable(cracks: Int) extends Tile {
  override def landingEffect: Tile = 
    if (cracks < 2) Crackable(cracks + 1) else Fall
}

case class ViewModel(
  currentInput: Input
)

abstract class Input{
  def dx: Int = 0
  def dy: Int = 0
}

case object NoInput extends Input
case object Up extends Input {
  override def dy = -1
}
case object Down extends Input {
  override def dy = 1
}
case object Left extends Input {
  override def dx = -1
}
case object Right extends Input {
  override def dx = 1
}

