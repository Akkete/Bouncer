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
      Model(seconds, Player(x, y, dx, dy), floor)
  }

object Model {
  def test: Model =
    Model(
      seconds = Seconds(0),
      player = Player(4, 4, 0, 0),
      floor = (for i <- 0 until 20; j <- 0 until 20 yield (i, j) -> Solid).toMap
    )
}

case class Player(x: Int, y: Int, dx: Int, dy: Int)

abstract class Tile

case object Fall extends Tile
case object Solid extends Tile
case class Crackable(cracks: Int) extends Tile

case class ViewModel(
  currentInput: Input
)

abstract class Input{
  def dx: Int
  def dy: Int
}

case object NoInput extends Input {
  def dx = 0
  def dy = 0
}
case object Up extends Input {
  def dx = 0
  def dy = -1
}
case object Down extends Input {
  def dx = 0
  def dy = 1
}
case object Left extends Input {
  def dx = -1
  def dy = 0
}
case object Right extends Input {
  def dx = 1
  def dy = 0
}

