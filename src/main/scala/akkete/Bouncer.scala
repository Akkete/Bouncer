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


