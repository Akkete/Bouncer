package akkete

import indigo.*
import indigo.scenes.*

object GameScene extends Scene[Unit, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("game")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: SceneContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case FrameTick if context.running - model.seconds > Seconds(1.05)=>
      val inputDirection = context.keyboard.lastKeyHeldDown
        .flatMap(directionKeys.get(_))
        .headOption.getOrElse(NoInput)
      Outcome(model.turn(seconds = context.running, input = inputDirection))
    case _ => Outcome(model)

  def updateViewModel(
      context: SceneContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case FrameTick =>
      val inputDirection = context.keyboard.lastKeyHeldDown
        .flatMap(directionKeys.get(_))
        .headOption.getOrElse(NoInput)
      Outcome(ViewModel(inputDirection))
    case _ => Outcome(viewModel)

  def present(
      context: SceneContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    val tileSize = 32
    val time = context.running - model.seconds
    val tiles = Batch.fromSet(
      (for ((x, y), tile) <- model.floor yield
        Shape.Box(Rectangle(0, 0, tileSize*3/4, tileSize*3/4), Fill.Color(RGBA.Yellow))
          .moveTo(tileSize/2 + tileSize * x, tileSize/2 + tileSize * y)
      ).toSet
    )
    val playerY = model.player.y * time.toDouble + (model.player.y - model.player.dy) * (1 - time.toDouble)
    val playerX = model.player.x * time.toDouble + (model.player.x - model.player.dx) * (1 - time.toDouble)
    val player = Shape.Circle(
      center = Point(
        (tileSize/2 + tileSize*3/4/2 + playerX * tileSize).toInt, 
        (tileSize/2 + tileSize*3/4/2 + playerY * tileSize).toInt - (bounceHeight.at(time) * tileSize*3/4).toInt
      ),
      radius = tileSize*3/4/2,
      fill = Fill.Color(RGBA.Red)
    )
    val shadow = Shape.Circle(
      center = Point(
        (tileSize/2 + tileSize*3/4/2 + playerX * tileSize).toInt, 
        (tileSize/2 + tileSize*3/4/2 + playerY * tileSize).toInt
      ),
      radius = (tileSize*3/4/2 * (0.4 + 0.6 * (1 - bounceHeight.at(time)))).toInt,
      fill = Fill.Color(RGBA.SlateGray)
    ).scaleBy(1, 0.8)
    val helper = Shape.Line(
      Point(
        tileSize/2 + tileSize*3/4/2 + model.player.x * tileSize, 
        tileSize/2 + tileSize*3/4/2 + model.player.y * tileSize
      ),
      Point(
        tileSize/2 + tileSize*3/4/2 + (model.player.x + model.player.dx + viewModel.currentInput.dx) * tileSize, 
        tileSize/2 + tileSize*3/4/2 + (model.player.y + model.player.dy + viewModel.currentInput.dy) * tileSize
      ),
      Stroke(width = 2, color = RGBA.Blue)
    )

    Outcome(
      SceneUpdateFragment(tiles)
      |+| SceneUpdateFragment(shadow)
      |+| SceneUpdateFragment(helper)
      |+| SceneUpdateFragment(player)
    )


val directionKeys: Map[Key, Input] = Map(
  Key.UP_ARROW    -> Up,
  Key.DOWN_ARROW  -> Down,
  Key.LEFT_ARROW  -> Left,
  Key.RIGHT_ARROW -> Right,
  )

val bounceHeight: Signal[Double] = 
  Signal(
    t => {
      val tl = 1.05
      val tp = (t.toDouble - tl/2)
      ((tl * tl / 4) - (tp * tp)) * 4 / (tl * tl)
    }
  )

