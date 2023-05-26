package akkete

import indigo.*
import indigo.scenes.*
import indigo.shared.materials.Material.*


object GameScene extends Scene[Dice, Model, ViewModel]:

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
      context: SceneContext[Dice],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case FrameTick if context.running - model.seconds > Seconds(1.45)=>
      val inputDirection = context.keyboard.lastKeyHeldDown
        .flatMap(directionKeys.get(_))
        .headOption.getOrElse(NoDirection)
      Outcome(model.turn(seconds = context.running, input = inputDirection))
    case _ => Outcome(model)

  def updateViewModel(
      context: SceneContext[Dice],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case FrameTick =>
      val inputDirection = context.keyboard.lastKeyHeldDown
        .flatMap(directionKeys.get(_))
        .headOption.getOrElse(NoDirection)
      Outcome(viewModel.copy(currentInput = inputDirection))
    case ViewportResize(viewport) =>
      Outcome(viewModel.copy(viewport = viewport))
    case _ => Outcome(viewModel)

  def present(
      context: SceneContext[Dice],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =

    val gridSize     = 32
    val tileSize     = gridSize
    val playerSize   = gridSize*3/4
    val bounceHeight = gridSize*15/16
    val turnTime = 1.45
    val time = context.running - model.seconds
    val timeFraction = time / turnTime

    val tileCloneBlank = CloneBlank(
      CloneId("tile"),
      Graphic(
        bounds = Rectangle(0, 0, tileSize, tileSize),
        depth = 1,
        material = Bitmap(AssetName("tiles"))
      )
    )

    val playerX = model.player.x * timeFraction.toDouble + (model.player.x - model.player.dx) * (1 - timeFraction.toDouble)
    val playerY = model.player.y * timeFraction.toDouble + (model.player.y - model.player.dy) * (1 - timeFraction.toDouble)

    val viewPortWidthTiles = viewModel.viewport.width / tileSize + 1
    val viewPortHeightTiles = viewModel.viewport.height / tileSize + 1

    val cameraX = if (viewPortWidthTiles < model.width + 4) {
       (viewPortWidthTiles.toDouble/2 - 4) max playerX min (model.width + 2 - viewPortWidthTiles.toDouble/2)
      } else {
        (model.width.toDouble/2) - 1
      }
    val cameraY = if (viewPortWidthTiles < model.width + 4) {
        (viewPortHeightTiles.toDouble/2 - 4) max playerY min (model.height + 2 - viewPortHeightTiles.toDouble/2)
      } else {
        (model.height.toDouble/2) - 1
      }

    def tileGraphic(tile: Tile): (Int, Int) = 
      tile match {
        case Fall              => (0, 0)
        case Solid             => (0, 1)
        case Goal(id)          => 
          (if(model.goalAreas(id).active) then 2 else 1, 1)
        case Crackable(0)      => (0, 2)
        case Crackable(1)      => (1, 2)
        case Crackable(2)      => (2, 2)
        case Sand              => (2, 0)
        case Booster(Up, n)    => (0, (2 + n) min 6)
        case Booster(Down, n)  => (1, (2 + n) min 6)
        case Booster(Left, n)  => (2, (2 + n) min 6)
        case Booster(Right, n) => (3, (2 + n) min 6)
        case _                 => (1, 0)
      }
      

    val tiles = CloneTiles(
      CloneId("tile"),
      Batch.fromSet(
        (for x <- cameraX.toInt - viewPortWidthTiles/2 to cameraX.toInt + viewPortWidthTiles/2 + 1;
             y <- cameraY.toInt - viewPortHeightTiles/2 to cameraY.toInt + viewPortHeightTiles/2 + 1 yield
          val tile = model.floor.getOrElse((x, y), Fall)
          val (col, row) = tileGraphic(tile)
          CloneTileData(
            gridSize * x,
            gridSize * y,
            tileSize * col,
            tileSize * row,
            tileSize,
            tileSize
          )
        ).toSet
      )
    )

    val player = Shape.Circle(
      center = Point(
        (tileSize/2 + playerX * gridSize).toInt, 
        (tileSize/2 + playerY * gridSize).toInt - (bounceAnimation.at(timeFraction) * bounceHeight).toInt
      ),
      radius = playerSize/2,
      fill = 
        if (model.player.dead) {
          Fill.Color(RGBA.Blue)
        } else {
          Fill.Color(RGBA.Red)
        }
    )
    val shadow = Shape.Circle(
      center = Point(
        (tileSize/2 + playerX * gridSize).toInt, 
        (tileSize/2 + playerY * gridSize).toInt
      ),
      radius = (playerSize/2 * (0.4 + 0.6 * (1 - bounceAnimation.at(timeFraction)))).toInt,
      fill = Fill.Color(RGBA.Black.withAlpha(0.5))
    ).scaleBy(1, 0.8)

    val previewModel = model.turn(viewModel.currentInput, model.seconds + turnTime)
    val helper = Shape.Line(
      Point(
        tileSize/2 + model.player.x * gridSize, 
        tileSize/2 + model.player.y * gridSize
      ),
      Point(
        tileSize/2 + previewModel.player.x * gridSize, 
        tileSize/2 + previewModel.player.y * gridSize
      ),
      Stroke(width = 2, color = RGBA.Blue)
    )

    Outcome(
      (SceneUpdateFragment(tiles).addCloneBlanks(tileCloneBlank)
      |+| SceneUpdateFragment(shadow)
      |+| SceneUpdateFragment(helper)
      |+| SceneUpdateFragment(player))
      .withCamera(Camera.LookAt(Point(
        (gridSize/2 + tileSize/2 + cameraX * gridSize).toInt,
        (gridSize/2 + tileSize/2 + cameraY * gridSize).toInt
        )))
    )


val directionKeys: Map[Key, Direction] = 
  Map(
    Key.UP_ARROW    -> Up,
    Key.DOWN_ARROW  -> Down,
    Key.LEFT_ARROW  -> Left,
    Key.RIGHT_ARROW -> Right,
  )

val bounceAnimation: Signal[Double] = 
  Signal(
    t => {
      val tp = (t.toDouble - 0.5)
      (0.25 - (tp * tp)) * 4
    }
  )


