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
    val turnTime = Seconds(1.45)
    val turnPartTime = turnTime / 8
    {
      case FrameTick if context.running - model.seconds >= turnPartTime =>
        val inputDirection = context.keyboard.lastKeyHeldDown
          .flatMap(directionKeys.get(_))
          .headOption.getOrElse(NoDirection)
        Outcome(model.turn(seconds = context.running, input = inputDirection))
      case KeyboardEvent.KeyDown(Key.KEY_R) =>
        Outcome(Model.arena1(context.running, context.dice))
      case _ => Outcome(model)
    }

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
    val playerSize   = gridSize * 3/4
    val bounceHeight = gridSize * 15/16
    val turnTime = 1.45
    val time = (context.running - model.seconds).toDouble

    val tileCloneBlank = CloneBlank(

      CloneId("tile"),
      Graphic(
        bounds = Rectangle(0, 0, tileSize, tileSize),
        depth = 1,
        material = Bitmap(AssetName("tiles"))
      )
    )

    val viewPortWidthTiles = viewModel.viewport.width / tileSize + 1
    val viewPortHeightTiles = viewModel.viewport.height / tileSize + 1

    val ballCoords: Vector[Option[(Double, Double, Double)]] =
      (for i <- 0 until 8 yield { model.balls(i) map { ball =>
        val ballInterpolation = 
          (((model.part - i - 1) % 8 + 8) % 8 * turnTime / 8 + time) / turnTime
        val ballX = 
          ball.x * ballInterpolation 
          + (ball.x - ball.dx) * (1 - ballInterpolation)
        val ballY = 
          ball.y * ballInterpolation 
          + (ball.y - ball.dy) * (1 - ballInterpolation)
        (ballX, ballY, ballInterpolation)
      }}).toVector

    val balls = Batch.fromIndexedSeq(
      (for i <- 0 until 8 yield { ballCoords(i) map { coords =>
        Shape.Circle(
          center = Point(
            (tileSize/2 + coords._1 * gridSize).toInt, 
            (tileSize/2 + coords._2 * gridSize).toInt 
            - (bounceAnimation.at(Seconds(coords._3)) 
            * bounceHeight).toInt
          ),
          radius = playerSize/2,
          fill = 
            if i == 0 then Fill.Color(RGBA.Red) else Fill.Color(RGBA.Silver)
        )
      }}).flatten
    )
    val shadows = Batch.fromIndexedSeq(
      (for i <- 0 until 8 yield { ballCoords(i) map { coords =>
        Shape.Circle(
          center = Point(
            (tileSize/2 + coords._1 * gridSize).toInt, 
            (tileSize/2 + coords._2 * gridSize).toInt
          ),
          radius = (
            playerSize/2 
            * (0.4 + 0.6 * (1 - bounceAnimation.at(Seconds(coords._3))))
          ).toInt,
          fill = Fill.Color(RGBA.Black.withAlpha(0.5))
        ).scaleBy(1, 0.8)
      }}).flatten
    )

    val previewModel = 
      model.turn(viewModel.currentInput, model.seconds + turnTime * 1)
           .turn(viewModel.currentInput, model.seconds + turnTime * 2)
           .turn(viewModel.currentInput, model.seconds + turnTime * 3)
           .turn(viewModel.currentInput, model.seconds + turnTime * 4)
           .turn(viewModel.currentInput, model.seconds + turnTime * 5)
           .turn(viewModel.currentInput, model.seconds + turnTime * 6)
           .turn(viewModel.currentInput, model.seconds + turnTime * 7)
           .turn(viewModel.currentInput, model.seconds + turnTime * 8)
    val helper = Shape.Line(
      Point(
        tileSize/2 + model.balls(0).map(_.x).getOrElse(0) * gridSize, 
        tileSize/2 + model.balls(0).map(_.y).getOrElse(0) * gridSize
      ),
      Point(
        tileSize/2 + previewModel.balls(0).map(_.x).getOrElse(0) * gridSize, 
        tileSize/2 + previewModel.balls(0).map(_.y).getOrElse(0) * gridSize
      ),
      Stroke(width = 2, color = RGBA.Blue)
    )

    val cameraX = if (viewPortWidthTiles < model.width + 4) {
       (viewPortWidthTiles.toDouble/2 - 4) max 
       ballCoords(0).map(_._1).getOrElse((model.width.toDouble/2) - 1) min 
       (model.width + 2 - viewPortWidthTiles.toDouble/2)
      } else {
        (model.width.toDouble/2) - 1
      }
    val cameraY = if (viewPortWidthTiles < model.width + 4) {
        (viewPortHeightTiles.toDouble/2 - 4) max 
        ballCoords(0).map(_._2).getOrElse((model.height.toDouble/2) - 1) min 
        (model.height + 2 - viewPortHeightTiles.toDouble/2)
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

    val scoreDisplay = TextBox(s"${model.score}/${model.scoreGoal}", 128, 64)
        .withFontFamily(FontFamily.sansSerif)
        .withColor(RGBA.White)
        .withFontSize(Pixels(32))
        .withStroke(TextStroke(RGBA.Black, Pixels(4)))
        .bold
        .alignCenter
        .moveTo(
          (gridSize/2 + tileSize/2 + cameraX * gridSize - 64).toInt,
          (gridSize/2 + tileSize/2 + cameraY * gridSize - viewModel.viewport.height/2).toInt
        )

    Outcome((
      SceneUpdateFragment(tiles).addCloneBlanks(tileCloneBlank)
      |+| SceneUpdateFragment(shadows)
      |+| SceneUpdateFragment(helper)
      |+| SceneUpdateFragment(balls)
      |+| SceneUpdateFragment(scoreDisplay)
      ).withCamera(Camera.LookAt(Point(
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


