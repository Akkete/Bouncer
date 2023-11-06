package akkete

import indigo.*
import indigo.scenes.*
import indigo.shared.datatypes.Fill.Color
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
    val turnTime = Seconds(1.0/(85.0/60.0))
    val turnPartTime = turnTime / 8
    {
      case FrameTick if context.running - model.seconds >= turnPartTime =>
        val inputDirection = context.keyboard.lastKeyHeldDown
          .flatMap(directionKeys.get(_))
          .headOption.getOrElse(NoDirection)
        Outcome(model.turn(seconds = context.running, input = inputDirection))
      case KeyboardEvent.KeyDown(Key.KEY_R) =>
        Outcome(Model.arena1(context.running, context.dice))
      case KeyboardEvent.KeyDown(Key.KEY_2) =>
        Outcome(Model.arena2(context.running, context.dice))
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

    val tileSize     = 32
    val gridSize     = tileSize
    val bounceHeight = gridSize * 15/16
    val turnTime = 1.0/(85.0/60.0)
    val time = (context.running - model.seconds).toDouble

    val tileCloneBlank = CloneBlank(

      CloneId("tile"),
      Graphic(
        bounds = Rectangle(0, 0, tileSize, tileSize),
        depth = 1,
        material = Bitmap(AssetName("tiles"))
      )
    )

    def ballSize(ball: Ball): Int = 
      ball match {
        case Player(_, _, _, _)        => tileSize * 3/4
        case CannonBall(_, _, _, _)    => tileSize * 5/8
        case Chaser(_, _, _, _)        => tileSize * 11/16
        case CarefulChaser(_, _, _, _) => tileSize * 11/16
        case _ => tileSize * 1/2
      }

    def ballColor(ball: Ball): Fill =
      ball match {
        case Player(_, _, _, _)        => Fill.Color(RGBA.Red)
        case CannonBall(_, _, _, _)    => Fill.Color(RGBA.Silver)
        case Chaser(_, _, _, _)        => Fill.Color(RGBA.Yellow mix RGBA.Orange)
        case CarefulChaser(_, _, _, _) => Fill.Color(RGBA.Yellow)
        case _ => Fill.Color(RGBA.White)
      }

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
          radius = ballSize(model.balls(i).get)/2,
          fill = ballColor(model.balls(i).get)
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
            ballSize(model.balls(i).get)/2
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

    val viewPortWidthTiles = viewModel.viewport.width / gridSize + 1
    val viewPortHeightTiles = viewModel.viewport.height / gridSize + 1

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

    /** Maps xy-coordinates to spritesheet columns and rows. */
    def tileGraphic(x: Int, y: Int): (Int, Int) = 
      model.floor.getOrElse((x, y), Fall) match {
        case Fall              => 
          model.floor.getOrElse((x, y-1), Fall) match {
            case Fall          => (3, 0)
            case Crackable(_)  => (3, 2)
            case Sand          => (3, 2)
            case _             => (3, 1)
          }
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
        // List out xy-coordinates visible on screen
        (for {
          x <- cameraX.toInt - viewPortWidthTiles/2 to 
               cameraX.toInt + viewPortWidthTiles/2 + 1
          y <- cameraY.toInt - viewPortHeightTiles/2 to 
               cameraY.toInt + viewPortHeightTiles/2 + 1 
        }
        yield
          val tile = model.floor.getOrElse((x, y), Fall)
          val (col, row) = tileGraphic(x, y)
          CloneTileData(
            x          = gridSize * x,
            y          = gridSize * y,
            cropX      = tileSize * col,
            cropY      = tileSize * row,
            cropWidth  = tileSize,
            cropHeight = tileSize
          )
        ).toSet
      )
    )

    val scoreDisplay = TextBox(s"${model.score}", 128, 64)
        .withFontFamily(FontFamily.sansSerif)
        .withColor(RGBA.White)
        .withFontSize(Pixels(32))
        .withStroke(TextStroke(RGBA.Black, Pixels(4)))
        .bold
        .alignCenter
        .moveTo(
          (tileSize/2 + cameraX * gridSize - 64).toInt,
          (tileSize/2 + cameraY * gridSize - viewModel.viewport.height/2).toInt
        )

    val moveInstructions = TextBox(
        "Use arrow keys to accelerate between bounces.",
        1000, 32)
        .withFontFamily(FontFamily.sansSerif)
        .withColor(RGBA.White)
        .withFontSize(Pixels(16))
        .withStroke(TextStroke(RGBA.Black, Pixels(4)))
        .alignLeft
        .moveTo(
          (tileSize/2 + cameraX * gridSize 
          - viewModel.viewport.width/2 + 16).toInt,
          (tileSize/2 + cameraY * gridSize 
          - viewModel.viewport.height/2 + 16).toInt
        )

    val restartInstructions = TextBox(
        "Press 'R' to restart.",
        1000, 32)
        .withFontFamily(FontFamily.sansSerif)
        .withColor(RGBA.White)
        .withFontSize(Pixels(16))
        .withStroke(TextStroke(RGBA.Black, Pixels(4)))
        .alignLeft
        .moveTo(
          (tileSize/2 
          + cameraX * gridSize 
          - viewModel.viewport.width/2 + 16).toInt,
          (tileSize/2 
          + cameraY * gridSize 
          - viewModel.viewport.height/2 + 16 + 32).toInt
        )

    val meterDisplay = Batch(
      Shape.Box(
        dimensions = Rectangle(0, 0, 256, 16), 
        fill = Fill.Color(RGBA.White.withAlpha(0.5))
      )
        .moveTo(
          (tileSize/2 + cameraX * gridSize - 128).toInt,
          (tileSize/2 + cameraY * gridSize 
          - viewModel.viewport.height/2 + 48).toInt
        ),
      Shape.Box(
                dimensions = Rectangle(0, 0, 256 * model.meter / 100, 16), 
        fill = Fill.Color(RGBA.Yellow)
      )
        .moveTo(
          (tileSize/2 + cameraX * gridSize - 128).toInt,
          (tileSize/2 + cameraY * gridSize 
          - viewModel.viewport.height/2 + 48).toInt
        ),
    )

    Outcome((
      SceneUpdateFragment(tiles).addCloneBlanks(tileCloneBlank)
      |+| SceneUpdateFragment(shadows)
      |+| SceneUpdateFragment(helper)
      |+| SceneUpdateFragment(balls)
      |+| SceneUpdateFragment(scoreDisplay)
      |+| SceneUpdateFragment(meterDisplay)
      |+| SceneUpdateFragment(moveInstructions)
      |+| SceneUpdateFragment(restartInstructions)
      ).withCamera(Camera.LookAt(Point(
        (tileSize/2 + cameraX * gridSize).toInt,
        (tileSize/2 + cameraY * gridSize).toInt
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


