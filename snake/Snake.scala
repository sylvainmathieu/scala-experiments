package snake

import scala.util.{Random}
import swing.{SimpleSwingApplication, MainFrame, Panel, event}
import event.{KeyPressed}

object App extends SimpleSwingApplication {
	import event.Key._
	import java.awt.{Dimension, Graphics2D, Rectangle}
	import java.awt.{Color}
	import java.awt.event.{ActionEvent}
	import javax.swing.{Timer, AbstractAction}

	override def top = frame

	val frame = new MainFrame {
		title = "Scala Snake"
		contents = mainPanel

		lazy val mainPanel = new Panel {
			focusable = true
			background = Color.black
			preferredSize = new Dimension(200, 200)

			override def paint(g: Graphics2D) {
				g.setColor(new Color(0x1B, 0x00, 0x3F))
				g.fillRect(0, 0, size.width, size.height)
				onPaint(g)
			}

			listenTo(keys)

			reactions += {
				case KeyPressed(_, key, _, _) =>
					onKeyPress(key)
			}

		}

		def onKeyPress(keyCode: Value) = keyCode match {
			case Left => game.moveBy(-1, 0)
			case Right => game.moveBy(1, 0)
			case Up => game.moveBy(0, -1)
			case Down => game.moveBy(0, 1)
			case _ =>
		}

		val timer = new Timer(100, new AbstractAction() {
			override def actionPerformed(e: ActionEvent) {
				repaint()
			}
		})

		timer.start

	}

	var game = Game.newGame

	def onPaint(g: Graphics2D) {

		def drawBlock(x: Int, y: Int) {
			g.draw(new Rectangle(x * 10 + 1, y * 10 + 1, 8, 8))
		}

		game = game.tick

		if (game.over) frame.timer.stop

		// Display Food
		g.setColor(Color.green)
		drawBlock(game.foodPosition.x, game.foodPosition.y)

		// Display Snake
		g.setColor(Color.gray)
		game.grid.snake.foreach { cell: Cell =>
			drawBlock(cell.x, cell.y)
		}

	}

}

object Game {

	def newGame: Game = {
		new Game(Grid.newGrid, (0, 1), new Position(2, 2))
	}

}

class Game(
	val grid: Grid,
	val lastMove: Tuple2[Int, Int],
	val foodPosition: Position) {

	var nextMove = lastMove

	def tick: Game = {
		val eatResult = eat
		val newGrid = new Grid(grid.snake, grid.playerPosition + nextMove, eatResult._2)
		new Game(newGrid.tick, nextMove, eatResult._1)
	}

	def moveBy(move: Tuple2[Int, Int]) = {
		if (move != (lastMove._1 * (-1), lastMove._2 * (-1)))
			nextMove = move
	}

	def eat: Tuple2[Position, Int] = {
		if (grid.playerPosition == foodPosition) {
			(grid.popFood, grid.lifeTime + 1)
		}
		else (foodPosition, grid.lifeTime)
	}

	def over: Boolean = grid.isPlayerOut || grid.isPlayerOnObstacle

}

object Grid {
	def newGrid: Grid = {
		new Grid(Nil, new Position(10, 10), 10)
	}
}

class Grid(
	val snake: List[Cell],
	val playerPosition: Position,
	val lifeTime: Int
) {

	val width = 20
	val height = 20

	def tick: Grid = {
		new Grid(
			new Cell(playerPosition.x, playerPosition.y, lifeTime) :: decrement(snake),
			playerPosition,
			lifeTime
		)
	}

	def decrement(initSnake: List[Cell]) = {
		initSnake.map{ cell: Cell =>
			new Cell(cell.x, cell.y, cell.lifeTime - 1)
		}.filter(_.lifeTime >= 1)
	}

	def popFood: Position = {
		def foodPosition: Position = {
			val position = new Position(Random.nextInt(width), Random.nextInt(height))
			if (isOnObstacle(position)) return foodPosition
			return position
		}
		return foodPosition
	}

	def isPlayerOut: Boolean =
		playerPosition.x < 0 || playerPosition.x > width ||
		playerPosition.y < 0 || playerPosition.y > height

	def isPlayerOnObstacle: Boolean = isOnObstacle(playerPosition)

	def isOnObstacle(position: Position): Boolean = {
		snake.filter { cell: Cell =>
			(cell.x == position.x) && (cell.y == position.y)
		}.length > 1
	}

}

class Cell(val x: Int, val y: Int, val lifeTime: Int)

class Position(val x: Int, val y: Int) {

	def ==(pos: Position) = pos.x == x && pos.y == y

	def +(move: Tuple2[Int, Int]) = new Position(x + move._1, y + move._2)

}
