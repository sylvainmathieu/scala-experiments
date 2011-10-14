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

	var game = new Game(new Grid(20, 20))

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
				game.tick
				repaint()
			}
		})

		timer.start

		def stopTimer = timer.stop;

	}

	def onPaint(g: Graphics2D) {

		def drawBlock(x: Int, y: Int) {
			g.draw(new Rectangle(x * 10 + 1, y * 10 + 1, 8, 8))
		}

		if (game.isOver) {
			frame.stopTimer
		}

		g.setColor(Color.gray)

		game.grid.forEach { (x: Int, y: Int) =>
			if (game.grid.get(x, y) > 0) drawBlock(x, y)
		}

		g.setColor(Color.green)

		drawBlock(game.foodPosition.x, game.foodPosition.y)

	}

}

class Game(val grid: Grid) {

	var currentMove = (1, 0)
	var nextMove = (1, 0)
	var currentPosition = new Position(10, 10)
	var isOver = false
	var foodPosition = grid.popFood
	var lifeTime = 10

	def tick = {
		currentMove = nextMove
		currentPosition = currentPosition.move(currentMove)

		grid.forEach { (x: Int, y: Int) =>
			grid.set(x, y, grid.get(x, y) - 1)
		}

		if (currentPosition == foodPosition)
			eat
		if (currentPosition.isOut(grid) || currentPosition.isOnObstacle(grid))
			isOver = true
		else grid.pop(currentPosition, lifeTime)

	}

	def moveBy(move: Tuple2[Int, Int]) = {
		if (move != (currentMove._1 * (-1), currentMove._2 * (-1)))
			nextMove = move
	}

	def eat = {
		lifeTime += 1
		foodPosition = grid.popFood
	}

}

class Grid(val width: Int, val height: Int) {

	var grid = {
		(for (y <- 0 until height)
			yield
				(for (x <- 0 until width)
					yield 0
				).toArray
		).toArray
	}

	def forEach(func: (Int, Int) => Any) {
		for (y <- 0 until height; x <- 0 until width)
			func(x, y)
	}

	def set(x: Int, y: Int, value: Int) = grid(x)(y) = value

	def get(x: Int, y: Int): Int = grid(x)(y)

	def pop(pos: Position, lifeTime: Int) = set(pos.x, pos.y, lifeTime)

	def popFood: Position = {

		val rand = new Random()

		def foodPosition: Position = {
			val pos = new Position(rand.nextInt(width), rand.nextInt(height))
			if (pos.isOnObstacle(this)) foodPosition
			return pos
		}

		return foodPosition

	}

}

class Position(val x: Int, val y: Int) {

	def ==(pos: Position) = pos.x == x && pos.y == y

	def move(move: Tuple2[Int, Int]) = new Position(x + move._1, y + move._2)

	def isOut(grid: Grid): Boolean = x < 0 || x >= grid.width || y < 0 || y >= grid.height

	def isOnObstacle(grid: Grid): Boolean = grid.get(x, y) > 0

}
