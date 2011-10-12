package snake

import swing.{SimpleSwingApplication, MainFrame, Panel, event}
import event.{KeyPressed}

object App extends SimpleSwingApplication {
	import event.Key._
	import java.awt.{Dimension, Graphics2D, Rectangle}
	import java.awt.{Color}
	import java.awt.event.{ActionEvent}
	import javax.swing.{Timer, AbstractAction}

	var game = Game.newGame

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
			case Space => game.restart
			case _ =>
		}

		val timer = new Timer(100, new AbstractAction() {
			override def actionPerformed(e: ActionEvent) {
				game = game.tick
				repaint()
			}
		})

		timer.start

	}

	def onPaint(g: Graphics2D) {

		def drawBlock(x: Int, y: Int) {
			g.draw(new Rectangle(x * 10, y * 10, 10, 10))
		}

		g.setColor(Color.gray)

		// Draw grid
		game.grid.forEach { (x: Int, y: Int) =>
			if (game.grid.get(x, y) > 0) drawBlock(x, y)
		}

	}

}

object Game {

	def newGame = {
		new Game(new Grid(20, 20))
	}

}

class Game(val grid: Grid) {

	var currentMove = new Position(1, 0)
	var currentPosition = new Position(10, 10)

	def tick: Game = {
		currentPosition = currentPosition.move(currentMove)

		grid.pop(currentPosition)

		if(!grid.check()) {
			over()
		}

		return this
	}

	def moveBy(x: Int, y: Int) = {
		currentMove = new Position(x, y)
	}

	def over() = {

	}

	def restart() = {

	}

}

object Grid {

}

class Grid(width: Int, height: Int) {

	var grid =
			(for (y <- 0 until height)
				yield
					(for (x <- 0 until width)
						yield 0
					).toArray
			).toArray

	def forEach(func: (Int, Int) => Any) {
		for (y <- 0 until height; x <- 0 until width)
			func(x, y)
	}

	def set(x: Int, y: Int, value: Int) = grid(x)(y) = value

	def get(x: Int, y: Int): Int = grid(x)(y)

	def pop(pos: Position) = set(pos.x, pos.y, 10)

	def check(): Boolean = {
		true
	}

}

class Position(val x: Int, val y: Int) {

	def += (move: Tuple2[Int, Int]) {
		new Position(x + move._1, y + move._2)
	}

	def move(move: Position) = new Position(x + move.x, y + move.y)

}
