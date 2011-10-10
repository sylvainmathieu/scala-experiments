package snake

import swing.{SimpleSwingApplication, MainFrame, Panel}

object App extends SimpleSwingApplication {

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

		game.grid.set(5, 10, 1)

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

	def tick: Game = {
		this
	}

	def move = {

	}

}

class Grid(width: Int, height: Int) {

	var grid = (for (y <- 0 until height) yield (for (x <- 0 until width) yield 0).toArray).toArray

	def forEach(func: (Int, Int) => Any) {
		for (y <- 0 until height)
			for (x <- 0 until width)
				func(x, y)
	}

	def set(x: Int, y: Int, value: Int) = grid(x)(y) = value

	def get(x: Int, y: Int): Int = grid(x)(y)
}
