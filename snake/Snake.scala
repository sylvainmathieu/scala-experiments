package snake

import swing.{SimpleSwingApplication, MainFrame, Panel}

object App extends SimpleSwingApplication {

	import java.awt.{Dimension, Graphics2D}
	import java.awt.{Color}
	import java.awt.event.{ActionEvent}
	import javax.swing.{Timer, AbstractAction}

	var game = Game.newGame

	override def top = frame

	val frame = new MainFrame {
		title = "Scala Snake"
		contents = mainPanel

		lazy val mainPanel = new Panel() {
			focusable = true
			background = Color.black
			preferredSize = new Dimension(200, 200)

			override def paint(g: Graphics2D) {
				g.setColor(Color.black)
				g.fillRect(0, 0, size.width, size.height)
				onPaint(g)
			}
		}

		val timer = new Timer(100, new AbstractAction() {
			override def actionPerformed(e: ActionEvent) {
				game = game.tick
			}
		})

		timer.start

	}

	def onPaint(g: Graphics2D) {

	}

}

object Game {

	def newGame = new Game

}

class Game {

	def tick: Game = {
		this
	}

}
