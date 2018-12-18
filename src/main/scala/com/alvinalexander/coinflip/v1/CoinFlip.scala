package com.alvinalexander.coinflip.v1

import CoinFlipUtils._
import scala.annotation.tailrec
import scala.util.Random

case class GameState(numFlips: Int, numCorrect: Int)

object CoinFlip extends App {

    newGameLoop(Nil)

    @tailrec
    def newGameLoop(gamesHistory: List[GameState]): Unit = {
        showNewGamePrompt(gamesHistory)
        val userInput = getUserInput()

        userInput match {
            case "N" =>
                val r = Random
                val s = GameState(0, 0)
                val lastGameState = mainLoop(s, r)
                newGameLoop(lastGameState :: gamesHistory)
            case _ =>
                printExitGame()
        }
    }

    @tailrec
    def mainLoop(gameState: GameState, random: Random): GameState = {

        showPrompt()
        val userInput = getUserInput()

        // handle the result
        userInput match {
            case "H" | "T" => {
                val coinTossResult = tossCoin(random)
                val newNumFlips = gameState.numFlips + 1
                if (userInput == coinTossResult) {
                    val newNumCorrect = gameState.numCorrect + 1
                    val newGameState = gameState.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
                    printGameState(printableFlipResult(coinTossResult), newGameState)
                    mainLoop(newGameState, random)
                } else {
                    val newGameState = gameState.copy(numFlips = newNumFlips)
                    printGameState(printableFlipResult(coinTossResult), newGameState)
                    mainLoop(newGameState, random)
                }
            }
            case _   => {
                printGameOver()
                printGameState(gameState)
                gameState
                // return out of the recursion here
            }
        }
    }

}
