package com.alvinalexander.coinflip.v1

import scala.util.Random
import scala.io.StdIn.readLine

object CoinFlipUtils {

    def showPrompt(): Unit = { print("\n(h)eads, (t)ails, or (q)uit: ") }

    def getUserInput(): String = readLine.trim.toUpperCase

    def printableFlipResult(flip: String): String = flip match {
        case "H" => "Heads"
        case "T" => "Tails"
    }

    def printGameState(printableFlipResult: String, gameState: GameState): Unit = {
        print(s"Flip was $printableFlipResult. ")
        printGameState(gameState)
    }

    def printGameState(gameState: GameState): Unit = {
        println(s"#Flips: ${gameState.numFlips}, #Correct: ${gameState.numCorrect}")
    }

    def printGameOver(): Unit = println("\n=== GAME OVER ===")

    // returns "H" for heads, "T" for tails
    def tossCoin(r: Random): String = {
        val i = r.nextInt(2)
        i match {
            case 0 => "H"
            case 1 => "T"
        }
    }

    def showGameHistory(gameHistory: List[GameState]): Unit = gameHistory foreach printGameState
    def showNewGamePrompt(gameHistory: List[GameState]): Unit = {
        if (gameHistory.length != 0) println("\n=== GAME HISTORY ===")
        showGameHistory(gameHistory)
        println("\n(n)ew game, or (q)uit: ")
    }
    def printNewGame(): Unit = println("\n=== NEW GAME ===")
    def printExitGame(): Unit = println("\n=== EXIT GAME ===")


}
