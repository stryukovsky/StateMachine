import java.util.Scanner
import scala.collection.mutable
import scala.io.Source

case class ResultState(id: Int, isFinal: Boolean)

case class ParsedInstruction(fromState: Int, toState: ResultState, transactionalSymbol: Char)
object Main {
  private type StateMap = mutable.HashMap[Char, mutable.HashMap[Int, ResultState]]

  private def parseInstruction(instruction: String): ParsedInstruction = {
    val splitInstruction = instruction.split('=')
    val transactionalParamsSource = splitInstruction(0).split(',')
    val fromState = transactionalParamsSource(0).substring(1).toInt // trim 'q' letter
    val transactionalCharacter = transactionalParamsSource(1)(0) // take first char since it is the only one
    val resultStateSource = splitInstruction(1)
    val isFinite = resultStateSource(0) == 'f' // it is finite state if flag is zero
    val toStateId = resultStateSource.substring(1).toInt // trim 'f' or 'q' letter
    ParsedInstruction(fromState, ResultState(toStateId, isFinite), transactionalCharacter)
  }

  private def prepareStateMap(instructions: List[ParsedInstruction]): StateMap = {
    val result = mutable.HashMap.empty[Char, mutable.HashMap[Int, ResultState]]
    instructions.foreach(instruction => {
      if (result.contains(instruction.transactionalSymbol)) {
        result(instruction.transactionalSymbol).addOne(instruction.fromState -> instruction.toState)
      } else {
        val hashMap = mutable.HashMap.empty[Int, ResultState]
        hashMap.addOne(instruction.fromState -> instruction.toState)
        result.addOne(instruction.transactionalSymbol -> hashMap)
      }
    })
    result
  }

  private def describeStateMap(stateMap: StateMap) = {
    for ((symbol, controlMap) <- stateMap) {
      for ((fromState, toState) <- controlMap) {
        print(s"q$fromState,$symbol=")
        if (toState.isFinal)
          print('f')
        else
          print('q')
        println(s"${toState.id}")
      }
    }
  }
  private def parse(filename: String): StateMap = {
    val source = Source.fromFile(filename)
    val instructionsSource = source.getLines().toList
    source.close()
    val instructions = instructionsSource.map(parseInstruction)
    prepareStateMap(instructions)
  }

  private def execute(stateMap: StateMap, program: String): Unit = {
    var currentState = 0
    program.foreach(symbol => {
      println(s"Operation symbol $symbol")
      if (!stateMap.contains(symbol)) {
        println(s"Program is invalid: symbol $symbol is not defined")
        return
      }
      if (!stateMap(symbol).contains(currentState)) {
        println(s"Program is invalid: cannot move from state $currentState with transactional symbol $symbol")
        return
      }
      val resultState = stateMap(symbol)(currentState)
      println(s"State move $currentState -> ${resultState.id} ${if (resultState.isFinal) "(final)"}")
      if (resultState.isFinal) {
        println(s"Execution complete: final state ${resultState.id}")
        return
      }
      currentState = resultState.id
    })
  }
  def main(args: Array[String]): Unit = {
    val stateMap = parse("example.txt")
    println("Applying determination to state machine")
    describeStateMap(stateMap)
    val scanner = new Scanner(System.in)
    while (true) {
      println("Input your program")
      val program = scanner.nextLine()
      execute(stateMap, program)
    }
  }
}
