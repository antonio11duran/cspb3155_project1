package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



/* Function emulateSingleInstruction
    Given a list of doubles to represent a stack
            a map from string to double precision numbers for the environment
    and   a single instruction of type StackMachineInstruction
    Return a tuple that contains the
            modified stack that results when the instruction is executed.
            modified environment that results when the instruction is executed.

    Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
    being executed. Division by zero, log of a non negative number
    Throw an exception or assertion violation when error happens.
    */
def emulateSingleInstruction(stack: List[Double],
                                env: Map[String, Double],
                                ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
    ins match {
        // no exceptions
        case PushI(f) => (f :: stack, env)
        // // exception if 's' is not in env
        case StoreI(s) => {
            env.get(s) match {
                case Some(result) => ((result :: stack), env)
                case None => throw new IllegalArgumentException("Environment identifier does not exist!")
            }
        }
        // // exception if stack is empty
        case PopI => {
            stack match {
                case head :: tail => (tail, env)
                case _ => throw new IllegalArgumentException("Stack is empty!")
            }
        }
        case ExpI => {
            stack match {
                case head :: tail => ((Math.exp(head) :: tail), env)
                case _ => throw new IllegalArgumentException("Stack is empty!")
            }
        }
        case LogI => {
            stack match {
                case head :: tail if (head > 0) => ((Math.log(head) :: tail), env)
                case head :: tail if (head < 1) => throw new IllegalArgumentException("Log error with non positive number!")
                case _ => throw new IllegalArgumentException("Stack is empty!")
            }
        }
        case SinI => {
            stack match {
                case head :: tail => ((Math.sin(head) :: tail), env)
                case _ => throw new IllegalArgumentException("Stack is empty!")
            }
        }
        case CosI => {
            stack match {
                case head :: tail => ((Math.cos(head) :: tail), env)
                case _ => throw new IllegalArgumentException("Stack is empty!")
            }
        }
        // exception if stack is empty and requires two pops. Recursive call should be able to join under the one poppers
        case LoadI(s) => {
                stack match {
                    case head :: next => (next, env + (s -> head))
                    case _ => throw new IllegalArgumentException(s"No environment variable $s")
                }
        }

        case AddI => {
                stack match {
                    case v1 :: v2 :: tail => ((v1 + v2) :: tail, env)
                    case _ => throw new IllegalArgumentException("Not enough stack elements for AddI")
                }
        }
        case SubI => {
                stack match {
                    case v1 :: v2 :: tail => ((v2 - v1) :: tail, env)
                    case _ => throw new IllegalArgumentException("Not enough stack elements for SubI")
                }
        }
        case MultI => {
                stack match {
                    case v1 :: v2 :: tail => ((v1 * v2) :: tail, env)
                    case _ => throw new IllegalArgumentException("Not enough stack elements for MultI")
                }
        }
        case DivI => {
                stack match {
                    case v1 :: v2 :: tail if (v1 != 0) => ((v2 / v1) :: tail, env)
                    case v1 :: v2 :: tail if (v1 == 0) => throw new IllegalArgumentException("Cannot divide by 0!")
                    case _ => throw new IllegalArgumentException("Not enough stack elements for DivI")
                }
        }
        case _ => (stack, env)
    }
}

/* Function emulateStackMachine
    Execute the list of instructions provided as inputs using the
    emulateSingleInstruction function.
    Use foldLeft over list of instruction rather than a for loop if you can.
    Return value must be the final environment.

    Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
            initial value of this accumulator must be (Nil, Map.empty)
            You should use emulateSingleInstruction to update the accmulator.
            It will all fit nicely once you figure it out.
    */
def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] =
    {
        val stack = List.empty[Double]
        val env = Map.empty[String, Double]
        val finalStateEnv = instructionList.foldLeft((stack, env)) {
                case ((stack, env), instruction) =>
                    emulateSingleInstruction(stack, env, instruction)
        }
        finalStateEnv._2
    }
}