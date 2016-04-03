/**
 * This file is part of the "FunctionalConsole" project.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the LICENSE is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package console

import console.commands.{Command, CommandBuilder}

case class Application(
    commandBuilders: List[CommandBuilder[_ <: Command[_]]])
  extends InputParameters[ApplicationConfig] {

  override val initialState = ApplicationConfig()

  val initialContext = Application.RunContext(1)
  val definition = new InputDefinition()
    .withParameter(arg[String]("command", "").withAction((config, value) => config.copy(command = value)))
    .withParameter(opt[Boolean]("help", false).withAction((config, value) => config.copy(help = value)))
    .withParameter(opt[Int]("verbosity", 1).withAction((config, value) => config.copy(verbosity = value)))

  def run(input: Array[String]): Unit = {
    val commands = commandBuilders.map(_.build(this, initialContext))
    val parser = new Parser()
    val parsed = parser.parse(input)
    val command = findCommand(commands, parsed)

    command.run(initialContext, parsed)
  }

  private def findCommand(commands: List[Command[_]], input: List[ParsedInputParameter]): Command[_] = {
    val maybeCommand = for {
      commandArg <- input.collect { case arg: ParsedInputArgument => arg }.headOption
      command <- commands.find(_.name == commandArg.value)
    } yield command

    maybeCommand match {
      case Some(command) => command
      case _ => throw new RuntimeException("Couldn't find a command to run") // @todo: use either
    }
  }
}

case class ApplicationConfig(command: String = "", help: Boolean = false, verbosity: Int = 1)

object Application {
  case class RunContext(argumentIndex: Int)
}