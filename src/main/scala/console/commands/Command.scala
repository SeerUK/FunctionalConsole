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

package console.commands

import console.Application.RunContext
import console._

trait Command[C] extends InputParameters[C] {
  val name: String
  def parameters(): InputDefinition
  def execute(input: C): Unit

  def run(context: Application.RunContext, input: List[ParsedInputParameter]): Unit = {
    val configWithArgs = mapInputToConfig(context, input)

    execute(configWithArgs)
  }

  private[this] def mapInputToConfig(runContext: RunContext, input: List[ParsedInputParameter]): C = {
    val parsedArguments = input.collect { case arg: ParsedInputArgument => arg }
    val parsedOptions = input.collect { case opt: ParsedInputOption => opt }

    case class MapContext(runContext: RunContext, config: C)

    val context: MapContext = parameters().parameters
      .foldLeft(MapContext(runContext, initialState))((acc, param) => {
        param match {
          case a: InputArgument[_, C] =>
            val context = acc.runContext.copy(argumentIndex = acc.runContext.argumentIndex + 1)
            val config = a.callAction(acc.config, parsedArguments.lift(acc.runContext.argumentIndex))
            MapContext(context, config)
          case o: InputOption[_, C] =>
            val config = o.callAction(acc.config, parsedOptions.find(_.name == o.name))
            MapContext(acc.runContext, config)
          case _ =>
            // Something went really wrong...
            // Could use the context object to keep track of issues like this without halting
            // execution instantly or impure functions
            acc
        }
      })

    context.config
  }
}