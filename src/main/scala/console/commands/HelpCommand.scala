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

import console.{Application, InputDefinition}

class HelpCommand(application: Application) extends Command[HelpConfig] {
  override val initialState = HelpConfig()
  override val name = "help"

  override def parameters(): InputDefinition = {
    new InputDefinition()
      .withParameter(opt[Boolean]("full", false).withAction((config, value) => config.copy(full = value)))
  }

  override def execute(input: HelpConfig): Unit = {
    println(input)
  }
}

case class HelpConfig(full: Boolean = false)

class HelpCommandBuilder extends CommandBuilder[HelpCommand] {
  def build(application: Application): HelpCommand = new HelpCommand(application)
}