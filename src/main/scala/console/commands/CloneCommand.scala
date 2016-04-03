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

import console.{Application, ApplicationConfig, InputDefinition}

/**
 * CloneCommand
 *
 * @author Elliot Wright <elliot@elliotwright.co>
 */
class CloneCommand extends Command[CloneConfig] {
  override val initialState = CloneConfig()
  override val name = "clone"
  override val definition = new InputDefinition()
    .withParameter(arg[String]("source", "").withAction((config, value) => config.copy(source = value)))
    .withParameter(arg[String]("dest", "").withAction((config, value) => config.copy(dest = value)))
    .withParameter(opt[Boolean]("no-clean", false).withAction((config, value) => config.copy(noClean = value)))

  override def execute(input: CloneConfig): Unit = {
    println(input)
  }
}

case class CloneConfig(source: String = "", dest: String = "", noClean: Boolean = false)

class CloneCommandBuilder extends CommandBuilder[CloneCommand] {
  def build(application: Application, appInput: ApplicationConfig): CloneCommand =
    new CloneCommand()
}