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

import console._

trait Command[C] extends InputParameters[C] {
  val name: String
  def execute(input: C): Unit

  def run(context: Application.RunContext, input: List[ParsedInputParameter]): Unit = {
    val configWithArgs = mapInputToConfig(context, input)

    execute(configWithArgs)
  }
}