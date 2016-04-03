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

import console.Application
import console.commands.{CloneCommandBuilder, HelpCommandBuilder}

/**
 * Main
 *
 * @author Elliot Wright <elliot@elliotwright.co>
 */
object Main extends App {
  new Application(List(new HelpCommandBuilder(), new CloneCommandBuilder()))
    .run(args)
}
