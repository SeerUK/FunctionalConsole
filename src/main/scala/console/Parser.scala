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

/**
 * Parser
 *
 * @author Elliot Wright <elliot@elliotwright.co>
 */
class Parser {
  def parse(input: Array[String]): List[ParsedInputParameter] = {
    input.toList.map {
      case option if option.startsWith("--") || option.startsWith("-") =>
        parseInputOption(option)
      case argument =>
        new ParsedInputArgument(argument)
    }
  }

  private def parseInputOption(option: String): ParsedInputOption = {
    val dropped = option match {
      case opt if opt.startsWith("--") => opt.drop(2)
      case opt if opt.startsWith("-") => opt.drop(1)
      case _ => option
    }

    val (name, value) = dropped.split("=", 2) match {
      case Array(name: String, value: String) =>
        (name, Some(value))
      case Array(name: String) =>
        (name, None)
      case _ =>
        // This should not be reachable, but if there's some kind of critical failure:
        throw new RuntimeException("Invalid parameter specified: '%s'".format(option))
    }

    new ParsedInputOption(name, value)
  }
}

sealed trait ParsedInputParameter
case class ParsedInputArgument(value: String) extends ParsedInputParameter
case class ParsedInputOption(name: String, value: Option[String] = None) extends ParsedInputParameter