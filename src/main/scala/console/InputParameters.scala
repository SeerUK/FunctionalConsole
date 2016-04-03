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

trait InputParameters[C] {
  def initialState: C

  def arg[T: InputArgumentReader: ValueReader](name: String, default: T): InputArgument[T, C] =
    new InputArgument[T, C](name, default)

  def opt[T: InputOptionReader: ValueReader](name: String, default: T): InputOption[T, C] =
    new InputOption[T, C](name, default)
}

trait InputParameter[T, C, P] {
  val name: String
  val action: (C, T) => C

  def callAction(config: C, param: Option[P]): C
  def withAction(f: (C, T) => C): InputParameter[T, C, P]
}

case class InputArgument[T: InputArgumentReader: ValueReader, C](
    name: String,
    default: T,
    action: (C, T) => C)
  extends InputParameter[T, C, ParsedInputArgument] {

  final val reader = implicitly[InputArgumentReader[T]]

  def this(token: String, default: T) =
    this(token, default, { (c, v) => c })

  override def callAction(config: C, arg: Option[ParsedInputArgument]): C = {
    action(config, reader.read(default, arg))
  }

  override def withAction(f: (C, T) => C): InputArgument[T, C] = {
    copy(action = f)
  }
}

case class InputOption[T: InputOptionReader: ValueReader, C](
    name: String,
    default: T,
    action: (C, T) => C)
  extends InputParameter[T, C, ParsedInputOption] {

  final val reader = implicitly[InputOptionReader[T]]

  def this(token: String, default: T) =
    this(token, default, { (c, v) => c })

  override def callAction(config: C, opt: Option[ParsedInputOption]): C = {
    action(config, reader.read(default, opt))
  }

  override def withAction(f: (C, T) => C): InputOption[T, C] = {
    copy(action = f)
  }
}
