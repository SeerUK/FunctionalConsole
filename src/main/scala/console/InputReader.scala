/**
 * This file is part of the "SeerUK/console-lab" project.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the LICENSE is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package console

sealed trait InputArgumentReader[A] { self =>
  def read: (A, Option[ParsedInputArgument]) => A
}

sealed trait InputOptionReader[A] { self =>
  def read: (A, Option[ParsedInputOption]) => A
}

object InputArgumentReader {
  type ReaderFunc[A] = (A, Option[ParsedInputArgument]) => A

  def read[A: ValueReader](f: ReaderFunc[A]): InputArgumentReader[A] = new InputArgumentReader[A] {
    val read = f
  }

  implicit val readBool: InputArgumentReader[Boolean] = read[Boolean](reader())
  implicit val readDouble: InputArgumentReader[Double] = read[Double](reader())
  implicit val readInt: InputArgumentReader[Int] = read[Int](reader())
  implicit val readString: InputArgumentReader[String] = read[String](reader())

  private def reader[A: ValueReader](): ReaderFunc[A] = {
    (default: A, input: Option[ParsedInputArgument]) => {
      val reader = implicitly[ValueReader[A]]

      input match {
        case Some(argument) => reader.read(argument.value)
        case _ => default
      }
    }
  }
}

object InputOptionReader {
  type ReaderFunc[A] = (A, Option[ParsedInputOption]) => A

  def read[A: ValueReader](f: ReaderFunc[A]): InputOptionReader[A] = new InputOptionReader[A] {
    val read = f
  }

  implicit val readBool: InputOptionReader[Boolean] = read[Boolean](reader(emptyVal = Some(true)))
  implicit val readDouble: InputOptionReader[Double] = read[Double](reader())
  implicit val readInt: InputOptionReader[Int] = read[Int](reader())
  implicit val readString: InputOptionReader[String] = read[String](reader())

  private def reader[A: ValueReader](emptyVal: Option[A] = None): ReaderFunc[A] = {
    (default: A, input: Option[ParsedInputOption]) => {
      val reader = implicitly[ValueReader[A]]

      input match {
        case Some(option) => option.value match { // Option present in input
          case Some(value) => reader.read(value) // Actual option value (i.e. --foo=value)
          case _ => emptyVal match { // No value present
            case Some(value) => value // Defined empty value
            case _ => default // No empty value defined, use default
          }
        }
        case _ => default // Option not present in input
      }
    }
  }
}

sealed trait ValueReader[A] { self =>
  def read: String => A
}

object ValueReader {
  def read[A](f: String => A): ValueReader[A] = new ValueReader[A] {
    val read = f
  }

  implicit val intRead: ValueReader[Int] = read { _.toInt }
  implicit val stringRead: ValueReader[String] = read { identity }
  implicit val doubleRead: ValueReader[Double] = read { _.toDouble }
  implicit val boolRead: ValueReader[Boolean] = read {
    _.toLowerCase match {
      case "true" => true
      case "false" => false
      case "y" => true
      case "n" => false
      case "yes" => true
      case "no" => false
      case "1" => true
      case "0" => false
      case invalidInput =>
        // @todo: BooleanFormatException? (Similar to if toInt throws)
        throw new Exception(s"'$invalidInput' is not a boolean.")
    }
  }
}
