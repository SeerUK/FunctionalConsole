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


case class InputDefinition(parameters: List[InputParameter[_, _, _]] = List()) {
  def ++(definition: InputDefinition): InputDefinition = {
    copy(this.parameters ++ definition.parameters)
  }

  def withParameter(parameter: InputParameter[_, _, _]): InputDefinition = {
    copy(this.parameters :+ parameter)
  }
}
