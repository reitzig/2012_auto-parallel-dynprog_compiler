/*
 * This file is part of Parallel Dynamic Programming Automation Plugin Prototype (PDPAPP).
 *
 * PDPAPP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * PDPAPP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with PDPAPP.  If not, see <http://www.gnu.org/licenses/>.
 */

package de.unikl.reitzig.paralleldynprog.automation.examples;
import de.unikl.reitzig.paralleldynprog.automation._

object BellmanFord {
  /**
   * Computes the length of the shortest path from node with index `start` to
   * node with index `n-1` according to the graph given by cost matrix `costs`;
   * `n == costs.length`. It is assumed that `costs` is square.
   */
  @DynamicProgramming
  def shortestPath[T](costs : Array[Array[Option[Int]]], start : Int)
                     (i : Int = costs.length, j : Int = costs.length - 1) : Option[Int] = {
    (i,j) match {
      case (0,s) if s == start => Some(0)
      case (0,s) if s != start => None
      case (i,j) => ((0 until costs.length) map { k =>
          ({UL; U; UR; shortestPath(costs, start)(i-1,k)}, costs(k)(j)) match {
            case (Some(l),Some(c)) => Some(l + c)
            case _                 => None
          }
        }).flatten match {
          case Seq() => None
          case s     => Some(s.min)
        }
    }
  }
}

object BellmanFordTest {
  def main(args : Array[String]) {
    val exampleCost : Array[Array[Option[Int]]] = Array(
      Array(Some(0), Some(3), Some(1), None,    Some(5)),
      Array(None,    Some(0), None,    None,    Some(1)),
      Array(None,    Some(1), Some(0), None,    Some(2)),
      Array(None,    None,    None,    None,    None   ),
      Array(None,    None,    None,    None,    Some(0)))

    println("From 0 to 4: " + BellmanFord.shortestPath(exampleCost, 0)() + "\t(should be: 3)")
    println("From 1 to 4: " + BellmanFord.shortestPath(exampleCost, 1)() + "\t(should be: 1)")
    println("From 2 to 4: " + BellmanFord.shortestPath(exampleCost, 2)() + "\t(should be: 2)")
    println("From 4 to 4: " + BellmanFord.shortestPath(exampleCost, 3)() + "\t(should be: None)")
    println("From 4 to 4: " + BellmanFord.shortestPath(exampleCost, 4)() + "\t(should be: 0)")
  }
}
