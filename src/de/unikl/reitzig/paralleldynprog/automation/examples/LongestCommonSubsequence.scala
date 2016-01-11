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

package de.unikl.reitzig.paralleldynprog.automation.examples
import de.unikl.reitzig.paralleldynprog.automation._

object LongestCommonSubsequence {
  @DynamicProgramming
  def lcs(a : String, b : String)(i : Int = a.length, j : Int = b.length) : Int = {
    (i,j) match {
      case (0,_) | (_,0)             => 0
      case (i,j) if a(i-1) == b(j-1) => lcs(a,b)(i-1,j-1) + 1
      case (i,j)                     => lcs(a,b)(i,j-1) max lcs(a,b)(i-1,j)
    }
  }
}

object LCSTest {
  def main(args : Array[String]) {
    val tests = Seq(("monkey", "long", 2),
                    ("cape", "ape", 3),
                    ("class", "method", 0),
                    ("abcddeecba", "fabcfcbaf", 6))

    tests foreach { case (a,b,target) =>
      println(a + " vs " + b + ":\t" + LongestCommonSubsequence.lcs(a,b)() + "\t (should be: " + target + ")")
    }
  }
}
