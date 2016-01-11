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
import de.unikl.reitzig.paralleldynprog.automation.{DynamicProgramming,L,UL,U,UR}
import de.unikl.reitzig.paralleldynprog.automation.Case1
import de.unikl.reitzig.paralleldynprog.automation.Detect

/**
 * The infamous edit distance.
 * @author Raphael Reitzig, 2012
 */
object EditDistance {
  /**
   * Recursive version that is rewritten by our plugin
   */
  @DynamicProgramming
  def editd(a : String, b : String)(i : Int = a.length, j : Int = b.length) : Int = {
    (i,j) match {
      case (0,0) => 0
      case (0,j) => j
      case (i,0) => i
      case (i,j) => ({ /* U; */ editd(a,b)(i-1,j) } + 1) min
                    ({ /* L; */ editd(a,b)(i,j-1) } + 1) min
                    ({ /* UL;*/ editd(a,b)(i-1,j-1) } + (if (a(i-1) != b(j-1)) 1 else 0))
    }
  }
}

/**
 * The infamous edit distance, implemented in various variants.
 * @author Raphael Reitzig, 2012
 */
object EditDistanceTest {
  /**
   * Recursive version. Will have exponential runtime.
   */
  def compute(a : String, b : String)(i : Int = a.length, j : Int = b.length) : Int = {
    (i,j) match {
      case (0,0) => 0
      case (0,j) => j
      case (i,0) => i
      case (i,j) => (compute(a,b)(i-1,j) + 1) min
                    (compute(a,b)(i,j-1) + 1) min
                    (compute(a,b)(i-1,j-1) + (if (a(i-1) != b(j-1)) 1 else 0))
    }
  }

  /**
   * What the plugin should output
   */
  def targetcompute(a : String, b : String)() : Int = {
    val arr = Array.fill[Option[Int]](a.length + 1, b.length + 1)(None)
    val f = (i : Int, j : Int) => (i,j) match {
      case (0,0) => 0
      case (0,j) => j
      case (i,0) => i
      case (i,j) => (arr(i-1)(j).get + 1) min
                    (arr(i)(j-1).get + 1) min
                    (arr(i-1)(j-1).get + (if (a(i-1) != b(j-1)) 1 else 0))
    }

    de.unikl.reitzig.paralleldynprog.runtime.BlockCheck(arr, f);
    arr(a.length)(b.length).get
  }

  /**
   * A hand-written row by row table filling.
   */
  def nativecompute(a : String, b : String) : Int = {
    val arr = Array.fill[Int](a.length+1, b.length+1)(-1)
    var i, j = 0

    while ( i <= a.length ) {
      j = 0
      while ( j <= b.length ) {
        if ( i == 0 && j == 0) {
          arr(i)(j) = 0
        }
        else if ( i == 0 ) {
          arr(i)(j) = j
        }
        else if ( j == 0 ) {
          arr(i)(j) = i
        }
        else {
          arr(i)(j) = (arr(i-1)(j) + 1) min
                      (arr(i)(j-1) + 1) min
                      (arr(i-1)(j-1) + (if (a(i-1) != b(j-1)) 1 else 0))
        }
        j += 1
      }
      i += 1
    }

    arr(a.length)(b.length)
  }

  /*
  def main(args : Array[String]) {
    println("Testing dynamic programming transformation of " + this.getClass().getSimpleName())

    (1 to 100) foreach { _ =>
      val a = randomStringBetween(5, 10)
      val b = randomStringBetween(5, 10)
      val resA = compute(a, b)()
      val resB = pcompute(a,b)()
      val resC = targetcompute(a, b)()
      val resN = nativecompute(a, b)

      assert(resA == resB, "Naive and rewritten disagree")
      assert(resA == resC, "Naive and static disagree")
      assert(resB == resC, "Rewritten and static disagree")
      assert(resB == resN, "Rewritten and native disagree")
    }
  } */
}
