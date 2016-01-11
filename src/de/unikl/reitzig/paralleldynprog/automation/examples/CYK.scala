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

/**
 * Parses a string with respect to a grammar in CNF
 * Warning: Kills compiler
 */
object CYK {
  def cyk(input : String, g : CNFGrammar) = innercyk(input, g)() contains g.start

  @DynamicProgramming
  private def innercyk(input : String, g : CNFGrammar)(i : Int = input.length - 1, j : Int = 0) : Set[Grammar.N] = {
    (i,j) match {
      case (0,j) => g.nonterminals filter (g.trules contains (_,input(j)))
      case (i,j) if j + i < input.length => {
        (1 to i) map { k : Int =>
          g.nonterminals filter { n =>
            g.nrules exists { case (n0, (n1,n2)) =>
               (n0 == n) &&
               ({U;  innercyk(input, g)(i-k,j)}       contains n1) &&
               ({UR; innercyk(input, g)(k-1,j+i-k+1)} contains n2)
            }
          }
        } reduce (_ ++ _)
      }
      case _ => Set() : Set[Grammar.N]
    }
  }

  def printMatrix(input : String, g : CNFGrammar) {
    (0 until input.length) foreach { i =>
      (0 until input.length - i) foreach { j =>
        print("\t" + innercyk(input.substring(j, j+i+1), g)())
      }
      print("\n")
    }
  }
}


import Grammar._
class CNFGrammar(val terminals : Set[T], val nonterminals : Set[N], val start : N,
              val trules : Set[(N, T)], val nrules : Set[(N,(N,N))]) {
  assert(nonterminals contains start)
  assert(trules forall { case (n,t) => (nonterminals contains n) && (terminals contains t) })
  assert(nrules forall { case (n1,(n2,n3)) => (Set(n1,n2,n3) subsetOf nonterminals) })
}

object Grammar {
  type T = Char
  final case class N(val s : String)

  val f : (Int, Int) => Set[Grammar.N] = ((_,_) => Set[Grammar.N]())
}

object CykTest {
  def main(args : Array[String]) {
    val g = new CNFGrammar(Set('(', ')', '|'), Set(N("S"),N("S("),N("S)"),N("S|"),N("S1"),N("S2")), N("S"),
                           Set( (N("S("), '('), (N("S)"), ')'), (N("S|"), '|'), (N("S"), '|')),
                           Set( (N("S"),(N("S("),N("S1"))),  (N("S"),(N("S|"),N("S"))),
                                (N("S1"),(N("S"),N("S2"))),  (N("S1"),(N("S"),N("S)"))),
                                (N("S2"),(N("S)"),N("S"))) )
                          )

    val inputs = Seq("(|)(|)", "((|))(||)|", "(|(||)|(((|)))||)(|)(||(|))|", // true
                     "(|)(", "(a)", "(|(||)|(((|)))||))(|)(||(|))|")      // false

    println(g.nonterminals filter { n =>
            g.nrules exists { case (n0, (n1,n2)) => n0 == n && n1 == N("S") && n2 == N("S)") } })

    inputs foreach { s =>
      println(s + ": " + CYK.cyk(s, g))
      CYK.printMatrix(s, g)
    }
  }
}
