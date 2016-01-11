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
package de.unikl.reitzig.paralleldynprog.automation

/**
 * Marker objects for recursive calls
 * @author Raphael Reitzig, 2012
 */
sealed abstract class Area() {
  def apply() {}
}
case object L  extends Area
case object UL extends Area
case object U  extends Area
case object UR extends Area
case object Other extends Area
object Area {
  def apply(a : String) = {
    a match {
      case "L"  => L
      case "UL" => UL
      case "U"  => U
      case "UR" => UR
      case _    => Other
    }
  }
}

/**
 * Values to be used with @link{DynamicProgramming}.
 */
sealed abstract class Case()
case object Case1 extends Case
case object Case2 extends Case
case object Case3 extends Case
case object Detect extends Case
object Case {
  def apply(s : String) = {
    s match {
      case "Case1" => Case1
      case "Case2" => Case2
      case "Case3" => Case3
      case "Detect" => Detect
    }
  }
}

/**
 * Annotation that marks a method as dynamic programming recursion
 */
class DynamicProgramming(val use : Case = Detect) extends scala.annotation.StaticAnnotation
