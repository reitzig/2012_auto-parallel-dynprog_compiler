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
package de.unikl.reitzig.paralleldynprog.runtime

/**
 * Interface for dynamic programming solver implementations.
 * @author Raphael Reitiz, 2012
 */
trait Solver {
  /**
   * Fills the given array according to the specified function.
   * @param arr The memoisation matrix to be filled
   * @param f   The cell function, that is the function that is used to
   *            compute array elements from other array elements.
   */
  def apply[T](arr : Array[Array[Option[T]]], f : (Int,Int) => T) : Unit
}
