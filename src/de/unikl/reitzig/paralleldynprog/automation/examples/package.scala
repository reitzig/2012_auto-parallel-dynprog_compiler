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

import scala.util.Random

package object examples {
  private val SYMB = Array('a','b','c','d','e','f','g','h','i','j','k','l','m',
                           'n','o','p','q','r','s','t','u','v','w','x','y','z')

  {
    val seed = System.currentTimeMillis();
    println("Seed for random input generation: " + seed);
    Random.setSeed(seed);
  }

  def randomStringBetween(minLength : Int, maxLength : Int) : String = {
    randomStringOf(Random.nextInt(maxLength - minLength) + minLength)
  }

  def randomStringOf(length : Int) : String = {
    val res = StringBuilder.newBuilder
    (0 until length) foreach { _ =>
      res.append(SYMB(Random.nextInt(26)))
    }
    res.toString()
  }
}
