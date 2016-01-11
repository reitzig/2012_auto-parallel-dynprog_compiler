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
import java.util.concurrent.CountDownLatch

/**
 * Implements diagonal frontier scheme. See thesis and corresponding prototype for details.
 * @author Raphael Reitzig, 2012
 */
object BlockCheck extends Solver {
  private val blockSize = 100 // TODO parametrise somehow

  override def apply[T](arr : Array[Array[Option[T]]], f : (Int,Int) => T) {
    val p = Runtime.getRuntime().availableProcessors() // TODO parametrise somehow
    val endGate = new CountDownLatch(p);

    (0 until p) foreach { i =>
      new Thread(new Runnable() {
        override def run() {
          fillTable(arr, f, i, p);
          endGate.countDown();
        }
      }).start();
    }

    try {
      endGate.await();
    }
    catch {
      case e : InterruptedException => e.printStackTrace();
    }
  }

  def fillTable[T](arr : Array[Array[Option[T]]], f : (Int,Int) => T, w : Int, p : Int) {
    val param   = Array(0,0)
    val dim = Array(arr.length, arr(0).length)
    val k = if ( blockSize > 0 ) { blockSize } else { dim(1)/(p + 1) + 1 }
    var checki, checkj, offset, j = 0
    var i = w

    while ( i < dim(0) ) {
      checki = 0 max (i - 1)
      offset = 0

      while ( offset < dim(1) ) {
        // Wait until current block is computable
        checkj = (dim(1) - 1) min (offset + k - 1)
        while ( i != 0 && arr(checki)(checkj) == None ) { Thread.`yield`() }
        j = offset
        while ( j < (dim(1) min (offset + k)) ) {
          arr(i)(j) = Some(f(i,j))
          j += 1
        }

        offset += k
      }
      i += p
    }
  }
}
