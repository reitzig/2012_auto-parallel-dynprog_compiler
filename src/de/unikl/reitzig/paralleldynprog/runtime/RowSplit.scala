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

import java.util.concurrent.CyclicBarrier
import java.util.concurrent.CountDownLatch
import java.util.concurrent.BrokenBarrierException

/**
 * Implements row splitting scheme. See thesis and corresponding prototype for details.
 * @author Raphael Reitzig, 2012
 */
object RowSplit extends Solver {
  private val blockSize = 100 // TODO parametrise somehow

  override def apply[T](arr : Array[Array[Option[T]]], f : (Int,Int) => T) {
    val p = Runtime.getRuntime().availableProcessors() // TODO parametrise somehow
    val rowGate = new CyclicBarrier(p);
    val endGate = new CountDownLatch(p);


    (0 until p) foreach { i =>
    new Thread(new Runnable() {
        override def run() {
          val w = new Worker(i, arr, f, p);
          w.run(rowGate, endGate);
        }
      }).start();
    }

    try {
      endGate.await();
    }
    catch { case e : InterruptedException =>
      e.printStackTrace();
    }
  }

  private class Worker[T](val nr : Int, val arr : Array[Array[Option[T]]], f : (Int, Int) => T, p : Int) {
    private val dim = Array(arr.length, arr(0).length)

    def run(rowGate : CyclicBarrier, endGate : CountDownLatch) {
      val k = if ( blockSize > 0 ) { blockSize } else { dim(1)/p + 1 }
      var i,o,j = 0

      while ( i < dim(0) ) {
        o = nr*k
        while ( o < dim(1) ) {
          j = o
          while ( j < ((o + k) min dim(1)) ) {
            arr(i)(j) = Some(f(i,j))
            j += 1
          }
          o += p*k
        }

        try {
          rowGate.await();
        }
        catch {
          case e : InterruptedException   => e.printStackTrace
          case e : BrokenBarrierException => e.printStackTrace
        }

        i += 1
      }

      endGate.countDown();
    }
  }
}
