package QuantumStateUnit.QSU_Architecture

import chisel3._
import chisel3.util._
import scala.math._

//Self-Routing Permutation Layer
/*
Reorganizes the permutations into a new order for the gates in the Gate Pool to properly modify the data.
Instead of a single crossbar switch, it will have a network instead.
...
When a qubit gets pushed to the front, it starts to have a pattern that follows the code below.
The mux layer seperates each permutation from each other, so they could be individually be selected by a mux.

  There are two values being switched in order to move n-bit to the farthest right bit. (target bit gets "0 1 0 1" behavior)
  This works by switching every odd bit with 2^(gate size).
  The code below is the implementation of the algorithm.
  Example)
    0    0                    0     0
    0    1  new permutation   1     0
    1    0                    0     1
    1    1                    1     1
 */
class permutationLayer(val num_of_qubits: Int, val bit_width: Int) extends Module {
  val io = IO(new Bundle {
    val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n input channels (bw bits each)
    val in_sel = Input(UInt(log2Ceil(num_of_qubits).W)) // Determine Selected Qubit
    val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n output channels
  })
      //...
  val muxLayer1 = Module(new muxLayer(num_of_qubits, bit_width))

  muxLayer1.io.in_sel := io.in_sel

  //if the target is q0 then there is no switching of coefficients
  for(first_permutation <- 0 until pow(2,num_of_qubits).toInt) {
    muxLayer1.io.in_QSV(0)(first_permutation) := io.in_QSV(first_permutation)
  }

  //The switching algorithm
    for ( s <- 2 until num_of_qubits + 1) {
      for (i <- 0 until num_of_qubits - s + 1) {
        for (j <- 0 until s-1) {
          //switch
          muxLayer1.io.in_QSV(s-1)((2*j+1) + (4*i*(s-1))) := io.in_QSV(2*j + pow(2,s-1).toInt +(4*i*(s-1)))
          muxLayer1.io.in_QSV(s-1)(2*j + pow(2,s-1).toInt +(4*i*(s-1))) := io.in_QSV((2*j+1) + (4*i*(s-1)))
          //doesn't switch
          muxLayer1.io.in_QSV(s-1)((2*j) + (4*i*(s-1)))                     := io.in_QSV((2*j) + (4*i*(s-1)))
          muxLayer1.io.in_QSV(s-1)(2*j + 1 + pow(2,s-1).toInt +(4*i*(s-1))) := io.in_QSV(2*j + 1 + pow(2,s-1).toInt +(4*i*(s-1)))
        }
      }
    }
  //output from mux
  io.out_QSV := muxLayer1.io.out_QSV
}

//muxLayer is used in the permutation layer once
  //The output mux using vector indexing
class muxLayer(val num_of_qubits : Int, val bit_width : Int) extends Module {
  val io = IO(new Bundle{
    val in_QSV  = Input(Vec(num_of_qubits, Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W))))
    val in_sel  = Input(UInt(ceil(log(num_of_qubits)/log(2)).toInt.W))
    val out_QSV = Output(Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W)))
  })
  io.out_QSV := io.in_QSV(io.in_sel)
}
