package QuantumStateUnit.QSU_Architecture.Permutation_Designs

import chisel3._
import chisel3.util._
import scala.math._

class BenesNetwork(num_of_qubits : Int, bit_width : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV  =  Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n input channels (bw bits each)
    val in_sel  =  Input(UInt(ceil(log(num_of_qubits)/log(2)).toInt.W)) // Routing control signals
    val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n output channels
  })

}

//Switches two values using 2 muxes
//0: input = output         1: input(0) = output(1) and vice versa
class Switch2x2(val bit_width : Int) extends Module{
  val io = IO(new Bundle{
    val input   =  Input(Vec(2, UInt(bit_width.W)))
    val in_sel  =  Input(Bool())
    val output  = Output(Vec(2, UInt(bit_width.W)))
  })
  io.output(0) := Mux(io.in_sel, io.input(1), io.input(0))
  io.output(1) := Mux(io.in_sel, io.input(0), io.input(1))
}

//helps organize the inputs/outputs between layers of componenets
class WireLayer(val num_of_qubits : Int, val bit_width : Int) extends Module{
  val io = IO(new Bundle{
    val input   =  Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W)))
    val output  = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W)))
  })
  io.output := io.input
}