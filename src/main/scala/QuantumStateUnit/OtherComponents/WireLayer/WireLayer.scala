package QuantumStateUnit.OtherComponents.WireLayer

import chisel3._
import scala.math._

class WireLayer(val num_of_qubits : Int, val bw : Int) extends Module{
  val io = IO(new Bundle{
    val input  = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bw.W)))
    val output = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bw.W)))
  })
  io.output := io.input
}
