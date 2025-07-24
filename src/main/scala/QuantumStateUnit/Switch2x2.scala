package QuantumStateUnit.OtherComponents.Switch2x2

import chisel3._

class Switch2x2(val bw : Int) extends Module {
  val io = IO(new Bundle{
    val input  =  Input(Vec(2, UInt(bw.W)))
    val in_sel =  Input(Bool())
    val output = Output(Vec(2, UInt(bw.W)))
  })
  io.output(0) := Mux(io.in_sel, io.input(1), io.input(0))
  io.output(1) := Mux(io.in_sel, io.input(0), io.input(1))
}
