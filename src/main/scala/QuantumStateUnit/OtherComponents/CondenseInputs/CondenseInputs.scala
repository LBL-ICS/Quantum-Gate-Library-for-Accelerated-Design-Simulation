package QuantumStateUnit.OtherComponents.CondenseInputs

import chisel3._
import chisel3.util._

class ShiftRegisterInput(val num_of_inputs : Int, val bw : Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(num_of_inputs, Bool()))
    val en = Input(Bool())
    val out = Output(Vec(num_of_inputs, UInt(bw.W)))
  })
  io.out := VecInit(io.in.map(x => ShiftRegister(x, bw, io.en)))
}

class ShiftRegisterOutput(val num_of_inputs : Int, val bw : Int) extends Module {
  val io = IO(new Bundle {
    val in  =  Input(Vec(num_of_inputs, UInt(bw.W)))
    val en  =  Input(Bool())
    val out = Output(Vec(num_of_inputs, Bool()))
  })
  //counter
  val cnt = RegInit(0.U(log2Ceil(bw).W))

  //counter control
  when(io.en){
    cnt := cnt+1.U
  }
  when(!io.en){
    cnt := 0.U
  }

  //output
  for(i <- 0 until num_of_inputs){
    io.out(i) := io.in(i)(cnt)
  }
}