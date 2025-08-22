package QuantumStateUnit.Gates.flipMSB

import chisel3._

//USED IN THE GATE POOL file
/*
  This is used in gates that require an input to be turned negative.
    Flips the first bit and keeps everything else unchanged
    For IEEE 754 format and not other fixed point formats
 */
class turnNegative(bw : Int) extends Module{
  val io = IO(new Bundle{
    val  in =  Input(UInt(bw.W))
    val out = Output(UInt(bw.W))
  })
  io.out := Mux(io.in(bw-2,0)===0.U,0.U, ~io.in(bw-1)) ## io.in(bw-2,0)
}