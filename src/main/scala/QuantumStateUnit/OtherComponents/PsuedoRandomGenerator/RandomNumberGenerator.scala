package QuantumStateUnit.OtherComponents.PsuedoRandomGenerator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

//USED INSIDE OF THE MEASUREMENT GATE

/*
Generates a sequence of numbers from a seed. The given seed should come from an external source to generate randomness.
When implemented onto an FPGA, probably have the io.in_seed be connected to a thermostat or something similar.
 */

/*
Rules for selecting m, a, and c using Hull–Dobell Theorem: if m is a power of 2 and c is inequal to 0 then
  1. m and c are coprime ... 1 can be the only shared prime number
  2. a-1 is divisible by all prime factors of m
  3. a-1 is divisible by 4 if m is divisible by 4
 */
class LinearCongruentialGenerator(val bw : Int, val m : Int, val a : Int, val c : Int) extends Module{
  val io = IO(new Bundle{
    val in_seed     =  Input(UInt(bw.W))
    val in_feed     =  Input(Bool())
    val in_next     =  Input(Bool())
    val out_Value   = Output(UInt(bw.W))
  })
  //output
  val value  = RegInit(0.U(bw.W))
  io.out_Value  := value

  //new value
  val seedUsed  = RegInit(0.B) //first uses the seed before using the previous number
  when(io.in_next){
    value := (value * a.U + c.U) % m.U
  }
  when(io.in_feed){
    value := io.in_seed
  }
}