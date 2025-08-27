package QuantumStateUnit.Old

import chisel3._
import chisel3.util._

import scala.math._

//OLD, not updated

//Algorithm Manager
/*
  Takes the input QSV and stores it in the QSR and takes the input Alg(algorithm) and stores it into own reg.
    Alg is made of two pieces of information: sel_gate (the quantum gate) and sel_permutation (the target qubit).
  Recieves input from the GatePool to determine when to update the QSR and Alg output.

  Note: check the Quantum Gate Pool for num_of_gates.
 */
class AlgorithmManager(num_of_qubits : Int, bit_width : Int, Alg_length : Int, num_of_gates : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV              = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val in_Permutaiton_Sel      = Input(Vec(Alg_length, UInt(ceil(log(num_of_qubits)/log(2)).toInt.W)))
    val in_Gate_Sel             = Input(Vec(Alg_length, UInt(ceil(log(num_of_gates)/log(2)).toInt.W)))
    val in_en_newData       = Input(Bool()) //Replaces Initial state and algorithm with above inputs
    val in_en_next          = Input(Bool()) //For Updating the QSR
    val out_en_QSR          = Output(Bool()) //For updating QSV with new value from gate pool
    val out_en_QGP          = Output(Bool()) //Tell when QGP have valid input
    val out_newState        = Output(Bool()) //For replacing QSV from manager
    val out_endofAlg        = Output(Bool()) //Signifies that the algorithm has ended
    val out_sel_gate        = Output(UInt(ceil(log(num_of_gates)/log(2)).toInt.W))
    val out_sel_permutation = Output(UInt(num_of_qubits.W))
    val out_QSV             = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
  })
  io.out_sel_gate        := 0.U
  io.out_sel_permutation := 0.U

  //Mux that feeds into the counter
  val stop         = RegInit(true.B) //Must start stopped, so en_newdata can work for first time
  val next         = RegInit(false.B)
  next            := Mux(stop, 0.B, io.in_en_next)
  io.out_endofAlg := stop



  //Mux that request to replace current finished algorithm with new values
  val en_new_values = Mux(stop, io.in_en_newData, 0.B) //doesn't apply to counting
  /*
  The 'enable' should be 1.B until there is acknowledgement that the gate pool has finished through its calculation
   Waits until through both permutation blocks
   This is so that the FPU units may get a constant enable, so it can finish the calculations.
   */
  val prev   = RegInit(false.B)
  val enable = RegInit(false.B)
  prev       := next
  //when new values or when next changes: Flip Quantum Gate Pool enable.
  when(en_new_values || (next^prev)){
    enable := ~enable
  }
  io.out_en_QGP     := ShiftRegister(enable, 1)

  /*
  When 'enable' changes to 1.B, the QSV and counter needs to update.
    They should not change after 'enable' shifts to 0.B
   */
  val update = Mux(prev, 0.B, next) //Cuts the 1.B short
  //The outputs to update the QSR and prepare QGP
  io.out_QSV        := io.in_QSV
  io.out_newState   := en_new_values
  io.out_en_QSR     := en_new_values | ShiftRegister(update, 1) //Waits until through reverse permutation



  //The Counter: keeps track of what the output gate and permutation should be
  val cntEventsReg = RegInit (0.U(ceil(log(Alg_length)/log(2)).toInt.W))
  when(update) {
    cntEventsReg := cntEventsReg + 1.U
  }

  //Counter Resets when new ready for new inputs
  when(en_new_values){
    cntEventsReg := 0.U
    stop         := 0.B
  }



  //Registers for the Algorithm
  val quantumGate       = Reg(Vec(Alg_length, UInt(ceil(log(num_of_gates)/log(2)).toInt.W)))
  val selPermutation    = Reg(Vec(Alg_length, UInt(ceil(log(num_of_qubits)/log(2)).toInt.W)))
  //Register Inputs
  for(i <- 0 until Alg_length) {
    quantumGate(i)    := Mux(en_new_values, io.in_Gate_Sel(i), quantumGate(i))
    selPermutation(i) := Mux(en_new_values, io.in_Permutaiton_Sel(i), selPermutation(i))
  }
  //Mux Output from the Registers
  io.out_sel_gate       := quantumGate(cntEventsReg)
  io.out_sel_permutation:= selPermutation(cntEventsReg)

  //END OF ALGORITHM WHEN GATE REGISTER SENDS ONLY 1's
  //May potentially cause future errors if not handled correctly!!!
  when(io.out_sel_gate.andR){ //final gate should be all 1's
    stop   := 1.B
    enable := 0.B
  }
}
