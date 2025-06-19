package QuantumStateUnit

import QuantumStateUnit.Gates._
import QuantumStateUnit.QSU_Architecture._
import chisel3._

import scala.math.pow

//Five values to be adjusted (qubits, bw, length, pd, pd)
/*
Accepted bit width is:      32, 64,128,256
accepted propagation delay:  1,  3,  7,  8, 10, 13
 */
class TopQSU(num_of_qubits : Int, bit_width : Int, Alg_length :Int, mult_pd : Int, add_pd : Int, pd : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV              = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val in_Permutation      = Input(Vec(Alg_length, UInt(num_of_qubits.W)))
    val in_Gate             = Input(Vec(Alg_length, UInt(8.W)))
    val in_en_newData       = Input(Bool()) //Replaces Initial state and algorithm with above inputs
    val out_state           = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val out_flag            = Output(Bool())
  })
  val manager     = Module(new AlgorithmManager(num_of_qubits, bit_width, Alg_length, pd, 6/*change According to Gate Pool*/))
  val gatePool    = Module(new QGP(num_of_qubits, bit_width, mult_pd, add_pd,6/*replace with current number of unique gates*/))
  val permutation = Module(new permutationLayer(num_of_qubits, bit_width))
  val reversePerm = Module(new permutationLayer(num_of_qubits, bit_width))
  val QSR         = Module(new QuantumStateRegister(num_of_qubits, bit_width))

  //Quantum state Register inputs
  QSR.io.in_QSV         := reversePerm.io.out_QSV
  QSR.io.in_new_state   := manager.io.out_QSV
  QSR.io.in_en          := manager.io.out_en_QSR
  QSR.io.in_en_new_state:= manager.io.out_newState
  //Permutation inputs
  permutation.io.in_QSV := QSR.io.out_QSV
  permutation.io.in_sel := manager.io.out_sel_permutation
  //reverse permutation inputs
  reversePerm.io.in_QSV := gatePool.io.out_QSV
  reversePerm.io.in_sel := manager.io.out_sel_permutation
  //Gate Pool inputs
  gatePool.io.in_QSV    := permutation.io.out_QSV
  gatePool.io.in_sel    := manager.io.out_sel_gate
  gatePool.io.in_en     := manager.io.out_en_QGP
  //Manager Inputs
  manager.io.in_QSV         := io.in_QSV
  manager.io.in_Gate        := io.in_Gate
  manager.io.in_Permutation := io.in_Permutation
  manager.io.in_en_newData  := io.in_en_newData
  manager.io.in_en_next     := gatePool.io.out_valid

  //Outputs
  io.out_state := QSR.io.out_QSV
  io.out_flag  := manager.io.out_endofAlg
}

object main extends App{
  emitVerilog(new AlgorithmManager(2, 32, 5, 3, 6))
}