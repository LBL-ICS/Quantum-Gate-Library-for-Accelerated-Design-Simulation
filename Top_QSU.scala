package QuantumStateUnit

import QuantumStateUnit.Gates._
import QuantumStateUnit.OtherComponents.PsuedoRandomGenerator._
import QuantumStateUnit.QSU_Architecture._
import chisel3._

import scala.math.{ceil, log, pow}

//Five values to be adjusted (qubits, bw, length, pd, pd)
/*
Accepted bit width is:      32, 64,128,256
accepted propagation delay:  1,  3,  7,  8, 10, 13
*/
class TopQSU(num_of_qubits : Int, bit_width : Int, mult_pd : Int, add_pd : Int, L : Int) extends Module{
  override def desiredName = s"TopQSU${bit_width/2}bit_${num_of_qubits}qubit_v2"
  val io = IO(new Bundle{
    val in_QSV              = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val in_Permutation0     = Input(UInt(ceil(log(num_of_qubits)/log(2)).toInt.W))
    val in_Permutation1     = Input(UInt(ceil(log(num_of_qubits - 1)/log(2)).toInt.W))
    val in_Permutation2     = Input(UInt(ceil(log(num_of_qubits - 2)/log(2)).toInt.W))
    val in_Gate             = Input(UInt(5.W))
    val in_Ugate            = Input(Vec(4, UInt(bit_width.W)))
    val in_applyGate        = Input(Bool()) //Applies the input gate onto the register: doesn't work when flag is 1.B
    val in_en_replaceQSV    = Input(Bool()) //Replaces Initial state and algorithm with above inputs
    val in_noise            = Input(UInt(32.W))
    val out_state           = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val out_flag            = Output(Bool()) //says when ready to apply next gate. While 1.B, applyGate will not take any input
  })
  val manager     = Module(new QSUController)
  val gatePool    = Module(new QGP(num_of_qubits, bit_width, mult_pd, add_pd, L))
  val permutation = Module(new StackedPermutationSwitchGrids(num_of_qubits, bit_width, Seq(0, 1, 2)))
  val reversePerm = Module(new StackedPermutationSwitchGrids(num_of_qubits, bit_width, Seq(0, 1, 2)))
  val QSR         = Module(new QuantumStateRegister(num_of_qubits, bit_width))
  val hold        = Module(new QSUSelHold(num_of_qubits, Seq(0, 1, 2)))

  //Quantum state Register inputs
  QSR.io.in_QSV             := reversePerm.io.out_QSV
  QSR.io.in_new_state       := io.in_QSV
  QSR.io.in_en              := manager.io.out_update_QSR
  QSR.io.in_en_new_state    := manager.io.out_replaceQSV
  //Permutation inputs
  permutation.io.in_QSV     := QSR.io.out_QSV
  permutation.io.in_sel(0)  := io.in_Permutation0
  permutation.io.in_sel(1)  := io.in_Permutation1
  permutation.io.in_sel(2)  := io.in_Permutation2
  //reverse permutation inputs
  reversePerm.io.in_QSV     := gatePool.io.out_QSV
  reversePerm.io.in_sel(0)  := hold.io.out_sel_perm(0)
  reversePerm.io.in_sel(1)  := hold.io.out_sel_perm(1)
  reversePerm.io.in_sel(2)  := hold.io.out_sel_perm(2)
  //Gate Pool inputs
  gatePool.io.in_QSV        := permutation.io.out_QSV
  gatePool.io.in_sel        := hold.io.out_sel_gate
  gatePool.io.in_valid      := manager.io.out_en_QGP
  gatePool.io.in_Ugate      := io.in_Ugate
  gatePool.io.in_noise      := io.in_noise
  //Manager Inputs
  manager.io.in_valid       := gatePool.io.out_valid
  manager.io.in_applygate   := io.in_applyGate
  manager.io.in_replaceQSV  := io.in_en_replaceQSV
  //hold sel
  hold.io.in_sel_gate       := io.in_Gate
  hold.io.in_sel_perm(0)    := io.in_Permutation0
  hold.io.in_sel_perm(1)    := io.in_Permutation1
  hold.io.in_sel_perm(2)    := io.in_Permutation2
  hold.io.in_hold           := manager.io.out_readyFlag

  //Outputs
  io.out_state := QSR.io.out_QSV
  io.out_flag  := manager.io.out_readyFlag
}

object main extends App{
  emitVerilog(new TopQSU(6,32,3,3,10))
}