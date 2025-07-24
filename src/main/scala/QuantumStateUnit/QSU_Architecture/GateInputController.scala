package QuantumStateUnit.QSU_Architecture

import chisel3._
import chisel3.util._

import scala.math._

class QSUController extends Module {
  val io = IO(new Bundle {
    val in_replaceQSV   = Input(Bool()) //Tells the QSR to take the external QSV
    val in_applygate    = Input(Bool()) //Sends both select to the permutation and gate pool
    val in_valid        = Input(Bool()) //From QGP when it has finished its calculation
    //Outputs to the internal system
    val out_update_QSR  = Output(Bool()) //For updating QSV with new value from gate pool
    val out_replaceQSV  = Output(Bool()) //For replacing QSV from manager
    val out_en_QGP      = Output(Bool()) //Tell when QGP have valid input
    //Only real output to the user
    val out_readyFlag   = Output(Bool()) //Signifies that the gate finished
  })
  /*out_flag will be from a register that follows the following conditions:
      It will start and reset to 1.B
      It will change to 0 when in_applygate is 1.B
      It will change to 1 when QSR has updated
   */
  /*out_en_QGP will be from a register that follows the following conditions:
      It will start and reset to 0.B
      When it's 0.B, it will only change to 1.B when in_applygate = 1.B and in_valid = 0.B
      When it's 1.B, tt will only change to 0.B after the gates have been updated
   */
  val en_QGP        = RegInit(0.B)
  en_QGP           := en_QGP || (io.in_applygate & !en_QGP & !io.in_valid)
  io.out_en_QGP    := en_QGP
  io.out_readyFlag := !en_QGP && !io.in_valid

  /*out_update_QSR will follow the following traits:
      It will start, rest, and reset to 0.B
      It will remain 0.B until in_valid becomes 1.B
        It will not remain 1.B when in_valid is 1.B, but instead will only become 1.B for one clock cycle
        in_valid =/= previous in_valid will determine change between 0.B and 1.B
        **If en_QGP is 1.B and in_valid =/= previous, then an operation has been completed**
      It will remain 0.B unless replacing the QSV in the register when there is no operation being done
   */
  val prev        = RegInit(0.B) //contains previous value of in_valid to compare in XOR
  prev           := io.in_valid
  //The first part (en_QGP & io.in_valid & !prev) will become 1.B when io.in_valid turns 1.B during an operation
  //The second part (!en_QGP & io.in_replaceQSV) will become 1.B after an operation has finished
  val update      = Wire(Bool())
  val delayUpdate = ShiftRegister(update, 1)
  update         := (en_QGP & io.in_valid & !prev)
  io.out_update_QSR := (delayUpdate || (!en_QGP & io.in_replaceQSV))
  io.out_replaceQSV := (!en_QGP & io.in_replaceQSV)
  when(delayUpdate){
    en_QGP   := ~en_QGP
  }
}

//low is hold
class QSUSelHold(val num_of_qubits : Int, val permutation_Seq : Seq[Int]) extends Module{
  val io = IO(new Bundle{
    val in_sel_gate    =  Input(UInt(5.W))
    val in_sel_perm    =  Input(MixedVec(permutation_Seq.map(i => UInt(ceil(log(num_of_qubits - i)/log(2)).toInt.W))))
    val in_hold        =  Input(Bool())
    val out_sel_gate   = Output(UInt(5.W))
    val out_sel_perm   = Output(MixedVec(permutation_Seq.map(i => UInt(ceil(log(num_of_qubits - i)/log(2)).toInt.W))))
  })
  //Initiation of the Registers
  val holdGate = RegInit(0.U(5.W))
  val holdPerm : Seq[UInt] = permutation_Seq.map { i =>
    val reg = RegInit(0.U(ceil(log(num_of_qubits - i) / log(2)).toInt.W))
    reg
  }
  //IO of the registers
  holdGate := Mux(io.in_hold, io.in_sel_gate, holdGate) //input
  io.out_sel_gate := holdGate                           //output
  for(i <- 0 until permutation_Seq.length) {
    holdPerm(i) := Mux(io.in_hold, io.in_sel_perm(i), holdPerm(i))  //Input
    io.out_sel_perm(i) := holdPerm(i)                               //Output
  }
}