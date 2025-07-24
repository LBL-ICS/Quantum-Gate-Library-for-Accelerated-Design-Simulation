package QuantumStateUnit.QSU_Architecture

import chisel3._
import chisel3.util._
import QuantumStateUnit.Gates._
import scala.math._

/*
Selected Gates                 ID
            nonFPU
No-Op:                          0
Pauli-X:                        1
Pauli-Y:                        2
Pauli-Z:                        3
S Gate                          4
Inverse S Gate                  5
Controlled X                    6
Controlled Y                    7
Controlled Z                    8
Swap Gate                       9
Toffoli                         10
          Switch to FPU
Normalization                   16
Hadamard Gate                   17
V / sqrt(Not) / sqrt(X) gate    18
sqrt(y) gate                    19
T gate or sqrt(s) gate          20
inverse of T gate               21
U gate/ custom gate             22
            Measure
Measurement Gate                31
 */
class QGP(val num_of_qubits : Int, val bit_width :Int, val mult_pd : Int, val add_pd : Int, val L : Int) extends Module {
  val io = IO(new Bundle {
    val in_QSV    = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W)))
    val in_Ugate  = Input(Vec(4, UInt(bit_width.W)))
    val in_sel    = Input(UInt(5.W)) // 1 bit sel between nonfpu and fpu and 4 bit sel for the actual select
    val in_noise  = Input(UInt(32.W))
    val in_valid  = Input(Bool())
    val out_valid = Output(Bool())
    val out_QSV   = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W)))
    val out_MQ    = Output(Bool()) //The measured Qubits Value
  })
  //two gate pools and measurement
  val FPUGatepool    = Module(new FPUPool(num_of_qubits, bit_width, mult_pd, add_pd))
  val nonFPUGatepool = Module(new nonFPUPool(num_of_qubits, bit_width))
  val measure        = Module(new Measurement(num_of_qubits, bit_width, mult_pd, add_pd, L))
  //Mux output
  val outputmux = Module(new QGPMuxLayer(num_of_qubits, bit_width, 2))

  //Use Normalize gate if measuring.
  val enNormalize             = (io.in_sel === 16.U) || (io.in_sel === 31.U)

  //connecting layers: in -> Gate -> mux -> out
  nonFPUGatepool.io.in_QSV  := io.in_QSV
  FPUGatepool.io.in_QSV     := Mux(io.in_sel === 31.U, measure.io.out_QSV, io.in_QSV)
  measure.io.in_QSV         := io.in_QSV
  outputmux.io.in_QSV(0)    := nonFPUGatepool.io.out_QSV
  outputmux.io.in_QSV(1)    := FPUGatepool.io.out_QSV
  io.out_QSV                := outputmux.io.out_QSV

  //select
  outputmux.io.in_sel         := io.in_sel(4)   //picks between pools
  nonFPUGatepool.io.in_sel    := io.in_sel(3,0) //ID of gate
  FPUGatepool.io.in_sel       := Mux(enNormalize, 0.U,io.in_sel(3,0)) //ID of gate
  measure.io.in_sendNorm      := io.in_sel === 16.U

  //valid
  nonFPUGatepool.io.in_valid  := Mux(io.in_sel(4), 0.B, io.in_valid)
  FPUGatepool.io.in_valid     := Mux(enNormalize, measure.io.out_valid, Mux(io.in_sel(4), io.in_valid, 0.B))
  measure.io.in_valid         := Mux(enNormalize, io.in_valid, 0.B)

  //Output valid
  io.out_valid                := Mux(io.in_sel(4), FPUGatepool.io.out_valid, nonFPUGatepool.io.out_valid)

  //other
  for(i <- 0 until 4) {
    FPUGatepool.io.in_Ugate(i) := io.in_Ugate(i)
  }
  //A vector contains 2 value despite being one coefficient. The Imaginary part should be 0.
  FPUGatepool.io.in_normalize := measure.io.out_Normalize ## 0.U((bit_width/2).W)
  measure.io.in_noise         := io.in_noise
  io.out_MQ                   := measure.io.out_measured
}