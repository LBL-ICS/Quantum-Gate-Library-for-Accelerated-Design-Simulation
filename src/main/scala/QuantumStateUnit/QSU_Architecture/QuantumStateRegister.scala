package QuantumStateUnit.QSU_Architecture

import chisel3._
import chisel3.util._
import scala.math._

//QSR: Quantum State Register
/*
Notes about length of variables
  length of the QSV:                  bit_width*(2^num_of_qubits) - the number of coeffiecients
  length of the sel for permutation:  ceil(logBase2(number of qubits)) - the number of bits required to count to the number of qubits
 */

/*
The number contained will have the possibility multiplied into the phase(Euler's Formula: ie... cos(x)+j*sin(x))
 The amount of registers will be bit_width*(2^num_of_qubits)
 EXAMPLE) 1 Qubit of |0>
        |0>     |1>
        01_0... 00_0...
        2 Qubit of sqrt(1/2)|10> +sqrt(1/2)|01>
        |00>    |01>    |10>    |11>
        00_0... 00_7... 00_7... 00_0...

Each coefficient contains two numbers, the real and imaginary. These numbers are carried together in the same coefficient.
EXAMPLE) If the QSR is given 32 bits for its bit width then 16 bits will be dedicated for the real and imaginary.
                   32 bits storing the 1 + j2:   hex: 3c004000 -> Real: 3c00 and Imag: 4000
 */
class QuantumStateRegister(val num_of_qubits: Int, val bit_width:Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV          = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W))) //from gatepool
    val in_new_state    = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W))) //from manager
    val in_en           = Input(Bool()) //Replaces the current QSV with QSV from gate-pool or manager
    val in_en_new_state = Input(Bool()) //selects between new_state from manager or QSV from gate-pool when replacing values
    val out_QSV         = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
  })

  val QuantumStateReg = Reg(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))

  when(io.in_en){
    QuantumStateReg := Mux(io.in_en_new_state, io.in_new_state, io.in_QSV)
  }

  for (i <- 0 until pow(2,num_of_qubits).toInt) {
    io.out_QSV(i) := QuantumStateReg(i)
  }
}

//Same version, but uses a mux instead of a when statement.
/*
class QuantumStateRegister(val num_of_qubits: Int, val bit_width:Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV          = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W))) //from gatepool
    val in_new_state    = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W))) //from manager
    val in_en           = Input(Bool()) //Replaces the current QSV with QSV from gate-pool or manager
    val in_en_new_state = Input(Bool()) //selects between new_state from manager or QSV from gate-pool when replacing values
    val out_QSV         = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
  })

  val QuantumStateReg = Reg(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))

  for (i <- 0 until pow(2,num_of_qubits).toInt) {
    QuantumStateReg(i) := Mux(io.in_en, Mux(io.in_en_new_state, io.in_new_state(i), io.in_QSV(i)), QuantumStateReg(i))
  }

  for (i <- 0 until pow(2,num_of_qubits).toInt) {
    io.out_QSV(i) := QuantumStateReg(i)
  }
}
 */