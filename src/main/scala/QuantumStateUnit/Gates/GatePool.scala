package QuantumStateUnit.Gates

import QuantumStateUnit.Gates.flipMSB.turnNegative
import chisel3._
import chisel3.util._
//The number system used is the IEEE 754 format

/*
...
    To reduce the amount of resources used, this module avoids using FPU units.
  All the gates chosen in this file moves the location on numbers, or at most turns it negative.
  IEEE 754 format has the first bit determine if the number is negative without modifying the rest of the number.
...
 */


//No-op Gate
/*
Matrix: | 1 0 |
        | 0 1 |
 */
class No_op(val bw: Int) extends Module{
  val io = IO(new Bundle{
    val in        = Input(Vec(2, UInt(bw.W)))
    val in_en     = Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2, UInt(bw.W)))
  })
  io.out_valid := ShiftRegister(io.in_en, 1)
  io.out(0)    := io.in(0)
  io.out(1)    := io.in(1)
}

//Pauli_X gate
/*
Matrix: | 0 1 |
        | 1 0 |
 */
class Pauli_X(val bw: Int) extends Module {
  val io = IO(new Bundle{
    val in        = Input(Vec(2, UInt(bw.W)))
    val in_en     = Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2, UInt(bw.W)))
  })
  io.out_valid := ShiftRegister(io.in_en, 1)
  io.out(0)    := io.in(1)
  io.out(1)    := io.in(0)
}

//Pauli_Y gate
/*
Matrix:   | 0  j |
          | -j 0 |
QSV:      | a+jb |  -> new QSV ->  |-d+jc |
          | c+jd |                 | b-ja |
 */
class Pauli_Y(bw : Int) extends Module{
  val io = IO(new  Bundle{
    val in        = Input(Vec(2, UInt(bw.W)))
    val in_en     = Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2,UInt(bw.W)))
  })
  //flip the first bit of the real of the first vector and the imaginary of the second vector
  val flipA = Module(new turnNegative(bw/2))
  val flipD = Module(new turnNegative(bw/2))
  //name of component based on QSV shown above
  flipA.io.in := io.in(0)(bw-1,bw/2)
  flipD.io.in := io.in(1)((bw/2)-1,0)

  //rearrange the imaginary and real coefficients according to the pattern above
  io.out(0) := Cat(flipD.io.out, io.in(1)(bw-1,bw/2))
  io.out(1) := Cat(io.in(0)((bw/2)-1,0), flipA.io.out)
  io.out_valid := ShiftRegister(io.in_en, 1)
}

//Pauli_Z gate
/*
Matrix:   | 1  0 |
          | 0 -1 |
QSV:      | a+jb |  -> new QSV ->  | a+jb |
          | c+jd |                 |-c-jd |
 */
class Pauli_Z(bw : Int) extends Module {
  val io = IO(new Bundle {
    val in        =  Input(Vec(2, UInt(bw.W)))
    val in_en     =  Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2, UInt(bw.W)))
  })
  //flip the first bit of the second vectors real and imaginary number
  val flipC = Module(new turnNegative(bw / 2))
  val flipD = Module(new turnNegative(bw / 2))
  flipC.io.in := io.in(1)(bw-1,bw/2)
  flipD.io.in := io.in(1)((bw/2)-1,0)

  //OUTPUT
  io.out(0) := io.in(0)
  io.out(1) := flipC.io.out ## flipD.io.out
  io.out_valid := ShiftRegister(io.in_en, 1)
}

/*
 32
 real:0000 imag:0000
 */
//S or sqrt(Z) gate
/*
Matrix: | 1 0 |
        | 0 j |
QSV:      | a+jb |  -> new QSV ->  | a+jb |
          | c+jd |                 |-d+jc |
 */
class S_gate(val bw :Int) extends Module{
  val io = IO(new Bundle{
    val in        =  Input(Vec(2, UInt(bw.W)))
    val in_en     =  Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2, UInt(bw.W)))
  })
  val flipD = Module(new turnNegative(bw / 2))
  flipD.io.in := io.in(1)((bw/2)-1,0)

  //OUTPUT
  io.out(0) := io.in(0)
  io.out(1) := flipD.io.out ## io.in(1)(bw-1,bw/2)
  io.out_valid := ShiftRegister(io.in_en, 1)
}

//S^-1 or Inverse of S gate
/*
Matrix: | 1  0 |
        | 0 -j |
QSV:      | a+jb |  -> new QSV ->  | a+jb |
          | c+jd |                 | d-jc |
 */
class InverseS_gate(val bw :Int) extends Module{
  val io = IO(new Bundle{
    val in        =  Input(Vec(2, UInt(bw.W)))
    val in_en     =  Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2, UInt(bw.W)))
  })
  val flipC = Module(new turnNegative(bw / 2))
  flipC.io.in := io.in(1)(bw-1,bw/2)

  //OUTPUT
  io.out(0) := io.in(0)
  io.out(1) := io.in(1)((bw/2)-1,0) ## flipC.io.out
  io.out_valid := ShiftRegister(io.in_en, 1)
}

/*
...
...
  2 (or more?) INPUT GATES
...
...
 */

//Controlled-Not Gate
/*
Matrix: | 1 0 0 0 |
        | 0 1 0 0 |
        | 0 0 0 1 |
        | 0 0 1 0 |
 */
class CNOT(bw: Int) extends Module{
  val io = IO(new Bundle{
    val in        = Input(Vec(4, UInt(bw.W)))
    val in_en     = Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(4, UInt(bw.W)))
  })
  io.out(0) := io.in(0)
  io.out(1) := io.in(1)
  io.out(2) := io.in(3)
  io.out(3) := io.in(2)
  io.out_valid := ShiftRegister(io.in_en, 1)
}

//Controlled Pauli Y Gate
/*
Matrix: | 1  0  0  0 |
        | 0  1  0  0 |
        | 0  0  0  j |
        | 0  0 -j  0 |
QSV:      | a + jb |  -> new QSV ->  | a + jb |
          | c + jd |                 | c + jd |
          | e + jf |                 | h - jg |
          | g + jh |                 |-f + je |
 */
class CPauli_Y(bw: Int) extends Module{
  val io = IO(new Bundle{
    val in          = Input(Vec(4, UInt(bw.W)))
    val in_en       = Input(Bool())
    val out_valid   = Output(Bool())
    val out         = Output(Vec(4, UInt(bw.W)))
  })
  val flipG = Module(new turnNegative(bw/2))
  val flipF = Module(new turnNegative(bw/2))
  flipF.io.in := io.in(2)((bw/2)-1,0)
  flipG.io.in := io.in(3)(bw-1,bw/2)

  io.out(0) := io.in(0)
  io.out(1) := io.in(1)
  io.out(2) := io.in(3)((bw/2)-1,0) ## flipG.io.out
  io.out(3) := flipF.io.out ## io.in(2)(bw-1,bw/2)
  io.out_valid := ShiftRegister(io.in_en, 1)
}

//Controlled Pauli Z Gate
/*
Matrix: | 1  0  0  0 |
        | 0  1  0  0 |
        | 0  0  1  0 |
        | 0  0  0 -1 |
QSV:      | a + jb |  -> new QSV ->  | a + jb |
          | c + jd |                 | c + jd |
          | e + jf |                 | e + jf |
          | g + jh |                 |-g - jh |
 */
class CPauli_Z(bw: Int) extends Module{
  val io = IO(new Bundle{
    val in        = Input(Vec(4, UInt(bw.W)))
    val in_en     = Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(4, UInt(bw.W)))
  })
  val flipG = Module(new turnNegative(bw/2))
  val flipH = Module(new turnNegative(bw/2))
  flipG.io.in := io.in(3)(bw-1,bw/2)
  flipH.io.in := io.in(3)((bw/2)-1,0)

  io.out(0) := io.in(0)
  io.out(1) := io.in(1)
  io.out(2) := io.in(2)
  io.out(3) := flipG.io.out ## flipH.io.out
  io.out_valid := ShiftRegister(io.in_en, 1)
}

//Toffoli Gate
/*
May potentially not be included if not willing to do 3 bit permutation
Matrix: | 1 0 0 0 0 0 |
        | 0 1 0 0 0 0 |
        | 0 0 1 0 0 0 |
        | 0 0 0 1 0 0 |
        | 0 0 0 0 0 1 |
        | 0 0 0 0 1 0 |
 */
class Toffoli(bw: Int) extends Module{
  val io = IO(new Bundle{
    val in          =  Input(Vec(6, UInt(bw.W)))
    val in_en       =  Input(Bool())
    val out_valid   = Output(Bool())
    val out         = Output(Vec(6, UInt(bw.W)))
  })
  io.out(0) := io.in(0)
  io.out(1) := io.in(1)
  io.out(2) := io.in(2)
  io.out(3) := io.in(3)
  io.out(4) := io.in(5)
  io.out(5) := io.in(4)
  io.out_valid := ShiftRegister(io.in_en, 1)
}
