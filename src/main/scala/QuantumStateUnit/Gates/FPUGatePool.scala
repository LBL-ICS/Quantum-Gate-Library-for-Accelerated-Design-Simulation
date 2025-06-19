package QuantumStateUnit.Gates

import Complex_FPU.complex_conjugate_mult
import chisel3._
import chisel3.util._
//The number system used is the IEEE 754 format

/*
Recieves an input from the adder network and then multiplies the input with the rest of the Quantum Vector
 */
class normalization(val bw :Int, val mult_pd :Int, val add_pd :Int) extends Module{
  val io = IO(new Bundle{
    val in        = Input(Vec(2, UInt(bw.W)))
    val in_value  = Input(UInt(bw.W)) //normalization number
    val in_en     = Input(Bool())
    val in_valid  = Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2, UInt(bw.W)))
  })

  val FPUMultiplier = Seq.fill(2)(Module(new complex_conjugate_mult(bw,mult_pd,add_pd)))
  for (i <- 0 until 1) {
    FPUMultiplier(i).io.complexA := io.in
    FPUMultiplier(i).io.complexB := io.in_value
    FPUMultiplier(i).io.in_en    := io.in_en
    FPUMultiplier(i).io.in_valid := io.in_valid
    io.out(i)                    := Mux(FPUMultiplier(i).io.out_valid, FPUMultiplier(i).io.out_s,0.U)
  }
  io.out_valid := FPUMultiplier(0).io.out_valid & FPUMultiplier(1).io.out_valid
}

//Hadamard gate
/*
Matrix: sqrt(1/2)*| 0  j |
                  | -j 0 |
 */
class Hadamard(val bw :Int, val mult_pd : Int, val add_pd : Int) extends Module{
  val io = IO(new Bundle{
    val in        = Input(Vec(2, UInt(bw.W)))
    val in_en     = Input(Bool())
    val out_valid = Output(Bool())
    val out       = Output(Vec(2, UInt(bw.W)))
  })
  val FPUMultiplier = Seq.fill(2)(Module(new complex_conjugate_mult(bw,mult_pd,add_pd)))

  FPUMultiplier(0).io.complexA := io.in(1)
  FPUMultiplier(0).io.complexB := Cat(1.U, 1.U)//some value sqrt(1/2)*j
  FPUMultiplier(0).io.in_en    := io.in_en
  FPUMultiplier(0).io.in_valid := io.in_en
  io.out(0)                    := Mux(FPUMultiplier(0).io.out_valid, FPUMultiplier(0).io.out_s,0.U)

  FPUMultiplier(1).io.complexA := io.in(0)
  FPUMultiplier(1).io.complexB := Cat(1.U, 1.U)//some value sqrt(1/2)*j
  FPUMultiplier(1).io.in_en    := io.in_en
  FPUMultiplier(1).io.in_valid := io.in_en
  io.out(1)                    := Mux(FPUMultiplier(1).io.out_valid, FPUMultiplier(1).io.out_s,0.U)

  io.out_valid := FPUMultiplier(0).io.out_valid & FPUMultiplier(1).io.out_valid
}

//V or sqrt(NOT) or sqrt(X) gate
/*
Matrix:   (1/2) * | (1+j) (1-j) |
                  | (1-j) (1+j) |
 */
class V_gate(val bw :Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, UInt(bw.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bw.W)))
  })
}

//sqrtY gate
/*
Matrix: (1/2)*| (1+j) -(1+j) |
              | (1+j)  (1+j) |
 */
class sqrtY_gate(val bw :Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, UInt(bw.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bw.W)))
  })
}

//T or sqrt(S) gate
/*
Matrix: | 1        0     |
        | 0 e^(j*pi/4))  |
   e^(j*pi/4) = cos(pi/4) + j*sin(pi/4) = 0.707... + j*0.707...
 */
class T_gate(val bw :Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, UInt(bw.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bw.W)))
  })
}

//T^-1 or inverse of T gate
/*
Matrix: | 1       0    |
        | 0 e^(-j*pi/4) |
 e^(j*pi/4) = cos(pi/4) + j*sin(pi/4) = 0.707... - j*0.707...
 */
class inverseT_gate(val bw :Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, UInt(bw.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bw.W)))
  })
}

//Error in X
/*
Applies Pauli X with a designated error probability
 */
class Ex_gate(val bit_width :Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, UInt(bit_width.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bit_width.W)))
  })
}

//Error in Z
/*
Applies Pauli Z with a designated error probability
 */
class Ez_gate(val bit_width :Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, UInt(bit_width.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bit_width.W)))
  })
}

//Error in Y
/*
Applies Pauli Y with a designated error probability
 */
class Ey_gate(val bit_width :Int) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(2, UInt(bit_width.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bit_width.W)))
  })
}

//Measurement gate
/*
Measures and collapses the state for the specified qubit
Final thing to be worked on
 */
class Measurement(val bit_width :Int) extends Module{
  val io = IO(new Bundle {
    val in = Input(Vec(2, UInt(bit_width.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out = Output(Vec(2, UInt(bit_width.W)))
  })
}