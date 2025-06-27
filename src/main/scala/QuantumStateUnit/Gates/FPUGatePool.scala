package QuantumStateUnit.Gates

import QuantumStateUnit.Gates.flipMSB.turnNegative
import Complex_FPU._
import chisel3._
import chisel3.util._

import scala.math._

//The number system used is the IEEE 754 format

/*
The FPUPool has a row of multipliers and adders to calculate the new matrix.
  The sel determines what the QSV gets multiplied by changing the input to the multipliers with the respective number.
 */

/*
Current FPU options

Name                            ID
Normalization                   0
Hadamard Gate                   1
V / sqrt(Not) / sqrt(X) gate    2
sqrt(y) gate                    3
T gate or sqrt(s) gate          4
inverse of T gate               5
U gate/ custom gate             6
 */
class FPUPool(val num_of_qubits : Int, val bw :Int, val mult_pd : Int, val add_pd : Int) extends Module{
  require(bw == 32 || bw == 64 || bw == 128 || bw == 256)
  val io = IO(new Bundle{
    val in_QSV        =  Input(Vec(pow(2,num_of_qubits).toInt, UInt(bw.W)))
    val in_Ugate      =  Input(Vec(4, UInt(bw.W)))
    val in_normalize  =  Input(UInt(bw.W))
    val in_sel        =  Input(UInt(4.W))
    val in_en         =  Input(Bool())
    val out_valid     = Output(Bool())
    val out_QSV       = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bw.W)))
  })

  //The list of Gate Values Begin

  //Hadamard gate
  /*  Matrix: sqrt(1/2)*| 1  1 |
                        | 1 -1 |  */
  val hadamard0 = bw match { //positive sqrt(1/2)
    case 32 => "h39A80000".U(bw.W)
    case 64 => "h3F3504F300000000".U(bw.W)
    case 128 =>"h3FE6A09E667E556E0000000000000000".U(bw.W)
    case 256 =>"h3FFE6A09E667E556DB52D257F79DE6C700000000000000000000000000000000".U(bw.W)
  }
  val hadamard1 = bw match { //negative sqrt(1/2)
    case 32 => "hB9A80000".U(bw.W)
    case 64 => "hBF3504F300000000".U(bw.W)
    case 128 =>"hBFE6A09E667E556E0000000000000000".U(bw.W)
    case 256 =>"hBFFE6A09E667E556DB52D257F79DE6C700000000000000000000000000000000".U(bw.W)
  }

  //V or sqrt(NOT) or sqrt(X) gate
  /*   Matrix:   (1/2) * | (1+j) (1-j) |
                         | (1-j) (1+j) |  */
  val sqrtNot0 = bw match { // (1 + j)/2
    case 32 => "h38003800".U(bw.W)
    case 64 => "h3F0000003F000000".U(bw.W)
    case 128 =>"h3FE00000000000003FE0000000000000".U(bw.W)
    case 256 =>"h3FFE00000000000000000000000000003FFE0000000000000000000000000000".U(bw.W)
  }
  val sqrtNot1 = bw match { // (1 - j)/2
    case 32 => "h3800B800".U(bw.W)
    case 64 => "h3F000000BF000000".U(bw.W)
    case 128 =>"h3FE0000000000000BFE0000000000000".U(bw.W)
    case 256 =>"h3FFE0000000000000000000000000000BFFE0000000000000000000000000000".U(bw.W)
  }

  //sqrt(Y)
  /*  Matrix: (1/2)*| (1+j) -(1+j) |
                    | (1+j)  (1+j) |  */
  val sqrtY0 = bw match {
    case 32 => "h38003800".U(bw.W)
    case 64 => "h3F0000003F000000".U(bw.W)
    case 128 =>"h3FE00000000000003FE0000000000000".U(bw.W)
    case 256 =>"h3FFE00000000000000000000000000003FFE0000000000000000000000000000".U(bw.W)
  }
  val sqrtY1 = bw match {
    case 32 => "hB800B800".U(bw.W)
    case 64 => "hBF000000BF000000".U(bw.W)
    case 128 =>"hBFE0000000000000BFE0000000000000".U(bw.W)
    case 256 =>"hBFFE0000000000000000000000000000BFFE0000000000000000000000000000".U(bw.W)
  }

  //T gate or sqrt(s) gate
  /*  Matrix: | 1        0     |
              | 0 e^(j*pi/4))  |  */
  val gateT0 = bw match { // 1
    case 32 => "h3C000000".U(bw.W)
    case 64 => "h3F80000000000000".U(bw.W)
    case 128 =>"h3FF00000000000000000000000000000".U(bw.W)
    case 256 =>"h3FFF000000000000000000000000000000000000000000000000000000000000".U(bw.W)
  }
  val gateT1 = bw match { // e^(j*pi/4) = sqrt(1/2) + j*sqrt(1/2) = 0.707... + 0.707...
    case 32 => "h39A839A8".U(bw.W)
    case 64 => "h3F3504F33F3504F3".U(bw.W)
    case 128 =>"h3FE6A09E667E556E3FE6A09E667E556E".U(bw.W)
    case 256 =>"h3FFE6A09E667E556DB52D257F79DE6C73FFE6A09E667E556DB52D257F79DE6C7".U(bw.W)
  }

  //T gate or sqrt(s) gate
  /*  Matrix: | 1        0     |
              | 0 e^(-j*pi/4))  |  */
  val inverseT0 = bw match { // 1
    case 32 => "h3C000000".U(bw.W)
    case 64 => "h3F80000000000000".U(bw.W)
    case 128 =>"h3FF00000000000000000000000000000".U(bw.W)
    case 256 =>"h3FFF000000000000000000000000000000000000000000000000000000000000".U(bw.W)
  }
  val inverseT1 = bw match { // e^(j*pi/4) = sqrt(1/2) + j*sqrt(1/2) = 0.707... + 0.707...
    case 32 => "h39A8B9A8".U(bw.W)
    case 64 => "h3F3504F3BF3504F3".U(bw.W)
    case 128 =>"h3FE6A09E667E556EBFE6A09E667E556E".U(bw.W)
    case 256 =>"h3FFE6A09E667E556DB52D257F79DE6C7BFFE6A09E667E556DB52D257F79DE6C7".U(bw.W)
  }

  //Putting each value into a mux to be feed into the multipliers
  /*
  Matrix Corresponding Location
      | 0   1 |
      | 2   3 |
   */
  val FPUInput0 = MuxLookup(io.in_sel, 0.U)(Seq(0.U -> io.in_normalize,
                                                1.U -> hadamard0,
                                                2.U -> sqrtNot0,
                                                3.U -> sqrtY0,
                                                4.U -> gateT0,
                                                5.U -> inverseT0,
                                                6.U -> io.in_Ugate(0)))
  val FPUInput1 = MuxLookup(io.in_sel, 0.U)(Seq(0.U -> 0.U,
                                                1.U -> hadamard0,
                                                2.U -> sqrtNot1,
                                                3.U -> sqrtY1,
                                                4.U -> 0.U,
                                                5.U -> 0.U,
                                                6.U -> io.in_Ugate(1)))
  val FPUInput2 = MuxLookup(io.in_sel, 0.U)(Seq(0.U -> 0.U,
                                                1.U -> hadamard0,
                                                2.U -> sqrtNot0,
                                                3.U -> sqrtY0,
                                                4.U -> 0.U,
                                                5.U -> 0.U,
                                                6.U -> io.in_Ugate(2)))
  val FPUInput3 = MuxLookup(io.in_sel, 0.U)(Seq(0.U -> io.in_normalize,
                                                1.U -> hadamard1,
                                                2.U -> sqrtNot1,
                                                3.U -> sqrtY1,
                                                4.U -> gateT1,
                                                5.U -> inverseT1,
                                                6.U -> io.in_Ugate(3)))


  //complexA will be the input numbers while complexB will be the gate
  val FPUMultiplier = Seq.fill(pow(2,num_of_qubits+1).toInt)(Module(new complex_conjugate_mult(bw,mult_pd,add_pd)))
  val FPUAdder      = Seq.fill(pow(2,num_of_qubits).toInt)(Module(new complex_adder(bw, add_pd)))
  val FPUvalid      = Module(new AndGate(num_of_qubits))

  for(i<-0 until pow(2,num_of_qubits-1).toInt){
    //Number inputs for each multiplication
    FPUMultiplier(4*i  ).io.complexA := io.in_QSV(2*i  )
    FPUMultiplier(4*i  ).io.complexB := FPUInput0
    FPUMultiplier(4*i+1).io.complexA := io.in_QSV(2*i+1)
    FPUMultiplier(4*i+1).io.complexB := FPUInput1
    FPUMultiplier(4*i+2).io.complexA := io.in_QSV(2*i  )
    FPUMultiplier(4*i+2).io.complexB := FPUInput2
    FPUMultiplier(4*i+3).io.complexA := io.in_QSV(2*i+1)
    FPUMultiplier(4*i+3).io.complexB := FPUInput3
    //Bool inputs
    FPUMultiplier(4*i  ).io.in_en    := 1.B
    FPUMultiplier(4*i  ).io.in_valid := io.in_en
    FPUMultiplier(4*i+1).io.in_en    := 1.B
    FPUMultiplier(4*i+1).io.in_valid := io.in_en
    FPUMultiplier(4*i+2).io.in_en    := 1.B
    FPUMultiplier(4*i+2).io.in_valid := io.in_en
    FPUMultiplier(4*i+3).io.in_en    := 1.B
    FPUMultiplier(4*i+3).io.in_valid := io.in_en
    //Attach Multiplier to the Adder
    FPUAdder(2*i  ).io.complexA      := FPUMultiplier(4*i  ).io.out_s
    FPUAdder(2*i  ).io.complexB      := FPUMultiplier(4*i+1).io.out_s
    FPUAdder(2*i+1).io.complexA      := FPUMultiplier(4*i+2).io.out_s
    FPUAdder(2*i+1).io.complexB      := FPUMultiplier(4*i+3).io.out_s
    //Multiplier Bool out into Adder Bool in
    FPUAdder(2*i  ).io.in_en         := 1.B
    FPUAdder(2*i+1).io.in_en         := 1.B
    FPUAdder(2*i  ).io.in_valid      := FPUMultiplier(4*i  ).io.out_valid && FPUMultiplier(4*i+1).io.out_valid
    FPUAdder(2*i+1).io.in_valid      := FPUMultiplier(4*i+2).io.out_valid && FPUMultiplier(4*i+3).io.out_valid
    //Output
    io.out_QSV(2*i  )                := FPUAdder(2*i  ).io.out_s
    io.out_QSV(2*i+1)                := FPUAdder(2*i+1).io.out_s
    //and gate for valid
    FPUvalid.io.in_valid(2*i  )      := FPUAdder(2*i  ).io.out_valid
    FPUvalid.io.in_valid(2*i+1)      := FPUAdder(2*i+1).io.out_valid
  }
  //out is valid
  io.out_valid := FPUvalid.io.out_valid
}

//Used in the main pool
class AndGate(val num_of_qubits : Int) extends Module {
  val io = IO(new Bundle{
    val in_valid   = Input(Vec(pow(2,num_of_qubits).toInt, Bool()))
    val out_valid  = Output(Bool())
  })
  //for reset
  io.out_valid := 0.B
  //And output
  io.out_valid := io.in_valid.reduce(_ & _)
}