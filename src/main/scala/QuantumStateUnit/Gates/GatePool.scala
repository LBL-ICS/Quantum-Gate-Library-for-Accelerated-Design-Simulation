package QuantumStateUnit.Gates

import chisel3._
import chisel3.util.ShiftRegister

import scala.math._

//The number system used is the IEEE 754 format

//QGP: Quantum Gate Pool
/*
Contains all the created quantum gates in OneGatePool and TwoGatePool
This will be the longest component depending on the amount of gates included into the function
  NAME          ID
No-Op:          0
Pauli-X:        1
Pauli-Y:        2
Pauli-Z:        3
S Gate          4
Inverse S Gate  5
Controlled X    6
Controlled Y    7
Controlled Z    8
Swap Gate       9
Toffoli         10
...
num_of_gates will change depending on the number of unique gates that have been implemented into the pool
    Current Count: 11
 */
class nonFPUPool(val num_of_qubits : Int, val bit_width : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV    = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val in_sel    = Input(UInt(4.W))
    val in_en     = Input(Bool())
    val out_QSV   = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val out_valid = Output(Bool())
  })

  //Out Mux layer
  val muxLayer        = Module(new QGPMuxLayer(num_of_qubits,bit_width, 11/*number of gates in module*/))
  muxLayer.io.in_sel  := io.in_sel


  //NO-OP GATE    ID: 0
  val no_op = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new No_op(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    no_op(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    no_op(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(0)(2*i)  := no_op(i).io.out(0)
    muxLayer.io.in_QSV(0)(2*i+1):= no_op(i).io.out(1)
  }

  //Pauli-X GATE    ID: 1
  val pauliX = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new Pauli_X(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    pauliX(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    pauliX(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(1)(2*i)  := pauliX(i).io.out(0)
    muxLayer.io.in_QSV(1)(2*i+1):= pauliX(i).io.out(1)
  }

  //Pauli-Y gate    ID: 2
  val pauliY = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new Pauli_Y(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    pauliY(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    pauliY(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(2)(2*i)  := pauliY(i).io.out(0)
    muxLayer.io.in_QSV(2)(2*i+1):= pauliY(i).io.out(1)
  }

  //Pauli-Z gate    ID: 3
  val pauliZ = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new Pauli_Z(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    pauliZ(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    pauliZ(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(3)(2*i)  := pauliZ(i).io.out(0)
    muxLayer.io.in_QSV(3)(2*i+1):= pauliZ(i).io.out(1)
  }

  //S gate   ID: 4
  val SGate = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new S_gate(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    SGate(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    SGate(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(4)(2*i)  := SGate(i).io.out(0)
    muxLayer.io.in_QSV(4)(2*i+1):= SGate(i).io.out(1)
    //Always be enabled
  }

  //Inverse S Gate  ID: 5
  val InverseSGate = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new InverseS_gate(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    InverseSGate(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    InverseSGate(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(5)(2*i  ):= InverseSGate(i).io.out(0)
    muxLayer.io.in_QSV(5)(2*i+1):= InverseSGate(i).io.out(1)
  }

  //Controlled Not gate, CX, or CNOT    ID: 6
  val CNOTGate = Seq.fill(pow(2,num_of_qubits-2).toInt)(Module(new CNOT(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-2).toInt){
    for(j <- 0 until 4) {
      CNOTGate(i).io.in(j)              := io.in_QSV(4 * i + j) //into the Gate
      muxLayer.io.in_QSV(6)(4 * i + j)  := CNOTGate(i).io.out(j) //Into the mux
    }
  }

  //Controlled Y gate    ID: 7
  val CYGate = Seq.fill(pow(2,num_of_qubits-2).toInt)(Module(new CPauli_Y(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-2).toInt){
    for(j <- 0 until 4) {
      CYGate(i).io.in(j)                := io.in_QSV(4 * i + j) //into the Gate
      muxLayer.io.in_QSV(7)(4 * i + j)  := CYGate(i).io.out(j) //Into the mux
    }
  }

  //Controlled Z Gate    ID: 8
  val CZGate = Seq.fill(pow(2,num_of_qubits-2).toInt)(Module(new CPauli_Z(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-2).toInt){
    for(j <- 0 until 4) {
      CZGate(i).io.in(j)                := io.in_QSV(4 * i + j) //into the Gate
      muxLayer.io.in_QSV(8)(4 * i + j)  := CZGate(i).io.out(j) //Into the mux
    }
  }

  //Controlled Z Gate    ID: 9
  val swapGate = Seq.fill(pow(2,num_of_qubits-2).toInt)(Module(new SwapGate(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-2).toInt){
    for(j <- 0 until 4) {
      swapGate(i).io.in(j)              := io.in_QSV(4 * i + j) //into the Gate
      muxLayer.io.in_QSV(9)(4 * i + j)  := swapGate(i).io.out(j) //Into the mux
    }
  }

  //Toffoli             ID: 10
  val ToffoliGate = Seq.fill(pow(2,num_of_qubits-3).toInt)(Module(new Toffoli(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-3).toInt){
    for(j <- 0 until 8) {
      ToffoliGate(i).io.in(j)           := io.in_QSV(8 * i + j) //into the Gate
      muxLayer.io.in_QSV(10)(8 * i + j)  := ToffoliGate(i).io.out(j) //Into the mux
    }
  }

  /*
  ADD MORE GATES HERE STARTING ADD ID: 11
   */

  //OUTPUT
  io.out_QSV   := muxLayer.io.out_QSV
  io.out_valid := ShiftRegister(io.in_en, 1)
}

/*
  The mux layer will collect all outputs from the QGP and output the selected gate's output.
 NOTE: Instead of relying on num of qubits for number of inputs, instead it needs to be explicitly stated for the gates.
       Adjust num_of_Gates depending on progress into the project
 */
class QGPMuxLayer(val num_of_qubits : Int, val bit_width : Int, val num_of_gates /*Unique Gates*/ : Int) extends Module {
  val io = IO(new Bundle{
    val in_QSV    = Input(Vec(num_of_gates, Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W))))
    val in_sel    = Input(UInt(ceil(log(num_of_gates)/log(2)).toInt.W))
    val out_QSV   = Output(Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W)))
  })
  //actual circuit
  io.out_QSV  := io.in_QSV(io.in_sel)
}


