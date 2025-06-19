package QuantumStateUnit.QSU_Architecture

import QuantumStateUnit.Gates._
import chisel3._

import scala.math._


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
...
...
num_of_gates will change depending on the number of unique gates that have been implemented into the pool
    Current Count: 6
 */
class QGP(val num_of_qubits : Int, val bit_width : Int, val mult_pd : Int, val add_pd : Int, val num_of_gates : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV    = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val in_sel    = Input(UInt(ceil(log(num_of_gates)/log(2)).toInt.W))
    val in_en     = Input(Bool())
    val out_QSV   = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
    val out_valid = Output(Bool())
  })

  //Out Mux layer
  //Combines all valid gates for each gate type
  val andLayer        = Seq.fill(num_of_gates)(Module(new QGPAndGate(num_of_qubits)))
  val muxLayer        = Module(new QGPMuxLayer(num_of_qubits,bit_width, num_of_gates))
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
    //Always be enabled
    no_op(i).io.in_en           := io.in_en
    //Combining valid gates
    andLayer(0).io.in_valid(i)  := no_op(i).io.out_valid
  }
  muxLayer.io.in_valid(0)       := andLayer(0).io.out_valid

  //Pauli-X GATE    ID: 1
  val pauliX = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new Pauli_X(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    pauliX(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    pauliX(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(1)(2*i)  := pauliX(i).io.out(0)
    muxLayer.io.in_QSV(1)(2*i+1):= pauliX(i).io.out(1)
    //Always be enabled
    pauliX(i).io.in_en          := io.in_en
    //Combining valid gates
    andLayer(1).io.in_valid(i)  := pauliX(i).io.out_valid
  }
  muxLayer.io.in_valid(1)       := andLayer(1).io.out_valid

  //Pauli-Y gate    ID: 2
  val pauliY = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new Pauli_Y(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    pauliY(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    pauliY(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(2)(2*i)  := pauliY(i).io.out(0)
    muxLayer.io.in_QSV(2)(2*i+1):= pauliY(i).io.out(1)
    //Always be enabled
    pauliY(i).io.in_en          := io.in_en
    //Combining valid gates
    andLayer(2).io.in_valid(i)  := pauliY(i).io.out_valid
  }
  muxLayer.io.in_valid(2)       := andLayer(2).io.out_valid

  //Pauli-Z gate    ID: 3
  val pauliZ = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new Pauli_Z(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    pauliZ(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    pauliZ(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(3)(2*i)  := pauliZ(i).io.out(0)
    muxLayer.io.in_QSV(3)(2*i+1):= pauliZ(i).io.out(1)
    //Always be enabled
    pauliZ(i).io.in_en          := io.in_en
    //Combining valid gates
    andLayer(3).io.in_valid(i)  := pauliZ(i).io.out_valid
  }
  muxLayer.io.in_valid(3)       := andLayer(3).io.out_valid

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
    SGate(i).io.in_en          := io.in_en
    //Combining valid gates
    andLayer(4).io.in_valid(i)  := SGate(i).io.out_valid
  }
  muxLayer.io.in_valid(4)       := andLayer(4).io.out_valid

  //Inverse S Gate  ID: 5
  val InverseSGate = Seq.fill(pow(2,num_of_qubits-1).toInt)(Module(new InverseS_gate(bit_width)))
  for(i <- 0 until pow(2,num_of_qubits-1).toInt){
    //Into the Gate
    InverseSGate(i).io.in(0)           := io.in_QSV(2*i)  //some even number
    InverseSGate(i).io.in(1)           := io.in_QSV(2*i+1)//some odd number
    //Into the mux
    muxLayer.io.in_QSV(5)(2*i)  := InverseSGate(i).io.out(0)
    muxLayer.io.in_QSV(5)(2*i+1):= InverseSGate(i).io.out(1)
    //Always be enabled
    InverseSGate(i).io.in_en          := io.in_en
    //Combining valid gates
    andLayer(5).io.in_valid(i)  := InverseSGate(i).io.out_valid
  }
  muxLayer.io.in_valid(5)       := andLayer(5).io.out_valid

  /*
  ADD MORE GATES HERE STARTING ADD ID: 6
   */

  //OUTPUT
  io.out_QSV   := muxLayer.io.out_QSV
  io.out_valid := muxLayer.io.out_valid
}

/*
  The mux layer will collect all outputs from the QGP and output the selected gate's output.
  There will be multiple And gate layer for each layer of gates.
    This is to simplify the valid output going into the mux layer

 NOTE: Instead of relying on num of qubits for number of inputs, instead it needs to be explicitly stated for the gates.
       Adjust num_of_Gates depending on progress into the project
 */
class QGPMuxLayer(val num_of_qubits : Int, val bit_width : Int, val num_of_gates /*Unique Gates*/ : Int) extends Module {
  val io = IO(new Bundle{
    val in_QSV    = Input(Vec(num_of_gates, Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W))))
    val in_sel    = Input(UInt(ceil(log(num_of_gates)/log(2)).toInt.W))
    val in_valid  = Input(Vec(num_of_gates,Bool()))
    val out_valid = Output(Bool())
    val out_QSV   = Output(Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W)))
  })
  //actual circuit
  io.out_QSV  := io.in_QSV(io.in_sel)
  io.out_valid:= io.in_valid(io.in_sel)
}

class QGPAndGate(val num_of_qubits : Int) extends Module {
  val io = IO(new Bundle{
    val in_valid   = Input(Vec(pow(2,num_of_qubits-1).toInt, Bool()))
    val out_valid  = Output(Bool())
  })
  //for reset
  io.out_valid := 0.B
  //And output
  io.out_valid := io.in_valid.reduce(_ & _)
}

