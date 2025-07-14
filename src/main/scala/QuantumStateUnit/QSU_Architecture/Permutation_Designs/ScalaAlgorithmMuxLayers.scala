package QuantumStateUnit.QSU_Architecture.Permutation_Designs

import chisel3._
import chisel3.util._

import scala.math._

/*
  Basically permutationMux_Top: This allows to create x amount of mux layers, where each layer can switch the
    position/"behaviors"/permutation of the vector. The position/"behaviors"/permutation is where it is located
    on a truth table.

  Take the example below where Q1 changes values every 2 bits while Q0 changes every bit.
    If we want to switch the pattern of when the bits change to 0 to 1, we can switch position 1 and 2.
      Q1 Q0  Position                       Q1  Q0  Position
       0  0     0         new position       0   0     0
       0  1     1                            1   0     2
       1  0     2                            0   1     1
       1  1     3                            1   1     3
   This is what the Mux does, and this behavior has a consistent pattern that is further elaborated on
    in the internal hardware in permutationMux().
 */
/*
How Select will look for each layer
 ect...
 Layer X -> Q2  3  2  1  0          <- All of these are inputs to select the qubit below
 Layer X -> Q1  4  3  2  1  0
 Layer X -> Q0: 5  4  3  2  1  0
 Target Qubits: Q5 Q4 Q3 Q2 Q1 Q0
 */
//permutation_Seq should be a sequence that starts at 0 and goes to x by increments of 1
class StackedMuxPermutation(val num_of_qubits: Int, val bit_width: Int, val permutation_Seq : Seq[Int]) extends Module{
  val io = IO(new Bundle{
    val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W)))   // n input channels (bw bits each)
    //When moving a qubit to a higher position
    val in_sel = Input(MixedVec(permutation_Seq.map(i => UInt(ceil(log(num_of_qubits - i)/log(2)).toInt.W))))
    val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n output channels
  })
  //contains the three permutation layers

    val permutation = permutation_Seq.map { i =>
      val s = Module(new permutationLayer(num_of_qubits, bit_width, permutation_Seq(i)))
      s //I don't know why it needs to be here, but it's here
  }

  //Connecting Layer to Layer
  for(num_of_perm <- 0 until permutation_Seq.length + 1){ //plus one is the io.input/io.output QSV
    if(num_of_perm == 0) {
      permutation(0).io.in_QSV := io.in_QSV
    } else if(num_of_perm == permutation_Seq.length) {
      io.out_QSV := permutation(num_of_perm - 1).io.out_QSV
    } else {
      permutation(num_of_perm).io.in_QSV := permutation(num_of_perm - 1).io.out_QSV
    }
  }

  //select
  for(num_of_permutations <- 0 until permutation_Seq.length) {
    permutation(num_of_permutations).io.in_sel := io.in_sel(num_of_permutations)
  }
}

//Permutation Layer
/* SHAKY EXPLANATION
When a qubit gets pushed to the front, it starts to have a pattern that follows the code below.
The mux layer separates each permutation from each other, so they could be individually be selected by a mux.

The pattern is controlled by 2 elements
                target location:        determines how many coefficients switch at a time
                distance from location: determines how many times the pattern will repeat
The code below employments an algorithm that determines where each Coefficient goes to create the new permutation
The pattern has a couple of traits:
        switch every other element
                ex)  1 won't switch 0
                     2 will switch  1
                     3 won't        0
                     4 will         1
        Target location will affect how many elements will switch at a time
                ex) 3 -> 2
                    0 won't switch
                    0 won't switch
                    1 will switch -|
                    1 will switch -|--|
                    1 will <-------|  |
                    1 will <----------|
                    0 won't
                    0 won't
       The distance between the two will determine  how long the switch pattern is before repeating
                ex) 3 -> 1: pattern length 2^(3-1) repeats: 3-3 times | 2 -> 1: pattern length 2^(2-1) repeats 3-2 times
                    0 wont                                              0 won't
                    1 will --|                                          1 will --|
                    2 wont   |                                          2 will <-|
                    3 will --|-|                                        3 won't
                    4 will <-| |                                        4 won't
                    5 wont     |                                        5 will --|
                    6 will <---|                                        6 will <-|
                    7 wont                                              7 won't

    The algorithm:

       for(number of qubits - target location) //Creates a new permutation for all of X ∈ {qubitAmount,amount -1, ..., 0}
          for(2^(number of qubits - target location)) //Will repeat the difference amount
            for(2^(current distance from target location)) //The pattern length
                //The switching value
                        x + 1           2^(distance)              x                        2^(distance+1)
                          ↓                 ↓                     ↓               same            ↓
                QSV(some odd number + Repeat below) <-> QSV(some even number + repeat below + distance)
                  ↑                                      ↑
          Value that is one less stays same      Value that is one more stays same
 */

/*
THIS SHOULD MOVE QY to QX where Y > X
  target_location refers to the qubit it's switching with starting at Q0 to Q(N-1) where N is the total number of qubits
 */
  class permutationLayer(val num_of_qubits: Int, val bit_width: Int, val target_location : Int) extends Module {
    val io = IO(new Bundle {
      val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n input channels (bw bits each)
      val in_sel = Input(UInt(ceil(log(num_of_qubits - target_location)/log(2)).toInt.W)) // Routing control signals
      val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n output channels
    })

    val muxLayer1 = Module(new muxLayer(num_of_qubits - target_location, bit_width * pow(2,target_location).toInt)) //ignores the permutation with the first qubits
    muxLayer1.io.in_sel := io.in_sel

    //combining 2^x vectors together to fit through the algorithm. Used for permutation to Q1 and Q2, but not Q0
    val   tieVectorLayer = Module(new   tieVecLayer(num_of_qubits, bit_width, target_location))
    val untieVectorLayer = Module(new untieVecLayer(num_of_qubits, bit_width, target_location))
    for(i <- 0 until pow(2,num_of_qubits).toInt){
      tieVectorLayer.io.in_QSV(i)   := io.in_QSV(i)
    }

    //if the target is itself then there is no switching of coefficients
    for(first_permutation <- 0 until pow(2,num_of_qubits - target_location).toInt) {
      muxLayer1.io.in_QSV(0)(first_permutation) := tieVectorLayer.io.out_QSV(first_permutation)
    }

    //The switching algorithm / MUX: tried explaining at above
    for ( s <- 2 until num_of_qubits + 1 - target_location) { //increases the distance from the target location.
      for (i <- 0 until pow(2,num_of_qubits - s - target_location).toInt) { //skips the mirrored part of the pattern
        for (j <- 0 until pow(2,s-2).toInt) { //the pattern
          //switch
          muxLayer1.io.in_QSV(s-1)((2*j+1) + i*pow(2,s).toInt)                   := tieVectorLayer.io.out_QSV(2*j + pow(2,s-1).toInt + i*pow(2,s).toInt)
          muxLayer1.io.in_QSV(s-1)(2*j + pow(2,s-1).toInt +i*pow(2,s).toInt)     := tieVectorLayer.io.out_QSV((2*j+1) + i*pow(2,s).toInt)
          //doesn't switch
          muxLayer1.io.in_QSV(s-1)((2*j) + i*pow(2,s).toInt)                     := tieVectorLayer.io.out_QSV((2*j) + i*pow(2,s).toInt)
          muxLayer1.io.in_QSV(s-1)(2*j + 1 + pow(2,s-1).toInt + i*pow(2,s).toInt):= tieVectorLayer.io.out_QSV(2*j + 1 + pow(2,s-1).toInt + i*pow(2,s).toInt)
        }
      }
    }

  //untie all vectors from the muxlayer
    untieVectorLayer.io.in_QSV := muxLayer1.io.out_QSV
  //output from mux
  io.out_QSV := untieVectorLayer.io.out_QSV
}


//muxLayer is used in the permutation layer once
/*
  The output mux using vector indexing
    ignore_qubits represent the first couple permutations that are ignored when dealing with moving qubits to positions
 */
class muxLayer(val num_of_qubits : Int, val bit_width : Int) extends Module {
  val io = IO(new Bundle{
    val in_QSV  = Input(Vec(num_of_qubits, Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W))))
    val in_sel  = Input(UInt(ceil(log(num_of_qubits)/log(2)).toInt.W))
    val out_QSV = Output(Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W)))
  })
  io.out_QSV := io.in_QSV(io.in_sel)
}


/*
...
Initialize what comes in and out of the muxes
Moving a qubit to the 2nd, 3rd, ect, follows the same algorithm but moves 2^qubit_position at a time

!!tie amount has to be one less than the total of vectors being put together
!!tie amount encapusulates 2^tie_amount vectors
...
 */
//combines n amount of vectors into one coefficient
class tieVec(val bit_width : Int, val tie_amount : Int) extends Module{
  val io = IO(new Bundle{
    val in  =  Input(Vec(tie_amount, UInt(bit_width.W)))
    val out = Output(UInt((tie_amount*bit_width).W))
  })

  //Collecting all vectors into one input
  val inputs: Seq[UInt] = Seq.fill(tie_amount)(Wire(UInt(bit_width.W)))
  for(i <- 0 until tie_amount){
    inputs(i) := io.in(i)
  }
  //correcting the direction of the data
  io.out := Cat(inputs.reverse)
}
class tieVecLayer(val num_of_qubits : Int, val bit_width : Int, val tie_amount : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV  =  Input(Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W)))
    val out_QSV = Output(Vec(pow(2,num_of_qubits-tie_amount).toInt,UInt(( bit_width * pow(2,tie_amount).toInt).W)))
  })
  //the zip tie to connect to vectors together
  val tieVector = Seq.fill(pow(2,num_of_qubits-tie_amount).toInt)(Module(new tieVec(bit_width, pow(2, tie_amount).toInt)))
  //putting into respective inputs and outputs
  for(j <- 0 until pow(2,num_of_qubits-tie_amount).toInt) {//number of tieVector
    for (i <- 0 until pow(2, tie_amount).toInt) { //number of input
      tieVector(j).io.in(i) := io.in_QSV(pow(2, tie_amount).toInt * j + i)
    }
    io.out_QSV(j) := tieVector(j).io.out
  }
}


/*
...
Ununiting the vectors
...
 */
//separates the vectors into their original form
class untieVec( val bit_width : Int, val tie_amount : Int) extends Module{
  val io = IO(new Bundle{
    val in  =  Input(UInt((tie_amount * bit_width).W))
    val out = Output(Vec(tie_amount , UInt(bit_width.W)))
  })
  for(i <- 0 until tie_amount){
    io.out(i) := io.in((i+1)*bit_width-1,i*bit_width)
  }
}
class untieVecLayer(val num_of_qubits : Int, val bit_width : Int, val tie_amount  : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV  =  Input(Vec(pow(2,num_of_qubits-tie_amount).toInt,UInt((bit_width * pow(2,tie_amount).toInt).W)))
    val out_QSV = Output(Vec(pow(2,num_of_qubits).toInt,UInt(bit_width.W)))
  })
  //To separate the vectors from each other
  val untieVector = Seq.fill(pow(2,num_of_qubits-tie_amount).toInt)(Module(new untieVec(bit_width, pow(2, tie_amount).toInt)))
  //putting into respective inputs and outputs
  for(j <- 0 until pow(2,num_of_qubits-tie_amount).toInt) {//number of untieVector
    // 1 input 2^x outputs
    untieVector(j).io.in := io.in_QSV(j)
      for (i <- 0 until pow(2, tie_amount).toInt) { //number of output
        io.out_QSV((pow(2, tie_amount).toInt * j) + i) := untieVector(j).io.out(i)
      }
    }
  }
