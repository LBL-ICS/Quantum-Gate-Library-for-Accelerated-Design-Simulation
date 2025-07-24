package QuantumStateUnit.QSU_Architecture

import QuantumStateUnit.Old.{tieVecLayer, untieVecLayer}
import QuantumStateUnit.OtherComponents.Switch2x2._
import QuantumStateUnit.OtherComponents.WireLayer._
import chisel3._
import chisel3.util._

import scala.math._

class StackedPermutationSwitchGrids(val num_of_qubits : Int, val bit_width : Int, val permutation_Seq : Seq[Int]) extends Module {
  val io = IO(new Bundle{
    val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W)))   // n input channels (bw bits each)
    //When moving a qubit to a higher position
    val in_sel = Input(MixedVec(permutation_Seq.map(i => UInt(ceil(log(num_of_qubits - i)/log(2)).toInt.W))))
    val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n output channels
  })
  //contains the three permutation layers

  val permutation = permutation_Seq.map { i =>
    val s = Module(new PermutationSwitchGrid(num_of_qubits, bit_width, permutation_Seq(i)))
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


//Includes the tie vector module in order to have a target qubit
//Also include the Demux to further simplify the input
class PermutationSwitchGrid(val num_of_qubits: Int, val bit_width: Int, val target_location : Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n input channels (bw bits each)
    val in_sel = Input(UInt((log2Ceil(num_of_qubits - target_location)).W)) // Routing control signals
    val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n output channels
  })

  //combining 2^x vectors together to fit through the algorithm. Used for permutation to Q1 and Q2, but not Q0
  val   tieVectorLayer = Module(new   tieVecLayer(num_of_qubits, bit_width, target_location))
  val untieVectorLayer = Module(new untieVecLayer(num_of_qubits, bit_width, target_location))
  val       switchGrid = Module(new PSGtarget0(num_of_qubits - target_location, bit_width * pow(2,target_location).toInt))

  // in -> tie -> switch grid -> untie -> out
  tieVectorLayer.io.in_QSV    := io.in_QSV
  switchGrid.io.in_QSV        := tieVectorLayer.io.out_QSV
  untieVectorLayer.io.in_QSV  := switchGrid.io.out_QSV
  io.out_QSV                  := untieVectorLayer.io.out_QSV

  //A decoder to turn a number into the proper sel
  val outWidth = 1 << num_of_qubits - target_location - 1
  switchGrid.io.in_sel := UIntToOH(io.in_sel, outWidth)
}



//Only targets the 0th qubit. Also determines the inputs and outputs of the switches
//PSG stands for permutation switch grid. Only switches one coefficient at a time
class PSGtarget0(val num_of_qubits: Int, val bit_width: Int) extends Module{
  val io = IO(new Bundle{
    val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n input channels (bw bits each)
    val in_sel = Input(UInt((num_of_qubits).W)) // Routing control signals: RightMostBit is ignored, expects decoder
    val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bit_width.W))) // n output channels
  })
  //2x2 switches: There is 2^qubit_amount - 1 for every permutation to a new spot.
  val switch  = Seq.fill((num_of_qubits-1)*pow(2, num_of_qubits - 2).toInt)(Module(new Switch2x2(bit_width)))
  val wireL   = Seq.fill(num_of_qubits)(Module(new WireLayer(num_of_qubits, bit_width)))

  wireL(0).io.input := io.in_QSV                          //Entering the Grid of switches
  io.out_QSV        := wireL(num_of_qubits - 1).io.output //Exiting the Grid of switches

  //My headache...
  /*
  i and j doesn't determine the number of qubits per layer, the number of qubits does.
  The number of qubits also determines the amount of layers there are.
 */
  for ( s <- 2 until num_of_qubits + 1) { //increases the distance from the target location. Determines layer.
    //Both i and j determines the inputs and outputs of each switch gate depending on S
    for (i <- 0 until pow(2,num_of_qubits - s).toInt) { //skips the mirrored part of the pattern
      for (j <- 0 until pow(2,s-2).toInt) { //the pattern
        //switch:        wire -> switch -> wire
          //wire to switch
          switch((s-2)*pow(2, num_of_qubits - 2).toInt +(i * pow(2, s - 2).toInt)+ j).io.input(0) := wireL(s-2).io.output(2*j + pow(2,s-1).toInt + i*pow(2,s).toInt)
          switch((s-2)*pow(2, num_of_qubits - 2).toInt +(i * pow(2, s - 2).toInt)+ j).io.input(1) := wireL(s-2).io.output((2*j+1) + i*pow(2,s).toInt)
          //switch to wire
          wireL(s-1).io.input(2*j + pow(2,s-1).toInt + i*pow(2,s).toInt)    := switch((s-2)*pow(2, num_of_qubits - 2).toInt +(i * pow(2, s - 2).toInt)+ j).io.output(0)
          wireL(s-1).io.input((2*j+1) + i*pow(2,s).toInt)                   := switch((s-2)*pow(2, num_of_qubits - 2).toInt +(i * pow(2, s - 2).toInt)+ j).io.output(1)
        //doesn't switch: wire -> wire
        wireL(s-1).io.input((2*j) + i*pow(2,s).toInt)                       := wireL(s-2).io.output((2*j) + i * pow(2,s).toInt)
        wireL(s-1).io.input(2*j + 1 + pow(2,s-1).toInt + i*pow(2,s).toInt)  := wireL(s-2).io.output(2*j + 1 + pow(2,s-1).toInt + i*pow(2,s).toInt)
        //Add the select for each layer: The Right Most Bit is excluded from the circuit due to being connected to not changing at all anyway
        switch(((s-2)*pow(2, num_of_qubits - 2).toInt +(i * pow(2, s - 2).toInt)+ j)).io.in_sel := io.in_sel(s-1)
      }
    }
  }
  //End of loop
}