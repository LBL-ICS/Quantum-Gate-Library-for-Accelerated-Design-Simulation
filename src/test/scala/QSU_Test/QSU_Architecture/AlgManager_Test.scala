package QSU_Test.QSU_Architecture

import QuantumStateUnit.QSU_Architecture._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/*
  2 qubits
  IEEE 754: 16 bit precision
  Circuit length: 9
  Delay: 3 clock cycles
 */

//Test the counter, to ensure that the correct permutation and gate is outputed
class AlgManager_Test1 extends AnyFlatSpec with ChiselScalatestTester {
  "AlgManager" should "Count" in
    test(new AlgorithmManager(2, 32, 5, 3, 6)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //All Inputs and Output for algManager
      /*
        val in_QSV              = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
        val in_Permutation      = Input(Vec(Alg_length, UInt(ceil(log(num_of_qubits)/log(2)).toInt.W))) //
        val in_Gate             = Input(Vec(Alg_length, UInt(ceil(log(num_of_gates)/log(2)).toInt.W))) //There is only so many gates, so it gets 1 byte
        val in_en_newData       = Input(Bool()) //Replaces Initial state and algorithm with above inputs
        val in_en_next          = Input(Bool()) //For Updating the QSR
        val out_en_QSR          = Output(Bool()) //For updating QSV with new value from gate pool
        val out_en_QGP          = Output(Bool()) //Tell when QGP have valid input
        val out_newState        = Output(Bool()) //For replacing QSV from manager
        val out_endofAlg        = Output(Bool()) //Signifies that the algorithm has ended
        val out_sel_gate        = Output(UInt(ceil(log(num_of_gates)/log(2)).toInt.W)))
        val out_sel_permutation = Output(UInt(num_of_qubits.W))
        val out_QSV             = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
       */
      //QSV is not a focus
      for(i <- 0 until 4){
          dut.io.in_QSV(i).poke(0.U)
      }
      //Permutation 0 1 0 1 0
      dut.io.in_Permutation(0).poke(0.U) //Always output 0.U despite input being 1.U
      dut.io.in_Permutation(1).poke(1.U)
      dut.io.in_Permutation(2).poke(0.U)
      dut.io.in_Permutation(3).poke(1.U)
      dut.io.in_Permutation(4).poke(0.U)
      //Gates: no-op X Y X stop
      dut.io.in_Gate(0).poke(0.U)
      dut.io.in_Gate(1).poke(1.U)
      dut.io.in_Gate(2).poke(0.U)
      dut.io.in_Gate(3).poke(1.U)
      dut.io.in_Gate(4).poke("b111".U)//all 1's to stop
      //Initial en
      dut.io.in_en_newData.poke(0.B)
      dut.io.in_en_next.poke(0.B)

      //Give register values
      dut.clock.step()
      dut.io.in_en_newData.poke(1.B)
      dut.clock.step()
      dut.io.in_en_newData.poke(0.B)
      dut.clock.step(2)
      //First gate and permutation
      dut.io.out_sel_permutation.expect(0.U)
      dut.io.out_sel_gate.expect(0.U)

      //Gate finished, so move on
      dut.clock.step()
      dut.io.in_en_next.poke(1.B)
      dut.clock.step()
      dut.io.in_en_next.poke(0.B)
      dut.clock.step()
      //Second gate and permutation
      dut.io.out_sel_permutation.expect(1.U)
      dut.io.out_sel_gate.expect(1.U)

      //Gate finished, so move on
      dut.clock.step()
      dut.io.in_en_next.poke(1.B)
      dut.clock.step()
      dut.io.in_en_next.poke(0.B)
      dut.clock.step()
      //Third gate and permutation
      dut.io.out_sel_permutation.expect(0.U)
      dut.io.out_sel_gate.expect(0.U)

      //Gate finished, so move on
      dut.clock.step()
      dut.io.in_en_next.poke(1.B)
      dut.clock.step()
      dut.io.in_en_next.poke(0.B)
      dut.clock.step()
      //Fourth gate and permutation
      dut.io.out_sel_permutation.expect(1.U)
      dut.io.out_sel_gate.expect(1.U)

      //Gate finished, so move on
      dut.clock.step()
      dut.io.in_en_next.poke(1.B)
      dut.clock.step()
      dut.io.in_en_next.poke(0.B)
      dut.clock.step(2)
      //Stop and end
      dut.io.out_endofAlg.expect(1.B)
    }
}

//Test the timing of when the manager sends en, to the QSR and gate-pool
class AlgManager_Test2 extends AnyFlatSpec with ChiselScalatestTester {
    "AlgManager" should "sendEnProperly" in
      test(new AlgorithmManager(2, 32, 5, 3, 6)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
          //All Inputs and Output for algManager
          /*
  val in_QSV              = Input(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
  val in_Permutation      = Input(Vec(Alg_length, UInt(ceil(log(num_of_qubits)/log(2)).toInt.W))) //
  val in_Gate             = Input(Vec(Alg_length, UInt(ceil(log(num_of_gates)/log(2)).toInt.W))) //There is only so many gates, so it gets 1 byte
  val in_en_newData       = Input(Bool()) //Replaces Initial state and algorithm with above inputs
  val in_en_next          = Input(Bool()) //For Updating the QSR
  val out_en_QSR          = Output(Bool()) //For updating QSV with new value from gate pool
  val out_en_QGP          = Output(Bool()) //Tell when QGP have valid input
  val out_newState        = Output(Bool()) //For replacing QSV from manager
  val out_endofAlg        = Output(Bool()) //Signifies that the algorithm has ended
  val out_sel_gate        = Output(UInt(ceil(log(num_of_gates)/log(2)).toInt.W)))
  val out_sel_permutation = Output(UInt(num_of_qubits.W))
  val out_QSV             = Output(Vec(pow(2,num_of_qubits).toInt, UInt(bit_width.W)))
 */
        //Not relevant to the test
        for(i <- 0 until 4){
            dut.io.in_Gate(i).poke(0.U)
            dut.io.in_Permutation(i).poke(0.U)
            dut.io.in_QSV(i).poke(0.U)
        }
        dut.io.in_Gate(4).poke("b111".U)
        dut.io.in_Permutation(4).poke(0.U)

        //update registers with new values
        dut.clock.step()
        dut.io.in_en_newData.poke(1.B)
        dut.clock.step()
        dut.io.in_en_newData.poke(0.B)

        //New values cause QSR and newstate === 1.B
        dut.io.out_newState.expect(1.B)
        dut.io.out_en_QSR.expect(1.B)
        dut.io.out_en_QGP.expect(0.B)

        //start the gates after initial delay
        dut.clock.step(2*3)// 2 * pd where pd = 3
        dut.io.out_en_QGP.expect(1.B)
        dut.clock.step()
        dut.io.out_en_QGP.expect(0.B)

        //Test the updating of registers after gate-pool finishes a calculation
        dut.clock.step()
        dut.io.in_en_next.poke(1.B)
        dut.clock.step()
        dut.io.in_en_next.poke(0.B)

        dut.clock.step(3) //pd = 3
        dut.io.out_newState.expect(0.B)
        dut.io.out_en_QSR.expect(1.B)
        dut.io.out_en_QGP.expect(0.B)

        dut.clock.step(3)//2*pd = 6
        dut.io.out_newState.expect(0.B)
        dut.io.out_en_QSR.expect(0.B)
        dut.io.out_en_QGP.expect(1.B)
      }
}