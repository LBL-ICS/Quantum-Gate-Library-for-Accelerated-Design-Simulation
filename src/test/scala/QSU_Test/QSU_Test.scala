package QSU_Test

import QuantumStateUnit._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/*
  name                num
number of qubits       3
bit width             32
Algorithm Length      10
mult pd                3
add pd                 3
Manager Delay          3
 */
class QSU_Test1 extends AnyFlatSpec with ChiselScalatestTester {
  "QSU" should "DoGateOperations" in
    test(new TopQSU(/*qubits*/3, /*bw*/32, /*AL*/10, 3, 3, 0)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //initial QSV: 1/2|000> + 1/2|111> + j(1/2)|101> + j(1/2)|010>
      dut.io.in_QSV(0).poke("h38000000".U) //000 : 1/2
      dut.io.in_QSV(1).poke("h00000000".U) //001 : 0...
      dut.io.in_QSV(2).poke("h38000000".U) //010 : 1/2
      dut.io.in_QSV(3).poke("h00000000".U) //011 : 0...
      dut.io.in_QSV(4).poke("h00000000".U) //100 : 0...
      dut.io.in_QSV(5).poke("h38000000".U) //101 : 1/2
      dut.io.in_QSV(6).poke("h00000000".U) //110 : 0...
      dut.io.in_QSV(7).poke("h38000000".U) //111 : 1/2

      /*
      Quantum Circuit:
        |Q0> -- X -- S    --
        |Q1> -- Y -- S^-1 -- stop
        |Q2> -- Z -- Y    --
      Permutation:  Q0  Q1  Q2  Q0  Q1   Q2  ...
      Gate:         X   Y   Z   S   S^-1 Y   STOP
       */
      dut.io.in_Permutation(0).poke(0.U) //doesn't matter
      dut.io.in_Permutation(1).poke(0.U) //Q0
      dut.io.in_Permutation(2).poke(1.U) //Q1
      dut.io.in_Permutation(3).poke(2.U) //Q2
      dut.io.in_Permutation(4).poke(0.U) //Q0
      dut.io.in_Permutation(5).poke(1.U) //Q1
      dut.io.in_Permutation(6).poke(2.U) //doesn't matter
      dut.io.in_Gate(0).poke(0.U) //no op
      dut.io.in_Gate(1).poke(1.U) //x
      dut.io.in_Gate(2).poke(2.U) //y
      dut.io.in_Gate(3).poke(3.U) //z
      dut.io.in_Gate(4).poke(4.U) //s
      dut.io.in_Gate(5).poke(5.U) //sqrt(s)
      dut.io.in_Gate(6).poke(2.U) //y
      for(i <- 0 until 3){ //Rest of the unused space
        dut.io.in_Permutation(i+7).poke(0.U)
        dut.io.in_Gate(i+7).poke("b111".U) //STOP
      }

      //give data to start algorithm
      dut.clock.step()
      dut.io.in_en_newData.poke(1.B)
      dut.clock.step()
      dut.io.in_en_newData.poke(0.B)


      //initial QSV: 1/2|000> - 1/2|111> + j(1/2)|101> + j(1/2)|010>
      dut.clock.step(75)
      dut.io.out_flag.expect(1.B)
      dut.io.out_state(0).expect("h38000000".U) //000 : 1/2
      dut.io.out_state(1).expect("h00000000".U) //001 : 0...
      dut.io.out_state(2).expect("h00003800".U) //010 : j1/2
      dut.io.out_state(3).expect("h00000000".U) //011 : 0...
      dut.io.out_state(4).expect("h00000000".U) //100 : 0...
      dut.io.out_state(5).expect("h00003800".U) //101 : j1/2
      dut.io.out_state(6).expect("h00000000".U) //110 : 0...
      dut.io.out_state(7).expect("hb8000000".U) //111 : -1/2
      /*
      NOTES FROM TEST 1:
        THE FIRST GATE SHOULD BE No-Op WITH ID 0
       */
    }
}