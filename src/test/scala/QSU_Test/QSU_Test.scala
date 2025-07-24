package QSU_Test

import QuantumStateUnit._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

//All test here are made for the VCD to give a broader idea of how everything is working together.

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
    test(new TopQSU(/*qubits*/3, /*bw*/32, 3, 3, 10)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //initial QSV: 1/2|000> + 1/2|111> + j(1/2)|101> + j(1/2)|010>
      dut.io.in_QSV(0).poke("h38000000".U) //000 : 1/2
      dut.io.in_QSV(1).poke("h00000000".U) //001 : 0...
      dut.io.in_QSV(2).poke("h38000000".U) //010 : 1/2
      dut.io.in_QSV(3).poke("h00000000".U) //011 : 0...
      dut.io.in_QSV(4).poke("h00000000".U) //100 : 0...
      dut.io.in_QSV(5).poke("h38000000".U) //101 : 1/2
      dut.io.in_QSV(6).poke("h00000000".U) //110 : 0...
      dut.io.in_QSV(7).poke("h38000000".U) //111 : 1/2
      dut.clock.step()
      dut.io.in_en_replaceQSV.poke(1.B)
      dut.clock.step()
      dut.io.in_en_replaceQSV.poke(0.B)
      /*
      Quantum Circuit:
        |Q0> -- X -- Swap --
        |Q1> -- Y --      -- end
        |Q2> -- Z -- Swap --
      Permutation:  Q0  Q1  Q2  Q2Q0
      Gate:         X   Y   Z   Swap
       */
      //Q0 X gate
      dut.io.in_Permutation0.poke(0.U)
      dut.io.in_Permutation1.poke(0.U)
      dut.io.in_Permutation2.poke(0.U)
      dut.io.in_Gate.poke(1.U)
      dut.clock.step()
      dut.io.in_applyGate.poke(1.B)
      dut.clock.step()
      dut.io.in_applyGate.poke(0.B)
      dut.clock.step(10)

      //Q1 Y gate
      dut.io.in_Permutation0.poke(1.U)
      dut.io.in_Permutation1.poke(0.U)
      dut.io.in_Permutation2.poke(0.U)
      dut.io.in_Gate.poke(2.U)
      dut.clock.step()
      dut.io.in_applyGate.poke(1.B)
      dut.clock.step()
      dut.io.in_applyGate.poke(0.B)
      dut.clock.step(10)

      //Q3 Z gate
      dut.io.in_Permutation0.poke(2.U)
      dut.io.in_Permutation1.poke(0.U)
      dut.io.in_Permutation2.poke(0.U)
      dut.io.in_Gate.poke(3.U)
      dut.clock.step()
      dut.io.in_applyGate.poke(1.B)
      dut.clock.step()
      dut.io.in_applyGate.poke(0.B)
      dut.clock.step(10)

      //Q0Q2 swap gate
      dut.io.in_Permutation0.poke(0.U)
      dut.io.in_Permutation1.poke(1.U)
      dut.io.in_Permutation2.poke(0.U)
      dut.io.in_Gate.poke(9.U)
      dut.clock.step()
      dut.io.in_applyGate.poke(1.B)
      dut.clock.step()
      dut.io.in_applyGate.poke(0.B)
      dut.clock.step(10)
    }
}

//Using strictly the FPUgates to see error generated
class QSU_Test2 extends AnyFlatSpec with ChiselScalatestTester {
    "QSU" should "notGenerateError" in
      test(new TopQSU(/*qubits*/ 3, /*bw*/ 32, 3, 3, 10)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        //initial QSV: 1/2|000> + 1/2|111> + j(1/2)|101> + j(1/2)|010>
        dut.io.in_QSV(0).poke("h38000000".U) //000 : 1/2
        dut.io.in_QSV(1).poke("h00000000".U) //001 : 0...
        dut.io.in_QSV(2).poke("h38000000".U) //010 : 1/2
        dut.io.in_QSV(3).poke("h00000000".U) //011 : 0...
        dut.io.in_QSV(4).poke("h00000000".U) //100 : 0...
        dut.io.in_QSV(5).poke("h38000000".U) //101 : 1/2
        dut.io.in_QSV(6).poke("h00000000".U) //110 : 0...
        dut.io.in_QSV(7).poke("h38000000".U) //111 : 1/2
        dut.clock.step()
        dut.io.in_en_replaceQSV.poke(1.B)
        dut.clock.step()
        dut.io.in_en_replaceQSV.poke(0.B)
          /*
          Quantum Circuit:
            |Q0> -- H -- H -- H --- H --- H --- H ---
            |Q1> --   --   --   ---   ---   ---   --- end
            |Q2> --   --   --   ---   ---   ---   ---
          Permutation:  Q0  Q0  Q0  Q0  Q0  Q0
          Gate:         H   H   H   H   H   H
           */

          for(i <- 0 until 24){
              //Q0 H gate
              dut.io.in_Permutation0.poke(0.U)
              dut.io.in_Permutation1.poke(0.U)
              dut.io.in_Permutation2.poke(0.U)
              dut.io.in_Gate.poke("h11".U)
              dut.clock.step()
              dut.io.in_applyGate.poke(1.B)
              dut.clock.step()
              dut.io.in_applyGate.poke(0.B)
              dut.clock.step(12)
          }
      }
}

//Do stuff with both pools, then apply measurement gate.
class QSU_Test3 extends AnyFlatSpec with ChiselScalatestTester{
    "QSU" should "Create state, then mesure" in
      test(new TopQSU(/*qubits*/ 3, /*bw*/ 32, 3, 3, 10)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        //Final QSV Before Measure: 1/2|000> + 1/2|111> + j(1/2)|101> + j(1/2)|010>
        dut.io.in_QSV(0).poke("h3C000000".U) //000 : 1...
        dut.io.in_QSV(1).poke("h00000000".U) //001 : 0...
        dut.io.in_QSV(2).poke("h00000000".U) //010 : 0...
        dut.io.in_QSV(3).poke("h00000000".U) //011 : 0...
        dut.io.in_QSV(4).poke("h00000000".U) //100 : 0...
        dut.io.in_QSV(5).poke("h00000000".U) //101 : 0...
        dut.io.in_QSV(6).poke("h00000000".U) //110 : 0...
        dut.io.in_QSV(7).poke("h00000000".U) //111 : 0...
        dut.clock.step()
        dut.io.in_en_replaceQSV.poke(1.B)
        dut.clock.step()
        dut.io.in_en_replaceQSV.poke(0.B)
        dut.clock.step(5)
        /*
          Quantum Circuit:
              |Q0> -- H --      -- Swap ---  Measure ---
              |Q1> -- H --  ()  -- Swap ---  Measure --- End
              |Q2> --   -- CNOT --      ---          ---
          Permutation:  Q0  Q0  Q0  Q0  Q0  Q0
          Gate:         H   H   H   H   H   H
        */

        //Q0 H gate
        dut.io.in_Permutation0.poke(0.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h11".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(20)
        dut.io.out_flag.expect(1.B)

        //Q1 H gate
        dut.io.in_Permutation0.poke(1.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h11".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(20)
        dut.io.out_flag.expect(1.B)

        //control: Q1 -> target: Q2 CNOT gate
        dut.io.in_Permutation0.poke(2.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h06".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(12)
        dut.io.out_flag.expect(1.B)

        //Q0 <-> Q1 Swap gate
        dut.io.in_Permutation0.poke(0.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h09".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(12)
        dut.io.out_flag.expect(1.B)

        //Q0 measure gate
        dut.io.in_Permutation0.poke(0.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h1f".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(100)
        dut.io.out_flag.expect(1.B)

        //Q1 measure gate
        dut.io.in_Permutation0.poke(1.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h1f".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(100)
        dut.io.out_flag.expect(1.B)
    }
}

//Normalize first, get weird entanglement to see possible error
class QSU_Test4 extends AnyFlatSpec with ChiselScalatestTester {
    "QSU" should "Normalize State, then apply gates" in
      test(new TopQSU(/*qubits*/ 3, /*bw*/ 32, 3, 3, 10)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>dut.io.in_QSV(0).poke("h3C000000".U) //000 : 1...
        dut.io.in_QSV(1).poke("h3C000000".U) //001 : 1...
        dut.io.in_QSV(2).poke("h00000000".U) //010 : 0...
        dut.io.in_QSV(3).poke("h00000000".U) //011 : 0...
        dut.io.in_QSV(4).poke("h00000000".U) //100 : 0...
        dut.io.in_QSV(5).poke("h00000000".U) //101 : 0...
        dut.io.in_QSV(6).poke("h00000000".U) //110 : 0...
        dut.io.in_QSV(7).poke("h3C000000".U) //111 : 1...
        dut.clock.step()
        dut.io.in_en_replaceQSV.poke(1.B)
        dut.clock.step()
        dut.io.in_en_replaceQSV.poke(0.B)
        dut.clock.step(5)

        //Normalize first before applying gates
        dut.io.in_Permutation0.poke(0.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h10".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(100)
        dut.io.out_flag.expect(1.B)

        //Should become sqrt(1/3)*|000> + sqrt(1/3)*|001> + sqrt(1/3)*|111>
          /*
            Quantum Circuit:
                |Q0> -- H --  ()  --      --- X ---
                |Q1> --   -- CNOT --  ()  ---   --- End
                |Q2> --   --      -- CNOT --- Y ---
            Permutation:  Q0  Q0Q1  Q1Q2  Q0  Q2
            Gate:         H   CNot  CNot  X   Y
          */

        //
        dut.io.in_Permutation0.poke(0.U)
        dut.io.in_Permutation1.poke(0.U)
        dut.io.in_Permutation2.poke(0.U)
        dut.io.in_Gate.poke("h10".U)
        dut.clock.step()
        dut.io.in_applyGate.poke(1.B)
        dut.clock.step()
        dut.io.in_applyGate.poke(0.B)
        dut.clock.step(100)
        dut.io.out_flag.expect(1.B)
        //... Not done
      }
}