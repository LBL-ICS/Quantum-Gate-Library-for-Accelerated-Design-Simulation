package QSU_Test.Gates

import QuantumStateUnit.Gates._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TestNo_Op extends AnyFlatSpec with ChiselScalatestTester {
  "No_Op" should "DoNothing" in
    test(new No_op(1)) { dut =>
      dut.io.in(0).poke(0.U)
      dut.io.in(1).poke(1.U)

      dut.clock.step()
      //should do nothing, but be valid
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)
    }
}

class TestPauliX extends AnyFlatSpec with ChiselScalatestTester {
  "PauliX" should "Switch" in
    test(new Pauli_X(1)) { dut =>
      dut.io.in(0).poke(0.U) //0
      dut.io.in(1).poke(1.U) //1

      dut.clock.step()
      //switch spots and be valid
      dut.io.out(0).expect(1.U) //1
      dut.io.out(1).expect(0.U) //0
    }
}

class TestPauliY extends AnyFlatSpec with ChiselScalatestTester {
  "PauliY" should "SwitchAndBecomeImaginary" in
    test(new Pauli_Y(32)) { dut =>
      dut.io.in(0).poke("h3c000000".U) //1 + j*0
      dut.io.in(1).poke("h00003c00".U) //0 + j*1

      dut.clock.step()
      //should be All values are now imaginary
      dut.io.out(0).expect("hbc000000".U) //-1 + j*0
      dut.io.out(1).expect("h0000bc00".U) // 0 + j*(-1)
    }
}

class TestPauliZ extends AnyFlatSpec with ChiselScalatestTester {
  "PauliZ" should "Make1Negative" in
    test(new Pauli_Z(32)) { dut =>
      dut.io.in(0).poke("h3c000000".U) //1 + j*0
      dut.io.in(1).poke("h3c000000".U) //1 + j*0

      dut.clock.step(2)
      //2nd value should be negative
      dut.io.out(0).expect("h3c000000".U) // 1 + j*0
      dut.io.out(1).expect("hbc000000".U) //-1 - j*0
    }
}

class TestSGate extends AnyFlatSpec with ChiselScalatestTester {
  "SGate" should "Make1Imaginary" in
    test(new S_gate(32)) { dut =>
      dut.io.in(0).poke("h3c000000".U) // 1 + j*0
      dut.io.in(1).poke("h40003c00".U) // 2 + j*1

      dut.clock.step()
      //second value should become imaginary
      dut.io.out(0).expect("h3c000000".U) // 1 + j*0
      dut.io.out(1).expect("hbc004000".U) //-1 + j*2
    }
}

class TestInverseSGate extends AnyFlatSpec with ChiselScalatestTester {
  "InverseSGate" should "Make1NegativeImaginary" in
    test(new InverseS_gate(32)) { dut =>
      dut.io.in(0).poke("h3c000000".U) // 1 + j*0
      dut.io.in(1).poke("h40003c00".U) // 2 + j*1

      dut.clock.step()
      //Second value should become negative and imaginary
      dut.io.out(0).expect("h3c000000".U) // 1 + j*0
      dut.io.out(1).expect("h3c00c000".U) // 1 - j*2
    }
}

/*
...
...
    2 input control gates
...
...
 */
class TestCNOT extends AnyFlatSpec with ChiselScalatestTester {
    "CNOT" should "SwitchSecond" in
      test(new CNOT(32)) { dut =>
          dut.io.in(0).poke("h3c000000".U) // 1 + j*0
          dut.io.in(1).poke("h00003c00".U) // 0 + j*1
          dut.io.in(2).poke("h40000000".U) // 2 + j*0
          dut.io.in(3).poke("h00004000".U) // 0 + j*2

          dut.clock.step()
          //Second value should become negative and imaginary
          dut.io.out(0).expect("h3c000000".U) // 1 + j*0
          dut.io.out(1).expect("h00003c00".U) // 0 + j*1
          dut.io.out(2).expect("h00004000".U) // 0 + j*2
          dut.io.out(3).expect("h40000000".U) // 2 + j*0
      }
}

class TestCPauliY extends AnyFlatSpec with ChiselScalatestTester {
    "CPauliY" should "Switch&TurnImaginarySecondVector" in
      test(new CPauli_Y(32)) { dut =>
          dut.io.in(0).poke("h3c000000".U) // 1 + j*0
          dut.io.in(1).poke("h00003c00".U) // 0 + j*1
          dut.io.in(2).poke("h40003c00".U) // 2 + j*1
          dut.io.in(3).poke("h40003c00".U) // 2 + j*1

          dut.clock.step()
          //Second value should become negative and imaginary
          dut.io.out(0).expect("h3c000000".U) // 1 + j*0
          dut.io.out(1).expect("h00003c00".U) // 0 + j*1
          dut.io.out(2).expect("h3c00c000".U) // 1 - j*2
          dut.io.out(3).expect("hbc004000".U) //-1 + j*2
      }
}

class TestCPauliZ extends AnyFlatSpec with ChiselScalatestTester {
    "CPauliZ" should "MakeVeryLastNegative" in
      test(new CPauli_Z(32)) { dut =>
          dut.io.in(0).poke("h3c000000".U) // 1 + j*0
          dut.io.in(1).poke("h00003c00".U) // 0 + j*1
          dut.io.in(2).poke("h40000000".U) // 2 + j*0
          dut.io.in(3).poke("h00004000".U) // 0 + j*2

          dut.clock.step()
          //Second value should become negative and imaginary
          dut.io.out(0).expect("h3c000000".U) // 1 + j*0
          dut.io.out(1).expect("h00003c00".U) // 0 + j*1
          dut.io.out(2).expect("h40000000".U) // 2 + j*0
          dut.io.out(3).expect("h0000c000".U) // 0 - j*2
      }
}

class TestToffoli extends AnyFlatSpec with ChiselScalatestTester {
    "Toffoli" should "SwitchLast" in
      test(new Toffoli(32)) { dut =>
          dut.io.in(0).poke("h3c000000".U) // 1 + j*0
          dut.io.in(1).poke("h00003c00".U) // 0 + j*1
          dut.io.in(2).poke("h40000000".U) // 2 + j*0
          dut.io.in(3).poke("h00004000".U) // 0 + j*2
          dut.io.in(4).poke("h42000000".U) // 3 + j*0
          dut.io.in(5).poke("h00004200".U) // 0 + j*3
          dut.io.in(6).poke("h3c000000".U) // 1 + j*0
          dut.io.in(7).poke("h00003c00".U) // 0 + j*1

          dut.clock.step()
          //Second value should become negative and imaginary
          dut.io.out(0).expect("h3c000000".U) // 1 + j*0
          dut.io.out(1).expect("h00003c00".U) // 0 + j*1
          dut.io.out(2).expect("h40000000".U) // 2 + j*0
          dut.io.out(3).expect("h00004000".U) // 0 + j*2
          dut.io.out(4).expect("h42000000".U) // 0 + j*3
          dut.io.out(5).expect("h00004200".U) // 3 + j*0
          dut.io.out(6).expect("h00003c00".U) // 0 + j*1
          dut.io.out(7).expect("h3c000000".U) // 1 + j*0
      }
}