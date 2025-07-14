package QSU_Test.QSU_Architecture

import QuantumStateUnit.QSU_Architecture._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

//Checks the mux input of the QSR
class QSR_Test extends AnyFlatSpec with ChiselScalatestTester {
  "Register" should "ProperlyUpdate" in
    test(new QuantumStateRegister(2, 2)) { dut =>
      //Initial State Given
      dut.io.in_new_state(0).poke(0.U)
      dut.io.in_new_state(1).poke(1.U)
      dut.io.in_new_state(2).poke(2.U)
      dut.io.in_new_state(3).poke(3.U)
      //Updated State
      dut.io.in_QSV(0).poke(3.U)
      dut.io.in_QSV(1).poke(2.U)
      dut.io.in_QSV(2).poke(1.U)
      dut.io.in_QSV(3).poke(0.U)

      //give new state
      dut.clock.step()
      dut.io.in_en.poke(1.B)
      dut.io.in_en_new_state.poke(1.B)

      //output === Initial given state
      dut.clock.step()
      dut.io.out_QSV(0).expect(0.U)
      dut.io.out_QSV(1).expect(1.U)
      dut.io.out_QSV(2).expect(2.U)
      dut.io.out_QSV(3).expect(3.U)

      //replace with updated state
      dut.clock.step(2)
      dut.io.in_en.poke(1.B)
      dut.io.in_en_new_state.poke(0.B)

      //output === Initial given state
      dut.clock.step()
      dut.io.out_QSV(0).expect(3.U)
      dut.io.out_QSV(1).expect(2.U)
      dut.io.out_QSV(2).expect(1.U)
      dut.io.out_QSV(3).expect(0.U)
    }
}