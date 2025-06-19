package QSU_Test.QSU_Architecture

import QuantumStateUnit.QSU_Architecture._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

//QGPTest 1: NonFPU GatePool
class QGPMuxLayerCheck extends AnyFlatSpec with ChiselScalatestTester {
  "MuxLayer" should "SwitchBetweenVectors" in
    test(new QGPMuxLayer(3,3,3)) { dut =>
      //input of no-op, pauliX, and a reverse order
      for (i <- 0 until 8) {
        dut.io.in_QSV(0)(i).poke(i.U)
        dut.io.in_QSV(2)(i).poke((7 - i).U)
      }
      for (i <- 0 until 4) {
        dut.io.in_QSV(1)(i).poke((i + 1).U)
        dut.io.in_QSV(1)(i + 1).poke(i.U)
      }
      dut.io.in_valid(0).poke(1.B)
      dut.io.in_valid(1).poke(1.B)
      dut.io.in_valid(2).poke(1.B)
      dut.io.in_sel.poke(0.U)
      dut.clock.step()
      for (i <- 0 until 8) {
        dut.io.out_QSV(i).expect(i.U)
      }
      dut.clock.step()
      dut.io.in_sel.poke(2.U)
      dut.clock.step()
      for (i <- 0 until 8) {
        dut.io.out_QSV(i).expect((7 - i).U)
      }
    }
}


//QGPTest 1: NonFPU GatePool
class QGPTest1 extends AnyFlatSpec with ChiselScalatestTester {
  "Gates" should "GenerateNewState" in
    test(new QGP(3,32,3,3,6)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //Initial Input
      dut.io.in_QSV(0).poke("h3c000000".U) //000: 1 + 0j
      dut.io.in_QSV(1).poke("h3c000000".U) //001: 1 + 0j
      dut.io.in_QSV(2).poke("h3c000000".U) //010: 1 + 0j
      dut.io.in_QSV(3).poke("h00003c00".U) //011: 0 + 1j
      dut.io.in_QSV(4).poke("h00003c00".U) //100: 0 + 1j
      dut.io.in_QSV(5).poke("h00003c00".U) //101: 0 + 1j
      dut.io.in_QSV(6).poke("h3c004000".U) //110: 1 + 2j
      dut.io.in_QSV(7).poke("h42004400".U) //111: 3 + 4j
      dut.io.in_en.poke(0.B)

      //Test Pauli_X
      dut.clock.step()
      dut.io.in_sel.poke("b001".U)
      dut.io.in_en.poke(1.B)

      dut.clock.step()
      dut.io.out_QSV(0).expect("h3c000000".U) //000: 1 + 0j
      dut.io.out_QSV(1).expect("h3c000000".U) //001: 1 + 0j
      dut.io.out_QSV(2).expect("h00003c00".U) //010: 0 + 1j
      dut.io.out_QSV(3).expect("h3c000000".U) //011: 1 + 0j
      dut.io.out_QSV(4).expect("h00003c00".U) //100: 0 + 1j
      dut.io.out_QSV(5).expect("h00003c00".U) //101: 0 + 1j
      dut.io.out_QSV(6).expect("h42004400".U) //110: 3 + 4j
      dut.io.out_QSV(7).expect("h3c004000".U) //111: 1 + 2j
      dut.io.out_valid.expect(1.B)
      dut.io.in_en.poke(0.B)

      //Test Pauli_Y
      dut.clock.step()
      dut.io.in_sel.poke(2.U)
      dut.io.in_en.poke(1.B)

      dut.clock.step()
      dut.io.out_QSV(0).expect("h00003c00".U) //000: 0 + 1j
      dut.io.out_QSV(1).expect("h0000bc00".U) //001: 0 - 1j
      dut.io.out_QSV(2).expect("hbc000000".U) //010:-1 + 0j
      dut.io.out_QSV(3).expect("h0000bc00".U) //011: 0 - 1j
      dut.io.out_QSV(4).expect("hbc000000".U) //100:-1 + 0j
      dut.io.out_QSV(5).expect("h3c000000".U) //101: 1 + 0j
      dut.io.out_QSV(6).expect("hc4004200".U) //110:-4 + 3j
      dut.io.out_QSV(7).expect("h4000bc00".U) //111: 2 - 1j
      dut.io.out_valid.expect(1.B)
    }
}
