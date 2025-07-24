package QSU_Test.QSU_Architecture.Controller_Test

import QuantumStateUnit.Old.AlgorithmManager
import QuantumStateUnit.QSU_Architecture.QSUController
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ControllerTest extends AnyFlatSpec with ChiselScalatestTester {
  "Controller" should "GiveCorrectOutputs" in
    test(new QSUController).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //human input
      dut.io.in_replaceQSV.poke(0.U)
      dut.io.in_applygate.poke(0.U)
      //internal input
      dut.io.in_valid.poke(0.U)

      //give a new QSV
      dut.clock.step()
      dut.io.in_replaceQSV.poke(1.U) //leave on until apply a gate: should not interrupt in_applygate
      dut.io.in_applygate.poke(0.U)
      dut.io.in_valid.poke(0.U)

      //expect replace QSV for the time that replaceQSV is 1.U
      dut.io.out_en_QGP.expect(0.B)
      dut.io.out_readyFlag.expect(1.B)
      dut.io.out_update_QSR.expect(1.B)
      dut.io.out_replaceQSV.expect(1.B)

      //apply a gate to the new QS
      dut.clock.step()
      dut.io.in_replaceQSV.poke(1.U)
      dut.io.in_applygate.poke(1.U)
      dut.io.in_valid.poke(0.U)

      //stop updating register and en_QGP should become 1.B
      dut.clock.step()
        dut.io.out_en_QGP.expect(1.B)
        dut.io.out_readyFlag.expect(0.B)
        dut.io.out_update_QSR.expect(0.B)
        dut.io.out_replaceQSV.expect(0.B)

      //let go of inputs
      dut.clock.step()
      dut.io.in_replaceQSV.poke(0.U)
      dut.io.in_applygate.poke(0.U)
      dut.io.in_valid.poke(0.U)

      //should remain stagnant until in_valid
      dut.clock.step()
        dut.io.out_en_QGP.expect(1.B)
        dut.io.out_readyFlag.expect(0.B)
        dut.io.out_update_QSR.expect(0.B)
        dut.io.out_replaceQSV.expect(0.B)

      //gate finished, input is now valid
      dut.clock.step()
      dut.io.in_replaceQSV.poke(0.U)
      dut.io.in_applygate.poke(0.U)
      dut.io.in_valid.poke(1.U)

      //Update Register
      dut.clock.step()
        dut.io.out_en_QGP.expect(1.B)
        dut.io.out_readyFlag.expect(0.B)
        dut.io.out_update_QSR.expect(1.B)
        dut.io.out_replaceQSV.expect(0.B)

      //stopped sending valid, and is now ready for the next set of inputs
      dut.clock.step()
      dut.io.in_replaceQSV.poke(0.U)
      dut.io.in_applygate.poke(0.U)
      dut.io.in_valid.poke(0.U)

      //finished all tasks and now flag for new input
      dut.clock.step()
        dut.io.out_en_QGP.expect(0.B)
        dut.io.out_readyFlag.expect(1.B)
        dut.io.out_update_QSR.expect(0.B)
        dut.io.out_replaceQSV.expect(0.B)

    }
}