package QSU_Test.QSU_Architecture

import QuantumStateUnit.QSU_Architecture._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Permutation_Test extends AnyFlatSpec with ChiselScalatestTester {
  "permutation" should "GeneratePattern" in
    test(new permutationLayer(3, 3)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //Each is unique state
      for(i <- 0 until 8){
        dut.io.in_QSV(i).poke(i.U)
      }
      //target qubit 0
      dut.clock.step()
      dut.io.in_sel.poke(0.U)
      dut.io.out_QSV(0).expect(0.U)
      dut.io.out_QSV(1).expect(1.U)
      dut.io.out_QSV(2).expect(2.U)
      dut.io.out_QSV(3).expect(3.U)
      dut.io.out_QSV(4).expect(4.U)
      dut.io.out_QSV(5).expect(5.U)
      dut.io.out_QSV(6).expect(6.U)
      dut.io.out_QSV(7).expect(7.U)

      //target qubit 1
      dut.clock.step()
      dut.io.in_sel.poke(1.U)
      dut.io.out_QSV(0).expect(0.U)
      dut.io.out_QSV(1).expect(2.U) //1 to 2
      dut.io.out_QSV(2).expect(1.U) //2 to 1
      dut.io.out_QSV(3).expect(3.U)
      dut.io.out_QSV(4).expect(4.U)
      dut.io.out_QSV(5).expect(6.U) //5 to 6
      dut.io.out_QSV(6).expect(5.U) //6 to 5
      dut.io.out_QSV(7).expect(7.U)

      //target qubit 2
      dut.clock.step()
      dut.io.in_sel.poke(2.U)
      dut.io.out_QSV(0).expect(0.U)
      dut.io.out_QSV(1).expect(4.U) //1 to 4
      dut.io.out_QSV(2).expect(2.U)
      dut.io.out_QSV(3).expect(6.U) //3 to 6
      dut.io.out_QSV(4).expect(1.U) //4 to 1
      dut.io.out_QSV(5).expect(5.U)
      dut.io.out_QSV(6).expect(3.U) //6 to 3
      dut.io.out_QSV(7).expect(7.U)
    }
}