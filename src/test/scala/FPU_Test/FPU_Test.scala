package FPU_Test

import New_FPU_Mario.FPUnits._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class TestFPSqrt extends AnyFlatSpec with ChiselScalatestTester {
  "Sqrt" should "outputNum" in
    test(new FP_sqrt(16, 10)) { dut =>
      dut.io.in_en.poke(1.B)

        //Test 1/2
      dut.clock.step()
      dut.io.in_a.poke("h3800".U)
      dut.io.in_valid.poke(1.B)
        //Outputs sqrt(2)
      dut.clock.step(10)
      dut.io.out_valid.expect(1.B)
      println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

      dut.clock.step()
      dut.io.in_valid.poke(0.B)
        //input about 1/2
      dut.clock.step()
      dut.io.in_a.poke("h37FF".U)
      dut.io.in_valid.poke(1.B)
        //output sqrt(8)
      dut.clock.step(10)
      dut.io.out_valid.expect(1.B)
      println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

      dut.clock.step()
      dut.io.in_valid.poke(0.B)
        //Input is 1/4
      dut.clock.step()
      dut.io.in_a.poke("h3400".U)
      dut.io.in_valid.poke(1.B)
        //Output is 2
      dut.clock.step(10)
      dut.io.out_valid.expect(1.B)
      println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

      dut.clock.step()
      dut.io.in_valid.poke(0.B)
        //Input is about 1/4
      dut.clock.step()
      dut.io.in_a.poke("h33ff".U)
      dut.io.in_valid.poke(1.B)
        //Output is 4
      dut.clock.step(10)
      dut.io.out_valid.expect(1.B)
      println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

      dut.clock.step()
      dut.io.in_valid.poke(0.B)
        //Input is 1/4
      dut.clock.step()
      dut.io.in_a.poke("h3000".U)
      dut.io.in_valid.poke(1.B)
        //Output is 2
      dut.clock.step(10)
      dut.io.out_valid.expect(1.B)
      println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

      dut.clock.step()
      dut.io.in_valid.poke(0.B)
        //Input is about 1/4
      dut.clock.step()
      dut.io.in_a.poke("h2FFF".U)
      dut.io.in_valid.poke(1.B)
        //Output is 4
      dut.clock.step(10)
      dut.io.out_valid.expect(1.B)
      println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")
    }
}

class TestFPmult extends AnyFlatSpec with ChiselScalatestTester {
    "Multiplier" should "outputNum" in
      test(new FP_mult(16,3)) { dut =>
        dut.io.in_en.poke(1.B)

        //Test 1/sqrt(2)
        dut.clock.step()
        dut.io.in_a.poke("h39A8".U)
        dut.io.in_b.poke("h39A8".U)
        dut.io.in_valid.poke(1.B)
        //Output is 1/2
        dut.clock.step(10)
        dut.io.out_valid.expect(1.B)
        println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

        dut.clock.step()
        dut.io.in_valid.poke(0.B)

        //Input is 1/sqrt(8)
        dut.clock.step()
        dut.io.in_a.poke("h35A8".U)
        dut.io.in_b.poke("h35A8".U)
        dut.io.in_valid.poke(1.B)
        //Output is 1/8
        dut.clock.step(10)
        dut.io.out_valid.expect(1.B)
        println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

        dut.clock.step()
        dut.io.in_valid.poke(0.B)
        //Input is about 1/sqrt(3)
        dut.clock.step()
        dut.io.in_a.poke("h389E".U)
        dut.io.in_b.poke("h389E".U)
        dut.io.in_valid.poke(1.B)
        //Output is 1/3
        dut.clock.step(10)
        dut.io.out_valid.expect(1.B)
        println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")

        dut.clock.step()
        dut.io.in_valid.poke(0.B)
        //Input is about 1/sqrt(3)
        dut.clock.step()
        dut.io.in_a.poke("h3688".U)
        dut.io.in_b.poke("h3688".U)
        dut.io.in_valid.poke(1.B)
        //Output is 1/3
        dut.clock.step(10)
        dut.io.out_valid.expect(1.B)
        println(s"Output value of ${dut.io.in_a.peek().litValue.toString(16)} is: ${dut.io.out_s.peek().litValue.toString(16)}")
      }
}