package QSU_Test.Gates

import QuantumStateUnit.Gates._
import QuantumStateUnit.OtherComponents.PsuedoRandomGenerator._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class TestCollectProbability extends AnyFlatSpec with ChiselScalatestTester {
  "Collapse" should "GiveProbability" in
    test(new CollapseProbability(4,32, 3, 3, 10)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //input: all even numbers are the chance of 0, while odd numbers are the chance of 1
      dut.io.in_QSV(0).poke("h34000000".U) //0.25*
      dut.io.in_QSV(1).poke("h00000000".U) //0
      dut.io.in_QSV(2).poke("h34000000".U) //0.25*
      dut.io.in_QSV(3).poke("h34000000".U) //0.25*
      dut.io.in_QSV(4).poke("h34000000".U) //0.25*
      dut.io.in_QSV(5).poke("h00000000".U) //0
      dut.io.in_QSV(6).poke("h00000000".U) //0
      dut.io.in_QSV(7).poke("h34000000".U) //0.25*
      dut.io.in_QSV(8).poke("h34000000".U) //0.25*
      dut.io.in_QSV(9).poke("h00000000".U) //0
      dut.io.in_QSV(10).poke("h00000000".U) //0
      dut.io.in_QSV(11).poke("h34000000".U) //0.25*
      dut.io.in_QSV(12).poke("h00000000".U) //0
      dut.io.in_QSV(13).poke("h00000000".U) //0
      dut.io.in_QSV(14).poke("h00000000".U) //0
      dut.io.in_QSV(15).poke("h34000000".U) //0.25*
      // 4/16 chance to be 0 and 4/16 chance to be 1 : it's unnormalized

      //The input is ready
      dut.io.in_valid.poke(1.B)

      dut.clock.step(30)

      //The output is done
      dut.io.out_valid.expect(1.B)

      val chanceOf_0 = dut.io.out_Measured(0).peek().litValue
      val chanceOf_1 = dut.io.out_Measured(1).peek().litValue
      println(s"Chance of 0: ${chanceOf_0.toString(16)}")
      println(s"Chance of 1: ${chanceOf_1.toString(16)}")
    }
}

//Currently wrong
class TestNormalization extends AnyFlatSpec with ChiselScalatestTester {
    "Normalization" should "getNormalizationValue" in
      test(new GetNormalization(16, 3, 8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        //Normalize factor of a normalized QSV should be 1
        dut.io.in_probability(0).poke("h3800".U) //0.5
        dut.io.in_probability(1).poke("h3800".U) //0.5
        dut.io.in_valid.poke(1.B)
        dut.io.in_en.poke(1.B)

        //skip time
        dut.clock.step(60)

        //final result
        dut.io.out_valid.expect(1.B)

        val NormalizationValue0 = dut.io.out_Normalize.peek().litValue
        println(s"The output value is: ${NormalizationValue0.toString(16)}") //Should output some value close to 1

        //change to new values
        dut.io.in_valid.poke(0.B)
        dut.clock.step(10)

        //Normalized QSV.
        dut.io.in_probability(0).poke("h33FF".U) //0.25
        dut.io.in_probability(1).poke("h33FF".U) //0.25
        dut.io.in_valid.poke(1)

        //skip time
        dut.clock.step(20)

        //final result
        dut.io.out_valid.expect(1.B)

        val NormalizationValue1 = dut.io.out_Normalize.peek().litValue
        println(s"The output value is: ${NormalizationValue1.toString(16)}") //Should output some value close to sqrt(2)
      }
}

//When given a measured state, it removes all probability of the opposite state and unnormalizes the state
class TestUnnormalizedState extends AnyFlatSpec with ChiselScalatestTester {
    "Mux" should "removeProbability" in
      test(new NewQSV(3, 3)){ dut =>
        //input
        for(i <- 0 until 8){
            dut.io.in_QSV(i).poke(i.U)
        }

        //Probability
        dut.io.in_sel.poke(0.B)

        //skip time
        dut.clock.step(5)

        //All 1's should become 0
        dut.io.out_QSV(0).expect(0.U)
        dut.io.out_QSV(1).expect(0.U)
        dut.io.out_QSV(2).expect(2.U)
        dut.io.out_QSV(3).expect(0.U)
        dut.io.out_QSV(4).expect(4.U)
        dut.io.out_QSV(5).expect(0.U)
        dut.io.out_QSV(6).expect(6.U)
        dut.io.out_QSV(7).expect(0.U)

        //Change in measurement outcome
        dut.io.in_sel.poke(1.B)

        //skip time
        dut.clock.step(5)

        //All 1's should become 0
        dut.io.out_QSV(0).expect(0.U)
        dut.io.out_QSV(1).expect(1.U)
        dut.io.out_QSV(2).expect(0.U)
        dut.io.out_QSV(3).expect(3.U)
        dut.io.out_QSV(4).expect(0.U)
        dut.io.out_QSV(5).expect(5.U)
        dut.io.out_QSV(6).expect(0.U)
        dut.io.out_QSV(7).expect(7.U)
      }
}

//m will be 64 because the max from the toFixed will be 64
class LinearCongruence_Test extends AnyFlatSpec with ChiselScalatestTester {
  "LinearCongruence" should "RNG" in
    test(new LinearCongruentialGenerator(32, 64, 5, 1)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //received random number
      dut.io.in_seed.poke(428.U)
      dut.clock.step()
      //feed random number into the number generator
      dut.io.in_feed.poke(1.B)
      dut.clock.step()
      dut.io.in_feed.poke(0.B)
      dut.clock.step()

      //look at numbers generated
      for(i <- 0 until 10){
        val peek = dut.io.out_Value.peek().litValue
        println(s"Out value($i): ${peek.toString(10)}")
        dut.clock.step()
        dut.io.in_next.poke(1.B)
        dut.clock.step()
        dut.io.in_next.poke(0.B)
        dut.clock.step()
      }

      //Leave on, to see left on behavior on vcd
      dut.io.in_next.poke(1.B)
      dut.clock.step(10)
    }
}

//
class CompareRandom_Test extends AnyFlatSpec with ChiselScalatestTester {
    "CompareToRand" should "Produce0or1" in
      test(new CompareWithRandom(16, 3)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
          dut.io.in_probability.poke("h3800".U) //0.50  <- the chance of 0
          dut.io.in_seed.poke(50.U)
          dut.io.in_valid.poke(0.B)
          dut.io.in_en.poke(0.B)
          dut.io.in_sel.poke(0.B)

          //skip time
          dut.clock.step(5)

          //input become valid
          dut.io.in_valid.poke(1.B)

          //skip time
          dut.clock.step(15)

          //Random input
          for (i <- 0 until 5) {
            val rnd       = new Random(i+100)
            val randomInput = rnd.nextInt(65)
            dut.io.in_seed.poke(randomInput.U)
            dut.clock.step()
            dut.io.in_en.poke(1.B)
            dut.clock.step()

            //Says value of the input and output
            val peekrnd   = dut.io.in_seed.peek().litValue
            val peek      = dut.io.out_value.peek().litValue
            println(s"The Rand value is ${peekrnd.toString(10)} \nThe out value is ${peek.toString(1)}")
            println(s"...")
            dut.clock.step()
            dut.io.in_en.poke(0.B)
            dut.clock.step()
          }
      }
}

//Test the Measurement action to get a random output and a normalization number
class MeasureGate0_Test extends AnyFlatSpec with ChiselScalatestTester {
  "MeasureGate" should "produceNewQSVandNormalization" in
    test(new Measurement(3,32,3,3,10)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.in_QSV(0).poke("h35A80000".U) //000: 0.35... <---- All values are sqrt(1/8)
      dut.io.in_QSV(1).poke("h35A80000".U) //001: 0.35... <-|
      dut.io.in_QSV(2).poke("h35A80000".U) //010: 0.35... <-|
      dut.io.in_QSV(3).poke("h35A80000".U) //011: 0.35... <-|
      dut.io.in_QSV(4).poke("h35A80000".U) //100: 0.35... <-|
      dut.io.in_QSV(5).poke("h35A80000".U) //101: 0.35... <-|
      dut.io.in_QSV(6).poke("h35A80000".U) //110: 0.35... <-|
      dut.io.in_QSV(7).poke("h35A80000".U) //111: 0.35... <-
      dut.io.in_noise.poke("h34872303".U)

      //now is valid
      dut.clock.step()
      dut.io.in_valid.poke(1.B)

      dut.clock.step(65)
      dut.io.out_valid.expect(1.B)

      //out
      println(s"|000> : ${dut.io.out_QSV(0).peek().litValue.toString(16)}")
      println(s"|001> : ${dut.io.out_QSV(1).peek().litValue.toString(16)}")
      println(s"|010> : ${dut.io.out_QSV(2).peek().litValue.toString(16)}")
      println(s"|011> : ${dut.io.out_QSV(3).peek().litValue.toString(16)}")
      println(s"|100> : ${dut.io.out_QSV(4).peek().litValue.toString(16)}")
      println(s"|101> : ${dut.io.out_QSV(5).peek().litValue.toString(16)}")
      println(s"|110> : ${dut.io.out_QSV(6).peek().litValue.toString(16)}")
      println(s"|111> : ${dut.io.out_QSV(7).peek().litValue.toString(16)}")
      println(s"Measured Qubit Value: ${dut.io.out_measured.peek().litValue.toString(1)}")
      println(s"Normalization Value:  ${dut.io.out_Normalize.peek().litValue.toString(16)}")

      dut.clock.step(5)
      dut.io.in_valid.poke(0.B)
      dut.clock.step(50)
    }
}

//Finding the Normalization value of the input to be feed; skipping the other tasks of RNG
class MeasureGate1_Test extends AnyFlatSpec with ChiselScalatestTester {
  "MeasureGate" should "GiveNormalizationValue" in
    test(new Measurement(3,32,3,3,10)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //Input: Is unnormalized: output should be sqrt(2)
      dut.io.in_QSV(0).poke("h00000000".U) //000: 0
      dut.io.in_QSV(1).poke("h38000000".U) //001: 0.50
      dut.io.in_QSV(2).poke("h00000000".U) //010: 0
      dut.io.in_QSV(3).poke("h00000000".U) //011: 0
      dut.io.in_QSV(4).poke("h00000000".U) //100: 0
      dut.io.in_QSV(5).poke("h00000000".U) //101: 0
      dut.io.in_QSV(6).poke("h38000000".U) //110: 0.50
      dut.io.in_QSV(7).poke("h00000000".U) //111: 0
      dut.io.in_noise.poke("h34872303".U)
      dut.io.in_sendNorm.poke(1.B)

      //now is valid
      dut.clock.step()
      dut.io.in_valid.poke(1.B)

      dut.clock.step(65)
      dut.io.out_valid.expect(1.B)

      //out
      println(s"Normalization Value:  ${dut.io.out_Normalize.peek().litValue.toString(16)}")
    }
}