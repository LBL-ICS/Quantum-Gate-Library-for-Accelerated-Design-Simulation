package QSU_Test.QSU_Architecture.Permutation_Test

import QuantumStateUnit.Old._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PermutationMux0_Test extends AnyFlatSpec with ChiselScalatestTester {
  "MuxPermutation" should "GeneratePattern" in
    test(new permutationLayer(3, 3, 0)) { dut =>
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

//Moving qubit into 2nd position
class PermutationMux1_Test extends AnyFlatSpec with ChiselScalatestTester {
    "permutation1" should "GeneratePattern" in
      test(new permutationLayer(3, 3, 1)) { dut =>
          //Each is unique state
          for(i <- 0 until 8){
              dut.io.in_QSV(i).poke(i.U)
          }
          //target qubit 1
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

          //target qubit 2
          dut.clock.step()
          dut.io.in_sel.poke(1.U)
          dut.io.out_QSV(0).expect(0.U)
          dut.io.out_QSV(1).expect(1.U)
          dut.io.out_QSV(2).expect(4.U) //2 to 4
          dut.io.out_QSV(3).expect(5.U) //3 to 5
          dut.io.out_QSV(4).expect(2.U) //4 to 2
          dut.io.out_QSV(5).expect(3.U) //5 to 3
          dut.io.out_QSV(6).expect(6.U)
          dut.io.out_QSV(7).expect(7.U)

        dut.clock.step()
      }
}

class StackedMuxPermutation_Test extends AnyFlatSpec with ChiselScalatestTester {
  "permutationLayers" should "notMakeMeMourn" in
  test(new StackedMuxPermutation(4, 4, Seq(0,1,2))){ dut =>

    //initial setup
    for(i <- 0 until 16){
      dut.io.in_QSV(i).poke(i.U)
    }
    for(i <- 0 until 3){
      dut.io.in_sel(i).poke(0.U)
    }

    //test Q1 -> Q0
    dut.clock.step()
    dut.io.in_sel(0).poke(1.U)
    dut.clock.step()
    dut.io.out_QSV( 0).expect( 0.U)
    dut.io.out_QSV( 1).expect( 2.U) //1 to 2
    dut.io.out_QSV( 2).expect( 1.U) //2 to 1
    dut.io.out_QSV( 3).expect( 3.U)
    dut.io.out_QSV( 4).expect( 4.U)
    dut.io.out_QSV( 5).expect( 6.U) //5 to 6
    dut.io.out_QSV( 6).expect( 5.U) //6 to 5
    dut.io.out_QSV( 7).expect( 7.U)
    dut.io.out_QSV( 8).expect( 8.U)
    dut.io.out_QSV( 9).expect(10.U) //9  to 10
    dut.io.out_QSV(10).expect( 9.U) //10 to 9
    dut.io.out_QSV(11).expect(11.U)
    dut.io.out_QSV(12).expect(12.U)
    dut.io.out_QSV(13).expect(14.U) //13 to 14
    dut.io.out_QSV(14).expect(13.U) //14 to 13
    dut.io.out_QSV(15).expect(15.U)

    //test Q2 -> Q1
    dut.clock.step()
    dut.io.in_sel(0).poke(0.U)
    dut.io.in_sel(1).poke(1.U)
    dut.clock.step()
    dut.io.out_QSV( 0).expect( 0.U)
    dut.io.out_QSV( 1).expect( 1.U)
    dut.io.out_QSV( 2).expect( 4.U) //2 to 4
    dut.io.out_QSV( 3).expect( 5.U) //3 to 5
    dut.io.out_QSV( 4).expect( 2.U) //4 to 2
    dut.io.out_QSV( 5).expect( 3.U) //5 to 3
    dut.io.out_QSV( 6).expect( 6.U)
    dut.io.out_QSV( 7).expect( 7.U)
    dut.io.out_QSV( 8).expect( 8.U)
    dut.io.out_QSV( 9).expect( 9.U)
    dut.io.out_QSV(10).expect(12.U) //10 to 12
    dut.io.out_QSV(11).expect(13.U) //11 to 13
    dut.io.out_QSV(12).expect(10.U) //12 to 10
    dut.io.out_QSV(13).expect(11.U) //13 to 11
    dut.io.out_QSV(14).expect(14.U)
    dut.io.out_QSV(15).expect(15.U)

    //test Q3 -> Q2
    dut.clock.step()
    dut.io.in_sel(1).poke(0.U)
    dut.io.in_sel(2).poke(1.U)
    dut.clock.step()
    dut.io.out_QSV( 0).expect( 0.U)
    dut.io.out_QSV( 1).expect( 1.U)
    dut.io.out_QSV( 2).expect( 2.U)
    dut.io.out_QSV( 3).expect( 3.U)
    dut.io.out_QSV( 4).expect( 8.U) //4  to 8
    dut.io.out_QSV( 5).expect( 9.U) //5  to 9
    dut.io.out_QSV( 6).expect(10.U) //6  to 10
    dut.io.out_QSV( 7).expect(11.U) //7  to 11
    dut.io.out_QSV( 8).expect( 4.U) //8  to 4
    dut.io.out_QSV( 9).expect( 5.U) //9  to 5
    dut.io.out_QSV(10).expect( 6.U) //10 to 6
    dut.io.out_QSV(11).expect( 7.U) //11 to 7
    dut.io.out_QSV(12).expect(12.U)
    dut.io.out_QSV(13).expect(13.U)
    dut.io.out_QSV(14).expect(14.U)
    dut.io.out_QSV(15).expect(15.U)

    //test Q3 -> Q0 and Q0 -> Q1
    dut.clock.step()
    dut.io.in_sel(0).poke(3.U)
    dut.io.in_sel(1).poke(2.U) //Because Q0 is in the spot of Q3, Q1 will switch spots with "Q3"(Q0 in the Q3 spot)
    dut.io.in_sel(2).poke(0.U)
    dut.clock.step()
    dut.io.out_QSV( 0).expect( 0.U)
    dut.io.out_QSV( 1).expect( 8.U) //1  to 8
    dut.io.out_QSV( 2).expect( 1.U) //2  to 1
    dut.io.out_QSV( 3).expect( 9.U) //3  to 9
    dut.io.out_QSV( 4).expect( 4.U) //4  to 4
    dut.io.out_QSV( 5).expect(12.U) //5  to 12
    dut.io.out_QSV( 6).expect( 5.U) //6  to 5
    dut.io.out_QSV( 7).expect(13.U) //7  to 13
    dut.io.out_QSV( 8).expect( 2.U) //8  to 2
    dut.io.out_QSV( 9).expect(10.U) //9  to 10
    dut.io.out_QSV(10).expect( 3.U) //10 to 3
    dut.io.out_QSV(11).expect(11.U) //11 to 11
    dut.io.out_QSV(12).expect( 6.U) //12 to 6
    dut.io.out_QSV(13).expect(14.U) //13 to 14
    dut.io.out_QSV(14).expect( 7.U) //14 to 7
    dut.io.out_QSV(15).expect(15.U)

    dut.clock.step()
  }
}
