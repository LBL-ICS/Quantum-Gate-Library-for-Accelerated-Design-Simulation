package Complex_FPU

import New_FPU_Mario.FPUnits._
import chisel3._
import chisel3.util.{Cat, ShiftRegister, log2Ceil}


class demux (bw: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bw.W))
    val sel = Input(Bool())
    val outputs = Output(Vec(2, UInt(bw.W)))
  })

  io.outputs(0) := 0.U
  io.outputs(1) := 0.U
  //io.outputs := VecInit(Seq.fill(2)(0.U))

  when(io.sel) {
    io.outputs(1) := io.input
    io.outputs(0) := 0.U
  }.otherwise {
    io.outputs(0) := io.input
    io.outputs(1) := 0.U
  }

  //io.outputs(io.sel) := io.input
}

class complex_conjugate_mult (bw: Int, mult_pd: Int, add_pd: Int) extends Module {
  val io = IO(new Bundle {
    val complexA = Input(UInt(bw.W))
    val complexB = Input(UInt(bw.W))
    val in_en = Input(Bool())
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_s = Output(UInt(bw.W))
    val out_real = Output(UInt((bw / 2).W))
    val out_imag = Output(UInt((bw / 2).W))
  })

  val negate: UInt = bw match {
    case 32 => "h8000".U((bw / 2).W)
    case 64 => "h80000000".U((bw / 2).W)
    case 128 => "h80000000000000000".U((bw / 2).W)
    case _ => 0.U((bw / 2).W)
  }

  val counter = RegInit(0.U(8.W))
  val latency = mult_pd + add_pd

  when (io.in_en) {
    counter := counter + 1.U
  }



  io.out_s := 0.U
  //io.out_valid := 0.U

  val FP_mult_inst = Seq.fill(4)(Module(new FP_mult((bw / 2), mult_pd)))
  FP_mult_inst.foreach { mod =>
    mod.io.in_a := 0.U
    mod.io.in_b := 0.U
    mod.io.in_en := false.B
    mod.io.in_valid := false.B
  }

  val FP_adder_Inst = Seq.fill(2)(Module(new FP_add((bw / 2), add_pd)))
  FP_adder_Inst.foreach { mod =>
    mod.io.in_a := 0.U
    mod.io.in_b := 0.U
    mod.io.in_en := false.B
    mod.io.in_valid := false.B
  }

  FP_mult_inst(0).io.in_a := io.complexA((bw - 1), (bw / 2)) //realA
  FP_mult_inst(0).io.in_b := io.complexB((bw - 1), (bw / 2)) //realB
  FP_mult_inst(1).io.in_a := io.complexA(((bw / 2) - 1), 0)  //imagA
  FP_mult_inst(1).io.in_b := io.complexB(((bw / 2) - 1), 0)  //imagB
  FP_mult_inst(2).io.in_a := io.complexA(((bw / 2) - 1), 0)  //imagA
  FP_mult_inst(2).io.in_b := io.complexB((bw - 1), (bw / 2)) //realB
  FP_mult_inst(3).io.in_a := io.complexA((bw - 1), (bw / 2)) //realA
  FP_mult_inst(3).io.in_b := io.complexB(((bw / 2) - 1), 0)  //imagB

  FP_adder_Inst(0).io.in_a := Mux(FP_adder_Inst(0).io.in_en, FP_mult_inst(0).io.out_s, 0.U)
  FP_adder_Inst(0).io.in_b := Mux(FP_adder_Inst(0).io.in_en, FP_mult_inst(1).io.out_s, 0.U)

  FP_adder_Inst(1).io.in_a := Mux(FP_adder_Inst(1).io.in_en, FP_mult_inst(2).io.out_s ^ negate, 0.U)
  FP_adder_Inst(1).io.in_b := Mux(FP_adder_Inst(1).io.in_en, FP_mult_inst(3).io.out_s, 0.U)

  for (i <- 0 until 4) {
    when(io.in_en) {
      FP_mult_inst(i).io.in_en := io.in_en
      FP_mult_inst(i).io.in_valid := io.in_valid
    }
  }

  for (i <- 0 until 2) {
    when(io.in_en && counter >= mult_pd.U) {
      FP_adder_Inst(i).io.in_en := io.in_en
      FP_adder_Inst(i).io.in_valid := io.in_valid
    }
  }

  io.out_s := Cat(FP_adder_Inst(0).io.out_s, FP_adder_Inst(1).io.out_s)
  io.out_valid := ShiftRegister(io.in_valid, latency, io.in_en)
  io.out_real := FP_adder_Inst(0).io.out_s
  io.out_imag := FP_adder_Inst(1).io.out_s


  //printf(p"Counter: $counter\n")
  //for (i <- 0 until 4 ) {
  //  printf(p"Mult${i}: ${FP_mult_inst(i).io.out_s}\n")
  //}
  //for (i <- 0 until 2) {
  //  printf(p"Adder${i}: ${FP_adder_Inst(i).io.out_s}\n")
  //}

  //printf(p"Out: ${io.out_real}\n")
  //printf(p"Out: ${io.out_imag}\n")
  //printf(p"Out: ${io.out_s}\n")

}

class complex_adder (bw: Int, add_pd: Int) extends Module {
  val io = IO(new Bundle {
    val complexA = Input(UInt(bw.W))
    val complexB = Input(UInt(bw.W))
    val in_en = Input(Bool())
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_s = Output(UInt(bw.W))
    val out_real = Output(UInt((bw / 2).W))
    val out_imag = Output(UInt((bw / 2).W))
  })

  val latency = add_pd

  val FP_adder_Inst = Seq.fill(2)(Module(new FP_add((bw / 2), add_pd)))
  FP_adder_Inst.foreach { mod =>
    mod.io.in_a := 0.U
    mod.io.in_b := 0.U
    mod.io.in_en := false.B
    mod.io.in_valid := false.B
  }

  FP_adder_Inst(0).io.in_a := io.complexA((bw - 1), (bw / 2)) //realA
  FP_adder_Inst(0).io.in_b := io.complexB((bw - 1), (bw / 2)) //realB
  FP_adder_Inst(1).io.in_a := io.complexA(((bw / 2) - 1), 0)  //imagA
  FP_adder_Inst(1).io.in_b := io.complexB(((bw / 2) - 1), 0)  //imagB

  when (io.in_en) {
    FP_adder_Inst(0).io.in_en := io.in_en
    FP_adder_Inst(1).io.in_en := io.in_en
    FP_adder_Inst(0).io.in_valid := io.in_valid
    FP_adder_Inst(1).io.in_valid := io.in_valid
  }

  io.out_s := Cat(FP_adder_Inst(0).io.out_s, FP_adder_Inst(1).io.out_s)
  io.out_real := FP_adder_Inst(0).io.out_s
  io.out_imag := FP_adder_Inst(1).io.out_s
  io.out_valid := ShiftRegister(io.in_valid, latency, io.in_en)
}

class complex_acc (bw: Int, x: Int, add_pd: Int, sel_bit: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bw.W))
    val in_en = Input(Bool())
    //val sel = Input(Bool())
    val in_valid = Input(Bool())
    val output = Output(UInt(bw.W))
    val out_real = Output(UInt((bw / 2).W))
    val out_imag = Output(UInt((bw / 2).W))
    val out_valid = Output(Bool())
  })

  val counter = RegInit(0.U(32.W))
  when (io.in_en){
    counter := counter + 1.U
  }
  //val myreg = RegInit(0.U(bw.W))
  //myreg := io.input
  val demux1 = Module(new demux(bw))
  demux1.io.input := io.input
  demux1.io.sel := (counter >> sel_bit) & 1.U//(0)

  val adder = Module(new complex_adder(bw, add_pd))
  adder.io.in_en := ShiftRegister(io.in_en, x)
  adder.io.complexA := ShiftRegister(demux1.io.outputs(0), x)
  adder.io.complexB:= demux1.io.outputs(1)
  adder.io.in_valid := false.B
  when (adder.io.complexA =/= 0.U && adder.io.complexB =/= 0.U) {
    adder.io.in_valid := io.in_valid
    //adder.io.in_en := true.B
  }

  io.output := adder.io.out_s
  io.out_real := adder.io.out_real
  io.out_imag := adder.io.out_imag
  io.out_valid := adder.io.out_valid


  //printf(p"Counter: $counter\n")
  //printf(p"Demux_Outputs:[0] ${demux1.io.outputs(0)}, [1] ${demux1.io.outputs(1)}\n")
  //printf(p"Adder_Input_A: ${adder.io.in_a}, Adder_Input_B: ${adder.io.in_b}\n")
  //printf(p"Adder_Valid: ${adder.io.in_valid}, Adder_Enable: ${adder.io.in_en}\n")
  //printf(p"Adder_Output: ${adder.io.out_s}, Adder_Out_Valid: ${adder.io.out_valid}\n")
  //printf(p"Final_Out: ${io.output}\n")
  //printf("\n")


}

class complex_dot_streaming (n: Int, bw: Int, mult_pd: Int, add_pd: Int) extends Module {
  require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
  val io = IO(new Bundle {
    val vec_a = Input(Vec(n, UInt(bw.W)))
    val vec_b = Input(Vec(n, UInt(bw.W)))
    val in_en = Input(Bool())
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_s = Output(UInt(bw.W))
    val out_real = Output(UInt((bw / 2).W))
    val out_imag = Output(UInt((bw / 2).W))
  })

  val latency = (mult_pd + add_pd) + (log2Ceil(n) * add_pd)
  val num_layers = log2Ceil(n)

  val Mult_Wire = Wire(Vec(n, UInt(bw.W)))
  for (i <- 0 until n) {
    val MultInst = Module(new complex_conjugate_mult(bw, mult_pd, add_pd))
    MultInst.io.complexA := io.vec_a(i)
    MultInst.io.complexB := io.vec_b(i)
    MultInst.io.in_en := io.in_en
    MultInst.io.in_valid := io.in_valid
    Mult_Wire(i) := MultInst.io.out_s
  }
  var currentlayer = Mult_Wire
  for (j <- 0 until num_layers) {
    val layerSize = (currentlayer.length + 1) / 2
    val adder_Wire = Wire(Vec(layerSize, UInt(bw.W)))

    for (h <- 0 until layerSize) {
      if (2 * h + 1 < currentlayer.length) {
        val adderInst = Module(new complex_adder(bw, add_pd))
        adderInst.io.complexA := currentlayer(2 * h)
        adderInst.io.complexB := currentlayer(2 * h + 1)
        adderInst.io.in_en := io.in_en
        adderInst.io.in_valid := io.in_valid
        adder_Wire(h) := adderInst.io.out_s
      }
      else {
        adder_Wire(h) := ShiftRegister(currentlayer(2 * h), 13)
      }
    }
    currentlayer = adder_Wire
  }

  io.out_s := Mux(io.in_en, currentlayer.head, 0.U)
  io.out_real := currentlayer.head((bw - 1), (bw / 2))
  io.out_imag := currentlayer.head(((bw / 2) - 1), 0)
  io.out_valid := ShiftRegister(io.in_valid, latency, io.in_en)
}

class complex_dot_iterative (n: Int, k: Int, bw: Int, mult_pd: Int, add_pd: Int) extends Module {
  val io = IO(new Bundle {
    val vec_a = Input(Vec(k, UInt(bw.W)))
    val vec_b = Input(Vec(k, UInt(bw.W)))
    val in_en = Input(Bool())
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_real = Output(UInt((bw / 2).W))
    val out_imag = Output(UInt((bw / 2).W))
    val out_s = Output(UInt(bw.W))

  })

  val num_batches = k / n //((k + (n - 1)) / n)
  val num_acc = log2Ceil(num_batches)
  val mult_latency = (mult_pd + add_pd) + (log2Ceil(n) * add_pd)
  val latency = mult_latency + (num_acc * add_pd) + (math.pow(2, (num_acc)).toInt - 1)

  val counter = RegInit(0.U(32.W))
  when(io.in_en && counter < latency.U) {
    counter := counter + 1.U
  }

  io.out_s := 0.U
  io.out_valid := false.B

  val index = RegInit(0.U(log2Ceil(num_batches).W))
  val VecA_batch = Wire(Vec(n, UInt(bw.W)))
  val VecB_batch = Wire(Vec(n, UInt(bw.W)))

  val complex_dot = Module(new complex_dot_streaming(n, bw, mult_pd, add_pd))
  complex_dot.io.vec_a := VecInit(Seq.fill(n)(0.U(bw.W)))
  complex_dot.io.vec_b := VecInit(Seq.fill(n)(0.U(bw.W)))
  complex_dot.io.in_en := false.B
  complex_dot.io.in_valid := false.B

  val d2aInst = Seq.tabulate(num_acc) { i =>
    val mod = Module(new complex_acc(bw, 1 << i, add_pd, i))
    mod.io.input := 0.U
    mod.io.in_en := false.B
    mod.io.in_valid := false.B
    mod
  }

  for (i <- 0 until n) {
    val batch_index = index * n.U + i.U
    VecA_batch(i) := io.vec_a(batch_index)
    VecB_batch(i) := io.vec_b(batch_index)
  }

  complex_dot.io.vec_a := VecA_batch
  complex_dot.io.vec_b := VecB_batch

  when(index + 1.U === num_batches.U) {
    index := 0.U
  }.otherwise {
    index := index + 1.U
  }

  when(io.in_en && counter < (mult_latency + num_batches).U) {
    complex_dot.io.in_en := true.B
    complex_dot.io.in_valid := true.B
  }.otherwise {
    complex_dot.io.in_en := false.B
    complex_dot.io.in_valid := false.B
  }

  for (i <- 0 until num_acc) {

    when(counter >= (mult_latency + (add_pd * i) + (math.pow(2, i).toInt - 1)).U) { //((mult_pd + add_pd) + (i * add_pd) + (math.pow(2, (i + 1)).toInt - 2)).U) { // && counter <= ((mult_pd + 1 + add_pd + 1) + (i * add_pd) + (math.pow(2, (i + 1)).toInt - 2) + add_pd).U) {
      d2aInst(i).io.in_en := true.B
      d2aInst(i).io.in_valid := true.B

    }
    if (i == 0) {
      d2aInst(i).io.input := complex_dot.io.out_s
    } else {
      d2aInst(i).io.input := d2aInst(i - 1).io.output
    }
  }

  io.out_s := d2aInst(num_acc - 1).io.output
  io.out_real := d2aInst(num_acc - 1).io.output((bw - 1), (bw / 2))
  io.out_imag := d2aInst(num_acc - 1).io.output(((bw / 2) - 1), 0)
  io.out_valid := ShiftRegister(io.in_valid, latency, io.in_en)

  //printf(p"Counter: $counter\n")

  //for (i <- 0 until n) {
  //  printf(p"Dot Input VecA :[${i}] ${complex_dot.io.vec_a(i)}\n")
  //}
  //for (i <- 0 until n) {
  //  printf(p"Dot Input VecB :[${i}] ${complex_dot.io.vec_b(i)}\n")
  //}

  //printf(p"Dot Product Output: ${complex_dot.io.out_s}\n")

  //for (i <- 0 until num_acc) {
  //  printf(p"acc_Input: ${d2aInst(i).io.input}, acc_Output: ${d2aInst(i).io.output}\n")
  //}

  //printf(p"Final_Out: ${io.out_s}\n")
  //printf(p"Real_Out: ${io.out_real}\n")
  //printf(p"Imag_Out: ${io.out_imag}\n")
  //printf("\n")

}

class SW_two_complex_dot (k: Int, bw: Int, mult_pd: Int, add_pd: Int) extends Module {
  val io = IO(new Bundle {
    val vec_a = Input(Vec(k, UInt(bw.W)))
    val vec_b = Input(Vec(k, UInt(bw.W)))
    val in_en = Input(Bool())
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_real = Output(UInt((bw / 2).W))
    val out_imag = Output(UInt((bw / 2).W))
    val out_s = Output(UInt(bw.W))

  })

  val num_batches = k / 2
  val num_acc = log2Ceil(num_batches)
  val mult_latency = ((mult_pd + add_pd) + add_pd)
  val latency = mult_latency + (num_acc * add_pd) + (math.pow(2, (num_acc)).toInt - 1)

  val indexwidth = log2Ceil(k + 1) // **
  val index = RegInit(0.U(indexwidth.W)) // added

  val counter = RegInit(0.U(32.W))
  when(io.in_en && counter < latency.U) {
    counter := counter + 1.U
  }

  io.out_s := 0.U
  io.out_valid := false.B
  io.out_real := 0.U
  io.out_imag := 0.U

  //val complex_dot = Module(new complex_dot_streaming(2, bw, mult_pd, add_pd))
  //complex_dot.io.vec_a := VecInit(Seq.fill(2)(0.U(bw.W)))
  //complex_dot.io.vec_b := VecInit(Seq.fill(2)(0.U(bw.W)))
  //complex_dot.io.in_en := false.B
  //complex_dot.io.in_valid := false.B

  val FP_mult_inst = Seq.fill(2)(Module(new complex_conjugate_mult(bw , mult_pd, add_pd)))
  FP_mult_inst.foreach { mod =>
    mod.io.complexA := 0.U
    mod.io.complexB := 0.U
    mod.io.in_en := false.B
    mod.io.in_valid := false.B
  }

  val FP_adder = Module(new complex_adder(bw, add_pd))
  FP_adder.io.complexA := 0.U
  FP_adder.io.complexB := 0.U
  FP_adder.io.in_en := false.B
  FP_adder.io.in_valid := false.B


  val d2aInst = Seq.tabulate(num_acc) { i =>
    val mod = Module(new complex_acc(bw, 1 << i, add_pd, i))
    mod.io.input := 0.U
    mod.io.in_en := false.B
    mod.io.in_valid := false.B
    mod
  }

  when (io.in_en && counter < (mult_latency + num_batches).U) {
    FP_mult_inst(0).io.in_valid := io.in_valid
    FP_mult_inst(0).io.in_en := true.B
    FP_mult_inst(1).io.in_valid := io.in_valid
    FP_mult_inst(1).io.in_en := true.B

    when(index < num_batches.U) {
      val updatedIndex = (index << 1)(indexwidth - 1, 0)  // bit shift, much like power of 2, more efficient use of hardware than multiply by 2.
      when(updatedIndex + 1.U < k.U) {                   // main input logic, feeding input vectors two elements at a time through DotProduct at every clcok cycle.
        FP_mult_inst(0).io.complexA := io.vec_a(updatedIndex)
        FP_mult_inst(1).io.complexA := io.vec_a(updatedIndex + 1.U)   // all uses of updatedIndex were (counter * 2.U) **
        FP_mult_inst(0).io.complexB := io.vec_b(updatedIndex)
        FP_mult_inst(1).io.complexB := io.vec_b(updatedIndex + 1.U)
      }.otherwise {
        FP_mult_inst(0).io.complexA := io.vec_a(updatedIndex)     // << The "odd" situation, if vector size has odd elements k the final pairing will be computed with 0.U in the second multiplier.
        FP_mult_inst(1).io.complexA := 0.U
        FP_mult_inst(0).io.complexB := io.vec_b(updatedIndex)
        FP_mult_inst(1).io.complexB := 0.U

      }
      index := index + 1.U // added **

    }
    //counter := counter + 1.U
  }//.otherwise {
   // complex_dot.io.in_en := false.B
  //}

  when (counter >= 23.U) {
    FP_adder.io.in_en := true.B
    FP_adder.io.complexA := FP_mult_inst(0).io.out_s
    FP_adder.io.complexB := FP_mult_inst(1).io.out_s
  }

  for (i <- 0 until num_acc) {
    when(counter >= ((mult_latency) + (add_pd * i) + (math.pow(2, i).toInt - 1)).U) { //((mult_pd + add_pd) + (i * add_pd) + (math.pow(2, (i + 1)).toInt - 2)).U) { // && counter <= ((mult_pd + 1 + add_pd + 1) + (i * add_pd) + (math.pow(2, (i + 1)).toInt - 2) + add_pd).U) {
      d2aInst(i).io.in_en := true.B
      d2aInst(i).io.in_valid := true.B

    }

    if (i == 0) {
      d2aInst(i).io.input := FP_adder.io.out_s
    }
    else {
      d2aInst(i).io.input := d2aInst(i - 1).io.output
    }

  }

  io.out_s := d2aInst(num_acc - 1).io.output
  io.out_real := d2aInst(num_acc - 1).io.output((bw - 1), (bw / 2))
  io.out_imag := d2aInst(num_acc - 1).io.output(((bw / 2) - 1), 0)
  io.out_valid := ShiftRegister(io.in_valid, latency, io.in_en)

  //printf(p"Counter: $counter\n")

  //for (i <- 0 until 2) {
  //  printf(p"Mult[${i}] Input ComplexA : ${FP_mult_inst(i).io.complexA} Input ComplexB: ${FP_mult_inst(i).io.complexB}\n")
  //}
  //for (i <- 0 until 2) {
  //  printf(p"Mult[${i}] Output: ${FP_mult_inst(i).io.out_s}\n")
  //}

  //printf(p"Adder Input:ComplexA: ${FP_adder.io.complexA} ComplexB: ${FP_adder.io.complexB}\n")

  //printf(p"Adder Output: ${FP_adder.io.out_s}\n")

  //for (i <- 0 until num_acc) {
  //  printf(p"acc_Input: ${d2aInst(i).io.input}, acc_Output: ${d2aInst(i).io.output}\n")
  //}

  //printf(p"Final_Out: ${io.out_s}\n")
  //printf(p"Real_Out: ${io.out_real}\n")
  //printf(p"Imag_Out: ${io.out_imag}\n")
  //printf("\n")
}