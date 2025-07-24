package QuantumStateUnit.Gates

import QuantumStateUnit.OtherComponents.PsuedoRandomGenerator._
import New_FPU_Mario.FPUnits._
import chisel3._
import chisel3.util._

import scala.math._

/*
Measurement is the top design of multiple components: retrieve probability, random number generator.
  When a qubit is measured, a new QSV is generated alongside with its normalization number.
  The measurement gate should also be able to gather the normalization constant if needed.
 */
/*
Measurement Module Valid RollerCoaster
  io.in -> collapsePossibilities -> RNG -> Trigger Normalization register -> when false valid trigger collapsvalid register
  -> collapsePossibilities -> Calculate Normalization Number -> io.out
 */
//Top of the design
class Measurement(val num_of_qubits : Int, val bw : Int, val mult_pd : Int, val add_pd : Int, val L : Int) extends Module {
  val io = IO(new Bundle {
    val in_QSV        = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bw.W)))
    val in_noise      = Input(UInt(32.W))
    val in_sendNorm   =  Input(Bool())
    val in_valid      =  Input(Bool())
    val out_valid     = Output(Bool())
    val out_QSV       = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bw.W)))
    val out_Normalize = Output(UInt(bw.W))
    val out_measured  = Output(Bool())
  })
  //Reducing and normalization is found in CollapseProbability: probably a mistake due to weird valid outputs
  val findProbability = Module(new CollapseProbability(num_of_qubits, bw, mult_pd, add_pd, L))
  val numberGenerator = Module(new CompareWithRandom(bw / 2, mult_pd))
  val QSVout          = Module(new NewQSV(num_of_qubits, bw))
  val normalizeNumber = Module(new GetNormalization(bw / 2, add_pd, L))

  //Normalize output is 1 if sendNorm is 0
  val IEEENumber_One = bw match { //This is to prevent accidental 'normalization'
    case 32 => "h3C00".U(bw.W)
    case 64 => "h3F800000".U(bw.W)
    case 128 => "h3FF0000000000000".U(bw.W)
    case 256 => "h3FFF0000000000000000000000000000".U(bw.W)
  }

  //The valid nightmare for reusing the same Module for two different task in a sequence
  val normalize = RegInit(0.B) //Bool to keep track of when we are finding the normalization. Reset when input is invalid
  val collapsevalid = RegInit(0.B) //An invalid 'valid' signal is goes through the collapseProb. When valid goes from high to low, then 1.B
  when(numberGenerator.io.out_valid || io.in_sendNorm) { //Switches focus to the normalization
    normalize := 1.B
  }
  when((!findProbability.io.out_valid && ShiftRegister(findProbability.io.out_valid, 1)) || io.in_sendNorm){ //detect falling edge
    collapsevalid := 1.B
  }
  when(!io.in_valid) {
    normalize     := 0.B
    collapsevalid := 0.B
  }

  normalizeNumber.io.in_en := !(normalize && normalizeNumber.io.out_valid) //disable to hold value

  //Initial Input: Collapsing inputs into two outputs ... switches to the unnormalized QSV
  val collapseNumberSel = numberGenerator.io.out_valid & !io.in_sendNorm //Only take in other vector if measuring
  findProbability.io.in_QSV         := Mux(collapseNumberSel, QSVout.io.out_QSV, io.in_QSV)
  findProbability.io.in_valid       := Mux(collapseNumberSel, ShiftRegister(normalize, 1), io.in_valid)
    //The shift register fixes a problem with the output flipping between two different values: one is the true value and the other is just 1

  //Part one - Actually obtaining the measurement
  numberGenerator.io.in_probability := findProbability.io.out_Measured(0) //Compare against prob of 0
  numberGenerator.io.in_sel         := 0.B                                //Compare against prob of 0
  numberGenerator.io.in_valid       := findProbability.io.out_valid || normalize
  numberGenerator.io.in_en          := normalize                          //hold value until QSR updated
  numberGenerator.io.in_seed        := io.in_noise

  QSVout.io.in_QSV                  := io.in_QSV
  QSVout.io.in_sel                  := numberGenerator.io.out_value //From the RNG

  //Part two - obtaining the Normalization number
  normalizeNumber.io.in_valid         := collapsevalid && findProbability.io.out_valid//Only starts when told to and input is valid
  normalizeNumber.io.in_probability(0):= findProbability.io.out_Measured(0)
  normalizeNumber.io.in_probability(1):= findProbability.io.out_Measured(1)

  //Part three - output
  io.out_QSV                        := QSVout.io.out_QSV
  io.out_valid                      := normalizeNumber.io.out_valid
  io.out_Normalize                  := Mux(normalize, normalizeNumber.io.out_Normalize, IEEENumber_One)
  io.out_measured                   := numberGenerator.io.out_value
  //normalization will be the last thing to be finished from this gates actions.
}


  /*
First, the magnitude of each complex number needs to be calculated by squaring both numbers then take the square.
   Then each coefficient needs to be squared to find the final probability, this cancels out the square.
   Finally, each value needs to be added together.
 */
  //Shows the probability of 0 and 1 of the 0th qubit. Note: to choose a different qubit, select via 0th permutation_sel
  //outputs the probability and not the vector itself.
class CollapseProbability(val num_of_qubits: Int, val bw: Int, val mult_pd: Int, val add_pd: Int, val L: Int) extends Module {
  require(bw == 32 || bw == 64 || bw == 128 || bw == 256)
  val io = IO(new Bundle {
    val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bw.W)))
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val out_Measured = Output(Vec(2, UInt((bw / 2).W))) //combined probability of 0 and 1
  })

  val square = Seq.fill(pow(2, num_of_qubits + 1).toInt)(Module(new FP_mult(bw / 2, mult_pd)))
  val addlayer = Seq.fill(pow(2, num_of_qubits).toInt)(Module(new FP_add(bw / 2, add_pd))) //initial add layer

  //reduce adders uses some recursion of a(n) = a(n-1) + 2^n
  var sum = 0 //recursive starting value
  for (n <- 1 until num_of_qubits) {
    sum += pow(2, n).toInt
  }
  val reduceadders = Seq.fill(sum)(Module(new FP_add(bw / 2, add_pd))) //The add layers that add the add layers together into two outputs

  //Inputs
  for (i <- 0 until pow(2, num_of_qubits).toInt) {
    //square real
    square(2 * i).io.in_a         := io.in_QSV(i)(bw - 1, bw / 2) //square real
    square(2 * i).io.in_b         := io.in_QSV(i)(bw - 1, bw / 2)
    square(2 * i).io.in_en        := 1.B
    square(2 * i).io.in_valid     := io.in_valid
    //square imaginary
    square(2 * i + 1).io.in_a     := io.in_QSV(i)((bw / 2) - 1, 0) //square imag
    square(2 * i + 1).io.in_b     := io.in_QSV(i)((bw / 2) - 1, 0)
    square(2 * i + 1).io.in_en    := 1.B
    square(2 * i + 1).io.in_valid := io.in_valid
    //add real and imaginary together
    addlayer(i).io.in_a           := square(2 * i).io.out_s
    addlayer(i).io.in_b           := square(2 * i + 1).io.out_s
    addlayer(i).io.in_valid       := square(2 * i).io.out_valid & square(2 * i + 1).io.out_valid
    addlayer(i).io.in_en          := 1.B
  }

  //Attach reduce layer outputs to the io.out
  io.out_Measured(0) := reduceadders(0).io.out_s
  io.out_Measured(1) := reduceadders(1).io.out_s
  //Start attaching wires from the output to the initial input
  var currentRow = 0 //initial value of recursion
  var nextRow = 2
  for (n <- 1 until num_of_qubits - 1) { //determines which layer we are one
    for (i <- 0 until pow(2, n - 1).toInt) { //determines the inputs of the adders in the layer
      //Add probability of 0
      reduceadders(currentRow + i * 2).io.in_a := reduceadders(nextRow + i * 4).io.out_s
      reduceadders(currentRow + i * 2).io.in_b := reduceadders(nextRow + i * 4 + 2).io.out_s
      reduceadders(currentRow + i * 2).io.in_en := 1.B
      reduceadders(currentRow + i * 2).io.in_valid := reduceadders(nextRow + i * 4).io.out_valid & reduceadders(nextRow + i * 4 + 2).io.out_valid
      //Add probability of 1
      reduceadders(currentRow + i * 2 + 1).io.in_a := reduceadders(nextRow + i * 4 + 1).io.out_s
      reduceadders(currentRow + i * 2 + 1).io.in_b := reduceadders(nextRow + i * 4 + 3).io.out_s
      reduceadders(currentRow + i * 2 + 1).io.in_en := 1.B
      reduceadders(currentRow + i * 2 + 1).io.in_valid := reduceadders(nextRow + i * 4 + 1).io.out_valid & reduceadders(nextRow + i * 4 + 3).io.out_valid
    }
    currentRow += pow(2, n).toInt
    nextRow += pow(2, n + 1).toInt
  }
  //add layer to the reduce layers
  for (i <- 0 until pow(2, num_of_qubits - 2).toInt) {
    //Add probability of 0
    reduceadders(currentRow + (2 * i)).io.in_a := addlayer(4 * i).io.out_s
    reduceadders(currentRow + (2 * i)).io.in_b := addlayer(4 * i + 2).io.out_s
    reduceadders(currentRow + (2 * i)).io.in_en := 1.B
    reduceadders(currentRow + (2 * i)).io.in_valid := addlayer(4 * i).io.out_valid & addlayer(4 * i + 2).io.out_valid
    //Add probability of 1
    reduceadders(currentRow + (2 * i) + 1).io.in_a := addlayer(4 * i + 1).io.out_s
    reduceadders(currentRow + (2 * i) + 1).io.in_b := addlayer(4 * i + 3).io.out_s
    reduceadders(currentRow + (2 * i) + 1).io.in_en := 1.B
    reduceadders(currentRow + (2 * i) + 1).io.in_valid := addlayer(4 * i + 1).io.out_valid & addlayer(4 * i + 3).io.out_valid
  }

  //Valid will come from the normalization module instead if it's chosen
  io.out_valid := reduceadders(0).io.out_valid && reduceadders(1).io.out_valid

}

/*
Ideally the input of io.in_random would be random noise between 0 and 100.
  For now, I will use scala random number generator as the input for the random decision between 0 and 1.

FloatToFixed converts 50 to 32 and 25 to 16, so 100 is 64.
If the ratio between the conversions is constant, then it doesn't matter
As long as the input is a random number between 0 and 64, this won't matter.
 */
class CompareWithRandom(val bw: Int, val mult_pd: Int) extends Module {
  val io = IO(new Bundle {
    val in_probability  =  Input(UInt(bw.W))
    val in_seed         =  Input(UInt(32.W)) //32 bit number
    val in_en           =  Input(Bool()) //0 stops updating output AND feeds a new seed
    val in_sel          =  Input(Bool()) //Determines if 0 or 1 takes the lower number / has the probability of input
    val in_valid        =  Input(Bool())
    val out_valid       = Output(Bool())
    val out_value       = Output(Bool())
  })
  val IEEENumber_Hundred = bw match { // 100
    case 16 => "h5640".U(bw.W)
    case 32 => "h42C80000".U(bw.W)
    case 64 => "h4059000000000000".U(bw.W)
    case 128 => "h40059000000000000000000000000000".U(bw.W)
  }

  //Multiply both numbers by 100, then convert to "fixed point" to be compared
  val multiplier0 = Module(new FP_mult(bw, mult_pd))
  val toFixed0    = Module(new FloatTOFixed(bw, 8, 0)) //Probability of chance for 0

  //pseudo random number generator
  val prev        = RegInit(0.B)
  prev           := io.in_en
  val RNG         = Module(new LinearCongruentialGenerator(32, 64, 5, 1)) //32 bit Linear Congruent RNG
  RNG.io.in_next := !io.in_en //continues down current sequence until enabled
  RNG.io.in_feed := !io.in_en && prev //changes sequence when enabled
  RNG.io.in_seed := io.in_seed//initialization


   //Convert values to a number within the range of 100
  multiplier0.io.in_a     := io.in_probability
  multiplier0.io.in_b     := IEEENumber_Hundred
  multiplier0.io.in_en    := 1.B
  multiplier0.io.in_valid := io.in_valid
  //Convert Number to fixedPoint
  toFixed0.io.in_float    := multiplier0.io.out_s
  toFixed0.io.in_en       := 1.B
  toFixed0.io.in_valid    := multiplier0.io.out_valid

  //The output that tells when 0 or 1
  val determinedValue = RegInit(0.B)
  val valid           = RegInit(0.B)
  valid               := toFixed0.io.out_valid
  when(!io.in_en){
    valid := toFixed0.io.out_valid //Stay valid until change is made with en being 0
  }
  io.out_value := determinedValue
  io.out_valid := valid

  //Compare to Unsigned: the input should be positive anyway because all numbers were squared.
  val probabilityUInt = toFixed0.io.out_fixed.asUInt
  val RNGnumber       = RNG.io.out_Value + 1.U
  //The chosen value will get the lower value
  //              input is lower number         enable
  when((RNGnumber < probabilityUInt) && io.in_en && toFixed0.io.out_valid){
    determinedValue := Mux(io.in_sel, 1.B ,0.B)
  }
  //  other probability is higher number       enable
  when((RNGnumber > probabilityUInt) && io.in_en && toFixed0.io.out_valid){
    determinedValue := Mux(io.in_sel, 0.B, 1.B)
  }
}

/*
  When the qubit is measured, all of the states where the qubit is opposite of the measured state becomes 0.
    Takes a binary input and outputs either all of the probability of 0 or 1. The opposite probability becomes 0.
 */
  class NewQSV(val num_of_qubits: Int, val bw: Int) extends Module {
    val io = IO(new Bundle {
      val in_QSV = Input(Vec(pow(2, num_of_qubits).toInt, UInt(bw.W)))
      val in_sel = Input(Bool())
      val out_QSV = Output(Vec(pow(2, num_of_qubits).toInt, UInt(bw.W)))
    })
    val layerofMux = Module(new QGPMuxLayer(num_of_qubits, bw, 2))

    for (i <- 0 until pow(2, num_of_qubits - 1).toInt) {
      layerofMux.io.in_QSV(0)(2 * i) := io.in_QSV(2 * i)
      layerofMux.io.in_QSV(0)(2 * i + 1) := 0.U
      layerofMux.io.in_QSV(1)(2 * i) := 0.U
      layerofMux.io.in_QSV(1)(2 * i + 1) := io.in_QSV(2 * i + 1)
    }
    layerofMux.io.in_sel := io.in_sel
    io.out_QSV := layerofMux.io.out_QSV
  }

  /*
To get the normalized number, we can use the output from the collapsing probability and combine it to one number.
  If the final number is 1, then it's fine; if not, then the normalization constant will be found to multiply the QSV.
  The number will be sent to the FP gate pool to be multiplied to the rest of the QSV
 */
  //L is a thing listed in the FPUnits, I have no idea what it means. Maybe it's room for operation?
  class GetNormalization(val bw: Int, val add_pd: Int, val L: Int) extends Module {
    require(bw == 16 || bw == 32 || bw == 64 || bw == 128)
    //       L <= 10     L <= 23     L <= 52     L <= 112
    val io = IO(new Bundle {
      val in_probability  =  Input(Vec(2, UInt(bw.W)))
      val in_en           =  Input(Bool())
      val in_valid        =  Input(Bool())
      val out_valid       = Output(Bool())
      val out_Normalize   = Output(UInt(bw.W))
    })

    //One in IEEE 754 format
    val IEEENumber_One = bw match { // 1
      case 16 => "h3C00".U(bw.W)
      case 32 => "h3F800000".U(bw.W)
      case 64 => "h3FF0000000000000".U(bw.W)
      case 128 => "h3FFF0000000000000000000000000000".U(bw.W)
    }
    // basically get total probability -> square root it -> find the recipricol
    //The output will be sent to the FPU units to be multiplied out into the system.
    val addtogether = Module(new FP_add(bw, add_pd))
    val sqrt        = Module(new FP_sqrt(bw, L))
    //val recipricolNum = Module(new FP_div(bw, L)) //divide 1 by number


    addtogether.io.in_a       := io.in_probability(0)
    addtogether.io.in_b       := io.in_probability(1)
    addtogether.io.in_en      := io.in_en
    addtogether.io.in_valid   := io.in_valid

    sqrt.io.in_a              := addtogether.io.out_s
    sqrt.io.in_en             := io.in_en
    sqrt.io.in_valid          := addtogether.io.out_valid

    //recipricolNum.io.in_a     := IEEENumber_One //numerator
    //recipricolNum.io.in_b     := sqrt.io.out_s //denominator
    //recipricolNum.io.in_en    := io.in_en
    //recipricolNum.io.in_valid := sqrt.io.out_valid

    //io.out_Normalize          := recipricolNum.io.out_s
    //io.out_valid              := recipricolNum.io.out_valid

    io.out_Normalize          := sqrt.io.out_s
    io.out_valid              := sqrt.io.out_valid
  }