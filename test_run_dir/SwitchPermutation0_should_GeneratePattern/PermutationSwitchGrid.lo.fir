FIRRTL version 1.2.0
circuit PermutationSwitchGrid :
  module tieVec : @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 184:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 184:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 184:7]
    input io_in_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 185:14]
    output io_out : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 185:14]

    node inputs_0 = io_in_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 191:52 193:15]
    io_out <= inputs_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 196:10]

  module tieVecLayer : @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 198:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 198:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 198:7]
    input io_in_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    input io_in_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    input io_in_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    input io_in_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    input io_in_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    input io_in_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    input io_in_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    input io_in_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]
    output io_out_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 199:14]

    inst tieVector_0 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    inst tieVector_1 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    inst tieVector_2 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    inst tieVector_3 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    inst tieVector_4 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    inst tieVector_5 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    inst tieVector_6 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    inst tieVector_7 of tieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 204:73]
    io_out_QSV_0 <= tieVector_0.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    io_out_QSV_1 <= tieVector_1.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    io_out_QSV_2 <= tieVector_2.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    io_out_QSV_3 <= tieVector_3.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    io_out_QSV_4 <= tieVector_4.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    io_out_QSV_5 <= tieVector_5.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    io_out_QSV_6 <= tieVector_6.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    io_out_QSV_7 <= tieVector_7.io_out @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 210:19]
    tieVector_0.clock <= clock
    tieVector_0.reset <= reset
    tieVector_0.io_in_0 <= io_in_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]
    tieVector_1.clock <= clock
    tieVector_1.reset <= reset
    tieVector_1.io_in_0 <= io_in_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]
    tieVector_2.clock <= clock
    tieVector_2.reset <= reset
    tieVector_2.io_in_0 <= io_in_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]
    tieVector_3.clock <= clock
    tieVector_3.reset <= reset
    tieVector_3.io_in_0 <= io_in_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]
    tieVector_4.clock <= clock
    tieVector_4.reset <= reset
    tieVector_4.io_in_0 <= io_in_QSV_4 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]
    tieVector_5.clock <= clock
    tieVector_5.reset <= reset
    tieVector_5.io_in_0 <= io_in_QSV_5 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]
    tieVector_6.clock <= clock
    tieVector_6.reset <= reset
    tieVector_6.io_in_0 <= io_in_QSV_6 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]
    tieVector_7.clock <= clock
    tieVector_7.reset <= reset
    tieVector_7.io_in_0 <= io_in_QSV_7 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 208:29]

  module untieVec : @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 221:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 221:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 221:7]
    input io_in : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 222:14]
    output io_out_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 222:14]

    node _io_out_0_T = bits(io_in, 2, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 227:23]
    io_out_0 <= _io_out_0_T @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 227:15]

  module untieVecLayer : @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 230:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 230:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 230:7]
    input io_in_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    input io_in_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    input io_in_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    input io_in_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    input io_in_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    input io_in_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    input io_in_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    input io_in_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]
    output io_out_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 231:14]

    inst untieVector_0 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    inst untieVector_1 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    inst untieVector_2 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    inst untieVector_3 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    inst untieVector_4 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    inst untieVector_5 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    inst untieVector_6 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    inst untieVector_7 of untieVec @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 236:75]
    io_out_QSV_0 <= untieVector_0.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    io_out_QSV_1 <= untieVector_1.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    io_out_QSV_2 <= untieVector_2.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    io_out_QSV_3 <= untieVector_3.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    io_out_QSV_4 <= untieVector_4.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    io_out_QSV_5 <= untieVector_5.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    io_out_QSV_6 <= untieVector_6.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    io_out_QSV_7 <= untieVector_7.io_out_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 242:56]
    untieVector_0.clock <= clock
    untieVector_0.reset <= reset
    untieVector_0.io_in <= io_in_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]
    untieVector_1.clock <= clock
    untieVector_1.reset <= reset
    untieVector_1.io_in <= io_in_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]
    untieVector_2.clock <= clock
    untieVector_2.reset <= reset
    untieVector_2.io_in <= io_in_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]
    untieVector_3.clock <= clock
    untieVector_3.reset <= reset
    untieVector_3.io_in <= io_in_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]
    untieVector_4.clock <= clock
    untieVector_4.reset <= reset
    untieVector_4.io_in <= io_in_QSV_4 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]
    untieVector_5.clock <= clock
    untieVector_5.reset <= reset
    untieVector_5.io_in <= io_in_QSV_5 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]
    untieVector_6.clock <= clock
    untieVector_6.reset <= reset
    untieVector_6.io_in <= io_in_QSV_6 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]
    untieVector_7.clock <= clock
    untieVector_7.reset <= reset
    untieVector_7.io_in <= io_in_QSV_7 @[\\src\\main\\scala\\QuantumStateUnit\\Old\\ScalaAlgorithmMuxLayers.scala 240:26]

  module Switch2x2 : @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 5:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 5:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 5:7]
    input io_input_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 6:14]
    input io_input_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 6:14]
    input io_in_sel : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 6:14]
    output io_output_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 6:14]
    output io_output_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 6:14]

    node _io_output_0_T = mux(io_in_sel, io_input_1, io_input_0) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 11:22]
    node _io_output_1_T = mux(io_in_sel, io_input_0, io_input_1) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 12:22]
    io_output_0 <= _io_output_0_T @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 11:16]
    io_output_1 <= _io_output_1_T @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\Switch2x2\\Switch2x2.scala 12:16]

  module WireLayer : @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 6:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 6:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 6:7]
    input io_input_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    input io_input_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    input io_input_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    input io_input_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    input io_input_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    input io_input_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    input io_input_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    input io_input_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]
    output io_output_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 7:14]

    io_output_0 <= io_input_0 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]
    io_output_1 <= io_input_1 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]
    io_output_2 <= io_input_2 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]
    io_output_3 <= io_input_3 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]
    io_output_4 <= io_input_4 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]
    io_output_5 <= io_input_5 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]
    io_output_6 <= io_input_6 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]
    io_output_7 <= io_input_7 @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\WireLayer\\WireLayer.scala 11:13]

  module PSGtarget0 : @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 72:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 72:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 72:7]
    input io_in_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    input io_in_sel : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]
    output io_out_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 73:14]

    inst switch_0 of Switch2x2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 79:83]
    inst switch_1 of Switch2x2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 79:83]
    inst switch_2 of Switch2x2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 79:83]
    inst switch_3 of Switch2x2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 79:83]
    inst wireL_0 of WireLayer @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 80:47]
    inst wireL_1 of WireLayer @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 80:47]
    inst wireL_2 of WireLayer @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 80:47]
    node _switch_0_io_in_sel_T = bits(io_in_sel, 1, 1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:109]
    node _switch_1_io_in_sel_T = bits(io_in_sel, 1, 1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:109]
    node _switch_2_io_in_sel_T = bits(io_in_sel, 2, 2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:109]
    node _switch_3_io_in_sel_T = bits(io_in_sel, 2, 2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:109]
    io_out_QSV_0 <= wireL_2.io_output_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    io_out_QSV_1 <= wireL_2.io_output_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    io_out_QSV_2 <= wireL_2.io_output_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    io_out_QSV_3 <= wireL_2.io_output_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    io_out_QSV_4 <= wireL_2.io_output_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    io_out_QSV_5 <= wireL_2.io_output_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    io_out_QSV_6 <= wireL_2.io_output_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    io_out_QSV_7 <= wireL_2.io_output_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 83:21]
    switch_0.clock <= clock
    switch_0.reset <= reset
    switch_0.io_input_0 <= wireL_0.io_output_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 96:99]
    switch_0.io_input_1 <= wireL_0.io_output_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 97:99]
    switch_0.io_in_sel <= _switch_0_io_in_sel_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:97]
    switch_1.clock <= clock
    switch_1.reset <= reset
    switch_1.io_input_0 <= wireL_0.io_output_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 96:99]
    switch_1.io_input_1 <= wireL_0.io_output_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 97:99]
    switch_1.io_in_sel <= _switch_1_io_in_sel_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:97]
    switch_2.clock <= clock
    switch_2.reset <= reset
    switch_2.io_input_0 <= wireL_1.io_output_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 96:99]
    switch_2.io_input_1 <= wireL_1.io_output_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 97:99]
    switch_2.io_in_sel <= _switch_2_io_in_sel_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:97]
    switch_3.clock <= clock
    switch_3.reset <= reset
    switch_3.io_input_0 <= wireL_1.io_output_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 96:99]
    switch_3.io_input_1 <= wireL_1.io_output_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 97:99]
    switch_3.io_in_sel <= _switch_3_io_in_sel_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 105:97]
    wireL_0.clock <= clock
    wireL_0.reset <= reset
    wireL_0.io_input_0 <= io_in_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_0.io_input_1 <= io_in_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_0.io_input_2 <= io_in_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_0.io_input_3 <= io_in_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_0.io_input_4 <= io_in_QSV_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_0.io_input_5 <= io_in_QSV_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_0.io_input_6 <= io_in_QSV_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_0.io_input_7 <= io_in_QSV_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 82:21]
    wireL_1.clock <= clock
    wireL_1.reset <= reset
    wireL_1.io_input_0 <= wireL_0.io_output_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 102:77]
    wireL_1.io_input_1 <= switch_0.io_output_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 100:77]
    wireL_1.io_input_2 <= switch_0.io_output_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 99:77]
    wireL_1.io_input_3 <= wireL_0.io_output_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 103:77]
    wireL_1.io_input_4 <= wireL_0.io_output_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 102:77]
    wireL_1.io_input_5 <= switch_1.io_output_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 100:77]
    wireL_1.io_input_6 <= switch_1.io_output_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 99:77]
    wireL_1.io_input_7 <= wireL_0.io_output_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 103:77]
    wireL_2.clock <= clock
    wireL_2.reset <= reset
    wireL_2.io_input_0 <= wireL_1.io_output_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 102:77]
    wireL_2.io_input_1 <= switch_2.io_output_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 100:77]
    wireL_2.io_input_2 <= wireL_1.io_output_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 102:77]
    wireL_2.io_input_3 <= switch_3.io_output_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 100:77]
    wireL_2.io_input_4 <= switch_2.io_output_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 99:77]
    wireL_2.io_input_5 <= wireL_1.io_output_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 103:77]
    wireL_2.io_input_6 <= switch_3.io_output_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 99:77]
    wireL_2.io_input_7 <= wireL_1.io_output_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 103:77]

  module PermutationSwitchGrid : @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 45:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 45:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 45:7]
    input io_in_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    input io_in_sel : UInt<2> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_5 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_6 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]
    output io_out_QSV_7 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 46:14]

    inst tieVectorLayer of tieVecLayer @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 53:32]
    inst untieVectorLayer of untieVecLayer @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 54:32]
    inst switchGrid of PSGtarget0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 55:32]
    node switchGrid_io_in_sel_shiftAmount = bits(io_in_sel, 1, 0) @[src/main/scala/chisel3/util/OneHot.scala 64:49]
    node _switchGrid_io_in_sel_T = dshl(UInt<1>("h1"), switchGrid_io_in_sel_shiftAmount) @[src/main/scala/chisel3/util/OneHot.scala 65:12]
    node _switchGrid_io_in_sel_T_1 = bits(_switchGrid_io_in_sel_T, 3, 0) @[src/main/scala/chisel3/util/OneHot.scala 65:27]
    io_out_QSV_0 <= untieVectorLayer.io_out_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    io_out_QSV_1 <= untieVectorLayer.io_out_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    io_out_QSV_2 <= untieVectorLayer.io_out_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    io_out_QSV_3 <= untieVectorLayer.io_out_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    io_out_QSV_4 <= untieVectorLayer.io_out_QSV_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    io_out_QSV_5 <= untieVectorLayer.io_out_QSV_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    io_out_QSV_6 <= untieVectorLayer.io_out_QSV_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    io_out_QSV_7 <= untieVectorLayer.io_out_QSV_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 61:31]
    tieVectorLayer.clock <= clock
    tieVectorLayer.reset <= reset
    tieVectorLayer.io_in_QSV_0 <= io_in_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    tieVectorLayer.io_in_QSV_1 <= io_in_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    tieVectorLayer.io_in_QSV_2 <= io_in_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    tieVectorLayer.io_in_QSV_3 <= io_in_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    tieVectorLayer.io_in_QSV_4 <= io_in_QSV_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    tieVectorLayer.io_in_QSV_5 <= io_in_QSV_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    tieVectorLayer.io_in_QSV_6 <= io_in_QSV_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    tieVectorLayer.io_in_QSV_7 <= io_in_QSV_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 58:31]
    untieVectorLayer.clock <= clock
    untieVectorLayer.reset <= reset
    untieVectorLayer.io_in_QSV_0 <= switchGrid.io_out_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    untieVectorLayer.io_in_QSV_1 <= switchGrid.io_out_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    untieVectorLayer.io_in_QSV_2 <= switchGrid.io_out_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    untieVectorLayer.io_in_QSV_3 <= switchGrid.io_out_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    untieVectorLayer.io_in_QSV_4 <= switchGrid.io_out_QSV_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    untieVectorLayer.io_in_QSV_5 <= switchGrid.io_out_QSV_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    untieVectorLayer.io_in_QSV_6 <= switchGrid.io_out_QSV_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    untieVectorLayer.io_in_QSV_7 <= switchGrid.io_out_QSV_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 60:31]
    switchGrid.clock <= clock
    switchGrid.reset <= reset
    switchGrid.io_in_QSV_0 <= tieVectorLayer.io_out_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_QSV_1 <= tieVectorLayer.io_out_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_QSV_2 <= tieVectorLayer.io_out_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_QSV_3 <= tieVectorLayer.io_out_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_QSV_4 <= tieVectorLayer.io_out_QSV_4 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_QSV_5 <= tieVectorLayer.io_out_QSV_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_QSV_6 <= tieVectorLayer.io_out_QSV_6 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_QSV_7 <= tieVectorLayer.io_out_QSV_7 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 59:31]
    switchGrid.io_in_sel <= bits(_switchGrid_io_in_sel_T_1, 2, 0) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\ScalaAlgorithmBlockSwitches.scala 65:24]
