FIRRTL version 1.2.0
circuit CPauli_Y :
  module turnNegative : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 11:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 11:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 11:7]
    input io_in : UInt<16> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 12:14]
    output io_out : UInt<16> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 12:14]

    node _io_out_T = bits(io_in, 14, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:22]
    node _io_out_T_1 = eq(_io_out_T, UInt<1>("h0")) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:30]
    node _io_out_T_2 = bits(io_in, 15, 15) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:48]
    node _io_out_T_3 = not(_io_out_T_2) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:42]
    node _io_out_T_4 = mux(_io_out_T_1, UInt<1>("h0"), _io_out_T_3) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:16]
    node _io_out_T_5 = bits(io_in, 14, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:64]
    node _io_out_T_6 = cat(_io_out_T_4, _io_out_T_5) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:56]
    io_out <= _io_out_T_6 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\flipMSB\\turnNegative.scala 16:10]

  module CPauli_Y : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 194:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 194:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 194:7]
    input io_in_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    input io_in_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    input io_in_2 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    input io_in_3 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    output io_out_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    output io_out_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    output io_out_2 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]
    output io_out_3 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 195:14]

    inst flipG of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 201:21]
    inst flipF of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 202:21]
    node _flipF_io_in_T = bits(io_in_2, 15, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 203:26]
    node _flipG_io_in_T = bits(io_in_3, 31, 16) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 204:26]
    node _io_out_2_T = bits(io_in_3, 15, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 208:24]
    node _io_out_2_T_1 = cat(_io_out_2_T, flipG.io_out) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 208:37]
    node _io_out_3_T = bits(io_in_2, 31, 16) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 209:40]
    node _io_out_3_T_1 = cat(flipF.io_out, _io_out_3_T) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 209:29]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 210:32]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_en, io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 210:{32,32,32}]
    io_out_valid <= io_out_valid_r @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 210:16]
    io_out_0 <= io_in_0 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 206:13]
    io_out_1 <= io_in_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 207:13]
    io_out_2 <= _io_out_2_T_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 208:13]
    io_out_3 <= _io_out_3_T_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 209:13]
    flipG.clock <= clock
    flipG.reset <= reset
    flipG.io_in <= _flipG_io_in_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 204:15]
    flipF.clock <= clock
    flipF.reset <= reset
    flipF.io_in <= _flipF_io_in_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 203:15]
    io_out_valid_r <= _GEN_0
