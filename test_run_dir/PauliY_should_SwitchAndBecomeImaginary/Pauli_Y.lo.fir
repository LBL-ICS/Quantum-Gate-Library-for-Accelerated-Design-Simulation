FIRRTL version 1.2.0
circuit Pauli_Y :
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

  module Pauli_Y : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 58:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 58:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 58:7]
    input io_in_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 59:14]
    input io_in_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 59:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 59:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 59:14]
    output io_out_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 59:14]
    output io_out_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 59:14]

    inst flipA of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 66:21]
    inst flipD of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 67:21]
    node _flipA_io_in_T = bits(io_in_0, 31, 16) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 69:26]
    node _flipD_io_in_T = bits(io_in_1, 15, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 70:26]
    node _io_out_0_T = bits(io_in_1, 31, 16) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 73:42]
    node _io_out_0_T_1 = cat(flipD.io_out, _io_out_0_T) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 73:19]
    node _io_out_1_T = bits(io_in_0, 15, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 74:28]
    node _io_out_1_T_1 = cat(_io_out_1_T, flipA.io_out) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 74:19]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 75:32]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_en, io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 75:{32,32,32}]
    io_out_valid <= io_out_valid_r @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 75:16]
    io_out_0 <= _io_out_0_T_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 73:13]
    io_out_1 <= _io_out_1_T_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 74:13]
    flipA.clock <= clock
    flipA.reset <= reset
    flipA.io_in <= _flipA_io_in_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 69:15]
    flipD.clock <= clock
    flipD.reset <= reset
    flipD.io_in <= _flipD_io_in_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 70:15]
    io_out_valid_r <= _GEN_0
