FIRRTL version 1.2.0
circuit InverseS_gate :
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

  module InverseS_gate : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 138:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 138:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 138:7]
    input io_in_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 139:14]
    input io_in_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 139:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 139:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 139:14]
    output io_out_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 139:14]
    output io_out_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 139:14]

    inst flipC of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 145:21]
    node _flipC_io_in_T = bits(io_in_1, 31, 16) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 146:26]
    node _io_out_1_T = bits(io_in_1, 15, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 150:24]
    node _io_out_1_T_1 = cat(_io_out_1_T, flipC.io_out) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 150:37]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 151:32]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_en, io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 151:{32,32,32}]
    io_out_valid <= io_out_valid_r @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 151:16]
    io_out_0 <= io_in_0 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 149:13]
    io_out_1 <= _io_out_1_T_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 150:13]
    flipC.clock <= clock
    flipC.reset <= reset
    flipC.io_in <= _flipC_io_in_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 146:15]
    io_out_valid_r <= _GEN_0
