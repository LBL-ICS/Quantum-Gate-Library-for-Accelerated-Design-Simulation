FIRRTL version 1.2.0
circuit S_gate :
  module turnNegative : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 258:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 258:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 258:7]
    input io_in : UInt<16> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 259:14]
    output io_out : UInt<16> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 259:14]

    node _io_out_T = bits(io_in, 14, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:22]
    node _io_out_T_1 = eq(_io_out_T, UInt<1>("h0")) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:30]
    node _io_out_T_2 = bits(io_in, 15, 15) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:48]
    node _io_out_T_3 = not(_io_out_T_2) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:42]
    node _io_out_T_4 = mux(_io_out_T_1, UInt<1>("h0"), _io_out_T_3) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:16]
    node _io_out_T_5 = bits(io_in, 14, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:64]
    node _io_out_T_6 = cat(_io_out_T_4, _io_out_T_5) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:56]
    io_out <= _io_out_T_6 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 263:10]

  module S_gate : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 101:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 101:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 101:7]
    input io_in_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 102:14]
    input io_in_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 102:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 102:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 102:14]
    output io_out_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 102:14]
    output io_out_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 102:14]

    inst flipD of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 108:21]
    node _flipD_io_in_T = bits(io_in_1, 15, 0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 109:26]
    node _io_out_1_T = bits(io_in_1, 31, 16) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 113:40]
    node _io_out_1_T_1 = cat(flipD.io_out, _io_out_1_T) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 113:29]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 114:32]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_en, io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 114:{32,32,32}]
    io_out_valid <= io_out_valid_r @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 114:16]
    io_out_0 <= io_in_0 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 112:13]
    io_out_1 <= _io_out_1_T_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 113:13]
    flipD.clock <= clock
    flipD.reset <= reset
    flipD.io_in <= _flipD_io_in_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 109:15]
    io_out_valid_r <= _GEN_0
