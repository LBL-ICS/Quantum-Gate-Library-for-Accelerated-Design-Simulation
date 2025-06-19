FIRRTL version 1.2.0
circuit Pauli_X :
  module Pauli_X : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 39:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 39:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 39:7]
    input io_in_0 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 40:14]
    input io_in_1 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 40:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 40:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 40:14]
    output io_out_0 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 40:14]
    output io_out_1 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 40:14]

    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 46:32]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_en, io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 46:{32,32,32}]
    io_out_valid <= io_out_valid_r @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 46:16]
    io_out_0 <= io_in_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 47:16]
    io_out_1 <= io_in_0 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 48:16]
    io_out_valid_r <= _GEN_0
