FIRRTL version 1.2.0
circuit CNOT :
  module CNOT : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 169:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 169:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 169:7]
    input io_in_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    input io_in_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    input io_in_2 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    input io_in_3 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    output io_out_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    output io_out_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    output io_out_2 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]
    output io_out_3 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 170:14]

    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 180:32]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_en, io_out_valid_r) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 180:{32,32,32}]
    io_out_valid <= io_out_valid_r @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 180:16]
    io_out_0 <= io_in_0 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 176:13]
    io_out_1 <= io_in_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 177:13]
    io_out_2 <= io_in_3 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 178:13]
    io_out_3 <= io_in_2 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\GatePool.scala 179:13]
    io_out_valid_r <= _GEN_0
