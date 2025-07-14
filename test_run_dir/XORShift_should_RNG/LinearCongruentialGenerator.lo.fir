FIRRTL version 1.2.0
circuit LinearCongruentialGenerator :
  module LinearCongruentialGenerator : @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 13:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 13:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 13:7]
    input io_in_seed : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 14:14]
    input io_in_feed : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 14:14]
    input io_in_next : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 14:14]
    output io_out_Value : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 14:14]

    reg value : UInt<32>, clock with :
      reset => (UInt<1>("h0"), value) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 21:23]
    reg seedUsed : UInt<1>, clock with :
      reset => (UInt<1>("h0"), seedUsed) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 25:26]
    node _value_T = mul(value, UInt<3>("h5")) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 27:21]
    node _value_T_1 = add(_value_T, UInt<1>("h1")) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 27:27]
    node _value_T_2 = tail(_value_T_1, 1) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 27:27]
    node _value_T_3 = rem(_value_T_2, UInt<7>("h40")) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 27:34]
    node _GEN_0 = mux(io_in_next, _value_T_3, value) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 26:19 27:11 21:23]
    node _GEN_1 = mux(io_in_feed, io_in_seed, _GEN_0) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 29:19 30:11]
    io_out_Value <= value @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 22:17]
    value <= mux(reset, UInt<32>("h0"), _GEN_1) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 21:{23,23}]
    seedUsed <= mux(reset, UInt<1>("h0"), seedUsed) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 25:{26,26,26}]
