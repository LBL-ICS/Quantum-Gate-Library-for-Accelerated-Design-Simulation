FIRRTL version 1.2.0
circuit XORshiftNumGenerator :
  module XORshiftNumGenerator : @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 32:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 32:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 32:7]
    input io_in_seed : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 34:14]
    input io_in_feed : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 34:14]
    input io_in_next : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 34:14]
    output io_out_Value : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 34:14]

    reg currentValue : UInt<32>, clock with :
      reset => (UInt<1>("h0"), currentValue) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 40:30]
    reg shiftReg : UInt<32>, clock with :
      reset => (UInt<1>("h0"), shiftReg) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 41:30]
    node _GEN_0 = mux(io_in_feed, io_in_seed, currentValue) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 42:19 43:18 40:30]
    node _shiftReg_T = shl(currentValue, 13) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 46:34]
    node _currentValue_T = xor(currentValue, shiftReg) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 47:34]
    node _shiftReg_T_1 = shr(currentValue, 17) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 48:34]
    node _currentValue_T_1 = xor(currentValue, shiftReg) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 49:34]
    node _shiftReg_T_2 = shl(currentValue, 5) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 50:34]
    node _currentValue_T_2 = xor(currentValue, shiftReg) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 51:34]
    node _GEN_1 = mux(io_in_next, _shiftReg_T_2, shiftReg) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 45:19 50:18 41:30]
    node _GEN_2 = mux(io_in_next, _currentValue_T_2, _GEN_0) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 45:19 51:18]
    node _io_out_Value_T = rem(currentValue, UInt<7>("h40")) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 55:32]
    io_out_Value <= pad(_io_out_Value_T, 32) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 55:16]
    currentValue <= mux(reset, UInt<32>("h0"), _GEN_2) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 40:{30,30}]
    shiftReg <= bits(mux(reset, UInt<32>("h0"), _GEN_1), 31, 0) @[\\src\\main\\scala\\QuantumStateUnit\\OtherComponents\\PsuedoRandomGenerator\\RandomNumberGenerator.scala 41:{30,30}]
