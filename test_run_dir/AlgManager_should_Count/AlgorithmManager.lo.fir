FIRRTL version 1.2.0
circuit AlgorithmManager :
  module AlgorithmManager : @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 15:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 15:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 15:7]
    input io_in_QSV_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_QSV_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_QSV_2 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_QSV_3 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Permutaiton_Sel_0 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Permutaiton_Sel_1 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Permutaiton_Sel_2 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Permutaiton_Sel_3 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Permutaiton_Sel_4 : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Gate_Sel_0 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Gate_Sel_1 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Gate_Sel_2 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Gate_Sel_3 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_Gate_Sel_4 : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_en_newData : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    input io_in_en_next : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_en_QSR : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_en_QGP : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_newState : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_endofAlg : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_sel_gate : UInt<3> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_sel_permutation : UInt<2> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_QSV_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_QSV_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_QSV_2 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]
    output io_out_QSV_3 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 16:14]

    reg stop : UInt<1>, clock with :
      reset => (UInt<1>("h0"), stop) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 34:29]
    reg next : UInt<1>, clock with :
      reset => (UInt<1>("h0"), next) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 35:29]
    node _next_T = mux(stop, UInt<1>("h0"), io_in_en_next) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 36:25]
    reg en_new_values : UInt<1>, clock with :
      reset => (UInt<1>("h0"), en_new_values) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 40:30]
    node _en_new_values_T = mux(stop, io_in_en_newData, UInt<1>("h0")) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 41:27]
    reg io_out_en_QSR_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QSR_r) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:53]
    node _GEN_0 = mux(UInt<1>("h1"), next, io_out_en_QSR_r) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:{53,53,53}]
    reg io_out_en_QSR_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QSR_r_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:53]
    node _GEN_1 = mux(UInt<1>("h1"), io_out_en_QSR_r, io_out_en_QSR_r_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:{53,53,53}]
    reg io_out_en_QSR_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QSR_r_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:53]
    node _GEN_2 = mux(UInt<1>("h1"), io_out_en_QSR_r_1, io_out_en_QSR_r_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:{53,53,53}]
    node _io_out_en_QSR_T = or(en_new_values, io_out_en_QSR_r_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:38]
    node _io_out_en_QGP_T = or(next, en_new_values) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:43]
    reg io_out_en_QGP_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QGP_r) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:37]
    node _GEN_3 = mux(UInt<1>("h1"), _io_out_en_QGP_T, io_out_en_QGP_r) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:{37,37,37}]
    reg io_out_en_QGP_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QGP_r_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:37]
    node _GEN_4 = mux(UInt<1>("h1"), io_out_en_QGP_r, io_out_en_QGP_r_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:{37,37,37}]
    reg io_out_en_QGP_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QGP_r_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:37]
    node _GEN_5 = mux(UInt<1>("h1"), io_out_en_QGP_r_1, io_out_en_QGP_r_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:{37,37,37}]
    reg io_out_en_QGP_r_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QGP_r_3) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:37]
    node _GEN_6 = mux(UInt<1>("h1"), io_out_en_QGP_r_2, io_out_en_QGP_r_3) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:{37,37,37}]
    reg io_out_en_QGP_r_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QGP_r_4) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:37]
    node _GEN_7 = mux(UInt<1>("h1"), io_out_en_QGP_r_3, io_out_en_QGP_r_4) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:{37,37,37}]
    reg io_out_en_QGP_r_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_en_QGP_r_5) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:37]
    node _GEN_8 = mux(UInt<1>("h1"), io_out_en_QGP_r_4, io_out_en_QGP_r_5) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:{37,37,37}]
    reg cntEventsReg : UInt<3>, clock with :
      reset => (UInt<1>("h0"), cntEventsReg) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 50:30]
    node _cntEventsReg_T = add(cntEventsReg, UInt<1>("h1")) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 52:34]
    node _cntEventsReg_T_1 = tail(_cntEventsReg_T, 1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 52:34]
    node _GEN_9 = mux(next, _cntEventsReg_T_1, cntEventsReg) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 51:14 52:18 50:30]
    node _GEN_10 = mux(en_new_values, UInt<1>("h0"), _GEN_9) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 56:22 57:18]
    node _GEN_11 = mux(en_new_values, UInt<1>("h0"), stop) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 56:22 58:18 34:29]
    reg quantumGate_0 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), quantumGate_0) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 62:30]
    reg quantumGate_1 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), quantumGate_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 62:30]
    reg quantumGate_2 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), quantumGate_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 62:30]
    reg quantumGate_3 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), quantumGate_3) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 62:30]
    reg quantumGate_4 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), quantumGate_4) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 62:30]
    reg selPermutation_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), selPermutation_0) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 63:30]
    reg selPermutation_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), selPermutation_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 63:30]
    reg selPermutation_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), selPermutation_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 63:30]
    reg selPermutation_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), selPermutation_3) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 63:30]
    reg selPermutation_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), selPermutation_4) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 63:30]
    node _quantumGate_0_T = mux(en_new_values, io_in_Gate_Sel_0, quantumGate_0) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:29]
    node _selPermutation_0_T = mux(en_new_values, io_in_Permutaiton_Sel_0, selPermutation_0) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:29]
    node _quantumGate_1_T = mux(en_new_values, io_in_Gate_Sel_1, quantumGate_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:29]
    node _selPermutation_1_T = mux(en_new_values, io_in_Permutaiton_Sel_1, selPermutation_1) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:29]
    node _quantumGate_2_T = mux(en_new_values, io_in_Gate_Sel_2, quantumGate_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:29]
    node _selPermutation_2_T = mux(en_new_values, io_in_Permutaiton_Sel_2, selPermutation_2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:29]
    node _quantumGate_3_T = mux(en_new_values, io_in_Gate_Sel_3, quantumGate_3) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:29]
    node _selPermutation_3_T = mux(en_new_values, io_in_Permutaiton_Sel_3, selPermutation_3) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:29]
    node _quantumGate_4_T = mux(en_new_values, io_in_Gate_Sel_4, quantumGate_4) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:29]
    node _selPermutation_4_T = mux(en_new_values, io_in_Permutaiton_Sel_4, selPermutation_4) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:29]
    node _GEN_12 = validif(eq(UInt<1>("h0"), cntEventsReg), quantumGate_0) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 70:{25,25}]
    node _GEN_13 = mux(eq(UInt<1>("h1"), cntEventsReg), quantumGate_1, _GEN_12) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 70:{25,25}]
    node _GEN_14 = mux(eq(UInt<2>("h2"), cntEventsReg), quantumGate_2, _GEN_13) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 70:{25,25}]
    node _GEN_15 = mux(eq(UInt<2>("h3"), cntEventsReg), quantumGate_3, _GEN_14) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 70:{25,25}]
    node _GEN_16 = mux(eq(UInt<3>("h4"), cntEventsReg), quantumGate_4, _GEN_15) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 70:{25,25}]
    node _GEN_17 = validif(eq(UInt<1>("h0"), cntEventsReg), selPermutation_0) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 71:{25,25}]
    node _GEN_18 = mux(eq(UInt<1>("h1"), cntEventsReg), selPermutation_1, _GEN_17) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 71:{25,25}]
    node _GEN_19 = mux(eq(UInt<2>("h2"), cntEventsReg), selPermutation_2, _GEN_18) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 71:{25,25}]
    node _GEN_20 = mux(eq(UInt<2>("h3"), cntEventsReg), selPermutation_3, _GEN_19) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 71:{25,25}]
    node _GEN_21 = mux(eq(UInt<3>("h4"), cntEventsReg), selPermutation_4, _GEN_20) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 71:{25,25}]
    node _T = andr(io_out_sel_gate) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 75:24]
    node _GEN_22 = mux(_T, UInt<1>("h1"), _GEN_11) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 75:29 76:11]
    node _quantumGate_cntEventsReg = _GEN_16 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 70:25]
    node _selPermutation_cntEventsReg = _GEN_21 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 71:25]
    io_out_en_QSR <= _io_out_en_QSR_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 46:21]
    io_out_en_QGP <= io_out_en_QGP_r_5 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 47:21]
    io_out_newState <= en_new_values @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 45:21]
    io_out_endofAlg <= stop @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 37:19]
    io_out_sel_gate <= _quantumGate_cntEventsReg @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 70:25]
    io_out_sel_permutation <= pad(_selPermutation_cntEventsReg, 2) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 71:25]
    io_out_QSV_0 <= io_in_QSV_0 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 44:21]
    io_out_QSV_1 <= io_in_QSV_1 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 44:21]
    io_out_QSV_2 <= io_in_QSV_2 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 44:21]
    io_out_QSV_3 <= io_in_QSV_3 @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 44:21]
    stop <= mux(reset, UInt<1>("h1"), _GEN_22) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 34:{29,29}]
    next <= mux(reset, UInt<1>("h0"), _next_T) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 35:{29,29} 36:19]
    en_new_values <= mux(reset, UInt<1>("h0"), _en_new_values_T) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 40:{30,30} 41:21]
    io_out_en_QSR_r <= _GEN_0
    io_out_en_QSR_r_1 <= _GEN_1
    io_out_en_QSR_r_2 <= _GEN_2
    io_out_en_QGP_r <= _GEN_3
    io_out_en_QGP_r_1 <= _GEN_4
    io_out_en_QGP_r_2 <= _GEN_5
    io_out_en_QGP_r_3 <= _GEN_6
    io_out_en_QGP_r_4 <= _GEN_7
    io_out_en_QGP_r_5 <= _GEN_8
    cntEventsReg <= mux(reset, UInt<3>("h0"), _GEN_10) @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 50:{30,30}]
    quantumGate_0 <= _quantumGate_0_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:23]
    quantumGate_1 <= _quantumGate_1_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:23]
    quantumGate_2 <= _quantumGate_2_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:23]
    quantumGate_3 <= _quantumGate_3_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:23]
    quantumGate_4 <= _quantumGate_4_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 66:23]
    selPermutation_0 <= _selPermutation_0_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:23]
    selPermutation_1 <= _selPermutation_1_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:23]
    selPermutation_2 <= _selPermutation_2_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:23]
    selPermutation_3 <= _selPermutation_3_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:23]
    selPermutation_4 <= _selPermutation_4_T @[\\src\\main\\scala\\QuantumStateUnit\\QSU_Architecture\\AlgorithmManager.scala 67:23]
