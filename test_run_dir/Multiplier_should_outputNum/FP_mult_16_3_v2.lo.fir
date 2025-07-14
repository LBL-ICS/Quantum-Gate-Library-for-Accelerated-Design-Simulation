FIRRTL version 1.2.0
circuit FP_mult_16_3_v2 :
  module full_adder : @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input io_in_a : UInt<5> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_b : UInt<5> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_s : UInt<5> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]

    node _result_T = add(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 87:23]
    node _result_T_1 = add(_result_T, io_in_c) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 87:34]
    node result = bits(_result_T_1, 5, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 86:22 87:12]
    node _io_out_s_T = bits(result, 4, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 88:23]
    node _io_out_c_T = bits(result, 5, 5) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 89:23]
    io_out_s <= _io_out_s_T @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 88:14]
    io_out_c <= _io_out_c_T @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 89:14]

  module multiplier : @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input io_in_a : UInt<11> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]
    input io_in_b : UInt<11> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]
    output io_out_s : UInt<22> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]

    node _result_T = mul(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 113:23]
    node result = _result_T @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 112:22 113:12]
    io_out_s <= result @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 114:14]

  module FP_mult_16_3_v2 : @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 150:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 150:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 150:9]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 153:16]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 153:16]
    input io_in_a : UInt<16> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 153:16]
    input io_in_b : UInt<16> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 153:16]
    output io_out_s : UInt<16> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 153:16]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 153:16]

    inst exp_adder of full_adder @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 228:27]
    inst multiplier of multiplier @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 239:33]
    reg in_a : UInt<16>, clock with :
      reset => (UInt<1>("h0"), in_a) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 177:29]
    node _GEN_0 = mux(io_in_en, io_in_a, in_a) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 177:{29,29,29}]
    reg in_b : UInt<16>, clock with :
      reset => (UInt<1>("h0"), in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 178:29]
    node _GEN_1 = mux(io_in_en, io_in_b, in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 178:{29,29,29}]
    node _sign_wire_0_T = bits(in_a, 15, 15) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 181:25]
    node _sign_wire_1_T = bits(in_b, 15, 15) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 182:25]
    node _T = bits(in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 192:14]
    node _T_1 = gt(_T, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 192:33]
    node _T_2 = bits(in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 194:20]
    node _T_3 = lt(_T_2, UInt<5>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 194:36]
    node _exp_wire_0_T = bits(in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 197:26]
    node _GEN_2 = mux(_T_3, UInt<5>("h1"), _exp_wire_0_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 194:46 195:19 197:19]
    node _GEN_3 = mux(_T_1, UInt<5>("h1e"), _GEN_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 192:44 193:19]
    node _T_4 = bits(in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 199:14]
    node _T_5 = gt(_T_4, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 199:33]
    node _T_6 = bits(in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 201:20]
    node _T_7 = lt(_T_6, UInt<5>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 201:36]
    node _exp_wire_1_T = bits(in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 204:26]
    node _GEN_4 = mux(_T_7, UInt<5>("h1"), _exp_wire_1_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 201:46 202:19 204:19]
    node _GEN_5 = mux(_T_5, UInt<5>("h1e"), _GEN_4) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 199:44 200:19]
    node _frac_wire_0_T = bits(in_a, 9, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 209:25]
    node _frac_wire_1_T = bits(in_b, 9, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 210:25]
    node frac_wire_0 = _frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 208:25 209:18]
    node _whole_frac_wire_0_T = cat(UInt<1>("h1"), frac_wire_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 214:31]
    node frac_wire_1 = _frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 208:25 210:18]
    node _whole_frac_wire_1_T = cat(UInt<1>("h1"), frac_wire_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 215:31]
    node sign_wire_0 = _sign_wire_0_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 180:25 181:18]
    node sign_wire_1 = _sign_wire_1_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 180:25 182:18]
    node _new_sign_wire_T = xor(sign_wire_0, sign_wire_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 222:33]
    node exp_wire_1 = _GEN_5 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 191:24]
    node _exp_adder_io_in_b_T = sub(exp_wire_1, UInt<5>("hf")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 230:38]
    node _exp_adder_io_in_b_T_1 = tail(_exp_adder_io_in_b_T, 1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 230:38]
    reg exp_adder_outs_sr : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outs_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 233:42]
    node _GEN_6 = mux(io_in_en, exp_adder.io_out_s, exp_adder_outs_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 233:{42,42,42}]
    reg exp_adder_outc_sr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outc_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 234:42]
    node _GEN_7 = mux(io_in_en, exp_adder.io_out_c, exp_adder_outc_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 234:{42,42,42}]
    reg new_sign_sr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), new_sign_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 235:38]
    node new_sign_wire = _new_sign_wire_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 221:29 222:19]
    node _GEN_8 = mux(io_in_en, new_sign_wire, new_sign_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 235:{38,38,38}]
    reg exp_sr_3_0 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 236:33]
    reg exp_sr_3_1 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 236:33]
    node exp_wire_0 = _GEN_3 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 191:24]
    node _GEN_9 = mux(io_in_en, exp_wire_0, exp_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 236:{33,33,33}]
    node _GEN_10 = mux(io_in_en, exp_wire_1, exp_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 236:{33,33,33}]
    reg whole_frac_sr_3_0 : UInt<11>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 237:40]
    reg whole_frac_sr_3_1 : UInt<11>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 237:40]
    node whole_frac_wire_0 = _whole_frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 213:31 214:24]
    node _GEN_11 = mux(io_in_en, whole_frac_wire_0, whole_frac_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 237:{40,40,40}]
    node whole_frac_wire_1 = _whole_frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 213:31 215:24]
    node _GEN_12 = mux(io_in_en, whole_frac_wire_1, whole_frac_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 237:{40,40,40}]
    reg frac_mult_out_sr : UInt<22>, clock with :
      reset => (UInt<1>("h0"), frac_mult_out_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 243:41]
    node _GEN_13 = mux(io_in_en, multiplier.io_out_s, frac_mult_out_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 243:{41,41,41}]
    reg exp_adder_outs_sr_2 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 244:44]
    node _GEN_14 = mux(io_in_en, exp_adder_outs_sr, exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 244:{44,44,44}]
    reg exp_adder_outc_sr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outc_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 245:44]
    node _GEN_15 = mux(io_in_en, exp_adder_outc_sr, exp_adder_outc_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 245:{44,44,44}]
    reg new_sign_sr_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), new_sign_sr_3) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 246:38]
    node _GEN_16 = mux(io_in_en, new_sign_sr_2, new_sign_sr_3) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 246:{38,38,38}]
    reg exp_sr_4_0 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_4_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 247:33]
    reg exp_sr_4_1 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_4_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 247:33]
    node _GEN_17 = mux(io_in_en, exp_sr_3_0, exp_sr_4_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 247:{33,33,33}]
    node _GEN_18 = mux(io_in_en, exp_sr_3_1, exp_sr_4_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 247:{33,33,33}]
    node uo_check = lt(exp_sr_4_1, UInt<5>("hf")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 250:69]
    node carry_flag = bits(exp_adder_outc_sr_2, 0, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 251:56]
    node msb_check = bits(frac_mult_out_sr, 21, 21) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 254:51]
    node _u_flag_reg_T = eq(carry_flag, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 259:49]
    node _u_flag_reg_T_1 = add(exp_adder_outs_sr_2, msb_check) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 259:74]
    node _u_flag_reg_T_2 = lt(_u_flag_reg_T_1, UInt<5>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 259:95]
    node _u_flag_reg_T_3 = or(_u_flag_reg_T, _u_flag_reg_T_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 259:61]
    node u_flag_reg = mux(uo_check, _u_flag_reg_T_3, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 259:39]
    node _o_flag_reg_T = eq(uo_check, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 260:40]
    node _o_flag_reg_T_1 = add(exp_adder_outs_sr_2, msb_check) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 260:74]
    node _o_flag_reg_T_2 = gt(_o_flag_reg_T_1, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 260:95]
    node _o_flag_reg_T_3 = or(carry_flag, _o_flag_reg_T_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 260:61]
    node o_flag_reg = mux(_o_flag_reg_T, _o_flag_reg_T_3, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 260:39]
    node _out_exp_innermux_T = bits(frac_mult_out_sr, 21, 21) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 268:60]
    node _out_exp_innermux_T_1 = add(exp_adder_outs_sr_2, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 268:105]
    node _out_exp_innermux_T_2 = tail(_out_exp_innermux_T_1, 1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 268:105]
    node out_exp_innermux = mux(_out_exp_innermux_T, _out_exp_innermux_T_2, exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 268:45]
    node _out_frac_innermux_T = bits(frac_mult_out_sr, 21, 21) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 269:61]
    node _out_frac_innermux_T_1 = bits(frac_mult_out_sr, 20, 11) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 269:108]
    node _out_frac_innermux_T_2 = bits(frac_mult_out_sr, 19, 10) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 269:162]
    node out_frac_innermux = mux(_out_frac_innermux_T, _out_frac_innermux_T_1, _out_frac_innermux_T_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 269:46]
    node out_exp_innermux_2 = mux(o_flag_reg, UInt<5>("h1e"), out_exp_innermux) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 276:47]
    node out_frac_innermux_2 = mux(o_flag_reg, UInt<10>("h3ff"), out_frac_innermux) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 277:48]
    node out_exp = mux(u_flag_reg, UInt<5>("h1"), out_exp_innermux_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 282:22]
    node out_frac = mux(u_flag_reg, UInt<10>("h0"), out_frac_innermux_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:23]
    node _check_if_0_T = bits(io_in_a, 14, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 285:30]
    node _check_if_0_T_1 = orr(_check_if_0_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 285:40]
    node _check_if_0_T_2 = eq(_check_if_0_T_1, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 285:22]
    node _check_if_0_T_3 = bits(io_in_b, 14, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 285:55]
    node _check_if_0_T_4 = orr(_check_if_0_T_3) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 285:65]
    node _check_if_0_T_5 = eq(_check_if_0_T_4, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 285:47]
    node check_if_0 = or(_check_if_0_T_2, _check_if_0_T_5) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 285:44]
    node _io_out_s_T = cat(new_sign_sr_3, out_exp) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 289:45]
    node _io_out_s_T_1 = cat(_io_out_s_T, out_frac) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 289:56]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 290:35]
    node _GEN_19 = mux(io_in_en, io_in_valid, io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 290:{35,35,35}]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 290:35]
    node _GEN_20 = mux(io_in_en, io_out_valid_r, io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 290:{35,35,35}]
    reg io_out_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 290:35]
    node _GEN_21 = mux(io_in_en, io_out_valid_r_1, io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 290:{35,35,35}]
    io_out_s <= _io_out_s_T_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 289:19]
    io_out_valid <= io_out_valid_r_2 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 290:19]
    in_a <= _GEN_0
    in_b <= _GEN_1
    exp_adder.clock <= clock
    exp_adder.reset <= reset
    exp_adder.io_in_a <= exp_wire_0 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 229:23]
    exp_adder.io_in_b <= _exp_adder_io_in_b_T_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 230:23]
    exp_adder.io_in_c <= UInt<1>("h0") @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 231:23]
    exp_adder_outs_sr <= _GEN_6
    exp_adder_outc_sr <= _GEN_7
    new_sign_sr_2 <= _GEN_8
    exp_sr_3_0 <= _GEN_9
    exp_sr_3_1 <= _GEN_10
    whole_frac_sr_3_0 <= _GEN_11
    whole_frac_sr_3_1 <= _GEN_12
    multiplier.clock <= clock
    multiplier.reset <= reset
    multiplier.io_in_a <= whole_frac_sr_3_0 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 240:26]
    multiplier.io_in_b <= whole_frac_sr_3_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 241:26]
    frac_mult_out_sr <= _GEN_13
    exp_adder_outs_sr_2 <= _GEN_14
    exp_adder_outc_sr_2 <= _GEN_15
    new_sign_sr_3 <= _GEN_16
    exp_sr_4_0 <= _GEN_17
    exp_sr_4_1 <= _GEN_18
    io_out_valid_r <= _GEN_19
    io_out_valid_r_1 <= _GEN_20
    io_out_valid_r_2 <= _GEN_21
