FIRRTL version 1.2.0
circuit CompareWithRandom :
  module full_adder : @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input io_in_a : UInt<8> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_b : UInt<8> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_s : UInt<8> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]

    node _result_T = add(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 87:23]
    node _result_T_1 = add(_result_T, io_in_c) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 87:34]
    node result = bits(_result_T_1, 8, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 86:22 87:12]
    node _io_out_s_T = bits(result, 7, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 88:23]
    node _io_out_c_T = bits(result, 8, 8) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 89:23]
    io_out_s <= _io_out_s_T @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 88:14]
    io_out_c <= _io_out_c_T @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 89:14]

  module multiplier : @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input io_in_a : UInt<24> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]
    input io_in_b : UInt<24> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]
    output io_out_s : UInt<48> @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]

    node _result_T = mul(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 113:23]
    node result = _result_T @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 112:22 113:12]
    io_out_s <= result @[\\src\\main\\scala\\FloatingPointUnit\\New_Binary_Mario\\BinaryDesignsNew.scala 114:14]

  module FP_mult_32_3_v2 : @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 148:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 148:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 148:9]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 151:16]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 151:16]
    input io_in_a : UInt<32> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 151:16]
    input io_in_b : UInt<32> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 151:16]
    output io_out_s : UInt<32> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 151:16]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 151:16]

    inst exp_adder of full_adder @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 226:27]
    inst multiplier of multiplier @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 237:33]
    reg in_a : UInt<32>, clock with :
      reset => (UInt<1>("h0"), in_a) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 175:29]
    node _GEN_0 = mux(io_in_en, io_in_a, in_a) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 175:{29,29,29}]
    reg in_b : UInt<32>, clock with :
      reset => (UInt<1>("h0"), in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 176:29]
    node _GEN_1 = mux(io_in_en, io_in_b, in_b) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 176:{29,29,29}]
    node _sign_wire_0_T = bits(in_a, 31, 31) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 179:25]
    node _sign_wire_1_T = bits(in_b, 31, 31) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 180:25]
    node _T = bits(in_a, 30, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 190:14]
    node _T_1 = gt(_T, UInt<8>("hfe")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 190:33]
    node _T_2 = bits(in_a, 30, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 192:20]
    node _T_3 = lt(_T_2, UInt<8>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 192:36]
    node _exp_wire_0_T = bits(in_a, 30, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 195:26]
    node _GEN_2 = mux(_T_3, UInt<8>("h1"), _exp_wire_0_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 192:46 193:19 195:19]
    node _GEN_3 = mux(_T_1, UInt<8>("hfe"), _GEN_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 190:44 191:19]
    node _T_4 = bits(in_b, 30, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 197:14]
    node _T_5 = gt(_T_4, UInt<8>("hfe")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 197:33]
    node _T_6 = bits(in_b, 30, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 199:20]
    node _T_7 = lt(_T_6, UInt<8>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 199:36]
    node _exp_wire_1_T = bits(in_b, 30, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 202:26]
    node _GEN_4 = mux(_T_7, UInt<8>("h1"), _exp_wire_1_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 199:46 200:19 202:19]
    node _GEN_5 = mux(_T_5, UInt<8>("hfe"), _GEN_4) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 197:44 198:19]
    node _frac_wire_0_T = bits(in_a, 22, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 207:25]
    node _frac_wire_1_T = bits(in_b, 22, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 208:25]
    node frac_wire_0 = _frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 206:25 207:18]
    node _whole_frac_wire_0_T = cat(UInt<1>("h1"), frac_wire_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 212:31]
    node frac_wire_1 = _frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 206:25 208:18]
    node _whole_frac_wire_1_T = cat(UInt<1>("h1"), frac_wire_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 213:31]
    node sign_wire_0 = _sign_wire_0_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 178:25 179:18]
    node sign_wire_1 = _sign_wire_1_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 178:25 180:18]
    node _new_sign_wire_T = xor(sign_wire_0, sign_wire_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 220:33]
    node exp_wire_1 = _GEN_5 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 189:24]
    node _exp_adder_io_in_b_T = sub(exp_wire_1, UInt<8>("h7f")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 228:38]
    node _exp_adder_io_in_b_T_1 = tail(_exp_adder_io_in_b_T, 1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 228:38]
    reg exp_adder_outs_sr : UInt<8>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outs_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 231:42]
    node _GEN_6 = mux(io_in_en, exp_adder.io_out_s, exp_adder_outs_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 231:{42,42,42}]
    reg exp_adder_outc_sr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outc_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 232:42]
    node _GEN_7 = mux(io_in_en, exp_adder.io_out_c, exp_adder_outc_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 232:{42,42,42}]
    reg new_sign_sr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), new_sign_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 233:38]
    node new_sign_wire = _new_sign_wire_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 219:29 220:19]
    node _GEN_8 = mux(io_in_en, new_sign_wire, new_sign_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 233:{38,38,38}]
    reg exp_sr_3_0 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), exp_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 234:33]
    reg exp_sr_3_1 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), exp_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 234:33]
    node exp_wire_0 = _GEN_3 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 189:24]
    node _GEN_9 = mux(io_in_en, exp_wire_0, exp_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 234:{33,33,33}]
    node _GEN_10 = mux(io_in_en, exp_wire_1, exp_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 234:{33,33,33}]
    reg whole_frac_sr_3_0 : UInt<24>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 235:40]
    reg whole_frac_sr_3_1 : UInt<24>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 235:40]
    node whole_frac_wire_0 = _whole_frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 211:31 212:24]
    node _GEN_11 = mux(io_in_en, whole_frac_wire_0, whole_frac_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 235:{40,40,40}]
    node whole_frac_wire_1 = _whole_frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 211:31 213:24]
    node _GEN_12 = mux(io_in_en, whole_frac_wire_1, whole_frac_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 235:{40,40,40}]
    reg frac_mult_out_sr : UInt<48>, clock with :
      reset => (UInt<1>("h0"), frac_mult_out_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 241:41]
    node _GEN_13 = mux(io_in_en, multiplier.io_out_s, frac_mult_out_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 241:{41,41,41}]
    reg exp_adder_outs_sr_2 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 242:44]
    node _GEN_14 = mux(io_in_en, exp_adder_outs_sr, exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 242:{44,44,44}]
    reg exp_adder_outc_sr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outc_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 243:44]
    node _GEN_15 = mux(io_in_en, exp_adder_outc_sr, exp_adder_outc_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 243:{44,44,44}]
    reg new_sign_sr_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), new_sign_sr_3) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 244:38]
    node _GEN_16 = mux(io_in_en, new_sign_sr_2, new_sign_sr_3) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 244:{38,38,38}]
    reg exp_sr_4_0 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), exp_sr_4_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 245:33]
    reg exp_sr_4_1 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), exp_sr_4_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 245:33]
    node _GEN_17 = mux(io_in_en, exp_sr_3_0, exp_sr_4_0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 245:{33,33,33}]
    node _GEN_18 = mux(io_in_en, exp_sr_3_1, exp_sr_4_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 245:{33,33,33}]
    node uo_check = lt(exp_sr_4_1, UInt<8>("h7f")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 248:69]
    node carry_flag = bits(exp_adder_outc_sr_2, 0, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 249:56]
    node msb_check = bits(frac_mult_out_sr, 47, 47) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 252:51]
    node _u_flag_reg_T = eq(carry_flag, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 257:49]
    node _u_flag_reg_T_1 = add(exp_adder_outs_sr_2, msb_check) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 257:74]
    node _u_flag_reg_T_2 = lt(_u_flag_reg_T_1, UInt<8>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 257:95]
    node _u_flag_reg_T_3 = or(_u_flag_reg_T, _u_flag_reg_T_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 257:61]
    node u_flag_reg = mux(uo_check, _u_flag_reg_T_3, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 257:39]
    node _o_flag_reg_T = eq(uo_check, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 258:40]
    node _o_flag_reg_T_1 = add(exp_adder_outs_sr_2, msb_check) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 258:74]
    node _o_flag_reg_T_2 = gt(_o_flag_reg_T_1, UInt<8>("hfe")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 258:95]
    node _o_flag_reg_T_3 = or(carry_flag, _o_flag_reg_T_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 258:61]
    node o_flag_reg = mux(_o_flag_reg_T, _o_flag_reg_T_3, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 258:39]
    node _out_exp_innermux_T = bits(frac_mult_out_sr, 47, 47) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 266:60]
    node _out_exp_innermux_T_1 = add(exp_adder_outs_sr_2, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 266:105]
    node _out_exp_innermux_T_2 = tail(_out_exp_innermux_T_1, 1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 266:105]
    node out_exp_innermux = mux(_out_exp_innermux_T, _out_exp_innermux_T_2, exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 266:45]
    node _out_frac_innermux_T = bits(frac_mult_out_sr, 47, 47) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 267:61]
    node _out_frac_innermux_T_1 = bits(frac_mult_out_sr, 46, 24) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 267:108]
    node _out_frac_innermux_T_2 = bits(frac_mult_out_sr, 45, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 267:162]
    node out_frac_innermux = mux(_out_frac_innermux_T, _out_frac_innermux_T_1, _out_frac_innermux_T_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 267:46]
    node out_exp_innermux_2 = mux(o_flag_reg, UInt<8>("hfe"), out_exp_innermux) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 274:47]
    node out_frac_innermux_2 = mux(o_flag_reg, UInt<23>("h7fffff"), out_frac_innermux) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 275:48]
    node out_exp = mux(u_flag_reg, UInt<8>("h1"), out_exp_innermux_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 280:22]
    node out_frac = mux(u_flag_reg, UInt<23>("h0"), out_frac_innermux_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 281:23]
    node _io_out_s_T = cat(new_sign_sr_3, out_exp) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 282:40]
    node _io_out_s_T_1 = cat(_io_out_s_T, out_frac) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 282:51]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:34]
    node _GEN_19 = mux(io_in_en, io_in_valid, io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:{34,34,34}]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:34]
    node _GEN_20 = mux(io_in_en, io_out_valid_r, io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:{34,34,34}]
    reg io_out_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:34]
    node _GEN_21 = mux(io_in_en, io_out_valid_r_1, io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:{34,34,34}]
    io_out_s <= _io_out_s_T_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 282:14]
    io_out_valid <= io_out_valid_r_2 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 283:18]
    in_a <= _GEN_0
    in_b <= _GEN_1
    exp_adder.clock <= clock
    exp_adder.reset <= reset
    exp_adder.io_in_a <= exp_wire_0 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 227:23]
    exp_adder.io_in_b <= _exp_adder_io_in_b_T_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 228:23]
    exp_adder.io_in_c <= UInt<1>("h0") @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 229:23]
    exp_adder_outs_sr <= _GEN_6
    exp_adder_outc_sr <= _GEN_7
    new_sign_sr_2 <= _GEN_8
    exp_sr_3_0 <= _GEN_9
    exp_sr_3_1 <= _GEN_10
    whole_frac_sr_3_0 <= _GEN_11
    whole_frac_sr_3_1 <= _GEN_12
    multiplier.clock <= clock
    multiplier.reset <= reset
    multiplier.io_in_a <= whole_frac_sr_3_0 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 238:26]
    multiplier.io_in_b <= whole_frac_sr_3_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 239:26]
    frac_mult_out_sr <= _GEN_13
    exp_adder_outs_sr_2 <= _GEN_14
    exp_adder_outc_sr_2 <= _GEN_15
    new_sign_sr_3 <= _GEN_16
    exp_sr_4_0 <= _GEN_17
    exp_sr_4_1 <= _GEN_18
    io_out_valid_r <= _GEN_19
    io_out_valid_r_1 <= _GEN_20
    io_out_valid_r_2 <= _GEN_21

  module FloatTOFixed : @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 701:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 701:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 701:9]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 711:16]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 711:16]
    input io_in_float : UInt<32> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 711:16]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 711:16]
    output io_out_fixed : SInt<9> @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 711:16]

    node sign = bits(io_in_float, 31, 31) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 718:27]
    node exp = bits(io_in_float, 30, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 719:26]
    node _frac_T = bits(io_in_float, 22, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 720:39]
    node frac = cat(UInt<1>("h1"), _frac_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 720:25]
    node _norm_frac_T = not(frac) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 722:32]
    node _norm_frac_T_1 = add(_norm_frac_T, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 722:46]
    node _norm_frac_T_2 = tail(_norm_frac_T_1, 1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 722:46]
    node norm_frac = mux(sign, _norm_frac_T_2, frac) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 722:24]
    node _result_wire_T = bits(frac, 23, 23) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 725:127]
    node _result_wire_T_1 = cat(UInt<7>("h0"), _result_wire_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 725:120]
    node _exp_diff_T = sub(exp, UInt<8>("h7f")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 726:24]
    node exp_diff = asUInt(_exp_diff_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 726:24]
    node _twoscomped_T = not(exp_diff) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 727:23]
    node _twoscomped_T_1 = add(_twoscomped_T, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 727:41]
    node twoscomped = tail(_twoscomped_T_1, 1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 727:41]
    node _result_reg_T = bits(exp_diff, 8, 8) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 728:48]
    node result_wire = _result_wire_T_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 724:27 725:17]
    node _result_reg_T_1 = dshr(result_wire, twoscomped) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 728:79]
    node _result_reg_T_2 = dshl(result_wire, exp_diff) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 728:108]
    node _result_reg_T_3 = mux(_result_reg_T, _result_reg_T_1, _result_reg_T_2) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 728:39]
    node _result_reg_T_4 = bits(_result_reg_T_3, 7, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 728:122]
    reg result_reg : UInt<8>, clock with :
      reset => (UInt<1>("h0"), result_reg) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 728:35]
    node _GEN_0 = mux(io_in_en, _result_reg_T_4, result_reg) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 728:{35,35,35}]
    reg sign_sr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), sign_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 729:32]
    node _GEN_1 = mux(io_in_en, sign, sign_sr) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 729:{32,32,32}]
    node _output_T = not(result_reg) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 730:32]
    node _output_T_1 = add(_output_T, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 730:52]
    node _output_T_2 = tail(_output_T_1, 1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 730:52]
    node _output_T_3 = mux(sign_sr, _output_T_2, result_reg) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 730:21]
    node output = bits(_output_T_3, 7, 0) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 730:77]
    node _io_out_fixed_T = cat(sign_sr, output) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 731:44]
    node _io_out_fixed_T_1 = asSInt(_io_out_fixed_T) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 731:55]
    reg io_out_fixed_r : SInt<9>, clock with :
      reset => (UInt<1>("h0"), io_out_fixed_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 731:34]
    node _GEN_2 = mux(io_in_en, _io_out_fixed_T_1, io_out_fixed_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 731:{34,34,34}]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 732:34]
    node _GEN_3 = mux(io_in_en, io_in_valid, io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 732:{34,34,34}]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 732:34]
    node _GEN_4 = mux(io_in_en, io_out_valid_r, io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 732:{34,34,34}]
    io_out_valid <= io_out_valid_r_1 @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 732:18]
    io_out_fixed <= io_out_fixed_r @[\\src\\main\\scala\\FloatingPointUnit\\New_FPU_Mario\\FPUnits.scala 731:18]
    result_reg <= _GEN_0
    sign_sr <= _GEN_1
    io_out_fixed_r <= _GEN_2
    io_out_valid_r <= _GEN_3
    io_out_valid_r_1 <= _GEN_4

  module CompareWithRandom : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 152:9]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 152:9]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 152:9]
    input io_in_probability_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 153:16]
    input io_in_probability_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 153:16]
    input io_in_random : SInt<8> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 153:16]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 153:16]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 153:16]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 153:16]
    output io_out_value : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 153:16]

    inst multiplier0 of FP_mult_32_3_v2 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 168:29]
    inst multiplier1 of FP_mult_32_3_v2 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 169:29]
    inst toFixed0 of FloatTOFixed @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 170:26]
    inst toFixed1 of FloatTOFixed @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 171:26]
    reg determinedValue : UInt<1>, clock with :
      reset => (UInt<1>("h0"), determinedValue) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 191:34]
    node _io_out_valid_T = and(toFixed0.io_out_valid, toFixed1.io_out_valid) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 193:43]
    node _T = geq(io_in_random, toFixed0.io_out_fixed) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 195:24]
    node _T_1 = eq(io_in_en, UInt<1>("h0")) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 195:52]
    node _T_2 = and(_T, _T_1) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 195:50]
    node _GEN_0 = mux(_T_2, UInt<1>("h0"), determinedValue) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 195:63 196:23 191:34]
    node _T_3 = lt(io_in_random, toFixed1.io_out_fixed) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 198:24]
    node _T_4 = eq(io_in_en, UInt<1>("h0")) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 198:51]
    node _T_5 = and(_T_3, _T_4) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 198:49]
    node _GEN_1 = mux(_T_5, UInt<1>("h1"), _GEN_0) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 198:62 199:23]
    io_out_valid <= _io_out_valid_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 193:18]
    io_out_value <= determinedValue @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 192:18]
    multiplier0.clock <= clock
    multiplier0.reset <= reset
    multiplier0.io_in_en <= UInt<1>("h1") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 178:29]
    multiplier0.io_in_valid <= io_in_valid @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 180:29]
    multiplier0.io_in_a <= io_in_probability_0 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 174:29]
    multiplier0.io_in_b <= UInt<32>("h42c80000") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 175:29]
    multiplier1.clock <= clock
    multiplier1.reset <= reset
    multiplier1.io_in_en <= UInt<1>("h1") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 179:29]
    multiplier1.io_in_valid <= io_in_valid @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 181:29]
    multiplier1.io_in_a <= io_in_probability_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 176:29]
    multiplier1.io_in_b <= UInt<32>("h42c80000") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 177:29]
    toFixed0.clock <= clock
    toFixed0.reset <= reset
    toFixed0.io_in_en <= UInt<1>("h1") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 184:29]
    toFixed0.io_in_valid <= multiplier0.io_out_valid @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 185:29]
    toFixed0.io_in_float <= multiplier0.io_out_s @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 183:29]
    toFixed1.clock <= clock
    toFixed1.reset <= reset
    toFixed1.io_in_en <= UInt<1>("h1") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 187:29]
    toFixed1.io_in_valid <= multiplier1.io_out_valid @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 188:29]
    toFixed1.io_in_float <= multiplier1.io_out_s @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 186:29]
    determinedValue <= mux(reset, UInt<1>("h0"), _GEN_1) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\Measurement.scala 191:{34,34}]
