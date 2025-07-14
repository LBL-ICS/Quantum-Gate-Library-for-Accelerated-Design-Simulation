FIRRTL version 1.2.0
circuit Hadamard :
  module full_adder : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input io_in_a : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_b : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_s : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]

    node _result_T = add(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 87:23]
    node _result_T_1 = add(_result_T, io_in_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 87:34]
    node result = bits(_result_T_1, 5, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 86:22 87:12]
    node _io_out_s_T = bits(result, 4, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 88:23]
    node _io_out_c_T = bits(result, 5, 5) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 89:23]
    io_out_s <= _io_out_s_T @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 88:14]
    io_out_c <= _io_out_c_T @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 89:14]

  module multiplier : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 106:9]
    input io_in_a : UInt<11> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]
    input io_in_b : UInt<11> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]
    output io_out_s : UInt<22> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 107:16]

    node _result_T = mul(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 113:23]
    node result = _result_T @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 112:22 113:12]
    io_out_s <= result @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 114:14]

  module FP_mult_16_3_v2 : @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 148:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 148:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 148:9]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 151:16]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 151:16]
    input io_in_a : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 151:16]
    input io_in_b : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 151:16]
    output io_out_s : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 151:16]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 151:16]

    inst exp_adder of full_adder @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 232:27]
    inst multiplier of multiplier @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 243:33]
    reg in_a : UInt<16>, clock with :
      reset => (UInt<1>("h0"), in_a) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 175:29]
    node _GEN_0 = mux(io_in_en, io_in_a, in_a) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 175:{29,29,29}]
    reg in_b : UInt<16>, clock with :
      reset => (UInt<1>("h0"), in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 176:29]
    node _GEN_1 = mux(io_in_en, io_in_b, in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 176:{29,29,29}]
    node _sign_wire_0_T = bits(in_a, 15, 15) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 179:25]
    node _sign_wire_1_T = bits(in_b, 15, 15) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 180:25]
    node _T = bits(in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 196:14]
    node _T_1 = gt(_T, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 196:33]
    node _T_2 = bits(in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 198:20]
    node _T_3 = lt(_T_2, UInt<5>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 198:36]
    node _exp_wire_0_T = bits(in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 201:26]
    node _GEN_2 = mux(_T_3, UInt<5>("h0"), _exp_wire_0_T) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 198:46 199:19 201:19]
    node _GEN_3 = mux(_T_1, UInt<5>("h1e"), _GEN_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 196:44 197:19]
    node _T_4 = bits(in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 203:14]
    node _T_5 = gt(_T_4, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 203:33]
    node _T_6 = bits(in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 205:20]
    node _T_7 = lt(_T_6, UInt<5>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 205:36]
    node _exp_wire_1_T = bits(in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 208:26]
    node _GEN_4 = mux(_T_7, UInt<5>("h0"), _exp_wire_1_T) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 205:46 206:19 208:19]
    node _GEN_5 = mux(_T_5, UInt<5>("h1e"), _GEN_4) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 203:44 204:19]
    node _frac_wire_0_T = bits(in_a, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 213:25]
    node _frac_wire_1_T = bits(in_b, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 214:25]
    node frac_wire_0 = _frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 212:25 213:18]
    node _whole_frac_wire_0_T = cat(UInt<1>("h1"), frac_wire_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 218:31]
    node frac_wire_1 = _frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 212:25 214:18]
    node _whole_frac_wire_1_T = cat(UInt<1>("h1"), frac_wire_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 219:31]
    node sign_wire_0 = _sign_wire_0_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 178:25 179:18]
    node sign_wire_1 = _sign_wire_1_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 178:25 180:18]
    node _new_sign_wire_T = xor(sign_wire_0, sign_wire_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 226:33]
    node exp_wire_1 = _GEN_5 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 195:24]
    node _exp_adder_io_in_b_T = sub(exp_wire_1, UInt<5>("hf")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 234:38]
    node _exp_adder_io_in_b_T_1 = tail(_exp_adder_io_in_b_T, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 234:38]
    reg exp_adder_outs_sr : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outs_sr) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 237:42]
    node _GEN_6 = mux(io_in_en, exp_adder.io_out_s, exp_adder_outs_sr) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 237:{42,42,42}]
    reg exp_adder_outc_sr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outc_sr) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 238:42]
    node _GEN_7 = mux(io_in_en, exp_adder.io_out_c, exp_adder_outc_sr) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 238:{42,42,42}]
    reg new_sign_sr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), new_sign_sr_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 239:38]
    node new_sign_wire = _new_sign_wire_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 225:29 226:19]
    node _GEN_8 = mux(io_in_en, new_sign_wire, new_sign_sr_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 239:{38,38,38}]
    reg exp_sr_3_0 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 240:33]
    reg exp_sr_3_1 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 240:33]
    node exp_wire_0 = _GEN_3 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 195:24]
    node _GEN_9 = mux(io_in_en, exp_wire_0, exp_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 240:{33,33,33}]
    node _GEN_10 = mux(io_in_en, exp_wire_1, exp_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 240:{33,33,33}]
    reg whole_frac_sr_3_0 : UInt<11>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 241:40]
    reg whole_frac_sr_3_1 : UInt<11>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 241:40]
    node whole_frac_wire_0 = _whole_frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 217:31 218:24]
    node _GEN_11 = mux(io_in_en, whole_frac_wire_0, whole_frac_sr_3_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 241:{40,40,40}]
    node whole_frac_wire_1 = _whole_frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 217:31 219:24]
    node _GEN_12 = mux(io_in_en, whole_frac_wire_1, whole_frac_sr_3_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 241:{40,40,40}]
    reg frac_mult_out_sr : UInt<22>, clock with :
      reset => (UInt<1>("h0"), frac_mult_out_sr) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 247:41]
    node _GEN_13 = mux(io_in_en, multiplier.io_out_s, frac_mult_out_sr) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 247:{41,41,41}]
    reg exp_adder_outs_sr_2 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 248:44]
    node _GEN_14 = mux(io_in_en, exp_adder_outs_sr, exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 248:{44,44,44}]
    reg exp_adder_outc_sr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), exp_adder_outc_sr_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 249:44]
    node _GEN_15 = mux(io_in_en, exp_adder_outc_sr, exp_adder_outc_sr_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 249:{44,44,44}]
    reg new_sign_sr_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), new_sign_sr_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 250:38]
    node _GEN_16 = mux(io_in_en, new_sign_sr_2, new_sign_sr_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 250:{38,38,38}]
    reg exp_sr_4_0 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_4_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 251:33]
    reg exp_sr_4_1 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_4_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 251:33]
    node _GEN_17 = mux(io_in_en, exp_sr_3_0, exp_sr_4_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 251:{33,33,33}]
    node _GEN_18 = mux(io_in_en, exp_sr_3_1, exp_sr_4_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 251:{33,33,33}]
    node uo_check = lt(exp_sr_4_1, UInt<5>("hf")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 254:69]
    node carry_flag = bits(exp_adder_outc_sr_2, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 255:56]
    node msb_check = bits(frac_mult_out_sr, 21, 21) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 258:51]
    node _u_flag_reg_T = eq(carry_flag, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 263:49]
    node _u_flag_reg_T_1 = add(exp_adder_outs_sr_2, msb_check) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 263:74]
    node _u_flag_reg_T_2 = lt(_u_flag_reg_T_1, UInt<5>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 263:95]
    node _u_flag_reg_T_3 = or(_u_flag_reg_T, _u_flag_reg_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 263:61]
    node u_flag_reg = mux(uo_check, _u_flag_reg_T_3, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 263:39]
    node _o_flag_reg_T = eq(uo_check, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 264:40]
    node _o_flag_reg_T_1 = add(exp_adder_outs_sr_2, msb_check) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 264:74]
    node _o_flag_reg_T_2 = gt(_o_flag_reg_T_1, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 264:95]
    node _o_flag_reg_T_3 = or(carry_flag, _o_flag_reg_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 264:61]
    node o_flag_reg = mux(_o_flag_reg_T, _o_flag_reg_T_3, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 264:39]
    node _out_exp_innermux_T = bits(frac_mult_out_sr, 21, 21) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 272:60]
    node _out_exp_innermux_T_1 = add(exp_adder_outs_sr_2, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 272:105]
    node _out_exp_innermux_T_2 = tail(_out_exp_innermux_T_1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 272:105]
    node out_exp_innermux = mux(_out_exp_innermux_T, _out_exp_innermux_T_2, exp_adder_outs_sr_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 272:45]
    node _out_frac_innermux_T = bits(frac_mult_out_sr, 21, 21) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 273:61]
    node _out_frac_innermux_T_1 = bits(frac_mult_out_sr, 20, 11) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 273:108]
    node _out_frac_innermux_T_2 = bits(frac_mult_out_sr, 19, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 273:162]
    node out_frac_innermux = mux(_out_frac_innermux_T, _out_frac_innermux_T_1, _out_frac_innermux_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 273:46]
    node out_exp_innermux_2 = mux(o_flag_reg, UInt<5>("h1e"), out_exp_innermux) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 280:47]
    node out_frac_innermux_2 = mux(o_flag_reg, UInt<10>("h3ff"), out_frac_innermux) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 281:48]
    node out_exp = mux(u_flag_reg, UInt<5>("h0"), out_exp_innermux_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 286:22]
    node out_frac = mux(u_flag_reg, UInt<10>("h0"), out_frac_innermux_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 287:23]
    node _io_out_s_T = cat(new_sign_sr_3, out_exp) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 288:40]
    node _io_out_s_T_1 = cat(_io_out_s_T, out_frac) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 288:51]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 289:34]
    node _GEN_19 = mux(io_in_en, io_in_valid, io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 289:{34,34,34}]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 289:34]
    node _GEN_20 = mux(io_in_en, io_out_valid_r, io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 289:{34,34,34}]
    reg io_out_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 289:34]
    node _GEN_21 = mux(io_in_en, io_out_valid_r_1, io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 289:{34,34,34}]
    io_out_s <= _io_out_s_T_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 288:14]
    io_out_valid <= io_out_valid_r_2 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 289:18]
    in_a <= _GEN_0
    in_b <= _GEN_1
    exp_adder.clock <= clock
    exp_adder.reset <= reset
    exp_adder.io_in_a <= exp_wire_0 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 233:23]
    exp_adder.io_in_b <= _exp_adder_io_in_b_T_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 234:23]
    exp_adder.io_in_c <= UInt<1>("h0") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 235:23]
    exp_adder_outs_sr <= _GEN_6
    exp_adder_outc_sr <= _GEN_7
    new_sign_sr_2 <= _GEN_8
    exp_sr_3_0 <= _GEN_9
    exp_sr_3_1 <= _GEN_10
    whole_frac_sr_3_0 <= _GEN_11
    whole_frac_sr_3_1 <= _GEN_12
    multiplier.clock <= clock
    multiplier.reset <= reset
    multiplier.io_in_a <= whole_frac_sr_3_0 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 244:26]
    multiplier.io_in_b <= whole_frac_sr_3_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 245:26]
    frac_mult_out_sr <= _GEN_13
    exp_adder_outs_sr_2 <= _GEN_14
    exp_adder_outc_sr_2 <= _GEN_15
    new_sign_sr_3 <= _GEN_16
    exp_sr_4_0 <= _GEN_17
    exp_sr_4_1 <= _GEN_18
    io_out_valid_r <= _GEN_19
    io_out_valid_r_1 <= _GEN_20
    io_out_valid_r_2 <= _GEN_21

  module full_subtractor : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 92:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 92:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 92:9]
    input io_in_a : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 93:16]
    input io_in_b : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 93:16]
    input io_in_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 93:16]
    output io_out_s : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 93:16]
    output io_out_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 93:16]

    node _result_T = sub(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 101:23]
    node _result_T_1 = asUInt(_result_T) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 101:23]
    node _result_T_2 = sub(_result_T_1, io_in_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 101:34]
    node _result_T_3 = asUInt(_result_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 101:34]
    node result = bits(_result_T_3, 5, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 100:22 101:12]
    node _io_out_s_T = bits(result, 4, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 102:23]
    node _io_out_c_T = bits(result, 5, 5) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 103:23]
    io_out_s <= _io_out_s_T @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 102:14]
    io_out_c <= _io_out_c_T @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 103:14]

  module full_adder_4 : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 78:9]
    input io_in_a : UInt<11> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_b : UInt<11> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    input io_in_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_s : UInt<11> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]
    output io_out_c : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 79:16]

    node _result_T = add(io_in_a, io_in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 87:23]
    node _result_T_1 = add(_result_T, io_in_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 87:34]
    node result = bits(_result_T_1, 11, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 86:22 87:12]
    node _io_out_s_T = bits(result, 10, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 88:23]
    node _io_out_c_T = bits(result, 11, 11) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 89:23]
    io_out_s <= _io_out_s_T @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 88:14]
    io_out_c <= _io_out_c_T @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 89:14]

  module LZC_enc2 : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 8:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 8:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 8:9]
    input io_in_r : UInt<2> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 9:16]
    output io_out_e : UInt<2> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 9:16]

    node seq_0_1 = eq(io_in_r, UInt<2>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 24:48]
    node seq_1_1 = eq(io_in_r, UInt<2>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 24:48]
    node seq_2_1 = eq(io_in_r, UInt<2>("h2")) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 24:48]
    node seq_3_1 = eq(io_in_r, UInt<2>("h3")) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 24:48]
    node _out_enc_T = mux(seq_3_1, UInt<2>("h0"), UInt<1>("h0")) @[src/main/scala/chisel3/util/Mux.scala 126:16]
    node _out_enc_T_1 = mux(seq_2_1, UInt<2>("h0"), _out_enc_T) @[src/main/scala/chisel3/util/Mux.scala 126:16]
    node _out_enc_T_2 = mux(seq_1_1, UInt<2>("h1"), _out_enc_T_1) @[src/main/scala/chisel3/util/Mux.scala 126:16]
    node _out_enc_T_3 = mux(seq_0_1, UInt<2>("h2"), _out_enc_T_2) @[src/main/scala/chisel3/util/Mux.scala 126:16]
    node out_enc = _out_enc_T_3 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 25:23 26:13]
    io_out_e <= out_enc @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 27:14]

  module LZC_Merge2 : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input io_in_h : UInt<2> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]
    input io_in_l : UInt<2> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]
    output io_out_m : UInt<3> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]

    node _result_h_1_T = bits(io_in_h, 1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:27]
    node _result_h_1_T_1 = bits(io_in_l, 1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:43]
    node _result_h_1_T_2 = and(_result_h_1_T, _result_h_1_T_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:34]
    node _result_h_0_T = bits(io_in_h, 1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:31]
    node _result_h_0_T_1 = bits(io_in_l, 1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:54]
    node _result_h_0_T_2 = eq(_result_h_0_T_1, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:46]
    node _result_h_0_T_3 = bits(io_in_h, 1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:69]
    node _result_h_0_T_4 = mux(_result_h_0_T, _result_h_0_T_2, _result_h_0_T_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:23]
    node _result_l_T = bits(io_in_h, 1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:28]
    node _result_l_T_1 = bits(io_in_l, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:50]
    node _result_l_T_2 = bits(io_in_h, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:67]
    node _result_l_T_3 = mux(_result_l_T, _result_l_T_1, _result_l_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:20]
    node result_h_1 = _result_h_1_T_2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 70:24 72:17]
    node result_h_0 = _result_h_0_T_4 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 70:24 73:17]
    node _io_out_m_T = cat(result_h_1, result_h_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:26]
    node result_l = _result_l_T_3 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 71:24 74:14]
    node _io_out_m_T_1 = cat(_io_out_m_T, result_l) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:33]
    io_out_m <= _io_out_m_T_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:14]

  module LZC_Merge3 : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input io_in_h : UInt<3> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]
    input io_in_l : UInt<3> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]
    output io_out_m : UInt<4> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]

    node _result_h_1_T = bits(io_in_h, 2, 2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:27]
    node _result_h_1_T_1 = bits(io_in_l, 2, 2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:43]
    node _result_h_1_T_2 = and(_result_h_1_T, _result_h_1_T_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:34]
    node _result_h_0_T = bits(io_in_h, 2, 2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:31]
    node _result_h_0_T_1 = bits(io_in_l, 2, 2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:54]
    node _result_h_0_T_2 = eq(_result_h_0_T_1, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:46]
    node _result_h_0_T_3 = bits(io_in_h, 2, 2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:69]
    node _result_h_0_T_4 = mux(_result_h_0_T, _result_h_0_T_2, _result_h_0_T_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:23]
    node _result_l_T = bits(io_in_h, 2, 2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:28]
    node _result_l_T_1 = bits(io_in_l, 1, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:50]
    node _result_l_T_2 = bits(io_in_h, 1, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:67]
    node _result_l_T_3 = mux(_result_l_T, _result_l_T_1, _result_l_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:20]
    node result_h_1 = _result_h_1_T_2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 70:24 72:17]
    node result_h_0 = _result_h_0_T_4 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 70:24 73:17]
    node _io_out_m_T = cat(result_h_1, result_h_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:26]
    node result_l = _result_l_T_3 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 71:24 74:14]
    node _io_out_m_T_1 = cat(_io_out_m_T, result_l) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:33]
    io_out_m <= _io_out_m_T_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:14]

  module LZC_Merge4 : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 63:9]
    input io_in_h : UInt<4> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]
    input io_in_l : UInt<4> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]
    output io_out_m : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 64:16]

    node _result_h_1_T = bits(io_in_h, 3, 3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:27]
    node _result_h_1_T_1 = bits(io_in_l, 3, 3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:43]
    node _result_h_1_T_2 = and(_result_h_1_T, _result_h_1_T_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 72:34]
    node _result_h_0_T = bits(io_in_h, 3, 3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:31]
    node _result_h_0_T_1 = bits(io_in_l, 3, 3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:54]
    node _result_h_0_T_2 = eq(_result_h_0_T_1, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:46]
    node _result_h_0_T_3 = bits(io_in_h, 3, 3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:69]
    node _result_h_0_T_4 = mux(_result_h_0_T, _result_h_0_T_2, _result_h_0_T_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 73:23]
    node _result_l_T = bits(io_in_h, 3, 3) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:28]
    node _result_l_T_1 = bits(io_in_l, 2, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:50]
    node _result_l_T_2 = bits(io_in_h, 2, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:67]
    node _result_l_T_3 = mux(_result_l_T, _result_l_T_1, _result_l_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 74:20]
    node result_h_1 = _result_h_1_T_2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 70:24 72:17]
    node result_h_0 = _result_h_0_T_4 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 70:24 73:17]
    node _io_out_m_T = cat(result_h_1, result_h_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:26]
    node result_l = _result_l_T_3 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 71:24 74:14]
    node _io_out_m_T_1 = cat(_io_out_m_T, result_l) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:33]
    io_out_m <= _io_out_m_T_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 75:14]

  module LZC16_2 : @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 30:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 30:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 30:9]
    input io_in_d : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 31:16]
    output io_out_c : UInt<5> @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 31:16]

    inst LZC_enc2 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_enc2_1 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_enc2_2 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_enc2_3 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_enc2_4 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_enc2_5 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_enc2_6 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_enc2_7 of LZC_enc2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 36:50]
    inst LZC_Merge2 of LZC_Merge2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 46:37]
    inst LZC_Merge2_1 of LZC_Merge2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 46:37]
    inst LZC_Merge2_2 of LZC_Merge2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 46:37]
    inst LZC_Merge2_3 of LZC_Merge2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 46:37]
    inst LZC_Merge3 of LZC_Merge3 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 46:37]
    inst LZC_Merge3_1 of LZC_Merge3 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 46:37]
    inst LZC_Merge4 of LZC_Merge4 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 46:37]
    node encoded_enc_in = bits(io_in_d, 1, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    node encoded_enc_in_1 = bits(io_in_d, 3, 2) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    node encoded_enc_in_2 = bits(io_in_d, 5, 4) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    node encoded_enc_in_3 = bits(io_in_d, 7, 6) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    node encoded_enc_in_4 = bits(io_in_d, 9, 8) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    node encoded_enc_in_5 = bits(io_in_d, 11, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    node encoded_enc_in_6 = bits(io_in_d, 13, 12) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    node encoded_enc_in_7 = bits(io_in_d, 15, 14) @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 38:27]
    io_out_c <= LZC_Merge4.io_out_m @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 60:14]
    LZC_enc2.clock <= clock
    LZC_enc2.reset <= reset
    LZC_enc2.io_in_r <= encoded_enc_in @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_enc2_1.clock <= clock
    LZC_enc2_1.reset <= reset
    LZC_enc2_1.io_in_r <= encoded_enc_in_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_enc2_2.clock <= clock
    LZC_enc2_2.reset <= reset
    LZC_enc2_2.io_in_r <= encoded_enc_in_2 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_enc2_3.clock <= clock
    LZC_enc2_3.reset <= reset
    LZC_enc2_3.io_in_r <= encoded_enc_in_3 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_enc2_4.clock <= clock
    LZC_enc2_4.reset <= reset
    LZC_enc2_4.io_in_r <= encoded_enc_in_4 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_enc2_5.clock <= clock
    LZC_enc2_5.reset <= reset
    LZC_enc2_5.io_in_r <= encoded_enc_in_5 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_enc2_6.clock <= clock
    LZC_enc2_6.reset <= reset
    LZC_enc2_6.io_in_r <= encoded_enc_in_6 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_enc2_7.clock <= clock
    LZC_enc2_7.reset <= reset
    LZC_enc2_7.io_in_r <= encoded_enc_in_7 @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 39:24]
    LZC_Merge2.clock <= clock
    LZC_Merge2.reset <= reset
    LZC_Merge2.io_in_h <= LZC_enc2_7.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 52:30]
    LZC_Merge2.io_in_l <= LZC_enc2_6.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 53:30]
    LZC_Merge2_1.clock <= clock
    LZC_Merge2_1.reset <= reset
    LZC_Merge2_1.io_in_h <= LZC_enc2_5.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 52:30]
    LZC_Merge2_1.io_in_l <= LZC_enc2_4.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 53:30]
    LZC_Merge2_2.clock <= clock
    LZC_Merge2_2.reset <= reset
    LZC_Merge2_2.io_in_h <= LZC_enc2_3.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 52:30]
    LZC_Merge2_2.io_in_l <= LZC_enc2_2.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 53:30]
    LZC_Merge2_3.clock <= clock
    LZC_Merge2_3.reset <= reset
    LZC_Merge2_3.io_in_h <= LZC_enc2_1.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 52:30]
    LZC_Merge2_3.io_in_l <= LZC_enc2.io_out_e @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 53:30]
    LZC_Merge3.clock <= clock
    LZC_Merge3.reset <= reset
    LZC_Merge3.io_in_h <= LZC_Merge2.io_out_m @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 55:30]
    LZC_Merge3.io_in_l <= LZC_Merge2_1.io_out_m @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 56:30]
    LZC_Merge3_1.clock <= clock
    LZC_Merge3_1.reset <= reset
    LZC_Merge3_1.io_in_h <= LZC_Merge2_2.io_out_m @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 55:30]
    LZC_Merge3_1.io_in_l <= LZC_Merge2_3.io_out_m @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 56:30]
    LZC_Merge4.clock <= clock
    LZC_Merge4.reset <= reset
    LZC_Merge4.io_in_h <= LZC_Merge3.io_out_m @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 55:30]
    LZC_Merge4.io_in_l <= LZC_Merge3_1.io_out_m @[\\src\\main\\scala\\FloatingPointUnits\\New_Binary_Mario\\BinaryDesignsNew.scala 56:30]

  module FP_add_16_3 : @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 293:9]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 293:9]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 293:9]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 296:16]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 296:16]
    input io_in_a : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 296:16]
    input io_in_b : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 296:16]
    output io_out_s : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 296:16]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 296:16]

    inst exp_subtractor of full_subtractor @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 374:32]
    inst full_adder of full_adder_4 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 401:28]
    inst LZC16_2 of LZC16_2 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 419:26]
    node _sign_wire_0_T = bits(io_in_a, 15, 15) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 339:25]
    node _sign_wire_1_T = bits(io_in_b, 15, 15) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 340:25]
    node _T = bits(io_in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 344:14]
    node _T_1 = sub(UInt<6>("h20"), UInt<2>("h2")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 344:61]
    node _T_2 = tail(_T_1, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 344:61]
    node _T_3 = gt(_T, _T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 344:33]
    node _exp_wire_0_T = sub(UInt<6>("h20"), UInt<2>("h2")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 345:48]
    node _exp_wire_0_T_1 = tail(_exp_wire_0_T, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 345:48]
    node _T_4 = bits(io_in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 346:20]
    node _T_5 = lt(_T_4, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 346:36]
    node _exp_wire_0_T_2 = bits(io_in_a, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 349:26]
    node _GEN_0 = mux(_T_5, UInt<1>("h1"), _exp_wire_0_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 346:42 347:19 349:19]
    node _GEN_1 = mux(_T_3, _exp_wire_0_T_1, _GEN_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 344:68 345:19]
    node _T_6 = bits(io_in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 351:14]
    node _T_7 = sub(UInt<6>("h20"), UInt<2>("h2")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 351:61]
    node _T_8 = tail(_T_7, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 351:61]
    node _T_9 = gt(_T_6, _T_8) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 351:33]
    node _exp_wire_1_T = sub(UInt<6>("h20"), UInt<2>("h2")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 352:48]
    node _exp_wire_1_T_1 = tail(_exp_wire_1_T, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 352:48]
    node _T_10 = bits(io_in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 353:20]
    node _T_11 = lt(_T_10, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 353:36]
    node _exp_wire_1_T_2 = bits(io_in_b, 14, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 356:26]
    node _GEN_2 = mux(_T_11, UInt<1>("h1"), _exp_wire_1_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 353:42 354:19 356:19]
    node _GEN_3 = mux(_T_9, _exp_wire_1_T_1, _GEN_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 351:68 352:19]
    node _frac_wire_0_T = bits(io_in_a, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 361:25]
    node _frac_wire_1_T = bits(io_in_b, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 362:25]
    node frac_wire_0 = _frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 360:25 361:18]
    node _whole_frac_wire_0_T = cat(UInt<1>("h1"), frac_wire_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 366:31]
    node frac_wire_1 = _frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 360:25 362:18]
    node _whole_frac_wire_1_T = cat(UInt<1>("h1"), frac_wire_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 367:31]
    reg exp_sub_out_c : UInt<1>, clock with :
      reset => (UInt<1>("h0"), exp_sub_out_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 379:38]
    node _GEN_4 = mux(io_in_en, exp_subtractor.io_out_c, exp_sub_out_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 379:{38,38,38}]
    reg exp_sub_out_s : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sub_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 380:38]
    node _GEN_5 = mux(io_in_en, exp_subtractor.io_out_s, exp_sub_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 380:{38,38,38}]
    reg exp_sr_2_0 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 381:33]
    reg exp_sr_2_1 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), exp_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 381:33]
    node exp_wire_0 = bits(_GEN_1, 4, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 343:24]
    node _GEN_6 = mux(io_in_en, exp_wire_0, exp_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 381:{33,33,33}]
    node exp_wire_1 = bits(_GEN_3, 4, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 343:24]
    node _GEN_7 = mux(io_in_en, exp_wire_1, exp_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 381:{33,33,33}]
    reg whole_frac_sr_2_0 : UInt<11>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 382:40]
    reg whole_frac_sr_2_1 : UInt<11>, clock with :
      reset => (UInt<1>("h0"), whole_frac_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 382:40]
    node whole_frac_wire_0 = _whole_frac_wire_0_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 365:31 366:24]
    node _GEN_8 = mux(io_in_en, whole_frac_wire_0, whole_frac_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 382:{40,40,40}]
    node whole_frac_wire_1 = _whole_frac_wire_1_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 365:31 367:24]
    node _GEN_9 = mux(io_in_en, whole_frac_wire_1, whole_frac_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 382:{40,40,40}]
    reg sign_sr_2_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), sign_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 383:34]
    reg sign_sr_2_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), sign_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 383:34]
    node sign_wire_0 = _sign_wire_0_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 338:25 339:18]
    node _GEN_10 = mux(io_in_en, sign_wire_0, sign_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 383:{34,34,34}]
    node sign_wire_1 = _sign_wire_1_T @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 338:25 340:18]
    node _GEN_11 = mux(io_in_en, sign_wire_1, sign_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 383:{34,34,34}]
    node _eqexp_arrange_T = eq(exp_sr_2_0, exp_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 385:54]
    node _eqexp_arrange_T_1 = gt(whole_frac_sr_2_1, whole_frac_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 385:88]
    node eqexp_arrange = mux(_eqexp_arrange_T, _eqexp_arrange_T_1, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 385:42]
    node _redundant_op_T = bits(exp_sub_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 393:58]
    node _redundant_op_T_1 = not(exp_sub_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 393:67]
    node _redundant_op_T_2 = add(_redundant_op_T_1, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 393:92]
    node _redundant_op_T_3 = tail(_redundant_op_T_2, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 393:92]
    node _redundant_op_T_4 = mux(_redundant_op_T, _redundant_op_T_3, exp_sub_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 393:41]
    node redundant_op = gt(_redundant_op_T_4, UInt<4>("ha")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 393:116]
    node _fracadd_in_a_T = bits(exp_sub_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 394:58]
    node _GEN_12 = validif(eq(UInt<1>("h0"), eqexp_arrange), whole_frac_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 394:{41,41}]
    node _GEN_13 = mux(eq(UInt<1>("h1"), eqexp_arrange), whole_frac_sr_2_1, _GEN_12) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 394:{41,41}]
    node _whole_frac_sr_2_eqexp_arrange = _GEN_13 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 394:41]
    node fracadd_in_a = mux(_fracadd_in_a_T, whole_frac_sr_2_1, _whole_frac_sr_2_eqexp_arrange) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 394:41]
    node _fracadd_in_b_T = bits(exp_sub_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:58]
    node _fracadd_in_b_T_1 = not(exp_sub_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:90]
    node _fracadd_in_b_T_2 = add(_fracadd_in_b_T_1, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:115]
    node _fracadd_in_b_T_3 = tail(_fracadd_in_b_T_2, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:115]
    node _fracadd_in_b_T_4 = dshr(whole_frac_sr_2_0, _fracadd_in_b_T_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:85]
    node _fracadd_in_b_T_5 = eq(eqexp_arrange, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:139]
    node _GEN_14 = validif(eq(UInt<1>("h0"), _fracadd_in_b_T_5), whole_frac_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:{155,155}]
    node _GEN_15 = mux(eq(UInt<1>("h1"), _fracadd_in_b_T_5), whole_frac_sr_2_1, _GEN_14) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:{155,155}]
    node _whole_frac_sr_2_fracadd_in_b_T_5 = _GEN_15 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:155]
    node _fracadd_in_b_T_6 = dshr(_whole_frac_sr_2_fracadd_in_b_T_5, exp_sub_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:155]
    node fracadd_in_b = mux(_fracadd_in_b_T, _fracadd_in_b_T_4, _fracadd_in_b_T_6) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 395:41]
    node _ref_exp_T = bits(exp_sub_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 396:53]
    node ref_exp = mux(_ref_exp_T, exp_sr_2_1, exp_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 396:36]
    node _ref_sign_T = bits(exp_sub_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 397:54]
    node _GEN_16 = validif(eq(UInt<1>("h0"), eqexp_arrange), sign_sr_2_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 397:{37,37}]
    node _GEN_17 = mux(eq(UInt<1>("h1"), eqexp_arrange), sign_sr_2_1, _GEN_16) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 397:{37,37}]
    node _sign_sr_2_eqexp_arrange = _GEN_17 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 397:37]
    node ref_sign = mux(_ref_sign_T, sign_sr_2_1, _sign_sr_2_eqexp_arrange) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 397:37]
    node diff_sign = xor(sign_sr_2_0, sign_sr_2_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 398:48]
    node _T_12 = bits(diff_sign, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 403:38]
    node _T_13 = not(fracadd_in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 403:47]
    node _T_14 = add(_T_13, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 403:69]
    node _T_15 = tail(_T_14, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 403:69]
    node _T_16 = mux(_T_12, _T_15, fracadd_in_b) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 403:27]
    reg frac_adder_out_c : UInt<1>, clock with :
      reset => (UInt<1>("h0"), frac_adder_out_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 406:41]
    node _GEN_18 = mux(io_in_en, full_adder.io_out_c, frac_adder_out_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 406:{41,41,41}]
    reg frac_adder_out_s : UInt<11>, clock with :
      reset => (UInt<1>("h0"), frac_adder_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 407:41]
    node _GEN_19 = mux(io_in_en, full_adder.io_out_s, frac_adder_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 407:{41,41,41}]
    reg diff_sign_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), diff_sign_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 408:36]
    node _GEN_20 = mux(io_in_en, diff_sign, diff_sign_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 408:{36,36,36}]
    reg redundant_op_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), redundant_op_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 409:39]
    node _GEN_21 = mux(io_in_en, redundant_op, redundant_op_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 409:{39,39,39}]
    reg ref_sign_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ref_sign_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 410:35]
    node _GEN_22 = mux(io_in_en, ref_sign, ref_sign_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 410:{35,35,35}]
    reg ref_exp_2 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), ref_exp_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 411:34]
    node _GEN_23 = mux(io_in_en, ref_exp, ref_exp_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 411:{34,34,34}]
    node _sign_out_T = bits(diff_sign_2, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 413:50]
    node _sign_out_T_1 = eq(redundant_op_2, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 413:60]
    node _sign_out_T_2 = and(_sign_out_T, _sign_out_T_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 413:57]
    node _sign_out_T_3 = bits(frac_adder_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 413:98]
    node _sign_out_T_4 = eq(ref_sign_2, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 413:118]
    node _sign_out_T_5 = mux(_sign_out_T_3, ref_sign_2, _sign_out_T_4) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 413:80]
    node sign_out = mux(_sign_out_T_2, _sign_out_T_5, ref_sign_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 413:37]
    node _WIRE_1 = UInt<1>("h1") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:{72,72}]
    node _WIRE_0 = UInt<1>("h1") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:{72,72}]
    node lo = cat(_WIRE_1, _WIRE_0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:82]
    node _WIRE_4 = UInt<1>("h1") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:{72,72}]
    node _WIRE_3 = UInt<1>("h1") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:{72,72}]
    node hi_hi = cat(_WIRE_4, _WIRE_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:82]
    node _WIRE_2 = UInt<1>("h1") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:{72,72}]
    node hi = cat(hi_hi, _WIRE_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:82]
    node _T_17 = cat(hi, lo) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:82]
    node _T_18 = cat(frac_adder_out_s, _T_17) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:35]
    node _red_T = sub(ref_exp_2, LZC16_2.io_out_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 428:25]
    node red = asUInt(_red_T) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 428:25]
    node inc = add(ref_exp_2, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 429:25]
    node _shifted_left_T = bits(frac_adder_out_s, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 431:38]
    node shifted_left = dshl(_shifted_left_T, LZC16_2.io_out_c) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 431:56]
    node _innermux_frac_true_T = bits(frac_adder_out_s, 10, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 434:62]
    node _innermux_frac_true_T_1 = bits(frac_adder_out_s, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 434:95]
    node _innermux_frac_true_T_2 = bits(red, 5, 5) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 434:118]
    node _innermux_frac_true_T_3 = lt(red, UInt<5>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 434:137]
    node _innermux_frac_true_T_4 = or(_innermux_frac_true_T_2, _innermux_frac_true_T_3) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 434:129]
    node _innermux_frac_true_T_5 = mux(_innermux_frac_true_T_4, UInt<10>("h0"), shifted_left) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 434:114]
    node innermux_frac_true = mux(_innermux_frac_true_T, _innermux_frac_true_T_1, _innermux_frac_true_T_5) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 434:47]
    node _innermux_frac_false_T = bits(frac_adder_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:64]
    node _innermux_frac_false_T_1 = bits(inc, 5, 5) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:79]
    node _innermux_frac_false_T_2 = gt(inc, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:98]
    node _innermux_frac_false_T_3 = or(_innermux_frac_false_T_1, _innermux_frac_false_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:90]
    node _innermux_frac_false_T_4 = bits(frac_adder_out_s, 10, 1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:134]
    node _innermux_frac_false_T_5 = mux(_innermux_frac_false_T_3, UInt<10>("h3ff"), _innermux_frac_false_T_4) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:75]
    node _innermux_frac_false_T_6 = bits(frac_adder_out_s, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:163]
    node innermux_frac_false = mux(_innermux_frac_false_T, _innermux_frac_false_T_5, _innermux_frac_false_T_6) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 435:48]
    node _innermux_exp_true_T = bits(frac_adder_out_s, 10, 10) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 436:61]
    node _innermux_exp_true_T_1 = bits(red, 5, 5) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 436:98]
    node _innermux_exp_true_T_2 = lt(red, UInt<5>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 436:117]
    node _innermux_exp_true_T_3 = or(_innermux_exp_true_T_1, _innermux_exp_true_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 436:109]
    node _innermux_exp_true_T_4 = bits(red, 4, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 436:141]
    node _innermux_exp_true_T_5 = mux(_innermux_exp_true_T_3, UInt<5>("h0"), _innermux_exp_true_T_4) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 436:94]
    node innermux_exp_true = mux(_innermux_exp_true_T, ref_exp_2, _innermux_exp_true_T_5) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 436:46]
    node _innermux_exp_false_T = bits(frac_adder_out_c, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 437:63]
    node _innermux_exp_false_T_1 = bits(inc, 5, 5) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 437:78]
    node _innermux_exp_false_T_2 = gt(inc, UInt<5>("h1e")) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 437:97]
    node _innermux_exp_false_T_3 = or(_innermux_exp_false_T_1, _innermux_exp_false_T_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 437:89]
    node _innermux_exp_false_T_4 = bits(inc, 4, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 437:121]
    node _innermux_exp_false_T_5 = mux(_innermux_exp_false_T_3, UInt<5>("h1e"), _innermux_exp_false_T_4) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 437:74]
    node innermux_exp_false = mux(_innermux_exp_false_T, _innermux_exp_false_T_5, ref_exp_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 437:47]
    node _norm_out_frac_T = bits(diff_sign_2, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 440:56]
    node _norm_out_frac_T_1 = mux(_norm_out_frac_T, innermux_frac_true, innermux_frac_false) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 440:43]
    reg norm_out_frac_r : UInt<41>, clock with :
      reset => (UInt<1>("h0"), norm_out_frac_r) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 440:39]
    node _GEN_24 = mux(io_in_en, _norm_out_frac_T_1, norm_out_frac_r) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 440:{39,39,39}]
    node norm_out_frac = bits(norm_out_frac_r, 9, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 443:30]
    node _norm_out_exp_T = bits(diff_sign_2, 0, 0) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 445:54]
    node _norm_out_exp_T_1 = mux(_norm_out_exp_T, innermux_exp_true, innermux_exp_false) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 445:41]
    reg norm_out_exp : UInt<5>, clock with :
      reset => (UInt<1>("h0"), norm_out_exp) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 445:37]
    node _GEN_25 = mux(io_in_en, _norm_out_exp_T_1, norm_out_exp) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 445:{37,37,37}]
    reg norm_out_sign : UInt<1>, clock with :
      reset => (UInt<1>("h0"), norm_out_sign) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 449:38]
    node _GEN_26 = mux(io_in_en, sign_out, norm_out_sign) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 449:{38,38,38}]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 451:34]
    node _GEN_27 = mux(io_in_en, io_in_valid, io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 451:{34,34,34}]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 451:34]
    node _GEN_28 = mux(io_in_en, io_out_valid_r, io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 451:{34,34,34}]
    reg io_out_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 451:34]
    node _GEN_29 = mux(io_in_en, io_out_valid_r_1, io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 451:{34,34,34}]
    node _io_out_s_T = cat(norm_out_sign, norm_out_exp) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 452:31]
    node _io_out_s_T_1 = cat(_io_out_s_T, norm_out_frac) @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 452:47]
    io_out_s <= _io_out_s_T_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 452:14]
    io_out_valid <= io_out_valid_r_2 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 451:18]
    exp_subtractor.clock <= clock
    exp_subtractor.reset <= reset
    exp_subtractor.io_in_a <= exp_wire_0 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 375:28]
    exp_subtractor.io_in_b <= exp_wire_1 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 376:28]
    exp_subtractor.io_in_c <= UInt<1>("h0") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 377:28]
    exp_sub_out_c <= _GEN_4
    exp_sub_out_s <= _GEN_5
    exp_sr_2_0 <= _GEN_6
    exp_sr_2_1 <= _GEN_7
    whole_frac_sr_2_0 <= _GEN_8
    whole_frac_sr_2_1 <= _GEN_9
    sign_sr_2_0 <= _GEN_10
    sign_sr_2_1 <= _GEN_11
    full_adder.clock <= clock
    full_adder.reset <= reset
    full_adder.io_in_a <= fracadd_in_a @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 402:21]
    full_adder.io_in_b <= _T_16 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 403:21]
    full_adder.io_in_c <= UInt<1>("h0") @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 404:21]
    frac_adder_out_c <= _GEN_18
    frac_adder_out_s <= _GEN_19
    diff_sign_2 <= _GEN_20
    redundant_op_2 <= _GEN_21
    ref_sign_2 <= _GEN_22
    ref_exp_2 <= _GEN_23
    LZC16_2.clock <= clock
    LZC16_2.reset <= reset
    LZC16_2.io_in_d <= _T_18 @[\\src\\main\\scala\\FloatingPointUnits\\New_FPU_Mario\\FPUnits.scala 420:19]
    norm_out_frac_r <= _GEN_24
    norm_out_exp <= _GEN_25
    norm_out_sign <= _GEN_26
    io_out_valid_r <= _GEN_27
    io_out_valid_r_1 <= _GEN_28
    io_out_valid_r_2 <= _GEN_29

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

  module complex_conjugate_mult : @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 31:7]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 31:7]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 31:7]
    input io_complexA : UInt<32> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]
    input io_complexB : UInt<32> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]
    output io_out_s : UInt<32> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]
    output io_out_real : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]
    output io_out_imag : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 32:14]

    inst FP_mult_inst_0 of FP_mult_16_3_v2 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 62:40]
    inst FP_mult_inst_1 of FP_mult_16_3_v2 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 62:40]
    inst FP_mult_inst_2 of FP_mult_16_3_v2 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 62:40]
    inst FP_mult_inst_3 of FP_mult_16_3_v2 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 62:40]
    inst FP_adder_Inst_0 of FP_add_16_3 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 70:41]
    inst FP_adder_Inst_1 of FP_add_16_3 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 70:41]
    inst flipbit of turnNegative @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 87:23]
    reg counter : UInt<8>, clock with :
      reset => (UInt<1>("h0"), counter) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 50:24]
    node _counter_T = add(counter, UInt<1>("h1")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 54:24]
    node _counter_T_1 = tail(_counter_T, 1) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 54:24]
    node _GEN_0 = mux(io_in_en, _counter_T_1, counter) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 53:19 54:13 50:24]
    node _FP_mult_inst_0_io_in_a_T = bits(io_complexA, 31, 16) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 78:41]
    node _FP_mult_inst_0_io_in_b_T = bits(io_complexB, 31, 16) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 79:41]
    node _FP_mult_inst_1_io_in_a_T = bits(io_complexA, 15, 0) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 80:41]
    node _FP_mult_inst_1_io_in_b_T = bits(io_complexB, 15, 0) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 81:41]
    node _FP_mult_inst_2_io_in_a_T = bits(io_complexA, 15, 0) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 82:41]
    node _FP_mult_inst_2_io_in_b_T = bits(io_complexB, 31, 16) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 83:41]
    node _FP_mult_inst_3_io_in_a_T = bits(io_complexA, 31, 16) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 84:41]
    node _FP_mult_inst_3_io_in_b_T = bits(io_complexB, 15, 0) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 85:41]
    node _FP_adder_Inst_0_io_in_a_T = mux(FP_adder_Inst_0.io_in_en, FP_mult_inst_0.io_out_s, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 90:34]
    node _FP_adder_Inst_0_io_in_b_T = mux(FP_adder_Inst_0.io_in_en, FP_mult_inst_1.io_out_s, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 91:34]
    node _FP_adder_Inst_1_io_in_a_T = mux(FP_adder_Inst_1.io_in_en, flipbit.io_out, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 93:34]
    node _FP_adder_Inst_1_io_in_b_T = mux(FP_adder_Inst_1.io_in_en, FP_mult_inst_3.io_out_s, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 94:34]
    node _GEN_1 = mux(io_in_en, io_in_en, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 66:18 97:20 98:32]
    node _GEN_2 = mux(io_in_en, io_in_valid, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 97:20 67:21 99:35]
    node _T = geq(counter, UInt<2>("h3")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:30]
    node _T_1 = and(io_in_en, _T) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:19]
    node _GEN_3 = mux(_T_1, io_in_en, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:44 105:33 74:18]
    node _GEN_4 = mux(_T_1, io_in_valid, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:44 106:36 75:21]
    node _T_2 = geq(counter, UInt<2>("h3")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:30]
    node _T_3 = and(io_in_en, _T_2) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:19]
    node _GEN_5 = mux(_T_3, io_in_en, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:44 105:33 74:18]
    node _GEN_6 = mux(_T_3, io_in_valid, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 104:44 106:36 75:21]
    node _io_out_s_T = cat(FP_adder_Inst_0.io_out_s, FP_adder_Inst_1.io_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 110:18]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:32]
    node _GEN_7 = mux(io_in_en, io_in_valid, io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:{32,32,32}]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:32]
    node _GEN_8 = mux(io_in_en, io_out_valid_r, io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:{32,32,32}]
    reg io_out_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:32]
    node _GEN_9 = mux(io_in_en, io_out_valid_r_1, io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:{32,32,32}]
    reg io_out_valid_r_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_3) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:32]
    node _GEN_10 = mux(io_in_en, io_out_valid_r_2, io_out_valid_r_3) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:{32,32,32}]
    reg io_out_valid_r_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_4) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:32]
    node _GEN_11 = mux(io_in_en, io_out_valid_r_3, io_out_valid_r_4) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:{32,32,32}]
    reg io_out_valid_r_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_5) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:32]
    node _GEN_12 = mux(io_in_en, io_out_valid_r_4, io_out_valid_r_5) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:{32,32,32}]
    io_out_valid <= io_out_valid_r_5 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 111:16]
    io_out_s <= _io_out_s_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 110:12]
    io_out_real <= FP_adder_Inst_0.io_out_s @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 112:15]
    io_out_imag <= FP_adder_Inst_1.io_out_s @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 113:15]
    counter <= mux(reset, UInt<8>("h0"), _GEN_0) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 50:{24,24}]
    FP_mult_inst_0.clock <= clock
    FP_mult_inst_0.reset <= reset
    FP_mult_inst_0.io_in_en <= _GEN_1
    FP_mult_inst_0.io_in_valid <= _GEN_2
    FP_mult_inst_0.io_in_a <= _FP_mult_inst_0_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 78:27]
    FP_mult_inst_0.io_in_b <= _FP_mult_inst_0_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 79:27]
    FP_mult_inst_1.clock <= clock
    FP_mult_inst_1.reset <= reset
    FP_mult_inst_1.io_in_en <= _GEN_1
    FP_mult_inst_1.io_in_valid <= _GEN_2
    FP_mult_inst_1.io_in_a <= _FP_mult_inst_1_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 80:27]
    FP_mult_inst_1.io_in_b <= _FP_mult_inst_1_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 81:27]
    FP_mult_inst_2.clock <= clock
    FP_mult_inst_2.reset <= reset
    FP_mult_inst_2.io_in_en <= _GEN_1
    FP_mult_inst_2.io_in_valid <= _GEN_2
    FP_mult_inst_2.io_in_a <= _FP_mult_inst_2_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 82:27]
    FP_mult_inst_2.io_in_b <= _FP_mult_inst_2_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 83:27]
    FP_mult_inst_3.clock <= clock
    FP_mult_inst_3.reset <= reset
    FP_mult_inst_3.io_in_en <= _GEN_1
    FP_mult_inst_3.io_in_valid <= _GEN_2
    FP_mult_inst_3.io_in_a <= _FP_mult_inst_3_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 84:27]
    FP_mult_inst_3.io_in_b <= _FP_mult_inst_3_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 85:27]
    FP_adder_Inst_0.clock <= clock
    FP_adder_Inst_0.reset <= reset
    FP_adder_Inst_0.io_in_en <= _GEN_3
    FP_adder_Inst_0.io_in_valid <= _GEN_4
    FP_adder_Inst_0.io_in_a <= _FP_adder_Inst_0_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 90:28]
    FP_adder_Inst_0.io_in_b <= _FP_adder_Inst_0_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 91:28]
    FP_adder_Inst_1.clock <= clock
    FP_adder_Inst_1.reset <= reset
    FP_adder_Inst_1.io_in_en <= _GEN_5
    FP_adder_Inst_1.io_in_valid <= _GEN_6
    FP_adder_Inst_1.io_in_a <= _FP_adder_Inst_1_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 93:28]
    FP_adder_Inst_1.io_in_b <= _FP_adder_Inst_1_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 94:28]
    flipbit.clock <= clock
    flipbit.reset <= reset
    flipbit.io_in <= FP_mult_inst_2.io_out_s @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 88:17]
    io_out_valid_r <= _GEN_7
    io_out_valid_r_1 <= _GEN_8
    io_out_valid_r_2 <= _GEN_9
    io_out_valid_r_3 <= _GEN_10
    io_out_valid_r_4 <= _GEN_11
    io_out_valid_r_5 <= _GEN_12

  module complex_adder : @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 130:7]
    input clock : Clock @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 130:7]
    input reset : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 130:7]
    input io_complexA : UInt<32> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]
    input io_complexB : UInt<32> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]
    output io_out_s : UInt<32> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]
    output io_out_real : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]
    output io_out_imag : UInt<16> @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 131:14]

    inst FP_adder_Inst_0 of FP_add_16_3 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 144:41]
    inst FP_adder_Inst_1 of FP_add_16_3 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 144:41]
    node _FP_adder_Inst_0_io_in_a_T = bits(io_complexA, 31, 16) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 152:42]
    node _FP_adder_Inst_0_io_in_b_T = bits(io_complexB, 31, 16) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 153:42]
    node _FP_adder_Inst_1_io_in_a_T = bits(io_complexA, 15, 0) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 154:42]
    node _FP_adder_Inst_1_io_in_b_T = bits(io_complexB, 15, 0) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 155:42]
    node _GEN_0 = mux(io_in_en, io_in_en, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 148:18 157:19 158:31]
    node _GEN_1 = mux(io_in_en, io_in_valid, UInt<1>("h0")) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 157:19 149:21 160:34]
    node _io_out_s_T = cat(FP_adder_Inst_0.io_out_s, FP_adder_Inst_1.io_out_s) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 164:18]
    reg io_out_valid_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 167:32]
    node _GEN_2 = mux(io_in_en, io_in_valid, io_out_valid_r) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 167:{32,32,32}]
    reg io_out_valid_r_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 167:32]
    node _GEN_3 = mux(io_in_en, io_out_valid_r, io_out_valid_r_1) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 167:{32,32,32}]
    reg io_out_valid_r_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 167:32]
    node _GEN_4 = mux(io_in_en, io_out_valid_r_1, io_out_valid_r_2) @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 167:{32,32,32}]
    io_out_valid <= io_out_valid_r_2 @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 167:16]
    io_out_s <= _io_out_s_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 164:12]
    io_out_real <= FP_adder_Inst_0.io_out_s @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 165:15]
    io_out_imag <= FP_adder_Inst_1.io_out_s @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 166:15]
    FP_adder_Inst_0.clock <= clock
    FP_adder_Inst_0.reset <= reset
    FP_adder_Inst_0.io_in_en <= _GEN_0
    FP_adder_Inst_0.io_in_valid <= _GEN_1
    FP_adder_Inst_0.io_in_a <= _FP_adder_Inst_0_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 152:28]
    FP_adder_Inst_0.io_in_b <= _FP_adder_Inst_0_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 153:28]
    FP_adder_Inst_1.clock <= clock
    FP_adder_Inst_1.reset <= reset
    FP_adder_Inst_1.io_in_en <= _GEN_0
    FP_adder_Inst_1.io_in_valid <= _GEN_1
    FP_adder_Inst_1.io_in_a <= _FP_adder_Inst_1_io_in_a_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 154:28]
    FP_adder_Inst_1.io_in_b <= _FP_adder_Inst_1_io_in_b_T @[\\src\\main\\scala\\FloatingPointUnits\\Complex_FPU\\complex.scala 155:28]
    io_out_valid_r <= _GEN_2
    io_out_valid_r_1 <= _GEN_3
    io_out_valid_r_2 <= _GEN_4

  module Hadamard : @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 42:7]
    input clock : Clock @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 42:7]
    input reset : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 42:7]
    input io_in_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 43:14]
    input io_in_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 43:14]
    input io_in_en : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 43:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 43:14]
    output io_out_0 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 43:14]
    output io_out_1 : UInt<32> @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 43:14]

    inst FPUMultiplier_0 of complex_conjugate_mult @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 58:41]
    inst FPUMultiplier_1 of complex_conjugate_mult @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 58:41]
    inst FPUAdder_0 of complex_adder @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 59:41]
    inst FPUAdder_1 of complex_adder @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 59:41]
    inst flipC of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 60:29]
    inst flipD of turnNegative @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 61:29]
    node _FPUAdder_0_io_in_en_T = and(FPUMultiplier_0.io_out_valid, FPUMultiplier_1.io_out_valid) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 82:65]
    node _FPUAdder_0_io_in_valid_T = and(FPUMultiplier_0.io_out_valid, FPUMultiplier_1.io_out_valid) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 83:65]
    node _FPUAdder_1_io_complexB_T = cat(flipC.io_out, flipD.io_out) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 87:48]
    node _FPUAdder_1_io_in_en_T = and(FPUMultiplier_0.io_out_valid, FPUMultiplier_1.io_out_valid) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 88:65]
    node _FPUAdder_1_io_in_valid_T = and(FPUMultiplier_0.io_out_valid, FPUMultiplier_1.io_out_valid) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 89:65]
    node _io_out_valid_T = and(FPUAdder_0.io_out_valid, FPUAdder_1.io_out_valid) @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 92:44]
    io_out_valid <= _io_out_valid_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 92:16]
    io_out_0 <= FPUAdder_0.io_out_s @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 84:32]
    io_out_1 <= FPUAdder_1.io_out_s @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 90:32]
    FPUMultiplier_0.clock <= clock
    FPUMultiplier_0.reset <= reset
    FPUMultiplier_0.io_complexA <= io_in_0 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 70:32]
    FPUMultiplier_0.io_complexB <= UInt<32>("h39a8") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 71:32]
    FPUMultiplier_0.io_in_en <= io_in_en @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 72:32]
    FPUMultiplier_0.io_in_valid <= io_in_en @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 73:32]
    FPUMultiplier_1.clock <= clock
    FPUMultiplier_1.reset <= reset
    FPUMultiplier_1.io_complexA <= io_in_1 @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 75:32]
    FPUMultiplier_1.io_complexB <= UInt<32>("h39a8") @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 76:32]
    FPUMultiplier_1.io_in_en <= io_in_en @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 77:32]
    FPUMultiplier_1.io_in_valid <= io_in_en @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 78:32]
    FPUAdder_0.clock <= clock
    FPUAdder_0.reset <= reset
    FPUAdder_0.io_complexA <= FPUMultiplier_0.io_out_s @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 80:32]
    FPUAdder_0.io_complexB <= FPUMultiplier_1.io_out_s @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 81:32]
    FPUAdder_0.io_in_en <= _FPUAdder_0_io_in_en_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 82:32]
    FPUAdder_0.io_in_valid <= _FPUAdder_0_io_in_valid_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 83:32]
    FPUAdder_1.clock <= clock
    FPUAdder_1.reset <= reset
    FPUAdder_1.io_complexA <= FPUMultiplier_0.io_out_s @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 86:32]
    FPUAdder_1.io_complexB <= _FPUAdder_1_io_complexB_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 87:32]
    FPUAdder_1.io_in_en <= _FPUAdder_1_io_in_en_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 88:32]
    FPUAdder_1.io_in_valid <= _FPUAdder_1_io_in_valid_T @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 89:32]
    flipC.clock <= clock
    flipC.reset <= reset
    flipC.io_in <= FPUMultiplier_1.io_out_real @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 62:21]
    flipD.clock <= clock
    flipD.reset <= reset
    flipD.io_in <= FPUMultiplier_1.io_out_imag @[\\src\\main\\scala\\QuantumStateUnit\\Gates\\FPUGatePool.scala 63:21]
