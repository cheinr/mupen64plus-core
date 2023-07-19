#include "idec.h"
#include "emscripten.h"


#define I32_CONST(i32) put8(0x41); putSLEB128(i32);
#define I32_LOAD(offset) put8(0x28); put8(0x02); putULEB128(offset, 0);
#define I32_LOAD8_S(offset) put8(0x2c); put8(0x00); putULEB128(offset, 0);
#define I32_LOAD8_U(offset) put8(0x2d); put8(0x00); putULEB128(offset, 0);
#define I32_LOAD16_S(offset) put8(0x2e); put8(0x01); putULEB128(offset, 0);
#define I32_LOAD16_U(offset) put8(0x2f); put8(0x01); putULEB128(offset, 0);
#define CALL_R4300_PC_STRUCT put8(0x41); putSLEB128((int) &(&g_dev.r4300)->pc);
#define LOCAL_SET(index) put8(0x21); putULEB128(index, 0);
#define LOCAL_GET(index) put8(0x20); putULEB128(index, 0);
#define LOCAL_TEE(index) put8(0x22); putULEB128(index, 0);
#define I32_ADD put8(0x6a);
#define I32_SUB put8(0x6b);
#define I32_MUL put8(0x6c);
#define I32_EQ put8(0x46);
#define I32_OR put8(0x72);
#define I32_AND put8(0x71);
#define I32_EQZ put8(0x45);
#define I32_NE put8(0x47);
#define I32_SHL put8(0x74);
#define I32_SHR_S put8(0x75);
#define I32_SHR_U put8(0x76);
#define I32_DIV_U put8(0x6e);
#define I32_DIV_S put8(0x6d);
#define I32_REM_U put8(0x70);
#define I32_REM_S put8(0x6f);
#define I32_GE_S put8(0x4e);
#define I32_WRAP_I64 put8(0xa7);
#define I32_STORE(offset) put8(0x36); put8(0x02); putULEB128(offset, 0);
#define I64_STORE(offset) put8(0x37); put8(0x03); putULEB128(offset, 0);
#define I32_XOR put8(0x73);
#define I64_LOAD(offset) put8(0x29); put8(0x03); putULEB128(offset, 0);
#define I64_LOAD8_S(offset) put8(0x30); put8(0x00); putULEB128(offset, 0);
#define I64_LOAD8_U(offset) put8(0x31); put8(0x00); putULEB128(offset, 0);
#define I64_LOAD16_S(offset) put8(0x32); put8(0x01); putULEB128(offset, 0);
#define I64_LOAD16_U(offset) put8(0x33); put8(0x01); putULEB128(offset, 0);
#define I64_LOAD32_S(offset) put8(0x34); put8(0x02); putULEB128(offset, 0);
#define I64_LOAD32_U(offset) put8(0x35); put8(0x02); putULEB128(offset, 0);
#define I64_ADD put8(0x7c);
#define I64_AND put8(0x83);
#define I64_MUL put8(0x7e);
#define I64_OR put8(0x84);
#define I64_NE put8(0x52);
#define I64_DIV_U put8(0x80);
#define I64_DIV_S put8(0x7f);
#define I64_REM_U put8(0x82);
#define I64_REM_S put8(0x81);
#define I64_EXTEND_I32_S put8(0xac);
#define I64_EXTEND_I32_U put8(0xad);
#define I64_LT_S put8(0x53);
#define I64_LT_U put8(0x54);
#define I64_SHL put8(0x86);
#define I64_SHR_S put8(0x87);
#define I64_SHR_U put8(0x88);
#define I64_SUB put8(0x7d);
#define I64_XOR put8(0x85);
#define I64_GE_S put8(0x59);
#define I64_EQZ put8(0x50);
#define I64_CONST(i64) put8(0x42); putSLEB128(i64);

#define VOID_BLOCK put8(0x02); put8(0x40);
#define I32_BLOCK put8(0x02); put8(0x7f);
#define I64_BLOCK put8(0x02); put8(0x7e);
#define VOID_LOOP put8(0x03); put8(0x40);
#define IF put8(0x04); put8(0x40);
#define IF_I64 put8(0x04); put8(0x7e);
#define IF_I32 put8(0x04); put8(0x7f);
#define ELSE put8(0x05);
#define BR(depth) put8(0x0c); put8(depth);
#define BR_IF(depth) put8(0x0d); put8(depth);
#define END put8(0x0b);
#define SELECT put8(0x1b);
#define DROP put8(0x1a);


int afterCondition = 0;

static int execute_no_ds(int opcode) {
  DECLARE_R4300
    uint32_t decodedOpcode =   (*r4300_pc_struct(r4300))->decodedOpcode;
  if (decodedOpcode != opcode) {
    printf("Wrong opcode! executing: %s (%u); decoded: %s (%u); addr: %u; inst: %u; invalid_code=%d; recomp_status=%d\n",
           opcode_names[opcode],
           opcode,
           opcode_names[decodedOpcode],
           decodedOpcode,
           (*r4300_pc_struct(r4300))->addr,
           (*r4300_pc_struct(r4300)),
           r4300->cached_interp.invalid_code[(*r4300_pc_struct(r4300))->addr >> 12],
           (*r4300_pc_struct(r4300))->recomp_status);
  }

  return wasm_ci_table[opcode]();
}

static void generate_interpretive_function_call(enum r4300_opcode opcode) {
  
  if (afterCondition) {
    printf("generating: %s (%d)\n", opcode_names[opcode], opcode);
  }

#if WASM_DEBUG
  generate_i32_indirect_call_u32_arg((uint32_t) execute_no_ds, opcode);
#else
  generate_i32_indirect_call_no_args((uint32_t) wasm_ci_table[opcode]);
#endif

  /* if interrupt was generated */
  /* instruction if  */
  put8(0x04);
  /* if type void */
  put8(0x40);
  
  /* instruction br (break out of the block) */
  put8(0x0c);
  /*break depth (1) */
  put8(0x01);
  /* end (if) */
  put8(0x0b);
}

int isBranchInstruction(enum r4300_opcode opcode) {

  switch(opcode) {
    
  case R4300_OP_J:
  case R4300_OP_JAL:
  case R4300_OP_J_OUT:
  case R4300_OP_JAL_OUT:
  case R4300_OP_J_IDLE:
  case R4300_OP_JAL_IDLE:

      // TODO
      //uint32_t instructionAddress = (inst->addr & ~0xfffffff) | (idec_imm(iw, idec) & 0xfffffff);
      return 1;

  case R4300_OP_BC0F:
  case R4300_OP_BC0FL:
  case R4300_OP_BC0T:
  case R4300_OP_BC0TL:
  case R4300_OP_BC1F:
  case R4300_OP_BC1FL:
  case R4300_OP_BC1T:
  case R4300_OP_BC1TL:
  case R4300_OP_BC2F:
  case R4300_OP_BC2FL:
  case R4300_OP_BC2T:
  case R4300_OP_BC2TL:
  case R4300_OP_BEQ:
  case R4300_OP_BEQL:
  case R4300_OP_BGEZ:
  case R4300_OP_BGEZAL:
  case R4300_OP_BGEZALL:
  case R4300_OP_BGEZL:
  case R4300_OP_BGTZ:
  case R4300_OP_BGTZL:
  case R4300_OP_BLEZ:
  case R4300_OP_BLEZL:
  case R4300_OP_BLTZ:
  case R4300_OP_BLTZAL:
  case R4300_OP_BLTZALL:
  case R4300_OP_BLTZL:
  case R4300_OP_BNE:
  case R4300_OP_BNEL:
  case R4300_OP_BC0F_OUT:
  case R4300_OP_BC0FL_OUT:
  case R4300_OP_BC0T_OUT:
  case R4300_OP_BC0TL_OUT:
  case R4300_OP_BC1F_OUT:
  case R4300_OP_BC1FL_OUT:
  case R4300_OP_BC1T_OUT:
  case R4300_OP_BC1TL_OUT:
  case R4300_OP_BC2F_OUT:
  case R4300_OP_BC2FL_OUT:
  case R4300_OP_BC2T_OUT:
  case R4300_OP_BC2TL_OUT:
  case R4300_OP_BEQ_OUT:
  case R4300_OP_BEQL_OUT:
  case R4300_OP_BGEZ_OUT:
  case R4300_OP_BGEZAL_OUT:
  case R4300_OP_BGEZALL_OUT:
  case R4300_OP_BGEZL_OUT:
  case R4300_OP_BGTZ_OUT:
  case R4300_OP_BGTZL_OUT:
  case R4300_OP_BLEZ_OUT:
  case R4300_OP_BLEZL_OUT:
  case R4300_OP_BLTZ_OUT:
  case R4300_OP_BLTZAL_OUT:
  case R4300_OP_BLTZALL_OUT:
  case R4300_OP_BLTZL_OUT:
  case R4300_OP_BNE_OUT:
  case R4300_OP_BNEL_OUT:
  case R4300_OP_BC0F_IDLE:
  case R4300_OP_BC0FL_IDLE:
  case R4300_OP_BC0T_IDLE:
  case R4300_OP_BC0TL_IDLE:
  case R4300_OP_BC1F_IDLE:
  case R4300_OP_BC1FL_IDLE:
  case R4300_OP_BC1T_IDLE:
  case R4300_OP_BC1TL_IDLE:
  case R4300_OP_BC2F_IDLE:
  case R4300_OP_BC2FL_IDLE:
  case R4300_OP_BC2T_IDLE:
  case R4300_OP_BC2TL_IDLE:
  case R4300_OP_BEQ_IDLE:
  case R4300_OP_BEQL_IDLE:
  case R4300_OP_BGEZ_IDLE:
  case R4300_OP_BGEZAL_IDLE:
  case R4300_OP_BGEZALL_IDLE:
  case R4300_OP_BGEZL_IDLE:
  case R4300_OP_BGTZ_IDLE:
  case R4300_OP_BGTZL_IDLE:
  case R4300_OP_BLEZ_IDLE:
  case R4300_OP_BLEZL_IDLE:
  case R4300_OP_BLTZ_IDLE:
  case R4300_OP_BLTZAL_IDLE:
  case R4300_OP_BLTZALL_IDLE:
  case R4300_OP_BLTZL_IDLE:
  case R4300_OP_BNE_IDLE:
  case R4300_OP_BNEL_IDLE:

    return 1;
  }

  return 0;

}

#define DECLARE_JUMP_DECIDER(name, destination, condition, link, likely, cop1) \
  static void idle_jump_##name(void) {                         \
    DECLARE_R4300 \
      /* printf("idle_jump_%s_IDLE, destination=%d, pc=%d\n", #name, (#destination), (*r4300_pc_struct(r4300)));*/ \
    uint32_t* cp0_regs = r4300_cp0_regs(&r4300->cp0); \
    int* cp0_cycle_count = r4300_cp0_cycle_count(&r4300->cp0); \
    const int take_jump = (condition); \
    if (cop1 && check_cop1_unusable(r4300)) return;     \
    if (take_jump) \
    { \
        cp0_update_count(r4300); \
        if(*cp0_cycle_count < 0) \
        { \
            cp0_regs[CP0_COUNT_REG] -= *cp0_cycle_count; \
            *cp0_cycle_count = 0; \
        } \
    } \
    return;                                                             \
  }                                                                     \
  static void wasm_gen_##name(struct precomp_instr* inst) { \
    generate_interpretive_function_call(R4300_OP_##name);        \
  }                                                                     \
  static void wasm_gen_##name##_OUT(struct precomp_instr* inst) {       \
    generate_interpretive_function_call(R4300_OP_##name##_OUT);         \
  }                                                                     \
  static void wasm_gen_##name##_IDLE(struct precomp_instr* inst) {      \
    generate_interpretive_function_call(R4300_OP_##name##_IDLE);           \
  }


DECLARE_JUMP_DECIDER(J, (jinst_index<<2) | ((PCADDR+4) & UINT32_C(0xF0000000)), 1, &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP_DECIDER(JAL, (jinst_index<<2) | ((PCADDR+4) & UINT32_C(0xF0000000)), 1, &r4300_regs(r4300)[31], 0, 0)

DECLARE_JUMP_DECIDER(JR,   irs32, 1, &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP_DECIDER(JALR, irs32, 1, &rrd,    0, 0)

DECLARE_JUMP_DECIDER(BEQ,     PCADDR + (iimmediate+1)*4, irs == irt, &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP_DECIDER(BEQL,    PCADDR + (iimmediate+1)*4, irs == irt, &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP_DECIDER(BNE,     PCADDR + (iimmediate+1)*4, irs != irt, &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP_DECIDER(BNEL,    PCADDR + (iimmediate+1)*4, irs != irt, &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP_DECIDER(BLEZ,    PCADDR + (iimmediate+1)*4, irs <= 0,   &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP_DECIDER(BLEZL,   PCADDR + (iimmediate+1)*4, irs <= 0,   &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP_DECIDER(BGTZ,    PCADDR + (iimmediate+1)*4, irs > 0,    &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP_DECIDER(BGTZL,   PCADDR + (iimmediate+1)*4, irs > 0,    &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP_DECIDER(BLTZ,    PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[0],  0, 0)
DECLARE_JUMP_DECIDER(BLTZAL,  PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[31], 0, 0)
DECLARE_JUMP_DECIDER(BLTZL,   PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[0],  1, 0)
DECLARE_JUMP_DECIDER(BLTZALL, PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[31], 1, 0)

DECLARE_JUMP_DECIDER(BGEZ,    PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[0],  0, 0)
DECLARE_JUMP_DECIDER(BGEZAL,  PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[31], 0, 0)
DECLARE_JUMP_DECIDER(BGEZL,   PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[0],  1, 0)
DECLARE_JUMP_DECIDER(BGEZALL, PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[31], 1, 0)

DECLARE_JUMP_DECIDER(BC1F,  PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)==0, &r4300_regs(r4300)[0], 0, 1)
DECLARE_JUMP_DECIDER(BC1FL, PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)==0, &r4300_regs(r4300)[0], 1, 1)
DECLARE_JUMP_DECIDER(BC1T,  PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)!=0, &r4300_regs(r4300)[0], 0, 1)
DECLARE_JUMP_DECIDER(BC1TL, PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)!=0, &r4300_regs(r4300)[0], 1, 1)


static void wasm_gen_CP1_CVT_D() {
  // Shouldn't happen
    generate_interpretive_function_call(R4300_OP_CP1_CVT_D);
}

static void wasm_gen_CP1_CVT_S() {
    generate_interpretive_function_call(R4300_OP_CP1_CVT_S);
}


// not compiled

static void wasm_gen_RESERVED() {
    generate_interpretive_function_call(R4300_OP_RESERVED);
}

int addGenerated = 0;


// - Hard to debug issue with inlined wasm --
static void wasm_gen_SDC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDC1);
}
static void wasm_gen_BC0F(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0F);
}
static void wasm_gen_BC0F_IDLE(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_BC0F_IDLE);
}
static void wasm_gen_BC0F_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0F_OUT);
}
static void wasm_gen_BC0FL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0FL);
}
static void wasm_gen_BC0FL_IDLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0FL_IDLE);
}
static void wasm_gen_BC0FL_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0FL_OUT);
}
static void wasm_gen_BC0T(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0T);
}
static void wasm_gen_BC0T_IDLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0T_IDLE);
}
static void wasm_gen_BC0T_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0T_OUT);
}
static void wasm_gen_BC0TL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0TL);
}
static void wasm_gen_BC0TL_IDLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0TL_IDLE);
}
static void wasm_gen_BC0TL_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC0TL_OUT);
}
static void wasm_gen_BC2F(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2F);
}
static void wasm_gen_BC2F_IDLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2F_IDLE);
}
static void wasm_gen_BC2F_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2F_OUT);
}
static void wasm_gen_BC2FL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2FL);
}
static void wasm_gen_BC2FL_IDLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2FL_IDLE);
}
static void wasm_gen_BC2FL_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2FL_OUT);
}
static void wasm_gen_BC2T(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2T);
}
static void wasm_gen_BC2T_IDLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2T_IDLE);
}
static void wasm_gen_BC2T_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2T_OUT);
}
static void wasm_gen_BC2TL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2TL);
}
static void wasm_gen_BC2TL_IDLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2TL_IDLE);
}
static void wasm_gen_BC2TL_OUT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BC2TL_OUT);
}
static void wasm_gen_BREAK(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_BREAK);
}
static void wasm_gen_CFC0(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CFC0);
}
static void wasm_gen_CFC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CFC2);
}
static void wasm_gen_CP1_ABS(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_ABS);
}
static void wasm_gen_CP1_ADD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_ADD);
}
static void wasm_gen_CP1_CEIL_L(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_CEIL_L);
}
static void wasm_gen_CP1_CEIL_W(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_CEIL_W);
}
static void wasm_gen_CP1_C_EQ(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_EQ);
}
static void wasm_gen_CP1_C_F(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_F);
}
static void wasm_gen_CP1_C_LE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_LE);
}
static void wasm_gen_CP1_C_LT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_LT);
}
static void wasm_gen_CP1_C_NGE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGE);
}
static void wasm_gen_CP1_C_NGL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGL);
}
static void wasm_gen_CP1_C_NGLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGLE);
}
static void wasm_gen_CP1_C_NGT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGT);
}
static void wasm_gen_CP1_C_OLE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_OLE);
}
static void wasm_gen_CP1_C_OLT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_OLT);
}
static void wasm_gen_CP1_C_SEQ(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_SEQ);
}
static void wasm_gen_CP1_C_SF(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_SF);
}
static void wasm_gen_CP1_C_UEQ(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_UEQ);
}
static void wasm_gen_CP1_C_ULE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_ULE);
}
static void wasm_gen_CP1_C_ULT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_ULT);
}
static void wasm_gen_CP1_C_UN(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_C_UN);
}
static void wasm_gen_CP1_CVT_L(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_CVT_L);
}
static void wasm_gen_CP1_CVT_W(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_CVT_W);
}
static void wasm_gen_CP1_DIV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_DIV);
}
static void wasm_gen_CP1_FLOOR_L(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_FLOOR_L);
}
static void wasm_gen_CP1_FLOOR_W(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_FLOOR_W);
}
static void wasm_gen_CP1_MOV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_MOV);
}
static void wasm_gen_CP1_MUL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_MUL);
}
static void wasm_gen_CP1_NEG(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_NEG);
}
static void wasm_gen_CP1_ROUND_L(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_ROUND_L);
}
static void wasm_gen_CP1_ROUND_W(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_ROUND_W);
}
static void wasm_gen_CP1_SQRT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_SQRT);
}
static void wasm_gen_CP1_SUB(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_SUB);
}
static void wasm_gen_CP1_TRUNC_L(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_TRUNC_L);
}
static void wasm_gen_CP1_TRUNC_W(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CP1_TRUNC_W);
}
static void wasm_gen_CTC0(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CTC0);
}
static void wasm_gen_CTC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CTC2);
}
static void wasm_gen_DMFC0(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMFC0);
}
static void wasm_gen_DMFC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMFC2);
}
static void wasm_gen_DMTC0(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMTC0);
}
static void wasm_gen_DMTC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMTC2);
}
static void wasm_gen_ERET(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_ERET);
}
static void wasm_gen_LB(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_LB);
}
static void wasm_gen_LBU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LBU);
}
static void wasm_gen_LDC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LDC2);
}
static void wasm_gen_LDL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LDL);
}
static void wasm_gen_LDR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LDR);
}
static void wasm_gen_LH(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LH);
}
static void wasm_gen_LHU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LHU);
}
static void wasm_gen_LL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LL);
}
static void wasm_gen_LLD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LLD);
}
static void wasm_gen_LW(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LW);
}
static void wasm_gen_LWC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LWC1);
}
static void wasm_gen_LWC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LWC2);
}
static void wasm_gen_LWL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LWL);
}
static void wasm_gen_LWR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LWR);
}
static void wasm_gen_LWU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LWU);
}
static void wasm_gen_MFC0(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFC0);
}
static void wasm_gen_MFC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFC2);
}
static void wasm_gen_MTC0(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_MTC0);
}
static void wasm_gen_MTC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MTC2);
}
static void wasm_gen_SCD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SCD);
}
static void wasm_gen_SDC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDC2);
}
static void wasm_gen_SWC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWC2);
}
static void wasm_gen_SYSCALL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SYSCALL);
}
static void wasm_gen_TEQ(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TEQ);
}
static void wasm_gen_TEQI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TEQI);
}
static void wasm_gen_TGE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TGE);
}
static void wasm_gen_TGEI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TGEI);
}
static void wasm_gen_TGEIU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TGEIU);
}
static void wasm_gen_TGEU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TGEU);
}
static void wasm_gen_TLBP(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLBP);
}
static void wasm_gen_TLBR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLBR);
}
static void wasm_gen_TLBWI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLBWI);
}
static void wasm_gen_TLBWR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLBWR);
}
static void wasm_gen_TLT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLT);
}
static void wasm_gen_TLTI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLTI);
}
static void wasm_gen_TLTIU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLTIU);
}
static void wasm_gen_TLTU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TLTU);
}
static void wasm_gen_TNE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TNE);
}
static void wasm_gen_TNEI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_TNEI);
}
static void wasm_gen_CTC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CTC1);
}

/*

/********************************************************/
/*                  INLINED                             */
/********************************************************/

/*static void wasm_gen_CFC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CFC1);
}
static void wasm_gen_DDIV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DDIV);
}
static void wasm_gen_DDIVU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DDIVU);
}
static void wasm_gen_DIV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DIV);
}
static void wasm_gen_DIVU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DIVU);
}
static void wasm_gen_DMFC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMFC1);
}
*/
/*static void wasm_gen_DMTC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMTC1);
}
static void wasm_gen_LDC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LDC1);
}
static void wasm_gen_MFC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFC1);
}
static void wasm_gen_MTC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MTC1);
}
static void wasm_gen_SWC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWC1);
}
static void wasm_gen_ADD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_ADD);
}
static void wasm_gen_ADDI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_ADDI);
}
static void wasm_gen_ADDIU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_ADDIU);
}
static void wasm_gen_ADDU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_ADDU);
}
static void wasm_gen_AND(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_AND);
}
static void wasm_gen_ANDI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_ANDI);
}
static void wasm_gen_CACHE(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CACHE);
}
static void wasm_gen_DADD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DADD);
}
static void wasm_gen_DADDI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DADDI);
}
static void wasm_gen_LD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LD);
    }*/
static void wasm_gen_SDL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDL);
}
static void wasm_gen_SDR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDR);
}/*
static void wasm_gen_SH(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SH);
}
static void wasm_gen_SW(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SW);
}
static void wasm_gen_DADDIU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DADDIU);
}
static void wasm_gen_DADDU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DADDU);
}
static void wasm_gen_DSLL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSLL);
}
static void wasm_gen_DSLL32(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSLL32);
}
static void wasm_gen_DSLLV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSLLV);
}
static void wasm_gen_DSRA(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSRA);
}
static void wasm_gen_DSRA32(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSRA32);
}
static void wasm_gen_DSRAV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSRAV);
}
static void wasm_gen_DSRL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSRL);
}
static void wasm_gen_DSRL32(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSRL32);
}
static void wasm_gen_DSRLV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSRLV);
}
static void wasm_gen_DSUB(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSUB);
    }*/
static void wasm_gen_SD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SD);
}/*
static void wasm_gen_DSUBU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DSUBU);
}
static void wasm_gen_LUI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LUI);
}
static void wasm_gen_NOP(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_NOP);
}
static void wasm_gen_NOR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_NOR);
}
static void wasm_gen_OR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_OR);
}
static void wasm_gen_ORI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_ORI);
}
static void wasm_gen_SLL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SLL);
}
static void wasm_gen_SLLV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SLLV);
}
static void wasm_gen_SLT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SLT);
}
static void wasm_gen_SWL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWL);
}
static void wasm_gen_SWR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWR);
}
static void wasm_gen_DMULT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMULT);
}
static void wasm_gen_DMULTU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMULTU);
}
static void wasm_gen_MTHI(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_MTHI);
}
static void wasm_gen_MTLO(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MTLO);
}
static void wasm_gen_MULT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MULT);
}
static void wasm_gen_MULTU(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_MULTU);
}
static void wasm_gen_SB(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SB);
}
static void wasm_gen_SC(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SC);
}
static void wasm_gen_MFHI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFHI);
}
static void wasm_gen_MFLO(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFLO);
}
static void wasm_gen_SLTI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SLTI);
}
static void wasm_gen_SLTIU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SLTIU);
}
static void wasm_gen_SLTU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SLTU);
}
static void wasm_gen_SRA(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SRA);
}
static void wasm_gen_SRAV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SRAV);
}
static void wasm_gen_SRL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SRL);
}
static void wasm_gen_SRLV(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SRLV);
}
static void wasm_gen_SUB(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SUB);
}
static void wasm_gen_SUBU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SUBU);
}
static void wasm_gen_SYNC(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SYNC);
}
static void wasm_gen_XOR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_XOR);
}
static void wasm_gen_XORI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_XORI);
}
*/


static void generate_block_exit_check() {
  /* if interrupt was generated */
  /* instruction if  */
  put8(0x04);
  /* if type void */
  put8(0x40);
  
  /* instruction br (break out of the block) */
  put8(0x0c);
  /*break depth (1) */
  put8(0x01);
  /* end (if) */
  put8(0x0b);
}


#define LOAD_RRS32_VALUE I32_CONST((uint32_t) inst->f.r.rs); I32_LOAD(0); //CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I32_LOAD(0);
#define LOAD_RRS_VALUE I32_CONST((uint32_t) inst->f.r.rs); I64_LOAD(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I64_LOAD(0);
#define LOAD_RRT32_VALUE I32_CONST((uint32_t) inst->f.r.rt); I32_LOAD(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I32_LOAD(0);
#define LOAD_RRT_VALUE I32_CONST((uint32_t) inst->f.r.rt); I64_LOAD(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I64_LOAD(0);

#define LOAD_RRT_ADDRESS I32_CONST((uint32_t) inst->f.r.rt);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12);
#define LOAD_RRD_ADDRESS I32_CONST((uint32_t) inst->f.r.rd);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(16);
#define LOAD_RRS32_ADDRESS I32_CONST((uint32_t) inst->f.r.rs);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); //<- new

#define LOAD_IRS32_VALUE I32_CONST((uint32_t) inst->f.i.rs); I32_LOAD(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I32_LOAD(0);
#define LOAD_IRS_VALUE I32_CONST((uint32_t) inst->f.i.rs); I64_LOAD(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I64_LOAD(0);
#define LOAD_IRT32_VALUE I32_CONST((uint32_t) inst->f.i.rt); I32_LOAD(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I32_LOAD(0);
#define LOAD_IRT_VALUE I32_CONST((uint32_t) inst->f.i.rt); I64_LOAD(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I64_LOAD(0);

#define LOAD_IRT_ADDRESS I32_CONST((uint32_t) inst->f.i.rt);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12);
#define LOAD_IRD_ADDRESS I32_CONST((uint32_t) inst->f.i.rd);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(16);

#define LOAD_IIMMEDIATE_32S CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD16_S(16); // I32_CONST((uint32_t) inst); I32_LOAD16_S(16);
#define LOAD_IIMMEDIATE_32U CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD16_U(16);
#define LOAD_IIMMEDIATE_64S CALL_R4300_PC_STRUCT; I32_LOAD(0); I64_LOAD16_S(16);
#define LOAD_IIMMEDIATE_64U CALL_R4300_PC_STRUCT; I32_LOAD(0); I64_LOAD16_U(16);

#define LOAD_RSA32 I32_CONST((uint32_t) &inst->f.r.sa); I32_LOAD8_U(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD8_U(20);
#define LOAD_RSA I32_CONST((uint32_t) &inst->f.r.sa); I64_LOAD8_U(0);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I64_LOAD8_U(20);

#define R4300_MULT_LO generate_i32_indirect_call_u32_arg((uint32_t) r4300_mult_lo, (uint32_t) &g_dev.r4300);
#define R4300_MULT_HI generate_i32_indirect_call_u32_arg((uint32_t) r4300_mult_hi, (uint32_t) &g_dev.r4300);
#define R4300_CP0_REGS generate_i32_indirect_call_u32_arg((uint32_t) r4300_cp0_regs, (uint32_t) &g_dev.r4300.cp0);

#define R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL put8(0x41); putSLEB128(getTranslatedFunctionIndex((int) r4300_write_aligned_word)); put8(0x11); putULEB128(0x06, 0); put8(0x00);
#define R4300_WRITE_ALIGNED_DWORD_INDIRECT_CALL put8(0x41); putSLEB128(getTranslatedFunctionIndex((int) r4300_write_aligned_dword)); put8(0x11); putULEB128(0x07, 0); put8(0x00);
#define R4300_READ_ALIGNED_WORD_INDIRECT_CALL put8(0x41); putSLEB128(getTranslatedFunctionIndex((int) r4300_read_aligned_word)); put8(0x11); putULEB128(0x05, 0); put8(0x00);
#define R4300_READ_ALIGNED_DWORD_INDIRECT_CALL put8(0x41); putSLEB128(getTranslatedFunctionIndex((int) r4300_read_aligned_dword)); put8(0x11); putULEB128(0x05, 0); put8(0x00);
#define CHECK_COP1_UNUSABLE_INDIRECT_CALL put8(0x41); putSLEB128(getTranslatedFunctionIndex((int) check_cop1_unusable)); put8(0x11); putULEB128(0x03, 0); put8(0x00);

#define INCREMENT_PC_BY_ONE I32_CONST((int) &(&g_dev.r4300)->pc); I32_CONST((int) &(&g_dev.r4300)->pc); I32_LOAD(0); I32_CONST(sizeof(struct precomp_instr)); I32_ADD; I32_STORE(0);

/* *********************************************************** */
/*                                                             */
/*                        Generated                            */
/*                                                             */
/* *********************************************************** */

static void wasm_gen_ADD(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRT32_VALUE;
    LOAD_RRS32_VALUE;
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_ADDI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_IIMMEDIATE_32S;
    LOAD_IRS32_VALUE;
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_ADDIU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_IIMMEDIATE_32S;
    LOAD_IRS32_VALUE;
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_ADDU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRT32_VALUE;
    LOAD_RRS32_VALUE;
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_AND(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_AND;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_ANDI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_IRS_VALUE;
    LOAD_IIMMEDIATE_64U;
    I64_AND;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_CACHE(struct precomp_instr* inst) {


    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_CFC1(struct precomp_instr* inst) {


    I32_CONST((int) &g_dev.r4300);
    CHECK_COP1_UNUSABLE_INDIRECT_CALL;
    IF_I32;
    I32_CONST(1);
    ELSE;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD8_U(21);
    I32_CONST(31);
    I32_EQ;
    IF;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->fcr31);
    I64_LOAD32_S(0);
    I64_STORE(0);
    END;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD8_U(21);
    I32_EQZ;
    IF;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->fcr0);
    I64_LOAD32_S(0);
    I64_STORE(0);
    END;
    INCREMENT_PC_BY_ONE;
    I32_CONST(0);
    END;
    generate_block_exit_check();
    release_locals();
}
static void wasm_gen_DADD(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_ADD;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DADDI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_IIMMEDIATE_64S;
    I64_ADD;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DADDIU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_IIMMEDIATE_64S;
    I64_ADD;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DADDU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_ADD;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DDIV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I64_LOAD(0);
    LOCAL_SET(local1);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    I64_BLOCK;
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_CONST(0);
    I64_NE;
    IF;
    VOID_BLOCK;
    LOCAL_GET(local1);
    I64_CONST(-9223372036854775808);
    I64_NE;
    BR_IF(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_CONST(-1);
    I64_NE;
    BR_IF(0);
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD(0);
    I64_STORE(0);
    I64_CONST(0);
    BR(2);
    END;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_DIV_S;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_REM_S;
    BR(1);
    END;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_CONST(-1);
    I64_CONST(1);
    LOCAL_GET(local1);
    I64_CONST(0);
    I64_GE_S;
    SELECT;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD(0);
    END;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DDIVU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->hi);
    I64_BLOCK;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_CONST(0);
    I64_NE;
    IF;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_DIV_U;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_REM_U;
    BR(1);
    END;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_CONST(-1);
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD(0);
    END;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DIV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local1);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local0);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    I64_BLOCK;
    LOCAL_GET(local1);
    I32_LOAD(12);
    I32_LOAD(0);
    IF;
    VOID_BLOCK;
    LOCAL_GET(local0);
    I32_CONST(-2147483648);
    I32_NE;
    BR_IF(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I32_LOAD(0);
    I32_CONST(-1);
    I32_NE;
    BR_IF(0);
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD32_S(0);
    I64_STORE(0);
    I64_CONST(0);
    BR(2);
    END;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I32_LOAD(0);
    I32_DIV_S;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I32_LOAD(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I32_LOAD(0);
    I32_REM_S;
    I64_EXTEND_I32_S;
    BR(1);
    END;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_CONST(-1);
    I64_CONST(1);
    LOCAL_GET(local0);
    I32_CONST(0);
    I32_GE_S;
    SELECT;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD32_S(0);
    END;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DIVU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->hi);
    I32_BLOCK;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I32_LOAD(0);
    IF;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I32_LOAD(0);
    I32_DIV_U;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I32_LOAD(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(12);
    I32_LOAD(0);
    I32_REM_U;
    BR(1);
    END;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_CONST(-1);
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I32_LOAD(0);
    END;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DMFC1(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &g_dev.r4300);
    CHECK_COP1_UNUSABLE_INDIRECT_CALL;
    IF_I32;
    I32_CONST(1);
    ELSE;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(12);
    LOCAL_GET(local0);
    I32_LOAD8_U(21);
    I32_CONST(2);
    I32_SHL;
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->regs_double);
    I32_ADD;
    I32_LOAD(0);
    I64_LOAD(0);
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    I32_CONST(0);
    END;
    generate_block_exit_check();
    release_locals();
}
static void wasm_gen_DMTC1(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &g_dev.r4300);
    CHECK_COP1_UNUSABLE_INDIRECT_CALL;
    IF_I32;
    I32_CONST(1);
    ELSE;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD8_U(21);
    I32_CONST(2);
    I32_SHL;
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->regs_double);
    I32_ADD;
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    I32_CONST(0);
    END;
    generate_block_exit_check();
    release_locals();
}
static void wasm_gen_DMULT(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i64_local();
    uint32_t local3 = claim_i64_local();
    uint32_t local4 = claim_i64_local();
    uint32_t local5 = claim_i64_local();
    uint32_t local6 = claim_i64_local();
    uint32_t local7 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_CONST(0);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local7);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_TEE(local0);
    I64_SUB;
    LOCAL_GET(local0);
    LOCAL_GET(local0);
    I64_CONST(0);
    I64_LT_S;
    SELECT;
    LOCAL_TEE(local1);
    I64_CONST(4294967295);
    I64_AND;
    LOCAL_TEE(local2);
    I64_CONST(0);
    LOCAL_GET(local7);
    I32_LOAD(8);
    I64_LOAD(0);
    LOCAL_TEE(local3);
    I64_SUB;
    LOCAL_GET(local3);
    LOCAL_GET(local3);
    I64_CONST(0);
    I64_LT_S;
    SELECT;
    LOCAL_TEE(local4);
    I64_CONST(4294967295);
    I64_AND;
    LOCAL_TEE(local5);
    I64_MUL;
    LOCAL_TEE(local6);
    I64_CONST(32);
    I64_SHR_U;
    LOCAL_GET(local1);
    I64_CONST(32);
    I64_SHR_U;
    LOCAL_TEE(local1);
    LOCAL_GET(local5);
    I64_MUL;
    I64_ADD;
    LOCAL_GET(local2);
    LOCAL_GET(local4);
    I64_CONST(32);
    I64_SHR_U;
    LOCAL_TEE(local4);
    I64_MUL;
    LOCAL_TEE(local2);
    I64_CONST(4294967295);
    I64_AND;
    I64_ADD;
    LOCAL_TEE(local5);
    I64_CONST(32);
    I64_SHL;
    LOCAL_GET(local6);
    I64_CONST(4294967295);
    I64_AND;
    I64_OR;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    LOCAL_GET(local1);
    LOCAL_GET(local4);
    I64_MUL;
    LOCAL_GET(local2);
    I64_CONST(32);
    I64_SHR_U;
    I64_ADD;
    LOCAL_GET(local5);
    I64_CONST(32);
    I64_SHR_U;
    I64_ADD;
    I64_STORE(0);
    VOID_BLOCK;
    LOCAL_GET(local0);
    LOCAL_GET(local3);
    I64_XOR;
    I64_CONST(0);
    I64_GE_S;
    BR_IF(0);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    I64_LOAD(0);
    I64_CONST(-1);
    I64_XOR;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_LOAD(0);
    I64_EQZ;
    IF;
    I32_CONST((int) &(&g_dev.r4300)->hi);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    I64_LOAD(0);
    I64_CONST(1);
    I64_ADD;
    I64_STORE(0);
    BR(1);
    END;
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_CONST(0);
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_LOAD(0);
    I64_SUB;
    I64_STORE(0);
    END;
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DMULTU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i64_local();
    uint32_t local3 = claim_i64_local();
    uint32_t local4 = claim_i64_local();
    uint32_t local5 = claim_i64_local();
    uint32_t local6 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    LOCAL_TEE(local1);
    I64_LOAD(0);
    I64_CONST(32);
    I64_SHR_U;
    LOCAL_TEE(local4);
    LOCAL_GET(local0);
    I32_LOAD(12);
    LOCAL_TEE(local0);
    I64_LOAD32_U(0);
    LOCAL_TEE(local2);
    I64_MUL;
    LOCAL_TEE(local5);
    I64_CONST(4294967295);
    I64_AND;
    LOCAL_GET(local2);
    LOCAL_GET(local1);
    I64_LOAD32_U(0);
    LOCAL_TEE(local3);
    I64_MUL;
    LOCAL_TEE(local2);
    I64_CONST(32);
    I64_SHR_U;
    I64_ADD;
    LOCAL_GET(local3);
    LOCAL_GET(local0);
    I64_LOAD(0);
    I64_CONST(32);
    I64_SHR_U;
    LOCAL_TEE(local6);
    I64_MUL;
    I64_ADD;
    LOCAL_TEE(local3);
    I64_CONST(32);
    I64_SHL;
    LOCAL_GET(local2);
    I64_CONST(4294967295);
    I64_AND;
    I64_OR;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    LOCAL_GET(local4);
    LOCAL_GET(local6);
    I64_MUL;
    LOCAL_GET(local5);
    I64_CONST(32);
    I64_SHR_U;
    I64_ADD;
    LOCAL_GET(local3);
    I64_CONST(32);
    I64_SHR_U;
    I64_ADD;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSLL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I64_LOAD8_U(20);
    I64_SHL;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSLL32(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I64_LOAD8_U(20);
    I64_CONST(32);
    I64_ADD;
    I64_SHL;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSLLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(8);
    I64_LOAD32_U(0);
    I64_SHL;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSRA(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I64_LOAD8_U(20);
    I64_SHR_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSRA32(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I64_LOAD8_U(20);
    I64_CONST(32);
    I64_ADD;
    I64_SHR_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSRAV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(8);
    I64_LOAD32_U(0);
    I64_SHR_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSRL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I64_LOAD8_U(20);
    I64_SHR_U;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSRL32(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I64_LOAD8_U(20);
    I64_CONST(32);
    I64_ADD;
    I64_SHR_U;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSRLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I64_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(8);
    I64_LOAD32_U(0);
    I64_SHR_U;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSUB(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_SUB;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_DSUBU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_SUB;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_LD(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local0);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local0);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local1);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_GET(local3);
    R4300_READ_ALIGNED_DWORD_INDIRECT_CALL;
    DROP;
    
    release_locals();
}
static void wasm_gen_LDC1(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD8_U(9);
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    I32_LOAD8_U(8);
    I32_CONST(3);
    I32_SHL;
    I32_CONST((int) &g_dev.r4300);
    I32_ADD;
    I64_LOAD(0);
    LOCAL_SET(local2);
    LOCAL_GET(local0);
    I32_LOAD16_S(10);
    LOCAL_SET(local0);
    I32_CONST((int) &g_dev.r4300);
    CHECK_COP1_UNUSABLE_INDIRECT_CALL;
    IF_I32;
    I32_CONST(1);
    ELSE;
    INCREMENT_PC_BY_ONE;
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local0);
    LOCAL_GET(local2);
    I32_WRAP_I64;
    I32_ADD;
    LOCAL_GET(local1);
    I32_CONST(2);
    I32_SHL;
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->regs_double);
    I32_ADD;
    I32_LOAD(0);
    R4300_READ_ALIGNED_DWORD_INDIRECT_CALL;
    DROP;
    I32_CONST(0);
    END;
    generate_block_exit_check();
    release_locals();
}
static void wasm_gen_LUI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(12);
    LOCAL_GET(local0);
    I32_LOAD16_U(16);
    I32_CONST(16);
    I32_SHL;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_MFC1(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &g_dev.r4300);
    CHECK_COP1_UNUSABLE_INDIRECT_CALL;
    IF_I32;
    I32_CONST(1);
    ELSE;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(12);
    LOCAL_GET(local0);
    I32_LOAD8_U(21);
    I32_CONST(2);
    I32_SHL;
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->regs_simple);
    I32_ADD;
    I32_LOAD(0);
    I64_LOAD32_S(0);
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    I32_CONST(0);
    END;
    generate_block_exit_check();
    release_locals();
}
static void wasm_gen_MFHI(struct precomp_instr* inst) {


    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(16);
    I32_CONST((int) &(&g_dev.r4300)->hi);
    I64_LOAD(0);
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_MFLO(struct precomp_instr* inst) {


    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(16);
    I32_CONST((int) &(&g_dev.r4300)->lo);
    I64_LOAD(0);
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_MTC1(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &g_dev.r4300);
    CHECK_COP1_UNUSABLE_INDIRECT_CALL;
    IF_I32;
    I32_CONST(1);
    ELSE;
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD8_U(21);
    I32_CONST(2);
    I32_SHL;
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->regs_simple);
    I32_ADD;
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I32_LOAD(0);
    I32_STORE(0);
    INCREMENT_PC_BY_ONE;
    I32_CONST(0);
    END;
    generate_block_exit_check();
    release_locals();
}
static void wasm_gen_MTHI(struct precomp_instr* inst) {


    I32_CONST((int) &(&g_dev.r4300)->hi);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD(0);
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_MTLO(struct precomp_instr* inst) {


    I32_CONST((int) &(&g_dev.r4300)->lo);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    I32_LOAD(8);
    I64_LOAD(0);
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_MULT(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->hi);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(12);
    I64_LOAD32_S(0);
    LOCAL_GET(local0);
    I32_LOAD(8);
    I64_LOAD32_S(0);
    I64_MUL;
    LOCAL_TEE(local1);
    I64_CONST(32);
    I64_SHR_S;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->lo);
    LOCAL_GET(local1);
    I64_CONST(32);
    I64_SHL;
    I64_CONST(32);
    I64_SHR_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_MULTU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->hi);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(12);
    I64_LOAD32_U(0);
    LOCAL_GET(local0);
    I32_LOAD(8);
    I64_LOAD32_U(0);
    I64_MUL;
    LOCAL_TEE(local1);
    I64_CONST(32);
    I64_SHR_S;
    I64_STORE(0);
    I32_CONST((int) &(&g_dev.r4300)->lo);
    LOCAL_GET(local1);
    I64_CONST(32);
    I64_SHL;
    I64_CONST(32);
    I64_SHR_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_NOP(struct precomp_instr* inst) {


    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_NOR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_OR;
    I64_CONST(-1);
    I64_XOR;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_OR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_OR;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_ORI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_IRS_VALUE;
    LOAD_IIMMEDIATE_64U;
    I64_OR;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SB(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local1);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local0);
    LOCAL_GET(local1);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local1);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local1);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local0);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_TEE(local0);
    LOCAL_GET(local3);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_CONST(-1);
    I32_XOR;
    I32_CONST(3);
    I32_SHL;
    LOCAL_TEE(local0);
    I32_SHL;
    I32_CONST(255);
    LOCAL_GET(local0);
    I32_SHL;
    R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL;
    DROP;
    
    release_locals();
}
static void wasm_gen_SC(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local2);
    LOCAL_GET(local0);
    I32_LOAD16_S(16);
    LOCAL_SET(local3);
    LOCAL_GET(local0);
    I32_LOAD(12);
    LOCAL_SET(local1);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local0);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    VOID_BLOCK;
    LOCAL_GET(local1);
    I32_CONST(1748088);
    I32_LOAD(0);
    IF_I64;
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_ADD;
    LOCAL_GET(local1);
    I32_LOAD(0);
    I32_CONST(-1);
    R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL;
    I32_EQZ;
    BR_IF(1);
    I32_CONST(1748088);
    I32_CONST(0);
    I32_STORE(0);
    I64_CONST(1);
    ELSE;
    I64_CONST(0);
    END;
    I64_STORE(0);
    END;
    
    release_locals();
}/*
static void wasm_gen_SD(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local0);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local0);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local1);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_GET(local3);
    I64_LOAD(0);
    I64_CONST(-1);
    R4300_WRITE_ALIGNED_DWORD_INDIRECT_CALL;
    
    release_locals();
}
static void wasm_gen_SDL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();
    uint32_t local4 = claim_i64_local();
    uint32_t local5 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local1);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local2);
    LOCAL_GET(local1);
    I32_LOAD16_S(16);
    LOCAL_SET(local0);
    LOCAL_GET(local1);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local1);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I64_BLOCK;
    LOCAL_GET(local0);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_TEE(local2);
    I32_CONST(7);
    I32_AND;
    LOCAL_TEE(local0);
    I32_EQZ;
    IF;
    I64_CONST(-1);
    LOCAL_SET(local4);
    I64_CONST(0);
    BR(1);
    END;
    I64_CONST(-1);
    I32_CONST(64);
    LOCAL_GET(local0);
    I32_CONST(3);
    I32_SHL;
    LOCAL_TEE(local0);
    I32_SUB;
    I64_EXTEND_I32_U;
    I64_SHL;
    I64_CONST(-1);
    I64_XOR;
    LOCAL_SET(local4);
    LOCAL_GET(local0);
    I64_EXTEND_I32_U;
    END;
    LOCAL_SET(local5);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local2);
    I32_CONST(-8);
    I32_AND;
    LOCAL_GET(local3);
    I64_LOAD(0);
    LOCAL_GET(local5);
    I64_SHR_U;
    LOCAL_GET(local4);
    R4300_WRITE_ALIGNED_DWORD_INDIRECT_CALL;
    
    release_locals();
}
static void wasm_gen_SDR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();
    uint32_t local4 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local0);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local0);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local1);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_TEE(local1);
    I32_CONST(-8);
    I32_AND;
    LOCAL_GET(local3);
    I64_LOAD(0);
    LOCAL_GET(local1);
    I32_CONST(-1);
    I32_XOR;
    I32_CONST(3);
    I32_SHL;
    I32_CONST(56);
    I32_AND;
    I64_EXTEND_I32_U;
    LOCAL_TEE(local4);
    I64_SHL;
    I64_CONST(-1);
    LOCAL_GET(local4);
    I64_SHL;
    R4300_WRITE_ALIGNED_DWORD_INDIRECT_CALL;
    
    release_locals();
}*/
static void wasm_gen_SH(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local1);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local0);
    LOCAL_GET(local1);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local1);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local1);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local0);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_TEE(local0);
    LOCAL_GET(local3);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_CONST(-1);
    I32_XOR;
    I32_CONST(3);
    I32_SHL;
    I32_CONST(16);
    I32_AND;
    LOCAL_TEE(local0);
    I32_SHL;
    I32_CONST(65535);
    LOCAL_GET(local0);
    I32_SHL;
    R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL;
    DROP;
    
    release_locals();
}
static void wasm_gen_SLL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD8_U(20);
    I32_SHL;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SLLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRT32_VALUE;
    LOAD_RRS32_VALUE;
    I32_SHL;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SLT(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_LT_S;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SLTI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_IIMMEDIATE_64S;
    I64_LT_S;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SLTIU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_IIMMEDIATE_64S;
    I64_LT_U;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SLTU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_LT_U;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SRA(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD8_U(20);
    I32_SHR_S;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SRAV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRT32_VALUE;
    LOAD_RRS32_VALUE;
    I32_SHR_S;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SRL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(16);
    LOCAL_GET(local0);
    I32_LOAD(12);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_LOAD8_U(20);
    I32_SHR_U;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SRLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRT32_VALUE;
    LOAD_RRS32_VALUE;
    I32_SHR_U;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SUB(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS32_VALUE;
    LOAD_RRT32_VALUE;
    I32_SUB;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SUBU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS32_VALUE;
    LOAD_RRT32_VALUE;
    I32_SUB;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_SW(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local0);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local0);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local1);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_GET(local3);
    I32_LOAD(0);
    I32_CONST(-1);
    R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL;
    DROP;
    
    release_locals();
}
static void wasm_gen_SWC1(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i64_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local0);
    I32_LOAD8_U(9);
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    I32_LOAD8_U(8);
    I32_CONST(3);
    I32_SHL;
    I32_CONST((int) &g_dev.r4300);
    I32_ADD;
    I64_LOAD(0);
    LOCAL_SET(local2);
    LOCAL_GET(local0);
    I32_LOAD16_S(10);
    LOCAL_SET(local0);
    I32_CONST((int) &g_dev.r4300);
    CHECK_COP1_UNUSABLE_INDIRECT_CALL;
    IF_I32;
    I32_CONST(1);
    ELSE;
    INCREMENT_PC_BY_ONE;
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local0);
    LOCAL_GET(local2);
    I32_WRAP_I64;
    I32_ADD;
    LOCAL_GET(local1);
    I32_CONST(2);
    I32_SHL;
    I32_CONST((int)  &(&(&g_dev.r4300)->cp1)->regs_simple);
    I32_ADD;
    I32_LOAD(0);
    I32_LOAD(0);
    I32_CONST(-1);
    R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL;
    DROP;
    I32_CONST(0);
    END;
    generate_block_exit_check();
    release_locals();
}
static void wasm_gen_SWL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local1);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local0);
    LOCAL_GET(local1);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local1);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local1);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local0);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_TEE(local0);
    I32_CONST(-4);
    I32_AND;
    LOCAL_GET(local3);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_CONST(3);
    I32_AND;
    LOCAL_TEE(local0);
    I32_CONST(3);
    I32_SHL;
    LOCAL_TEE(local2);
    I32_SHR_U;
    I32_CONST(-1);
    I32_CONST(32);
    LOCAL_GET(local2);
    I32_SUB;
    I32_SHL;
    I32_CONST(-1);
    I32_XOR;
    I32_CONST(-1);
    LOCAL_GET(local0);
    SELECT;
    R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL;
    DROP;
    
    release_locals();
}
static void wasm_gen_SWR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    I32_CONST((int) &(&g_dev.r4300)->pc);
    I32_LOAD(0);
    LOCAL_TEE(local1);
    I32_LOAD(8);
    I32_LOAD(0);
    LOCAL_SET(local0);
    LOCAL_GET(local1);
    I32_LOAD16_S(16);
    LOCAL_SET(local2);
    LOCAL_GET(local1);
    I32_LOAD(12);
    LOCAL_SET(local3);
    I32_CONST((int) &(&g_dev.r4300)->pc);
    LOCAL_GET(local1);
    I32_CONST(140);
    I32_ADD;
    I32_STORE(0);
    I32_CONST((int) &g_dev.r4300);
    LOCAL_GET(local0);
    LOCAL_GET(local2);
    I32_ADD;
    LOCAL_TEE(local0);
    I32_CONST(-4);
    I32_AND;
    LOCAL_GET(local3);
    I32_LOAD(0);
    LOCAL_GET(local0);
    I32_CONST(-1);
    I32_XOR;
    I32_CONST(3);
    I32_SHL;
    LOCAL_TEE(local0);
    I32_SHL;
    I32_CONST(-1);
    LOCAL_GET(local0);
    I32_SHL;
    R4300_WRITE_ALIGNED_WORD_INDIRECT_CALL;
    DROP;
    
    release_locals();
}
static void wasm_gen_SYNC(struct precomp_instr* inst) {


    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_XOR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_RRD_ADDRESS;
    LOAD_RRS_VALUE;
    LOAD_RRT_VALUE;
    I64_XOR;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
static void wasm_gen_XORI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();

    LOAD_IRT_ADDRESS;
    LOAD_IRS_VALUE;
    LOAD_IIMMEDIATE_64U;
    I64_XOR;
    I64_STORE(0);
    INCREMENT_PC_BY_ONE;
    
    release_locals();
}
