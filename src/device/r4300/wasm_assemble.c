#include "idec.h"
#include "emscripten.h"

uint32_t printCount = 0;
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

  
  //  if (afterCondition) {
  //    printf("executing: %s (%u)\n", opcode_names[opcode], opcode);
    //  }
  //  printf("executing: %s; decoded_opcode: %s\n", opcode_names[opcode], opcode_names[decodedOpcode]);
  return wasm_ci_table[opcode]();
}

static void print_opcode(int opcode) {
  if (opcode == 199) {
    //afterCondition = 1;
  }
  //printf("exited block early after: %s (%d)\n", opcode_names[opcode], opcode);
}

static void generate_interpretive_function_call(enum r4300_opcode opcode) {

  if (afterCondition) {
    printf("generating: %s (%d)\n", opcode_names[opcode], opcode);
  }
  
  generate_i32_indirect_call_u32_arg((uint32_t) execute_no_ds, opcode);

  /* if interrupt was generated */
  /* instruction if  */
  put8(0x04);
  /* if type void */
  put8(0x40);

  generate_void_indirect_call_i32_arg((uint32_t) print_opcode, opcode);
  
  /* instruction br (break out of the block) */
  put8(0x0c);
  /*break depth (1) */
  put8(0x01);
  /* end (if) */
  put8(0x0b);
}

static void before_ds() {
    DECLARE_R4300
    r4300->delay_slot=1;
}

static void after_ds() {
    DECLARE_R4300
    cp0_update_count(r4300);
    r4300->delay_slot=0;
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


static void generate_interpretive_function_call_ds(enum r4300_opcode opcode) {

  if (afterCondition) {
    printf("generating (ds): %s (%d)\n", opcode_names[opcode], opcode);
  }

  generate_void_indirect_call_no_args((uint32_t) before_ds);

  if (isBranchInstruction(next_opcode)) {
    printf("UNEXPECTED BRANCH INSTRUCTION IN DELAY SLOT! opcode=%u\n", next_opcode);
  }
  
  gen_inst(next_inst, next_opcode, next_idec, next_iw);

  generate_void_indirect_call_no_args((uint32_t) after_ds);
}


// "compiled"


static void increment_pc() {
  DECLARE_R4300
  (*r4300_pc_struct(r4300))++;
  cp0_update_count(r4300);
}

static int interrupt_check() {
  DECLARE_R4300
  r4300->cp0.last_addr = *r4300_pc(r4300);
  if (*r4300_cp0_cycle_count(&r4300->cp0) >= 0) {
    gen_interrupt(r4300);
    return 1;
  }
  return 0;
}

uint32_t jump_target = 0;

static int cop1_unusable_check() {
  DECLARE_R4300
  if (check_cop1_unusable(r4300)) {
    return 1;
  } else {
    return 0;
  }
}

static void wasm_do_jump() {
  DECLARE_R4300
    if (!r4300->skip_jump) {
      /*      printf("jumping! jump_target=%d\n", jump_target);        */
      (*r4300_pc_struct(r4300))=r4300->cached_interp.actual->block+((jump_target-r4300->cached_interp.actual->start)>>2);
    }
  r4300->cp0.last_addr = *r4300_pc(r4300);
  if (*r4300_cp0_cycle_count(&r4300->cp0) >= 0) {
    gen_interrupt(r4300);
  }
} 

int jumpCount = 0;
#define DECLARE_JUMP_DECIDER(name, destination, condition, link, likely, cop1) \
  static int jump_decider_##name(void) { \
    DECLARE_R4300 \
      int jumpTaken = (condition); /* irs != irt */     \
    jump_target = (destination); \
    if (afterCondition) printf("jump_decider_%s! jumpTaken=%d; jump_target=%u; irs=%d; irt=%d\n", #name, jumpTaken, jump_target, irs, irt); \
    int64_t *link_register = (link);                             \
    if (link_register != &r4300_regs(r4300)[0]) { \
        *link_register = SE32(*r4300_pc(r4300) + 8); \
    } \
    (*r4300_pc_struct(r4300))++; \
    return jumpTaken; \
  } \
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
  static void do_generic_jump_##name(void) { /* TODO - Remove this? */ \
    DECLARE_R4300            \
      /*printf("do_generic_jump: %s\n", #name);                 */       \
    if (!r4300->skip_jump) {                                    \
      /*      printf("jumping ('generic')! jump_target=%d\n", jump_target); */ \
      generic_jump_to(r4300, jump_target); \
    }                                                               \
    r4300->cp0.last_addr = *r4300_pc(r4300);                            \
    if (*r4300_cp0_cycle_count(&r4300->cp0) >= 0) gen_interrupt(r4300);     \
  } \
  static void wasm_gen_##name(struct precomp_instr* inst) { \
  if (afterCondition)    printf("wasm_gen_branch: %s\n", #name);  \
    skip_next_instruction_assembly = 1; \
    /* Step 0 - cop1_unusable check */ \
    if (cop1) { \
      generate_i32_indirect_call_no_args((uint32_t) cop1_unusable_check); \
      /* if cop1_unusable */                                            \
      /* instruction if  */                                             \
      put8(0x04);                                                       \
      /* if type void */                                                \
      put8(0x40);                                                       \
      /* instruction br (break out of the block) */                     \
      put8(0x0c);                                                       \
      /*break depth (2) */                                              \
      put8(0x01);                                                       \
      /* end (if) */                                                    \
      put8(0x0b);                                                       \
    }                                                                   \    
                                                \
    /* STEP 1 - Decide if we're branching */ \
    generate_i32_indirect_call_no_args((uint32_t) jump_decider_##name);     \
                                                                        \
    /* TODO - Currently assuming delay slots can't contain other branch instructions */\
    /* instruction local.set */\
    put8(0x21); \
    /* local index */\
    put8(JUMP_TAKEN_DECISION_LOCAL_INDEX); \
                                                        \
    /* STEP 2 - (delay slot) */\
    \
                                                                \
    if (likely) { \
      /* instruction local.get */               \
      put8(0x20);                               \
      /* local index */                         \
      put8(JUMP_TAKEN_DECISION_LOCAL_INDEX);    \
\
      /* instruction if */                      \
      put8(0x04); \
      /* if type void */                        \
      put8(0x40); \
      \
      generate_interpretive_function_call_ds(next_opcode); \
      /* --- else (if jump was not taken) --- */           \
      put8(0x05); \
      generate_void_indirect_call_no_args((uint32_t) increment_pc); \
      /* endif */                                       \
      put8(0x0b);                                       \
    } else {                                                    \
      /* TODO - Verify next_opcode is valid and log if not */   \
      /* TODO - Verify next_opcode is not a branch instruction */       \
      /* generate the wasm code for the instruction */                  \
      generate_interpretive_function_call_ds(next_opcode);                 \
    }\
    \
    /* STEP 3 - take the branch and exit the block if we decided to take the branch */\
    /* instruction local.get */\
    put8(0x20); \
    /* local index */\
    put8(JUMP_TAKEN_DECISION_LOCAL_INDEX); \
                                           \
    /* --- if jump was taken --- */\
    /* instruction if  */\
    put8(0x04); \
    /* if type void */\
    put8(0x40); \
    \
    generate_void_indirect_call_no_args((uint32_t) wasm_do_jump);   \
    \
    /* instruction br (break out of the block) */\
    put8(0x0c); \
    /*break depth (1) */                            \
    put8(0x01); \
\
    /* --- else (if jump was not taken) --- */                          \
    put8(0x05);                                                         \
    generate_i32_indirect_call_no_args((uint32_t) interrupt_check); \
    /* if interrupt was generated */                                    \
    /* instruction if  */                                               \
    put8(0x04);                                                         \
    /* if type void */                                                  \
    put8(0x40);                                                         \
    /* instruction br (break out of the block) */                       \
    put8(0x0c);                                                         \
    /*break depth (2) */                                                \
    put8(0x02);                                                         \
    /* end (if) */                                                      \
    put8(0x0b);                                                         \
\
    /* end (if) */\
    put8(0x0b); \
  } \
static void wasm_gen_##name##_OUT(struct precomp_instr* inst) {       \
  /*printf("wasm_gen_OUT: %s\n", #name);       */       \
    skip_next_instruction_assembly = 1; \
                                           \
    /* Step 0 - cop1_unusable check */     \
    if (cop1) { \
      generate_i32_indirect_call_no_args((uint32_t) cop1_unusable_check); \
      /* if cop1_unusable */                                            \
      /* instruction if  */                                             \
      put8(0x04);                                                       \
      /* if type void */                                                \
      put8(0x40);                                                       \
      /* instruction br (break out of the block) */                     \
      put8(0x0c);                                                       \
      /*break depth (2) */                                              \
      put8(0x01);                                                       \
      /* end (if) */                                                    \
      put8(0x0b);                                                       \
    }                                                                   \    
                                                \
    /* STEP 1 - Decide if we're branching */ \
    generate_i32_indirect_call_no_args((uint32_t) jump_decider_##name); \
                \
                                                                        \
    /* TODO - Currently assuming delay slots can't contain other branch instructions */\
    /* instruction local.set */\
    put8(0x21); \
    /* local index */\
    put8(JUMP_TAKEN_DECISION_LOCAL_INDEX); \
                                                        \
    /* STEP 2 - (delay slot) */\
    \
                                                                \
    if (likely) { \
      /* instruction local.get */               \
      put8(0x20);                               \
      /* local index */                         \
      put8(JUMP_TAKEN_DECISION_LOCAL_INDEX);    \
\
      /* instruction if */                      \
      put8(0x04); \
      /* if type void */                        \
      put8(0x40); \
      \
      generate_interpretive_function_call_ds(next_opcode); \
      /* --- else (if jump was not taken) --- */           \
      put8(0x05); \
      generate_void_indirect_call_no_args((uint32_t) increment_pc); \
      /* endif */                                       \
      put8(0x0b);                                       \
    } else {                                                    \
      /* TODO - Verify next_opcode is valid and log if not */   \
      /* TODO - Verify next_opcode is not a branch instruction */       \
      /* generate the wasm code for the instruction */                  \
      generate_interpretive_function_call_ds(next_opcode);                 \
    }\
    \
    /* STEP 3 - take the branch and exit the block if we decided to take the branch */\
    /* instruction local.get */\
    put8(0x20); \
    /* local index */\
    put8(JUMP_TAKEN_DECISION_LOCAL_INDEX); \
                                           \
    /* --- if jump was taken --- */\
    /* instruction if  */\
    put8(0x04); \
    /* if type void */\
    put8(0x40); \
    \
    generate_void_indirect_call_no_args((uint32_t) do_generic_jump_##name); \
    \
    /* instruction br (break out of the block) */\
    put8(0x0c); \
    /*break depth (1) */                            \
    put8(0x01); \
\
    /* --- else (if jump was not taken) --- */                          \
    put8(0x05);                                                         \
    generate_i32_indirect_call_no_args((uint32_t) interrupt_check); \
    /* if interrupt was generated */                                    \
    /* instruction if  */                                               \
    put8(0x04);                                                         \
    /* if type void */                                                  \
    put8(0x40);                                                         \
    /* instruction br (break out of the block) */                       \
    put8(0x0c);                                                         \
    /*break depth (2) */                                                \
    put8(0x02);                                                         \
    /* end (if) */                                                      \
    put8(0x0b);                                                         \
    /* end (if) */                                                      \
    put8(0x0b);                                                         \
  }                                                                     \
  static void wasm_gen_##name##_IDLE(struct precomp_instr* inst) {                              \
    /*printf("wasm_gen_IDLE: %s\n", #name);                           */ \
    /* Step 0 - cop1_unusable check */                                  \
    if (cop1) {                                                         \
      generate_i32_indirect_call_no_args((uint32_t) cop1_unusable_check); \
      /* if cop1_unusable */                                            \
      /* instruction if  */                                             \
      put8(0x04);                                                       \
      /* if type void */                                                \
      put8(0x40);                                                       \
      /* instruction br (break out of the block) */                     \
      put8(0x0c);                                                       \
      /*break depth (1) */                                              \
      put8(0x01);                                                       \
      /* end (if) */                                                    \
      put8(0x0b);                                                       \
    }                                                                   \    
                                                                           \
    /* Step 1: do "##name##_IDLE */ \
                                                                           \
    generate_void_indirect_call_no_args((uint32_t) idle_jump_##name); \
                                                                        \
    /* Step 2" call regular branch */                   \
    wasm_gen_##name(inst);                          \
  }



//DECLARE_JUMP_DECIDER(JAL, (jinst_index<<2) | ((PCADDR+4) & UINT32_C(0xF0000000)), 1, &r4300_regs(r4300)[31], 1);

//DECLARE_JUMP_DECIDER(JR, irs32, 1, &r4300_regs(r4300)[0], 0);
//DECLARE_JUMP_DECIDER(BNE, PCADDR + (iimmediate+1)*4, irs != irt, &r4300_regs(r4300)[0], 0);
//DECLARE_JUMP_DECIDER(BNEL, PCADDR + (iimmediate+1)*4, irs != irt, &r4300_regs(r4300)[0], 0);
//DECLARE_JUMP_DECIDER(BEQ, PCADDR + (iimmediate+1)*4, irs == irt, &r4300_regs(r4300)[0], 0);
//DECLARE_JUMP_DECIDER(BEQL, PCADDR + (iimmediate+1)*4, irs == irt, &r4300_regs(r4300)[0], 0);

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

#define I32_CONST(i32) put8(0x41); put32SLEB128(i32);
#define I32_LOAD(offset) put8(0x28); put8(0x02); put32ULEB128(offset);
#define I32_LOAD8_S(offset) put8(0x2c); put8(0x00); put32ULEB128(offset);
#define I32_LOAD8_U(offset) put8(0x2d); put8(0x00); put32ULEB128(offset);
#define I32_LOAD16_S(offset) put8(0x2e); put8(0x01); put32ULEB128(offset);
#define I32_LOAD16_U(offset) put8(0x2f); put8(0x01); put32ULEB128(offset);
/*put8(0x10); put8(0x1);*/ // TODO - must be first function defined after the "main" one
#define CALL_R4300_PC_STRUCT put8(0x41); put32SLEB128((int) &(&g_dev.r4300)->pc);//generate_i32_indirect_call_u32_arg((uint32_t) r4300_pc_struct, (uint32_t) &g_dev.r4300);//put8(0x41); put32SLEB128((int) r4300); put8(0x41); put32SLEB128((int) r4300_pc_struct); put8(0x11); put32ULEB128(0x03); put8(0x00);
#define LOCAL_SET(index) put8(0x21); put32ULEB128(index);
#define LOCAL_GET(index) put8(0x20); put32ULEB128(index);
#define LOCAL_TEE(index) put8(0x22); put32ULEB128(index);
#define I32_ADD put8(0x6a);
#define I32_SUB put8(0x6b);
#define I32_EQ put8(0x46);
#define I32_OR put8(0x72);
#define I32_EQZ put8(0x45);
#define I32_SHL put8(0x74);
#define I32_SHR_S put8(0x75);
#define I32_SHR_U put8(0x76);
#define I32_STORE(offset) put8(0x36); put8(0x02); put32ULEB128(offset);
#define I64_STORE(offset) put8(0x37); put8(0x03); put32ULEB128(offset);
#define I32_XOR put8(0x73);
#define I64_LOAD(offset) put8(0x29); put8(0x03); put32ULEB128(offset);
#define I64_LOAD8_S(offset) put8(0x30); put8(0x00); put32ULEB128(offset);
#define I64_LOAD8_U(offset) put8(0x31); put8(0x00); put32ULEB128(offset);
#define I64_LOAD16_S(offset) put8(0x32); put8(0x01); put32ULEB128(offset);
#define I64_LOAD16_U(offset) put8(0x33); put8(0x01); put32ULEB128(offset);
#define I64_LOAD32_S(offset) put8(0x34); put8(0x02); put32ULEB128(offset);
#define I64_LOAD32_U(offset) put8(0x35); put8(0x02); put32ULEB128(offset);
#define I64_ADD put8(0x7c);
#define I64_AND put8(0x83);
#define I64_OR put8(0x84);
#define I64_EXTEND_I32_S put8(0xac);
#define I64_EXTEND_I32_U put8(0xad);
#define I64_LT_S put8(0x53);
#define I64_LT_U put8(0x54);
#define I64_SHL put8(0x86);
#define I64_SHR_S put8(0x87);
#define I64_SHR_U put8(0x88);
#define I64_SUB put8(0x7d);
#define I64_XOR put8(0x85);
#define I64_CONST(i64) put8(0x42); put32SLEB128(i64); // TODO - support for 64 bits

int addGenerated = 0;

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
static void wasm_gen_CFC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CFC1);
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
static void wasm_gen_CTC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CTC1);
}
static void wasm_gen_CTC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_CTC2);
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
static void wasm_gen_DMFC0(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMFC0);
}
static void wasm_gen_DMFC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMFC1);
}
static void wasm_gen_DMFC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMFC2);
}
static void wasm_gen_DMTC0(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMTC0);
}
static void wasm_gen_DMTC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMTC1);
}
static void wasm_gen_DMTC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMTC2);
}
static void wasm_gen_DMULT(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMULT);
}
static void wasm_gen_DMULTU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_DMULTU);
}

//TODO
static void wasm_gen_ERET(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_ERET);
}
static void wasm_gen_LB(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LB);
}
static void wasm_gen_LBU(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LBU);
}
static void wasm_gen_LD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LD);
}
static void wasm_gen_LDC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_LDC1);
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
static void wasm_gen_MFC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFC1);
}
static void wasm_gen_MFC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFC2);
}
static void wasm_gen_MFHI(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFHI);
}
static void wasm_gen_MFLO(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MFLO);
}


static void wasm_gen_MTC0(struct precomp_instr* inst) {
  generate_interpretive_function_call(R4300_OP_MTC0);
}
static void wasm_gen_MTC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MTC1);
}
static void wasm_gen_MTC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_MTC2);
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
static void wasm_gen_SCD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SCD);
}
static void wasm_gen_SD(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SD);
}
static void wasm_gen_SDC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDC1);
}
static void wasm_gen_SDC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDC2);
}
static void wasm_gen_SDL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDL);
}
static void wasm_gen_SDR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SDR);
}
static void wasm_gen_SH(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SH);
}
static void wasm_gen_SW(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SW);
}
static void wasm_gen_SWC1(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWC1);
}
static void wasm_gen_SWC2(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWC2);
}
static void wasm_gen_SWL(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWL);
}
static void wasm_gen_SWR(struct precomp_instr* inst) {
    generate_interpretive_function_call(R4300_OP_SWR);
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

/********************************************************/
/*                  Not Generated                       */
/********************************************************/

/*
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
}
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

  //generate_void_indirect_call_i32_arg((uint32_t) print_opcode, opcode);
  
  /* instruction br (break out of the block) */
  put8(0x0c);
  /*break depth (1) */
  put8(0x01);
  /* end (if) */
  put8(0x0b);
}


/*
#define LOAD_RRS32_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I32_LOAD(0);
#define LOAD_RRS_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I64_LOAD(0);
#define LOAD_RRT32_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I32_LOAD(0);
#define LOAD_RRT_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I64_LOAD(0);

#define LOAD_RRT_ADDRESS CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12);
#define LOAD_RRD_ADDRESS CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(16);
#define LOAD_RRS32_ADDRESS CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); //<- new

#define LOAD_IRS32_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I32_LOAD(0);
#define LOAD_IRS_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(8); I64_LOAD(0);
#define LOAD_IRT32_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I32_LOAD(0);
#define LOAD_IRT_VALUE CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12); I64_LOAD(0);

#define LOAD_IRT_ADDRESS CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(12);
#define LOAD_IRD_ADDRESS CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD(16);

#define LOAD_IIMMEDIATE_32S CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD16_S(16); // I32_CONST((uint32_t) inst); I32_LOAD16_S(16);
#define LOAD_IIMMEDIATE_32U CALL_R4300_PC_STRUCT; I32_LOAD(0); I32_LOAD16_U(16);
#define LOAD_IIMMEDIATE_64S CALL_R4300_PC_STRUCT; I32_LOAD(0); I64_LOAD16_S(16);
#define LOAD_IIMMEDIATE_64U CALL_R4300_PC_STRUCT; I32_LOAD(0); I64_LOAD16_U(16);

#define LOAD_RSA CALL_R4300_PC_STRUCT; I32_LOAD(0); I64_LOAD8_U(20);
*/

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

#define LOAD_RSA I32_CONST((uint32_t) inst->f.r.sa); I64_LOAD8_U(20);//CALL_R4300_PC_STRUCT; I32_LOAD(0); I64_LOAD8_U(20);


/* *********************************************************** */
/*                                                             */
/*                        Generated                            */
/*                                                             */
/* *********************************************************** */

/*
static void wasm_gen_ADD(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    // ------------  Load rrs32?    ---------------
    CALL_R4300_PC_STRUCT;
    I32_LOAD(0);
    I32_LOAD(8);
    I32_LOAD(0);
    // --------------------------------------------
    LOCAL_SET(local0);
    // ------------  Load rrt32?    ---------------
    CALL_R4300_PC_STRUCT;
    I32_LOAD(0);
    I32_LOAD(12);
    I32_LOAD(0);
    // --------------------------------------------
    LOCAL_SET(local1);
    // ------------  Get +rrd?+    ---------------
    CALL_R4300_PC_STRUCT;
    I32_LOAD(0);
    I32_LOAD(16); // Memory location to store the result (rrd?)
    // --------------------------------------------
    LOCAL_GET(local0); // rrs32?
    LOCAL_GET(local1); // rrt32?
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);

    // -------------- Increment PC ----------------
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0); // Load PC
    I32_CONST(136); // The size of a single "precomp_instr"
    I32_ADD; // Increment PC
    LOCAL_SET(local1); // set incremented PC value
    LOCAL_GET(local0); // Get location of pc_struct
    LOCAL_GET(local1); // load incremented PC value
    I32_STORE(0); // Save incremented PC
    // --------------------------------------------
    
    release_locals();
}
*/

static void wasm_gen_ADD(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRS32_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT32_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_ADDI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_IRS32_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_32S;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_ADDIU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_IRS32_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_32S;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_ADDU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRS32_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT32_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_ADD;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_AND(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_AND;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_ANDI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_IRS_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_64U;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_AND;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_CACHE(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DADD(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_ADD;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DADDI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_IRS_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_64S;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_ADD;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DADDIU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_IRS_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_64S;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_ADD;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DADDU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_ADD;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSLL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RSA;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SHL;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSLL32(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RSA;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_CONST(32);
    I64_ADD;
    I64_SHL;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSLLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RRS32_ADDRESS;
    I64_LOAD32_U(0);
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SHL;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSRA(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RSA;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SHR_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSRA32(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RSA;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_CONST(32);
    I64_ADD;
    I64_SHR_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSRAV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RRS32_ADDRESS;
    I64_LOAD32_U(0);
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SHR_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSRL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RSA;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SHR_U;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSRL32(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RSA;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_CONST(32);
    I64_ADD;
    I64_SHR_U;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSRLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRT_VALUE;
    LOCAL_SET(local0);
    LOAD_RRS32_ADDRESS;
    I64_LOAD32_U(0);
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SHR_U;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSUB(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SUB;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_DSUBU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_SUB;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_LUI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_IIMMEDIATE_32U;
    LOCAL_SET(local0);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    I32_CONST(16);
    I32_SHL;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_NOP(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_NOR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_OR;
    I64_CONST(-1);
    I64_XOR;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_OR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_OR;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_ORI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_IRS_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_64U;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_OR;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SLL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRT32_VALUE;
    LOCAL_SET(local0);
    CALL_R4300_PC_STRUCT;
    I32_LOAD(0);
    I32_LOAD8_U(20);
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SHL;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SLLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRT32_VALUE;
    LOCAL_SET(local0);
    LOAD_RRS32_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SHL;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SLT(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_LT_S;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SLTI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_IRS_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_64S;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_LT_S;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SLTIU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_IRS_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_64S;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_LT_U;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SLTU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_LT_U;
    I64_EXTEND_I32_U;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SRA(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRT32_VALUE;
    LOCAL_SET(local0);
    CALL_R4300_PC_STRUCT;
    I32_LOAD(0);
    I32_LOAD8_U(20);
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SHR_S;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SRAV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRT32_VALUE;
    LOCAL_SET(local0);
    LOAD_RRS32_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SHR_S;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SRL(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRT32_VALUE;
    LOCAL_SET(local0);
    CALL_R4300_PC_STRUCT;
    I32_LOAD(0);
    I32_LOAD8_U(20);
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SHR_U;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SRLV(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRT32_VALUE;
    LOCAL_SET(local0);
    LOAD_RRS32_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SHR_U;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SUB(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRS32_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT32_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SUB;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SUBU(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    LOAD_RRS32_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT32_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_SUB;
    I64_EXTEND_I32_S;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_SYNC(struct precomp_instr* inst) {

    uint32_t local0 = claim_i32_local();
    uint32_t local1 = claim_i32_local();

    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local0);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local1);
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_XOR(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_RRS_VALUE;
    LOCAL_SET(local0);
    LOAD_RRT_VALUE;
    LOCAL_SET(local1);
    LOAD_RRD_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_XOR;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
static void wasm_gen_XORI(struct precomp_instr* inst) {

    uint32_t local0 = claim_i64_local();
    uint32_t local1 = claim_i64_local();
    uint32_t local2 = claim_i32_local();
    uint32_t local3 = claim_i32_local();

    LOAD_IRS_VALUE;
    LOCAL_SET(local0);
    LOAD_IIMMEDIATE_64U;
    LOCAL_SET(local1);
    LOAD_IRT_ADDRESS;
    LOCAL_GET(local0);
    LOCAL_GET(local1);
    I64_XOR;
    I64_STORE(0);
    CALL_R4300_PC_STRUCT;
    LOCAL_TEE(local2);
    I32_LOAD(0);
    I32_CONST(140);
    I32_ADD;
    LOCAL_SET(local3);
    LOCAL_GET(local2);
    LOCAL_GET(local3);
    I32_STORE(0);
    
    release_locals();
}
