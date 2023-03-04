#include "idec.h"
#include "emscripten.h"

uint32_t printCount = 0;
int afterCondition = 0;

static int execute_no_ds(int opcode) {
  DECLARE_R4300
    uint32_t decodedOpcode =   (*r4300_pc_struct(r4300))->decodedOpcode;
  if (decodedOpcode != opcode) {
    printf("Wrong opcode! executing: %s (%u); decoded: %s (%u); addr: %u; ops: %u; invalid_code=%d\n", opcode_names[opcode], opcode, opcode_names[decodedOpcode], decodedOpcode, (*r4300_pc_struct(r4300))->addr,
           (*r4300_pc_struct(r4300))->ops,
           r4300->cached_interp.invalid_code[(*r4300_pc_struct(r4300))->addr >> 12]);
  }

  
  if (afterCondition) {
    printf("executing: %s (%u)\n", opcode_names[opcode], opcode);
  }
  //  printf("executing: %s; decoded_opcode: %s\n", opcode_names[opcode], opcode_names[decodedOpcode]);
  return wasm_ci_table[opcode]();
}

static void print_opcode(int opcode) {
  if (opcode == 199) {
    //afterCondition = 1;
  }
  printf("exited block early after: %s (%d)\n", opcode_names[opcode], opcode);
}

static void generate_interpretive_function_call(enum r4300_opcode opcode) {
  
  //printf("generating: %u\n", opcode);
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32SLEB128((uint32_t) opcode);
  
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32ULEB128((uint32_t) execute_no_ds);

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int arg and int return type in the types section
  put32ULEB128(0x03); //TODO
  // table index (always 0)
  put8(0x00);

  /* if interrupt was generated */
  /* instruction if  */
  put8(0x04);
  /* if type void */
  put8(0x40);

  //---
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32SLEB128((uint32_t) opcode);
  
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32ULEB128((uint32_t) print_opcode);

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int arg and int return type in the types section
  put32ULEB128(0x02); //TODO
  // table index (always 0)
  put8(0x00);
  //---
  
  /* instruction br (break out of the block) */
  put8(0x0c);
  /*break depth (1) */
  put8(0x01);
  /* end (if) */
  put8(0x0b);


  /*
  uint32_t func = (uint32_t) ci_table[opcode];
  
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32ULEB128(func);

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 0 args/parameters in the types section
  put32ULEB128(0x00);
  // table index (always 0)
  put8(0x00);
  */
}

static void execute_with_ds(int opcode) {
    DECLARE_R4300
    r4300->delay_slot=1;

    if (afterCondition) {
      printf("executing (ds) %s\n", opcode_names[opcode]);
    }
    //    if (printCount++ < 1000) {
    //    if (opcode != 205 && opcode != 4 && opcode < 253) {
    //      printf("executing (ds) %s\n", opcode_names[opcode]);
    //    }
    //    }
    wasm_ci_table[opcode]();
    cp0_update_count(r4300);
    r4300->delay_slot=0;
}

static void generate_interpretive_function_call_ds(enum r4300_opcode opcode) {

  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32SLEB128((uint32_t) opcode);
  
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32ULEB128((uint32_t) execute_with_ds);

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int arg and no parameters in the types section
  put32ULEB128(0x02); //TODO
  // table index (always 0)
  put8(0x00);
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
    printf("interrupt in branch!\n");
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
    printf("interrupt after jump!\n");
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
    printf("idle_jump_%s_IDLE, destination=%d, pc=%d\n", #name, (#destination), (*r4300_pc_struct(r4300)));  \
    uint32_t* cp0_regs = r4300_cp0_regs(&r4300->cp0); \
    int* cp0_cycle_count = r4300_cp0_cycle_count(&r4300->cp0); \
    const int take_jump = (condition); \
    /*    if (cop1 && check_cop1_unusable(r4300)) return;     */        \
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
  static void wasm_gen_##name(void) { \
    /*    printf("wasm_gen_branch: %s\n", #name);     */        \
    skip_next_instruction_assembly = 1; \
    /* Step 0 - cop1_unusable check */ \
    if (cop1) { \
      put8(0x41);                                                       \
      /* i32 literal (func) */                                          \
      put32ULEB128((uint32_t) cop1_unusable_check);                     \
      /* call_indirect */                                               \
      put8(0x11);                                                       \
      /* signature index */                                             \
      /* references the function signature with 1 return arg in the types section */ \
      put32ULEB128(0x01); /* TODO */                                    \
      /* table index (always 0) */                                      \
      put8(0x00);                                                       \
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
    /* STEP 1 - Decide if we're branching */\
    /* instruction i32.const */\
    put8(0x41);\
    /* i32 literal (func) */\
    put32ULEB128((uint32_t) jump_decider_##name); \
    /* call_indirect */\
    put8(0x11); \
    /* signature index */\
    /* references the function signature with 1 return arg in the types section */\
    put32ULEB128(0x01); /* TODO */\
    /* table index (always 0) */\
    put8(0x00); \
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
      /* instruction i32.const */               \
      put8(0x41);                                          \
      /* i32 literal (func) */                             \
      put32ULEB128((uint32_t) increment_pc);     \
      /* call_indirect */                                  \
      put8(0x11);                                          \
      /* signature index */                                             \
      /* references the function signature with no return args in the types section */ \
      put32ULEB128(0x00);                                               \
      /* table index (always 0) */                                      \
      put8(0x00);                                                       \
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
    /* instruction i32.const */\
    put8(0x41); \
    /* i32 literal (func) */\
    put32ULEB128((uint32_t) wasm_do_jump); \
    /* call_indirect */\
    put8(0x11); \
    /* signature index */\
    /* references the function signature with no return args in the types section */\
    put32ULEB128(0x00); \
    /* table index (always 0) */\
    put8(0x00); \
    \
    /* instruction br (break out of the block) */\
    put8(0x0c); \
    /*break depth (1) */                            \
    put8(0x01); \
\
    /* --- else (if jump was not taken) --- */                          \
    put8(0x05);                                                         \
    /* instruction i32.const */                                         \
    put8(0x41);                                                         \
    /* i32 literal (func) */                                            \
    put32ULEB128((uint32_t) interrupt_check);                            \
    /* call_indirect */                                                 \
    put8(0x11);                                                         \
    /* signature index */                                               \
    /* references the function signature with no return args in the types section */ \
    put32ULEB128(0x01);                                                 \
    /* table index (always 0) */                                        \
    put8(0x00);                                                         \
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
static void wasm_gen_##name##_OUT(void) {       \
  /*printf("wasm_gen_OUT: %s\n", #name);       */       \
    skip_next_instruction_assembly = 1; \
                                           \
    /* Step 0 - cop1_unusable check */     \
    if (cop1) { \
      put8(0x41);                                                       \
      /* i32 literal (func) */                                          \
      put32ULEB128((uint32_t) cop1_unusable_check);                     \
      /* call_indirect */                                               \
      put8(0x11);                                                       \
      /* signature index */                                             \
      /* references the function signature with 1 return arg in the types section */ \
      put32ULEB128(0x01); /* TODO */                                    \
      /* table index (always 0) */                                      \
      put8(0x00);                                                       \
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
    /* STEP 1 - Decide if we're branching */\
    /* instruction i32.const */\
    put8(0x41);\
    /* i32 literal (func) */\
    put32ULEB128((uint32_t) jump_decider_##name); \
    /* call_indirect */\
    put8(0x11); \
    /* signature index */\
    /* references the function signature with 1 return arg in the types section */\
    put32ULEB128(0x01); /* TODO */\
    /* table index (always 0) */\
    put8(0x00); \
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
      /* instruction i32.const */               \
      put8(0x41);                                          \
      /* i32 literal (func) */                             \
      put32ULEB128((uint32_t) increment_pc);     \
      /* call_indirect */                                  \
      put8(0x11);                                          \
      /* signature index */                                             \
      /* references the function signature with no return args in the types section */ \
      put32ULEB128(0x00);                                               \
      /* table index (always 0) */                                      \
      put8(0x00);                                                       \
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
    /* instruction i32.const */\
    put8(0x41); \
    /* i32 literal (func) */\
    put32ULEB128((uint32_t) do_generic_jump_##name); \
    /* call_indirect */\
    put8(0x11); \
    /* signature index */\
    /* references the function signature with no return args in the types section */\
    put32ULEB128(0x00); \
    /* table index (always 0) */\
    put8(0x00); \
    \
    /* instruction br (break out of the block) */\
    put8(0x0c); \
    /*break depth (1) */                            \
    put8(0x01); \
\
    /* --- else (if jump was not taken) --- */                          \
    put8(0x05);                                                         \
    /* instruction i32.const */                                         \
    put8(0x41);                                                         \
    /* i32 literal (func) */                                            \
    put32ULEB128((uint32_t) interrupt_check);                            \
    /* call_indirect */                                                 \
    put8(0x11);                                                         \
    /* signature index */                                               \
    /* references the function signature with 1 return args in the types section */ \
    put32ULEB128(0x01);                                                 \
    /* table index (always 0) */                                        \
    put8(0x00);                                                         \
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
  static void wasm_gen_##name##_IDLE(void) {                              \
    printf("wasm_gen_IDLE: %s\n", #name);                               \
    /* Step 0 - cop1_unusable check */                                  \
    if (cop1) {                                                         \
      put8(0x41);                                                       \
      /* i32 literal (func) */                                          \
      put32ULEB128((uint32_t) cop1_unusable_check);                     \
      /* call_indirect */                                               \
      put8(0x11);                                                       \
      /* signature index */                                             \
      /* references the function signature with 1 return arg in the types section */ \
      put32ULEB128(0x01); /* TODO */                                    \
      /* table index (always 0) */                                      \
      put8(0x00);                                                       \
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
    put8(0x41);                                                         \
    /* i32 literal (func) */                                            \
    put32ULEB128((uint32_t) idle_jump_##name);                          \
    /* call_indirect */                                                 \
    put8(0x11);                                                         \
    /* signature index */                                               \
    /* references the function signature with 0 return args in the types section */ \
    put32ULEB128(0x00); /* TODO */                                      \
    /* table index (always 0) */                                        \
    put8(0x00);                                                         \
                                                                        \
    /* Step 2" call regular branch */                   \
    wasm_gen_##name();                          \
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


/*
static void wasm_gen_BNE() {

  // STEP 1 - Decide if we're branching
  
  // instruction i32.const
  put8(0x41);
  // i32 literal (func)
  put32ULEB128((uint32_t) jump_decider_BNE);
  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 return arg in the types section
  put32ULEB128(0x01); // TODO
  // table index (always 0)
  put8(0x00);


  // TODO - Currently assuming delay slots can't contain other branch instructions
  // instruction local.set
  put8(0x21);
  // local index
  put8(JUMP_TAKEN_DECISION_LOCAL_INDEX);
  
  // STEP 2 - execute next instruction (delay slot)

  // TODO - Verify next_opcode is valid and log if not
  // TODO - Verify next_opcode is not a branch instruction
  // generate the wasm code for the instruction
  generate_interpretive_function_call(next_opcode);
  
  // STEP 3 - take the branch and exit the block if we decided to take the branch

  // instruction local.get
  put8(0x20);
  // local index
  put8(JUMP_TAKEN_DECISION_LOCAL_INDEX);


  // --- if jump was taken ---
  // instruction if 
  put8(0x04);
  // if type void
  put8(0x40);

  // instruction i32.const
  put8(0x41);
  // i32 literal (func)
  put32ULEB128((uint32_t) do_jump_BNE);
  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with no return args in the types section
  put32ULEB128(0x00);
  // table index (always 0)
  put8(0x00);

  // --- else (if jump was not taken) ---

  // else
  put8(0x05);
  // instruction br (break out of the block)
  put8(0x0c);
  //break depth (0)
  put8(0x00);
  // end (if)
  put8(0x0b);
  }*/

/*
  DECLARE_JUMP(J,   (jinst_index<<2) | ((PCADDR+4) & UINT32_C(0xF0000000)), 1, &r4300_regs(r4300)[0],  0, 0)
DECLARE_JUMP(JAL, (jinst_index<<2) | ((PCADDR+4) & UINT32_C(0xF0000000)), 1, &r4300_regs(r4300)[31], 0, 0)

DECLARE_JUMP(JR,   irs32, 1, &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP(JALR, irs32, 1, &rrd,    0, 0)

DECLARE_JUMP(BEQ,     PCADDR + (iimmediate+1)*4, irs == irt, &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP(BEQL,    PCADDR + (iimmediate+1)*4, irs == irt, &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP(BNE,     PCADDR + (iimmediate+1)*4, irs != irt, &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP(BNEL,    PCADDR + (iimmediate+1)*4, irs != irt, &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP(BLEZ,    PCADDR + (iimmediate+1)*4, irs <= 0,   &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP(BLEZL,   PCADDR + (iimmediate+1)*4, irs <= 0,   &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP(BGTZ,    PCADDR + (iimmediate+1)*4, irs > 0,    &r4300_regs(r4300)[0], 0, 0)
DECLARE_JUMP(BGTZL,   PCADDR + (iimmediate+1)*4, irs > 0,    &r4300_regs(r4300)[0], 1, 0)

DECLARE_JUMP(BLTZ,    PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[0],  0, 0)
DECLARE_JUMP(BLTZAL,  PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[31], 0, 0)
DECLARE_JUMP(BLTZL,   PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[0],  1, 0)
DECLARE_JUMP(BLTZALL, PCADDR + (iimmediate+1)*4, irs < 0,    &r4300_regs(r4300)[31], 1, 0)

DECLARE_JUMP(BGEZ,    PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[0],  0, 0)
DECLARE_JUMP(BGEZAL,  PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[31], 0, 0)
DECLARE_JUMP(BGEZL,   PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[0],  1, 0)
DECLARE_JUMP(BGEZALL, PCADDR + (iimmediate+1)*4, irs >= 0,   &r4300_regs(r4300)[31], 1, 0)

DECLARE_JUMP(BC1F,  PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)==0, &r4300_regs(r4300)[0], 0, 1)
DECLARE_JUMP(BC1FL, PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)==0, &r4300_regs(r4300)[0], 1, 1)
DECLARE_JUMP(BC1T,  PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)!=0, &r4300_regs(r4300)[0], 0, 1)
DECLARE_JUMP(BC1TL, PCADDR + (iimmediate+1)*4, ((*r4300_cp1_fcr31(&r4300->cp1)) & FCR31_CMP_BIT)!=0, &r4300_regs(r4300)[0], 1, 1)
*/


static void wasm_gen_indirect_call(void* func) {
  printf("wasm_gen_indirect_call: %u\n", func);
    /* instruction i32.const */
    put8(0x41);
    /* i32 literal (func) */
    put32ULEB128((uint32_t) func);
    /* call_indirect */
    put8(0x11);
    /* signature index */
    /* references the function signature with no return args in the types section */
    put32ULEB128(0x00);
    /* table index (always 0) */
    put8(0x00);
}

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
static void wasm_gen_ADD() {
    generate_interpretive_function_call(R4300_OP_ADD);
}
static void wasm_gen_ADDI() {
    generate_interpretive_function_call(R4300_OP_ADDI);
}
static void wasm_gen_ADDIU() {
    generate_interpretive_function_call(R4300_OP_ADDIU);
}
static void wasm_gen_ADDU() {
    generate_interpretive_function_call(R4300_OP_ADDU);
}
static void wasm_gen_AND() {
    generate_interpretive_function_call(R4300_OP_AND);
}
static void wasm_gen_ANDI() {
    generate_interpretive_function_call(R4300_OP_ANDI);
}
static void wasm_gen_BC0F() {
    generate_interpretive_function_call(R4300_OP_BC0F);
}
 static void wasm_gen_BC0F_IDLE() {
     generate_interpretive_function_call(R4300_OP_BC0F_IDLE);
 }
static void wasm_gen_BC0F_OUT() {
    generate_interpretive_function_call(R4300_OP_BC0F_OUT);
}
static void wasm_gen_BC0FL() {
    generate_interpretive_function_call(R4300_OP_BC0FL);
}
static void wasm_gen_BC0FL_IDLE() {
    generate_interpretive_function_call(R4300_OP_BC0FL_IDLE);
}
static void wasm_gen_BC0FL_OUT() {
    generate_interpretive_function_call(R4300_OP_BC0FL_OUT);
}
static void wasm_gen_BC0T() {
    generate_interpretive_function_call(R4300_OP_BC0T);
}
static void wasm_gen_BC0T_IDLE() {
    generate_interpretive_function_call(R4300_OP_BC0T_IDLE);
}
static void wasm_gen_BC0T_OUT() {
    generate_interpretive_function_call(R4300_OP_BC0T_OUT);
}
static void wasm_gen_BC0TL() {
    generate_interpretive_function_call(R4300_OP_BC0TL);
}
static void wasm_gen_BC0TL_IDLE() {
    generate_interpretive_function_call(R4300_OP_BC0TL_IDLE);
}
static void wasm_gen_BC0TL_OUT() {
    generate_interpretive_function_call(R4300_OP_BC0TL_OUT);
}
static void wasm_gen_BC2F() {
    generate_interpretive_function_call(R4300_OP_BC2F);
}
static void wasm_gen_BC2F_IDLE() {
    generate_interpretive_function_call(R4300_OP_BC2F_IDLE);
}
static void wasm_gen_BC2F_OUT() {
    generate_interpretive_function_call(R4300_OP_BC2F_OUT);
}
static void wasm_gen_BC2FL() {
    generate_interpretive_function_call(R4300_OP_BC2FL);
}
static void wasm_gen_BC2FL_IDLE() {
    generate_interpretive_function_call(R4300_OP_BC2FL_IDLE);
}
static void wasm_gen_BC2FL_OUT() {
    generate_interpretive_function_call(R4300_OP_BC2FL_OUT);
}
static void wasm_gen_BC2T() {
    generate_interpretive_function_call(R4300_OP_BC2T);
}
static void wasm_gen_BC2T_IDLE() {
    generate_interpretive_function_call(R4300_OP_BC2T_IDLE);
}
static void wasm_gen_BC2T_OUT() {
    generate_interpretive_function_call(R4300_OP_BC2T_OUT);
}
static void wasm_gen_BC2TL() {
    generate_interpretive_function_call(R4300_OP_BC2TL);
}
static void wasm_gen_BC2TL_IDLE() {
    generate_interpretive_function_call(R4300_OP_BC2TL_IDLE);
}
static void wasm_gen_BC2TL_OUT() {
    generate_interpretive_function_call(R4300_OP_BC2TL_OUT);
}
/* static void wasm_gen_BEQ_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BEQ_IDLE); */
/* } */
/* static void wasm_gen_BEQL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BEQL_IDLE); */
/* } */
/* static void wasm_gen_BGEZ_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BGEZ_IDLE); */
/* } */
/* static void wasm_gen_BGEZAL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BGEZAL_IDLE); */
/* } */
/* static void wasm_gen_BGEZALL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BGEZALL_IDLE); */
/* } */
/* static void wasm_gen_BGEZL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BGEZL_IDLE); */
/* } */
/* static void wasm_gen_BGTZ_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BGTZ_IDLE); */
/* } */
/* static void wasm_gen_BGTZL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BGTZL_IDLE); */
/* } */
/* static void wasm_gen_BLEZ_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BLEZ_IDLE); */
/* } */
/* static void wasm_gen_BLEZL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BLEZL_IDLE); */
/* } */
/* static void wasm_gen_BLTZ_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BLTZ_IDLE); */
/* } */
/* static void wasm_gen_BLTZAL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BLTZAL_IDLE); */
/* } */
/* static void wasm_gen_BLTZALL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BLTZALL_IDLE); */
/* } */
/* static void wasm_gen_BLTZL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BLTZL_IDLE); */
/* } */
/* static void wasm_gen_BNE_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BNE_IDLE); */
/* } */
/* static void wasm_gen_BNEL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_BNEL_IDLE); */
/* } */
static void wasm_gen_BREAK() {
    generate_interpretive_function_call(R4300_OP_BREAK);
}
static void wasm_gen_CACHE() {
    generate_interpretive_function_call(R4300_OP_CACHE);
}
static void wasm_gen_CFC0() {
    generate_interpretive_function_call(R4300_OP_CFC0);
}
static void wasm_gen_CFC1() {
    generate_interpretive_function_call(R4300_OP_CFC1);
}
static void wasm_gen_CFC2() {
    generate_interpretive_function_call(R4300_OP_CFC2);
}
static void wasm_gen_CP1_ABS() {
    generate_interpretive_function_call(R4300_OP_CP1_ABS);
}
static void wasm_gen_CP1_ADD() {
    generate_interpretive_function_call(R4300_OP_CP1_ADD);
}
static void wasm_gen_CP1_CEIL_L() {
    generate_interpretive_function_call(R4300_OP_CP1_CEIL_L);
}
static void wasm_gen_CP1_CEIL_W() {
    generate_interpretive_function_call(R4300_OP_CP1_CEIL_W);
}
static void wasm_gen_CP1_C_EQ() {
    generate_interpretive_function_call(R4300_OP_CP1_C_EQ);
}
static void wasm_gen_CP1_C_F() {
    generate_interpretive_function_call(R4300_OP_CP1_C_F);
}
static void wasm_gen_CP1_C_LE() {
    generate_interpretive_function_call(R4300_OP_CP1_C_LE);
}
static void wasm_gen_CP1_C_LT() {
    generate_interpretive_function_call(R4300_OP_CP1_C_LT);
}
static void wasm_gen_CP1_C_NGE() {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGE);
}
static void wasm_gen_CP1_C_NGL() {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGL);
}
static void wasm_gen_CP1_C_NGLE() {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGLE);
}
static void wasm_gen_CP1_C_NGT() {
    generate_interpretive_function_call(R4300_OP_CP1_C_NGT);
}
static void wasm_gen_CP1_C_OLE() {
    generate_interpretive_function_call(R4300_OP_CP1_C_OLE);
}
static void wasm_gen_CP1_C_OLT() {
    generate_interpretive_function_call(R4300_OP_CP1_C_OLT);
}
static void wasm_gen_CP1_C_SEQ() {
    generate_interpretive_function_call(R4300_OP_CP1_C_SEQ);
}
static void wasm_gen_CP1_C_SF() {
    generate_interpretive_function_call(R4300_OP_CP1_C_SF);
}
static void wasm_gen_CP1_C_UEQ() {
    generate_interpretive_function_call(R4300_OP_CP1_C_UEQ);
}
static void wasm_gen_CP1_C_ULE() {
    generate_interpretive_function_call(R4300_OP_CP1_C_ULE);
}
static void wasm_gen_CP1_C_ULT() {
    generate_interpretive_function_call(R4300_OP_CP1_C_ULT);
}
static void wasm_gen_CP1_C_UN() {
    generate_interpretive_function_call(R4300_OP_CP1_C_UN);
}
static void wasm_gen_CP1_CVT_L() {
    generate_interpretive_function_call(R4300_OP_CP1_CVT_L);
}
static void wasm_gen_CP1_CVT_W() {
    generate_interpretive_function_call(R4300_OP_CP1_CVT_W);
}
static void wasm_gen_CP1_DIV() {
    generate_interpretive_function_call(R4300_OP_CP1_DIV);
}
static void wasm_gen_CP1_FLOOR_L() {
    generate_interpretive_function_call(R4300_OP_CP1_FLOOR_L);
}
static void wasm_gen_CP1_FLOOR_W() {
    generate_interpretive_function_call(R4300_OP_CP1_FLOOR_W);
}
static void wasm_gen_CP1_MOV() {
    generate_interpretive_function_call(R4300_OP_CP1_MOV);
}
static void wasm_gen_CP1_MUL() {
    generate_interpretive_function_call(R4300_OP_CP1_MUL);
}
static void wasm_gen_CP1_NEG() {
    generate_interpretive_function_call(R4300_OP_CP1_NEG);
}
static void wasm_gen_CP1_ROUND_L() {
    generate_interpretive_function_call(R4300_OP_CP1_ROUND_L);
}
static void wasm_gen_CP1_ROUND_W() {
    generate_interpretive_function_call(R4300_OP_CP1_ROUND_W);
}
static void wasm_gen_CP1_SQRT() {
    generate_interpretive_function_call(R4300_OP_CP1_SQRT);
}
static void wasm_gen_CP1_SUB() {
    generate_interpretive_function_call(R4300_OP_CP1_SUB);
}
static void wasm_gen_CP1_TRUNC_L() {
    generate_interpretive_function_call(R4300_OP_CP1_TRUNC_L);
}
static void wasm_gen_CP1_TRUNC_W() {
    generate_interpretive_function_call(R4300_OP_CP1_TRUNC_W);
}
static void wasm_gen_CTC0() {
    generate_interpretive_function_call(R4300_OP_CTC0);
}
static void wasm_gen_CTC1() {
    generate_interpretive_function_call(R4300_OP_CTC1);
}
static void wasm_gen_CTC2() {
    generate_interpretive_function_call(R4300_OP_CTC2);
}
static void wasm_gen_DADD() {
    generate_interpretive_function_call(R4300_OP_DADD);
}
static void wasm_gen_DADDI() {
    generate_interpretive_function_call(R4300_OP_DADDI);
}
static void wasm_gen_DADDIU() {
    generate_interpretive_function_call(R4300_OP_DADDIU);
}
static void wasm_gen_DADDU() {
    generate_interpretive_function_call(R4300_OP_DADDU);
}
static void wasm_gen_DDIV() {
    generate_interpretive_function_call(R4300_OP_DDIV);
}
static void wasm_gen_DDIVU() {
    generate_interpretive_function_call(R4300_OP_DDIVU);
}
static void wasm_gen_DIV() {
    generate_interpretive_function_call(R4300_OP_DIV);
}
static void wasm_gen_DIVU() {
    generate_interpretive_function_call(R4300_OP_DIVU);
}
static void wasm_gen_DMFC0() {
    generate_interpretive_function_call(R4300_OP_DMFC0);
}
static void wasm_gen_DMFC1() {
    generate_interpretive_function_call(R4300_OP_DMFC1);
}
static void wasm_gen_DMFC2() {
    generate_interpretive_function_call(R4300_OP_DMFC2);
}
static void wasm_gen_DMTC0() {
    generate_interpretive_function_call(R4300_OP_DMTC0);
}
static void wasm_gen_DMTC1() {
    generate_interpretive_function_call(R4300_OP_DMTC1);
}
static void wasm_gen_DMTC2() {
    generate_interpretive_function_call(R4300_OP_DMTC2);
}
static void wasm_gen_DMULT() {
    generate_interpretive_function_call(R4300_OP_DMULT);
}
static void wasm_gen_DMULTU() {
    generate_interpretive_function_call(R4300_OP_DMULTU);
}
static void wasm_gen_DSLL() {
    generate_interpretive_function_call(R4300_OP_DSLL);
}
static void wasm_gen_DSLL32() {
    generate_interpretive_function_call(R4300_OP_DSLL32);
}
static void wasm_gen_DSLLV() {
    generate_interpretive_function_call(R4300_OP_DSLLV);
}
static void wasm_gen_DSRA() {
    generate_interpretive_function_call(R4300_OP_DSRA);
}
static void wasm_gen_DSRA32() {
    generate_interpretive_function_call(R4300_OP_DSRA32);
}
static void wasm_gen_DSRAV() {
    generate_interpretive_function_call(R4300_OP_DSRAV);
}
static void wasm_gen_DSRL() {
    generate_interpretive_function_call(R4300_OP_DSRL);
}
static void wasm_gen_DSRL32() {
    generate_interpretive_function_call(R4300_OP_DSRL32);
}
static void wasm_gen_DSRLV() {
    generate_interpretive_function_call(R4300_OP_DSRLV);
}
static void wasm_gen_DSUB() {
    generate_interpretive_function_call(R4300_OP_DSUB);
}
static void wasm_gen_DSUBU() {
    generate_interpretive_function_call(R4300_OP_DSUBU);
}


//TODO
static void wasm_gen_ERET() {
  generate_interpretive_function_call(R4300_OP_ERET);
  /*  printf("executing ERET!\n");

  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32ULEB128((uint32_t) execute_ERET);

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int return type and no parameters in the types section
  put32ULEB128(0x01);
  // table index (always 0)
  put8(0x00);
  */
  /* if interrupt was generated */
  /* instruction if  */
  //put8(0x04);
  /* if type void */
  //put8(0x40);
  /* instruction br (break out of the block) */
  //put8(0x0c);
  /*break depth (1) */
  //put8(0x01);
  /* end (if) */
  //put8(0x0b);
}
/* static void wasm_gen_J_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_J_IDLE); */
/* } */
/* static void wasm_gen_JAL_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_JAL_IDLE); */
/* } */
/* static void wasm_gen_JALR_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_JALR_IDLE); */
/* } */
/* static void wasm_gen_JR_IDLE() { */
/*     generate_interpretive_function_call(R4300_OP_JR_IDLE); */
/* } */
static void wasm_gen_LB() {
    generate_interpretive_function_call(R4300_OP_LB);
}
static void wasm_gen_LBU() {
    generate_interpretive_function_call(R4300_OP_LBU);
}
static void wasm_gen_LD() {
    generate_interpretive_function_call(R4300_OP_LD);
}
static void wasm_gen_LDC1() {
    generate_interpretive_function_call(R4300_OP_LDC1);
}
static void wasm_gen_LDC2() {
    generate_interpretive_function_call(R4300_OP_LDC2);
}
static void wasm_gen_LDL() {
    generate_interpretive_function_call(R4300_OP_LDL);
}
static void wasm_gen_LDR() {
    generate_interpretive_function_call(R4300_OP_LDR);
}
static void wasm_gen_LH() {
    generate_interpretive_function_call(R4300_OP_LH);
}
static void wasm_gen_LHU() {
    generate_interpretive_function_call(R4300_OP_LHU);
}
static void wasm_gen_LL() {
    generate_interpretive_function_call(R4300_OP_LL);
}
static void wasm_gen_LLD() {
    generate_interpretive_function_call(R4300_OP_LLD);
}
static void wasm_gen_LUI() {
    generate_interpretive_function_call(R4300_OP_LUI);
}
static void wasm_gen_LW() {
    generate_interpretive_function_call(R4300_OP_LW);
}
static void wasm_gen_LWC1() {
    generate_interpretive_function_call(R4300_OP_LWC1);
}
static void wasm_gen_LWC2() {
    generate_interpretive_function_call(R4300_OP_LWC2);
}
static void wasm_gen_LWL() {
    generate_interpretive_function_call(R4300_OP_LWL);
}
static void wasm_gen_LWR() {
    generate_interpretive_function_call(R4300_OP_LWR);
}
static void wasm_gen_LWU() {
    generate_interpretive_function_call(R4300_OP_LWU);
}
static void wasm_gen_MFC0() {
    generate_interpretive_function_call(R4300_OP_MFC0);
}
static void wasm_gen_MFC1() {
    generate_interpretive_function_call(R4300_OP_MFC1);
}
static void wasm_gen_MFC2() {
    generate_interpretive_function_call(R4300_OP_MFC2);
}
static void wasm_gen_MFHI() {
    generate_interpretive_function_call(R4300_OP_MFHI);
}
static void wasm_gen_MFLO() {
    generate_interpretive_function_call(R4300_OP_MFLO);
}


static void wasm_gen_MTC0() {
  printf("Generating MTC0!\n");
  generate_interpretive_function_call(R4300_OP_MTC0);
  /*
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32ULEB128((uint32_t) execute_MTC0);

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int return type and no parameters in the types section
  put32ULEB128(0x01);
  // table index (always 0)
  put8(0x00);
  */
  /* if interrupt was generated */
  /* instruction if  */
  /* put8(0x04); */
  /* /\* if type void *\/ */
  /* put8(0x40); */
  /* /\* instruction br (break out of the block) *\/ */
  /* put8(0x0c); */
  /* /\*break depth (1) *\/ */
  /* put8(0x01); */
  /* /\* end (if) *\/ */
  /* put8(0x0b); */
}
static void wasm_gen_MTC1() {
    generate_interpretive_function_call(R4300_OP_MTC1);
}
static void wasm_gen_MTC2() {
    generate_interpretive_function_call(R4300_OP_MTC2);
}
static void wasm_gen_MTHI() {
    generate_interpretive_function_call(R4300_OP_MTHI);
}
static void wasm_gen_MTLO() {
    generate_interpretive_function_call(R4300_OP_MTLO);
}
static void wasm_gen_MULT() {
    generate_interpretive_function_call(R4300_OP_MULT);
}
static void wasm_gen_MULTU() {
    generate_interpretive_function_call(R4300_OP_MULTU);
}
static void wasm_gen_NOP() {
    generate_interpretive_function_call(R4300_OP_NOP);
}
static void wasm_gen_NOR() {
    generate_interpretive_function_call(R4300_OP_NOR);
}
static void wasm_gen_OR() {
    generate_interpretive_function_call(R4300_OP_OR);
}
static void wasm_gen_ORI() {
    generate_interpretive_function_call(R4300_OP_ORI);
}
static void wasm_gen_SB() {
    generate_interpretive_function_call(R4300_OP_SB);
}
static void wasm_gen_SC() {
    generate_interpretive_function_call(R4300_OP_SC);
}
static void wasm_gen_SCD() {
    generate_interpretive_function_call(R4300_OP_SCD);
}
static void wasm_gen_SD() {
    generate_interpretive_function_call(R4300_OP_SD);
}
static void wasm_gen_SDC1() {
    generate_interpretive_function_call(R4300_OP_SDC1);
}
static void wasm_gen_SDC2() {
    generate_interpretive_function_call(R4300_OP_SDC2);
}
static void wasm_gen_SDL() {
    generate_interpretive_function_call(R4300_OP_SDL);
}
static void wasm_gen_SDR() {
    generate_interpretive_function_call(R4300_OP_SDR);
}
static void wasm_gen_SH() {
    generate_interpretive_function_call(R4300_OP_SH);
}
static void wasm_gen_SLL() {
    generate_interpretive_function_call(R4300_OP_SLL);
}
static void wasm_gen_SLLV() {
    generate_interpretive_function_call(R4300_OP_SLLV);
}
static void wasm_gen_SLT() {
    generate_interpretive_function_call(R4300_OP_SLT);
}
static void wasm_gen_SLTI() {
    generate_interpretive_function_call(R4300_OP_SLTI);
}
static void wasm_gen_SLTIU() {
    generate_interpretive_function_call(R4300_OP_SLTIU);
}
static void wasm_gen_SLTU() {
    generate_interpretive_function_call(R4300_OP_SLTU);
}
static void wasm_gen_SRA() {
    generate_interpretive_function_call(R4300_OP_SRA);
}
static void wasm_gen_SRAV() {
    generate_interpretive_function_call(R4300_OP_SRAV);
}
static void wasm_gen_SRL() {
    generate_interpretive_function_call(R4300_OP_SRL);
}
static void wasm_gen_SRLV() {
    generate_interpretive_function_call(R4300_OP_SRLV);
}
static void wasm_gen_SUB() {
    generate_interpretive_function_call(R4300_OP_SUB);
}
static void wasm_gen_SUBU() {
    generate_interpretive_function_call(R4300_OP_SUBU);
}
static void wasm_gen_SW() {
    generate_interpretive_function_call(R4300_OP_SW);
}
static void wasm_gen_SWC1() {
    generate_interpretive_function_call(R4300_OP_SWC1);
}
static void wasm_gen_SWC2() {
    generate_interpretive_function_call(R4300_OP_SWC2);
}
static void wasm_gen_SWL() {
    generate_interpretive_function_call(R4300_OP_SWL);
}
static void wasm_gen_SWR() {
    generate_interpretive_function_call(R4300_OP_SWR);
}
static void wasm_gen_SYNC() {
    generate_interpretive_function_call(R4300_OP_SYNC);
}
static void wasm_gen_SYSCALL() {
    generate_interpretive_function_call(R4300_OP_SYSCALL);
}
static void wasm_gen_TEQ() {
    generate_interpretive_function_call(R4300_OP_TEQ);
}
static void wasm_gen_TEQI() {
    generate_interpretive_function_call(R4300_OP_TEQI);
}
static void wasm_gen_TGE() {
    generate_interpretive_function_call(R4300_OP_TGE);
}
static void wasm_gen_TGEI() {
    generate_interpretive_function_call(R4300_OP_TGEI);
}
static void wasm_gen_TGEIU() {
    generate_interpretive_function_call(R4300_OP_TGEIU);
}
static void wasm_gen_TGEU() {
    generate_interpretive_function_call(R4300_OP_TGEU);
}
static void wasm_gen_TLBP() {
    generate_interpretive_function_call(R4300_OP_TLBP);
}
static void wasm_gen_TLBR() {
    generate_interpretive_function_call(R4300_OP_TLBR);
}
static void wasm_gen_TLBWI() {
    generate_interpretive_function_call(R4300_OP_TLBWI);
}
static void wasm_gen_TLBWR() {
    generate_interpretive_function_call(R4300_OP_TLBWR);
}
static void wasm_gen_TLT() {
    generate_interpretive_function_call(R4300_OP_TLT);
}
static void wasm_gen_TLTI() {
    generate_interpretive_function_call(R4300_OP_TLTI);
}
static void wasm_gen_TLTIU() {
    generate_interpretive_function_call(R4300_OP_TLTIU);
}
static void wasm_gen_TLTU() {
    generate_interpretive_function_call(R4300_OP_TLTU);
}
static void wasm_gen_TNE() {
    generate_interpretive_function_call(R4300_OP_TNE);
}
static void wasm_gen_TNEI() {
    generate_interpretive_function_call(R4300_OP_TNEI);
}
static void wasm_gen_XOR() {
    generate_interpretive_function_call(R4300_OP_XOR);
}
static void wasm_gen_XORI() {
    generate_interpretive_function_call(R4300_OP_XORI);
}
