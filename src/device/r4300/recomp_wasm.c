#include "cached_interp.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <string.h>

#include "recomp.h"
#include "api/callbacks.h"
#include "api/debugger.h"
#include "api/m64p_types.h"
#include "device/r4300/r4300_core.h"
#include "device/r4300/idec.h"
#include "main/main.h"
#include "osal/preproc.h"

int compileCount = 0;
int instructionWasInterpreted = 0;

#define WASM_OPTIMIZED_RECOMP_STATUS 4

// In jslib/corelib.js
extern void compileAndPatchModule(int block,
                                      void* moduleDataPointer,
                                      int moduleLength,
                                      void* usedFunctionsPointerArray,
                                      int numFunctionsUsed,
                                      uint32_t* recompTargetsPointer,
                                      uint32_t numRecompTargets,
                                      uint32_t instructionSize);
extern void notifyBlockAccess(uint32_t address);
extern void wasmReleaseBlock(uint32_t block);


extern uint32_t viArrived;

#ifdef DBG
#define UPDATE_DEBUGGER() if (g_DebuggerActive) update_debugger(*r4300_pc(r4300))
#else
#define UPDATE_DEBUGGER() do { } while(0)
#endif

#define DECLARE_INSTRUCTION(name) int recomp_wasm_interp_##name(void)
#define DO_RETURN(x) return x
#define DECLARE_R4300 struct r4300_core* r4300 = &g_dev.r4300;
#define PCADDR *r4300_pc(r4300)
#define ADD_TO_PC(x) (*r4300_pc_struct(r4300)) += x;
#define DECLARE_JUMP(name, destination, condition, link, likely, cop1) \
int recomp_wasm_interp_##name(void) \
{ \
    DECLARE_R4300 \
    const int take_jump = (condition); \
    /*printf("recomp_wasm_interp_%s, destination=%u, pc=%u, take_jump=%u\n", #name, (#destination), (*r4300_pc_struct(r4300)), take_jump); */ \
    int did_jump = 0; \
    const uint32_t jump_target = (destination); \
    int64_t *link_register = (link); \
    if (cop1 && check_cop1_unusable(r4300)) return 1; \
    if (link_register != &r4300_regs(r4300)[0]) \
    { \
        *link_register = SE32(*r4300_pc(r4300) + 8); \
    } \
    if (!likely || take_jump) \
    { \
        (*r4300_pc_struct(r4300))++; \
        r4300->delay_slot=1; \
        UPDATE_DEBUGGER(); \
        (*r4300_pc_struct(r4300))->ops(); \
        cp0_update_count(r4300); \
        r4300->delay_slot=0; \
        if (take_jump && !r4300->skip_jump) \
        { \
            (*r4300_pc_struct(r4300))=r4300->cached_interp.actual->block+((jump_target-r4300->cached_interp.actual->start)>>2); \
            did_jump = 1; \
        } \
    } \
    else \
    { \
        (*r4300_pc_struct(r4300)) += 2; \
        cp0_update_count(r4300); \
    } \
    r4300->cp0.last_addr = *r4300_pc(r4300); \
    if (*r4300_cp0_cycle_count(&r4300->cp0) >= 0) { \
      gen_interrupt(r4300);                         \
      return 1;                                     \
    }                                               \
    return did_jump; \
} \
 \
int recomp_wasm_interp_##name##_OUT(void) \
{ \
    DECLARE_R4300 \
    /*printf("cached_interp_%s_OUT, destination=%d, pc=%d\n", #name, (#destination), (*r4300_pc_struct(r4300))); */ \
    const int take_jump = (condition); \
    int did_jump = 0; \
    const uint32_t jump_target = (destination); \
    int64_t *link_register = (link); \
    if (cop1 && check_cop1_unusable(r4300)) return 1; \
    if (link_register != &r4300_regs(r4300)[0]) \
    { \
        *link_register = SE32(*r4300_pc(r4300) + 8); \
    } \
    if (!likely || take_jump) \
    { \
        (*r4300_pc_struct(r4300))++; \
        r4300->delay_slot=1; \
        UPDATE_DEBUGGER(); \
        (*r4300_pc_struct(r4300))->ops(); \
        cp0_update_count(r4300); \
        r4300->delay_slot=0; \
        if (take_jump && !r4300->skip_jump) \
        { \
            generic_jump_to(r4300, jump_target); \
            did_jump = 1;                        \
        } \
    } \
    else \
    { \
        (*r4300_pc_struct(r4300)) += 2; \
        cp0_update_count(r4300); \
    } \
    r4300->cp0.last_addr = *r4300_pc(r4300); \
    if (*r4300_cp0_cycle_count(&r4300->cp0) >= 0) { \
      gen_interrupt(r4300);                         \
      return 1;                                     \
    }                                               \
    return did_jump;                                \
}                                               \
                                              \
int recomp_wasm_interp_##name##_IDLE(void)    \
{ \
    DECLARE_R4300 \
    /*      printf("cached_interp_%s_IDLE, destination=%d, pc=%d\n", #name, (#destination), (*r4300_pc_struct(r4300))); */ \
    uint32_t* cp0_regs = r4300_cp0_regs(&r4300->cp0); \
    int* cp0_cycle_count = r4300_cp0_cycle_count(&r4300->cp0); \
    const int take_jump = (condition); \
    if (cop1 && check_cop1_unusable(r4300)) return 1; \
    if (take_jump) \
    { \
        cp0_update_count(r4300); \
        if(*cp0_cycle_count < 0) \
        { \
            cp0_regs[CP0_COUNT_REG] -= *cp0_cycle_count; \
            *cp0_cycle_count = 0; \
        } \
    } \
    return recomp_wasm_interp_##name();         \
}

/* These macros allow direct access to parsed opcode fields. */
#define rrt *(*r4300_pc_struct(r4300))->f.r.rt
#define rrd *(*r4300_pc_struct(r4300))->f.r.rd
#define rfs (*r4300_pc_struct(r4300))->f.r.nrd
#define rrs *(*r4300_pc_struct(r4300))->f.r.rs
#define rsa (*r4300_pc_struct(r4300))->f.r.sa
#define irt *(*r4300_pc_struct(r4300))->f.i.rt
#define ioffset (*r4300_pc_struct(r4300))->f.i.immediate
#define iimmediate (*r4300_pc_struct(r4300))->f.i.immediate
#define irs *(*r4300_pc_struct(r4300))->f.i.rs
#define ibase *(*r4300_pc_struct(r4300))->f.i.rs
#define jinst_index (*r4300_pc_struct(r4300))->f.j.inst_index
#define lfbase (*r4300_pc_struct(r4300))->f.lf.base
#define lfft (*r4300_pc_struct(r4300))->f.lf.ft
#define lfoffset (*r4300_pc_struct(r4300))->f.lf.offset
#define cfft (*r4300_pc_struct(r4300))->f.cf.ft
#define cffs (*r4300_pc_struct(r4300))->f.cf.fs
#define cffd (*r4300_pc_struct(r4300))->f.cf.fd

/* 32 bits macros */
#ifndef M64P_BIG_ENDIAN
#define rrt32 *((int32_t*) (*r4300_pc_struct(r4300))->f.r.rt)
#define rrd32 *((int32_t*) (*r4300_pc_struct(r4300))->f.r.rd)
#define rrs32 *((int32_t*) (*r4300_pc_struct(r4300))->f.r.rs)
#define irs32 *((int32_t*) (*r4300_pc_struct(r4300))->f.i.rs)
#define irt32 *((int32_t*) (*r4300_pc_struct(r4300))->f.i.rt)
#else
#define rrt32 *((int32_t*) (*r4300_pc_struct(r4300))->f.r.rt + 1)
#define rrd32 *((int32_t*) (*r4300_pc_struct(r4300))->f.r.rd + 1)
#define rrs32 *((int32_t*) (*r4300_pc_struct(r4300))->f.r.rs + 1)
#define irs32 *((int32_t*) (*r4300_pc_struct(r4300))->f.i.rs + 1)
#define irt32 *((int32_t*) (*r4300_pc_struct(r4300))->f.i.rt + 1)
#endif

#include "mips_instructions.def"

/* TODO: implement them properly */
#define recomp_wasm_interp_BC0F        recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0F_IDLE   recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0F_OUT    recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0FL       recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0FL_IDLE  recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0FL_OUT   recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0T        recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0T_IDLE   recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0T_OUT    recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0TL       recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0TL_IDLE  recomp_wasm_interp_NI
#define recomp_wasm_interp_BC0TL_OUT   recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2F        recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2F_IDLE   recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2F_OUT    recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2FL       recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2FL_IDLE  recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2FL_OUT   recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2T        recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2T_IDLE   recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2T_OUT    recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2TL       recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2TL_IDLE  recomp_wasm_interp_NI
#define recomp_wasm_interp_BC2TL_OUT   recomp_wasm_interp_NI
#define recomp_wasm_interp_BREAK       recomp_wasm_interp_NI
#define recomp_wasm_interp_CFC0        recomp_wasm_interp_NI
#define recomp_wasm_interp_CFC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_CTC0        recomp_wasm_interp_NI
#define recomp_wasm_interp_CTC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_DMFC0       recomp_wasm_interp_NI
#define recomp_wasm_interp_DMFC2       recomp_wasm_interp_NI
#define recomp_wasm_interp_DMTC0       recomp_wasm_interp_NI
#define recomp_wasm_interp_DMTC2       recomp_wasm_interp_NI
#define recomp_wasm_interp_LDC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_LWC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_LLD         recomp_wasm_interp_NI
#define recomp_wasm_interp_MFC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_MTC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_SCD         recomp_wasm_interp_NI
#define recomp_wasm_interp_SDC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_SWC2        recomp_wasm_interp_NI
#define recomp_wasm_interp_JR_IDLE     recomp_wasm_interp_NI
#define recomp_wasm_interp_JALR_IDLE   recomp_wasm_interp_NI
#define recomp_wasm_interp_CP1_ABS     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_ADD     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_CEIL_L  recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_CEIL_W  recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_EQ    recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_F     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_LE    recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_LT    recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_NGE   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_NGL   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_NGLE  recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_NGT   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_OLE   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_OLT   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_SEQ   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_SF    recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_UEQ   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_ULE   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_ULT   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_C_UN    recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_CVT_D   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_CVT_L   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_CVT_S   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_CVT_W   recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_DIV     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_FLOOR_L recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_FLOOR_W recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_MOV     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_MUL     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_NEG     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_ROUND_L recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_ROUND_W recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_SQRT    recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_SUB     recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_TRUNC_L recomp_wasm_interp_RESERVED
#define recomp_wasm_interp_CP1_TRUNC_W recomp_wasm_interp_RESERVED


#define X(op) (int (*)(void)) recomp_wasm_interp_##op
static int (*wasm_ci_table[R4300_OPCODES_COUNT])(void) =
{
    #include "opcodes.md"
};
#undef X

#define X(op) #op
static char* opcode_names[] = 
{
    #include "opcodes.md"
};
#undef X

#define X(op) 0
static int generated_before[] = 
{
    #include "opcodes.md"
};
#undef X


const uint32_t NUM_RESERVED_I32_LOCALS = 1;
const unsigned char JUMP_TAKEN_DECISION_LOCAL_INDEX = 0;
const int MAX_BYTES_FOR_32ULEB128 = 5;

// TODO - Add as a new struct in r4300?
unsigned char *wasm_code;
int max_wasm_code_length = 0;
int wasm_code_length = 0;
int reusable_wasm_boilerplate_length = 0;

// TODO - dynamically resize
uint32_t usedFunctions[1024];
int numUsedFunctions = 0;

int code_section_start = 0;
int last_function_body_start = 0;

struct precomp_instr* next_inst;
enum r4300_opcode next_opcode;
struct r4300_idec* next_idec;
uint32_t next_iw;

// TODO - Move
int custom_local_declaration_end_index = 0;
const uint32_t DEFAULT_NUM_CUSTOM_LOCAL_DECLARATIONS = 2;

struct LocalDeclaration {
  int type; // 0=i32; 1=i64;
  int numDeclaredLocals;
  int numClaimedLocals;
};

struct LocalDeclaration localDeclarations[10];
int customLocalDeclarationCount = 0;
int totalNumberOfLocals = NUM_RESERVED_I32_LOCALS;

const uint32_t MAX_RECOMP_TARGETS = 512;
struct precomp_instr* recompTargets[MAX_RECOMP_TARGETS];
uint32_t numRecompTargets = 0;

static void resizeCodeBufferIfNeeded(int wasmCodeIndex) {
  if (wasmCodeIndex > wasm_code_length) {
    wasm_code_length = wasmCodeIndex;
    if (wasm_code_length == max_wasm_code_length) {
      wasm_code = (unsigned char *) realloc_exec(wasm_code,
                                                 max_wasm_code_length,
                                                 max_wasm_code_length+8192);
      max_wasm_code_length += 8192;
    }
  }
}

static void put8(unsigned char octet)
{

//  printf("writing byte %x at position %x\n", octet, wasm_code_length);
  wasm_code[wasm_code_length] = octet;
  wasm_code_length++;
  
  if (wasm_code_length == max_wasm_code_length)
    {
      wasm_code = (unsigned char *) realloc_exec(wasm_code,
                                                 max_wasm_code_length,
                                                 max_wasm_code_length+8192);
      max_wasm_code_length += 8192;
    }
}

static void editSLEB128(long dword, uint32_t destinationByteIndex) {
  if ((destinationByteIndex > wasm_code_length)) {
    printf("invalid destinationByteIndex=%d provided to 'editSLEB128' with code_length=%d\n",
           destinationByteIndex,
           wasm_code_length);
    return;
  }

  int value = dword;
  uint32_t wasmCodeIndex = destinationByteIndex;
  int more;
  
  do {
    // Get the lowest 7 bits
    unsigned char byte = (value & 0x7F);
    value = value >> 7;

    more = !((((value == 0 ) && ((byte & 0x40) == 0)) ||
                  ((value == -1) && ((byte & 0x40) != 0))));

    if (more) {
      // Set high order bit to 1
      byte = byte | 0x80;      
    }

    //    printf("writing byte %x at position %x\n", byte, wasmCodeIndex);
    wasm_code[wasmCodeIndex++] = byte;

    resizeCodeBufferIfNeeded(wasmCodeIndex);
    
  } while (more);
}

static void editULEB128(long dword, uint32_t destinationByteIndex, int padTo) {

  if ((destinationByteIndex > wasm_code_length)) {
    printf("invalid destinationByteIndex=%d provided to 'editULEB128' with code_length=%d\n",
           destinationByteIndex,
           wasm_code_length);
    return;
  }

  int value = dword;
  uint32_t wasmCodeIndex = destinationByteIndex;
  uint32_t count = 0;

  do {
    // Get the lowest 7 bits
    unsigned char byte = (value & 0x7F);
    value = value >> 7;
    count++;

    if (value != 0 || count < padTo) {
      // Set high order bit to 1
      byte = byte | 0x80;
    }

    //    printf("writing byte %x at position %x\n", byte, wasmCodeIndex);
    wasm_code[wasmCodeIndex++] = byte;

    resizeCodeBufferIfNeeded(wasmCodeIndex);
    
  } while (value != 0);

  if (count < padTo) {
    for (; count < padTo - 1; ++count) {
      wasm_code[wasmCodeIndex++] = 0x80;

      resizeCodeBufferIfNeeded(wasmCodeIndex);
    }
    wasm_code[wasmCodeIndex++] = 0x00;

    resizeCodeBufferIfNeeded(wasmCodeIndex);
  }
}

static void putSLEB128(long dword) {
  editSLEB128(dword, wasm_code_length);
}

static void putULEB128(unsigned long dword, int padTo) {
  editULEB128(dword, wasm_code_length, padTo);
}


uint32_t numCompiledBlocks = 0;

static void generate_void_indirect_call_no_args(uint32_t func);
static void generate_void_indirect_call_i32_arg(uint32_t func, int arg);
static void generate_i32_indirect_call_u32_arg(uint32_t func, uint32_t arg);
static void generate_i32_indirect_call_no_args(uint32_t func);

static void gen_inst(struct precomp_instr* inst, enum r4300_opcode opcode, struct r4300_idec* idec, uint32_t iw);
static int claim_i32_local();
static int claim_i64_local();
static void release_locals();
static uint32_t getTranslatedFunctionIndex(uint32_t func);

#include "./wasm_assemble.c"

#define X(op) wasm_gen_##op
static void (*gen_table[R4300_OPCODES_COUNT])(struct precomp_instr*) =
{
    #include "opcodes.md"
};
#undef X

static uint32_t num_bytes_needed_for_32ULEB128(unsigned int dword) {

  int value = dword;
  uint32_t numBytesNeeded = 0;

  do {
    value = value >> 7;
    numBytesNeeded++;
  } while (value != 0); // TODO

  return numBytesNeeded;
}

static void shiftBytesOver(uint32_t startByte, uint32_t numBytesToShift) {

  //printf("shifting bytes over %d bytes starting at %x\n", numBytesToShift, startByte);
  
  if (wasm_code_length < startByte) {
    printf("Invalid startByte=%d and numBytes=%d provided to shiftBytesOver with wasm_code_length=%d\n",
           startByte,
           numBytesToShift,
           wasm_code_length);
    return;
  }

  if ((wasm_code_length + numBytesToShift) > max_wasm_code_length) {
    wasm_code = (unsigned char *) realloc_exec(wasm_code,
                                               max_wasm_code_length,
                                               max_wasm_code_length+8192);
    max_wasm_code_length += 8192;
  }
  
  int i;
  for (i = (wasm_code_length + numBytesToShift); i >= (startByte + numBytesToShift); i--) {
    if (i - numBytesToShift <= 0x3c) {
      //      printf("shifting 0x%x\n", i - numBytesToShift);
    }
    wasm_code[i] = wasm_code[i - numBytesToShift];
  }

  wasm_code_length += numBytesToShift;
}


static void generate_types_section() {
  //  printf("generate_types_section\n");
  // section code
  put8(0x01);
  // section size
  putULEB128(0x2d, 0);
  // num types
  putULEB128(0x08, 0);

  // func type 0
  put8(0x60);
  // num params
  put8(0x00);
  // num results
  put8(0x00);

  // func type 1
  put8(0x60);
  // num params
  put8(0x00);
  // num results
  put8(0x01);
  // i32
  put8(0x7f);

  // func type 2
  put8(0x60);
  // num params
  put8(0x01);
  // i32
  put8(0x7f);
  // num results
  put8(0x00);

  // func type 3
  put8(0x60);
  // num params
  put8(0x01);
  // i32
  put8(0x7f);
  // num results
  put8(0x01);
  // i32
  put8(0x7f);

  // func type 4
  put8(0x60);
  // num params
  put8(0x01);
  // i64
  put8(0x7e);
  // num results
  put8(0x01);
  // i32
  put8(0x7f);

  // func type 5 (R4300_READ_ALIGNED_WORD / R4300_READ_ALIGNED_DWORD)
  put8(0x60);
  // num params
  put8(0x03);
  // i32
  put8(0x7f);
  // i32
  put8(0x7f);
  // i32
  put8(0x7f);
  // num results
  put8(0x01);
  // i32
  put8(0x7f);

  // func type 6 (R4300_WRITE_ALIGNED_WORD)
  put8(0x60);
  // num params
  put8(0x04);
  // i32
  put8(0x7f);
  // i32
  put8(0x7f);
  // i32
  put8(0x7f);
  // i32
  put8(0x7f);
  // num results
  put8(0x01);
  // i32
  put8(0x7f);

  // func type 7 (R4300_WRITE_ALIGNED_DWORD)
  put8(0x60);
  // num params
  put8(0x04);
  // i32
  put8(0x7f);
  // i32
  put8(0x7f);
  // i64
  put8(0x7e);
  // i64
  put8(0x7e);
  // num results
  put8(0x01);
  // i32
  put8(0x7f);
}

static void generate_function_section() {

  // TODO - Factor in recomp targets  
  numUsedFunctions = 0;
  
  //  printf("generate_function_section\n");
  // section code
  put8(0x03);
  //  printf("generate_function_section\n");

  uint32_t sectionSize = num_bytes_needed_for_32ULEB128(numRecompTargets)
    + numRecompTargets;
  // section size
  putULEB128(sectionSize, 0);

  // num functions
  putULEB128(numRecompTargets, 0);

  int i;
  for (i = 0; i < numRecompTargets; i++) {
    // function i signature index
    put8(0x00);
  }
}

static void generate_imports_section() {

  // section code
  put8(0x02);
  // section size
  // TODO - generate dynamically
  putULEB128(0x1c, 0); // 28
  // num imports
  put8(0x02);

  //  printf("generate_imports_section 1\n");
  // import header 0 (__indirect_function_table)
  // string length
  putULEB128(0x03, 0);
  // "env"
  put8(0x65); put8(0x6e); put8(0x76);
  // string length for import field name
  putULEB128(0x07, 0);
  // "funcref"
  put8(0x66); put8(0x75); put8(0x6e); put8(0x63); put8(0x72); put8(0x65); put8(0x66);
  // import kind
  put8(0x01);
  // funcref
  put8(0x70);
  // limits: flags
  put8(0x00);
  // limit: initial
  put8(0x00);

  //  printf("generate_imports_section 2\n");
  // import header 1 (mem)
  // string length
  putULEB128(0x03, 0);
  // "env"
  put8(0x65); put8(0x6e); put8(0x76);
  //  printf("generate_imports_section 2\n");
  // string length
  putULEB128(0x03, 0);
  // "mem"
  put8(0x6d); put8(0x65); put8(0x6d);
  // import kind
  put8(0x02);
  // limits: flags
  put8(0x00);
  // limits: initial
  put8(0x01);
  // limits: max
  //putULEB128(0xFFFFFFFF);
}

static void generate_exports_section() {
  
  // section code
  put8(0x07);

  uint32_t sectionStart = wasm_code_length;
  // section size (guess)
  putULEB128(0x00, MAX_BYTES_FOR_32ULEB128);

  // num exports
  putULEB128(numRecompTargets, 0);
  
  int i;
  for (i = 0; i < numRecompTargets; i++) {

    uint32_t digits[32];
    uint32_t numDigits = 0;
    
    uint32_t remainder = i;
    do {
      digits[numDigits++] = (remainder % 10) + 0x30;
      remainder = remainder / 10;
    } while(remainder != 0);

    // string length
    putULEB128(1 + numDigits, 0);
    // "f"
    put8(0x66);

    int k;
    for (k = numDigits - 1; k >= 0; k--) {
      put8(digits[k]);
    }
    
    // export kind
    put8(0x00);
    // export func index
    putULEB128(i, 0);
  }

  uint32_t sectionEnd = wasm_code_length;

  uint32_t sectionSize = sectionEnd - sectionStart - MAX_BYTES_FOR_32ULEB128;
  editULEB128(sectionSize, sectionStart, MAX_BYTES_FOR_32ULEB128);
}

static void start_wasm_code_section() {
  //printf("start_wasm_code_section\n");

  code_section_start = wasm_code_length;
  
  // section code
  put8(0x0a); // 3a
  // section size (guess)
  putULEB128(0x00, MAX_BYTES_FOR_32ULEB128); // 3b
  // num functions
  putULEB128(numRecompTargets, 0); // 3c
}

static void end_wasm_code_section() {
  // FIXUP code section size
  uint32_t codeSectionByteLength = wasm_code_length - (code_section_start + 1 + MAX_BYTES_FOR_32ULEB128);

  editULEB128(codeSectionByteLength, code_section_start + 1, MAX_BYTES_FOR_32ULEB128);
}

static int claim_i32_local() {

  //  printf("claim_i32_local\n");
  // Check for existing declarations with unclaimed locals
  int i;
  int localIndex = 1;
  for (i = 0; i < customLocalDeclarationCount; i++) {
    if (localDeclarations[i].type == 0) {
      if (localDeclarations[i].numClaimedLocals
          < localDeclarations[i].numDeclaredLocals) {

        // "claim" an existing local
        return localIndex + localDeclarations[i].numClaimedLocals++;
      }
    }

    localIndex += localDeclarations[i].numDeclaredLocals;
  }


  if (customLocalDeclarationCount > 0 && localDeclarations[customLocalDeclarationCount - 1].type == 0) {
    //    printf("Adding local to previous local definition (i32)!\n");
    localDeclarations[customLocalDeclarationCount - 1].numDeclaredLocals++;
    localDeclarations[customLocalDeclarationCount - 1].numClaimedLocals++;
  } else {
    if (customLocalDeclarationCount >= 10) {
      printf("Fatal: maximum number of wasm local declarations has been met!");
      return -1;
    }
    localDeclarations[customLocalDeclarationCount].type = 0;
    localDeclarations[customLocalDeclarationCount].numDeclaredLocals = 1;
    localDeclarations[customLocalDeclarationCount].numClaimedLocals = 1;
    customLocalDeclarationCount++;
    //    printf("Added i32 localDeclaration; numDeclaredLocals=%d, numClaimedLocals=%d\n",
    //           localDeclarations[customLocalDeclarationCount-1].numDeclaredLocals,
    //           localDeclarations[customLocalDeclarationCount-1].numClaimedLocals);
  }

  return totalNumberOfLocals++;
  /*
  if (num_claimed_i32_locals >= num_declared_i32_locals) {
    num_declared_i32_locals++;
  }
  return num_claimed_i32_locals++;
  */
}

static int claim_i64_local() {

  //  printf("claim_i64_local\n");
  // Check for existing declarations with unclaimed locals
  int i;
  int localIndex = 1;
  for (i = 0; i < customLocalDeclarationCount; i++) {
    if (localDeclarations[i].type == 1) {
      if (localDeclarations[i].numClaimedLocals
          < localDeclarations[i].numDeclaredLocals) {

        // "claim" an existing local
        return localIndex + localDeclarations[i].numClaimedLocals++;

        // "claim" an existing local
        //        localDeclarations[i].numClaimedLocals++;
        //        return totalNumberOfLocals++;
      }
    }
    localIndex += localDeclarations[i].numDeclaredLocals;
  }

  if (customLocalDeclarationCount > 0 && localDeclarations[customLocalDeclarationCount - 1].type == 1) {
    //    printf("Adding local to previous local definition (i64)!\n");
    localDeclarations[customLocalDeclarationCount - 1].numDeclaredLocals++;
    localDeclarations[customLocalDeclarationCount - 1].numClaimedLocals++;
  } else {
    if (customLocalDeclarationCount >= 10) {
      printf("Fatal: maximum number of wasm local declarations has been met!");
      return -1;
    }
    localDeclarations[customLocalDeclarationCount].type = 1;
    localDeclarations[customLocalDeclarationCount].numDeclaredLocals = 1;
    localDeclarations[customLocalDeclarationCount].numClaimedLocals = 1;
    customLocalDeclarationCount++;
  }

  return totalNumberOfLocals++;

  /*  
  if (num_claimed_i64_locals >= num_declared_i64_locals) {
    num_declared_i64_locals++;
  }
  return num_claimed_i64_locals++;
  */
}

static void release_locals() {

  //  printf("release_locals\n");
  int i;
  for (i = 0; i < customLocalDeclarationCount; i++) {
    localDeclarations[i].numClaimedLocals = 0;
  }
  //  num_claimed_i32_locals = NUM_RESERVED_I32_LOCALS;
  //  num_claimed_i64_locals = 0;
}

static char getWasmLocalType(int declaredType) {
  if (declaredType == 0) { //"i32"
    return 0x7f;
  } else if (declaredType == 1) { //"i64"
    return 0x7e;
  } else {
    printf("Unknown local type: %d\n", declaredType);
    return -1;
  }
}
static void update_wasm_code_section_local_counts() {

  //  printf("customLocalDeclarationCount: %u\n", customLocalDeclarationCount);
  int numBytesPerDeclaration = 1 + MAX_BYTES_FOR_32ULEB128;

  // Add additional declarations (if needed)
  if (customLocalDeclarationCount > DEFAULT_NUM_CUSTOM_LOCAL_DECLARATIONS) {
    shiftBytesOver(custom_local_declaration_end_index, numBytesPerDeclaration * (customLocalDeclarationCount - DEFAULT_NUM_CUSTOM_LOCAL_DECLARATIONS));

    int declarationCount = customLocalDeclarationCount + 1;
    editULEB128(declarationCount, custom_local_declaration_end_index - ((DEFAULT_NUM_CUSTOM_LOCAL_DECLARATIONS + 1) * numBytesPerDeclaration) - MAX_BYTES_FOR_32ULEB128, MAX_BYTES_FOR_32ULEB128);
  }

  // Set custom declarations
  int currentEditIndex = custom_local_declaration_end_index - (DEFAULT_NUM_CUSTOM_LOCAL_DECLARATIONS * numBytesPerDeclaration);
  int i;
  for (i = 0; i < customLocalDeclarationCount; i++) {
    //printf("numDeclaredLocals[%d]=%d; type=%d; currentEditIndex=%u\n", i, localDeclarations[i].numDeclaredLocals, localDeclarations[i].type, currentEditIndex);
    editULEB128(localDeclarations[i].numDeclaredLocals, currentEditIndex, MAX_BYTES_FOR_32ULEB128);
    currentEditIndex += MAX_BYTES_FOR_32ULEB128;
    wasm_code[currentEditIndex++] = getWasmLocalType(localDeclarations[i].type);
  }
}

static void start_wasm_code_section_function_body() {
  //  printf("start_wasm_code_section_function_body\n");

  // Used to calculate the function body size later
  last_function_body_start = wasm_code_length;

  // func body size (placeholder)
  putULEB128(0, MAX_BYTES_FOR_32ULEB128); // 3d

  // # of local declarations
  putULEB128(1 + DEFAULT_NUM_CUSTOM_LOCAL_DECLARATIONS, MAX_BYTES_FOR_32ULEB128);

  // local type count
  putULEB128(0x01, MAX_BYTES_FOR_32ULEB128);
  // i32
  put8(0x7f);

  int i;
  for (i = 0; i < DEFAULT_NUM_CUSTOM_LOCAL_DECLARATIONS; i++) {
    // local count (guess);
    putULEB128(0, MAX_BYTES_FOR_32ULEB128);
    // local type ('i32' guess);
    put8(0x7f);
  }
  custom_local_declaration_end_index = wasm_code_length;

  // block instruction
  put8(0x02);
  // void (block type)
  put8(0x40);
}

static void end_wasm_code_section_function_body() {

  // block end
  put8(0x0b);
  // function end
  put8(0x0b);

  update_wasm_code_section_local_counts();

  // FIXUP function body size
  uint32_t functionBodyByteLength = wasm_code_length - (last_function_body_start + MAX_BYTES_FOR_32ULEB128);
  editULEB128(functionBodyByteLength, last_function_body_start, MAX_BYTES_FOR_32ULEB128);
}

static void generate_reusable_wasm_module_boilerplate() {
  // WASM BINARY MAGIC HEADER
  put8(0x00); put8(0x61); put8(0x73); put8(0x6d);
  // WASM BINARY VERSION
  put8(0x01); put8(0x00); put8(0x00); put8(0x00);
  
  generate_types_section();
  generate_imports_section();
  //generate_function_section();
  //generate_exports_section();
}

static void init_wasm_module_code() {

  if (max_wasm_code_length == 0) {

    // Arbitrary. Can be increased
    max_wasm_code_length = 32768; 
    wasm_code = malloc(max_wasm_code_length);

    generate_reusable_wasm_module_boilerplate();
    reusable_wasm_boilerplate_length = wasm_code_length;
  } else {
    wasm_code_length = reusable_wasm_boilerplate_length;
  }

  customLocalDeclarationCount = 0;
  totalNumberOfLocals = NUM_RESERVED_I32_LOCALS;
}

// idec->opcode may not necessarily be == opcode
static void gen_inst(struct precomp_instr* inst, enum r4300_opcode opcode, struct r4300_idec* idec, uint32_t iw) {

  uint8_t dummy;
  
  switch(opcode) {

  case R4300_OP_CP1_CVT_D:
    idec_u53(iw, idec->u53[3], &dummy);
    switch(dummy) {
    case 0x10:
      generate_void_indirect_call_no_args((uint32_t) cached_interp_CVT_D_S);
      break;
    case 0x14:
      generate_void_indirect_call_no_args((uint32_t) cached_interp_CVT_D_W);
      break;
    case 0x15:
      generate_void_indirect_call_no_args((uint32_t) cached_interp_CVT_D_L);
      break;
    default: wasm_gen_CP1_CVT_D(inst);
    }
    break;
  case R4300_OP_CP1_CVT_S:
    idec_u53(iw, idec->u53[3], &dummy);
    switch(dummy) {
    case 0x11:
      generate_void_indirect_call_no_args((uint32_t) cached_interp_CVT_S_D);
      break;
    case 0x14:
      generate_void_indirect_call_no_args((uint32_t) cached_interp_CVT_S_W);
      break;
    case 0x15:
      generate_void_indirect_call_no_args((uint32_t) cached_interp_CVT_S_L);
      break;
    default:
      wasm_gen_CP1_CVT_S(inst);
      break;
    }
    break;

#define CP1_S_D(op)                                                     \
    case R4300_OP_CP1_##op:                                             \
      idec_u53(iw, idec->u53[3], &dummy);                               \
      switch(dummy)                                                     \
        {                                                               \
        case 0x10: \
          generate_void_indirect_call_no_args((uint32_t) cached_interp_##op##_S);               \
          break; \
        case 0x11: \
          generate_void_indirect_call_no_args((uint32_t) cached_interp_##op##_D); \
          break; \
        default: wasm_gen_CP1_##op(inst);                                         \
        }                                                               \
      break;

    CP1_S_D(ABS)
    CP1_S_D(ADD)
    CP1_S_D(CEIL_L)
    CP1_S_D(CEIL_W)
    CP1_S_D(C_EQ)
    CP1_S_D(C_F)
    CP1_S_D(C_LE)
    CP1_S_D(C_LT)
    CP1_S_D(C_NGE)
    CP1_S_D(C_NGL)
    CP1_S_D(C_NGLE)
    CP1_S_D(C_NGT)
    CP1_S_D(C_OLE)
    CP1_S_D(C_OLT)
    CP1_S_D(C_SEQ)
    CP1_S_D(C_SF)
    CP1_S_D(C_UEQ)
    CP1_S_D(C_ULE)
    CP1_S_D(C_ULT)
    CP1_S_D(C_UN)
    CP1_S_D(CVT_L)
    CP1_S_D(CVT_W)
    CP1_S_D(DIV)
    CP1_S_D(FLOOR_L)
    CP1_S_D(FLOOR_W)
    CP1_S_D(MOV)
    CP1_S_D(MUL)
    CP1_S_D(NEG)
    CP1_S_D(ROUND_L)
    CP1_S_D(ROUND_W)
    CP1_S_D(SQRT)
    CP1_S_D(SUB)
    CP1_S_D(TRUNC_L)
    CP1_S_D(TRUNC_W)
#undef CP1_S_D

  default: {
      //      printf("generating: %s\n", opcode_names[opcode]);
      //      if (!generated_before[opcode]) {
        //printf("Generating %s (%u_\n", opcode_names[opcode], opcode);
        //generated_before[opcode] = 1;
      //}

    gen_table[opcode](inst);
    break;
  }
  }
}

/*
static void block_access_check(uint32_t addr) {
  //  printf("block_access: %u", addr);
  notifyBlockAccess(addr);
}
*/

static uint32_t getTranslatedFunctionIndex(uint32_t func) {

  //  printf("getTranslatedFunctionIndex; func=%u; usedFunctions[0]=%u; numUsedFunctions=%u\n", func, usedFunctions[0], numUsedFunctions);
  int i;
  for (i = 0; i < numUsedFunctions; i++) {
    if (usedFunctions[i] == func) {
      return i;
    }
  }

  usedFunctions[numUsedFunctions] = func;
  //  printf("getTranslatedFunctionIndex2; func=%u; usedFunctions[0]=%u; numUsedFunctions=%u\n", func, usedFunctions[0], numUsedFunctions);
    
  return numUsedFunctions++;
}

static void generate_void_indirect_call_no_args(uint32_t func) {

  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  putSLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  putULEB128(0x00, 0); // TODO
  // table index (always 0)
  put8(0x00);
}

static void generate_void_indirect_call_i32_arg(uint32_t func, int arg) {
  // instruction i32.const
  put8(0x41);
  // i32 literal (arg)
  putSLEB128(arg);
  
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  putSLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 params and 0 results
  putULEB128(0x02, 0);
  // table index (always 0)
  put8(0x00);
}

// TODO - Combine with above?
static void generate_i32_indirect_call_u32_arg(uint32_t func, uint32_t arg) {
  // instruction i32.const
  put8(0x41);
  // i32 literal
  putSLEB128(arg);

  // instruction i32.const
  put8(0x41);
  // i32 literal (func)
  putSLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 i32 arg and int return type in the types section
  putULEB128(0x03, 0); //TODO
  // table index (always 0)
  put8(0x00);
}

static void generate_i32_indirect_call_no_args(uint32_t func) {
  // instruction i32.const
  put8(0x41);
  // i32 literal (func)
  putSLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int arg and int return type in the types section
  putULEB128(0x01, 0); //TODO
  // table index (always 0)
  put8(0x00);
}

/*
void generate_block_access_check(uint32_t codeBlockAddress) {
  generate_void_indirect_call_i32_arg((uint32_t) block_access_check, codeBlockAddress);
}
*/


void recomp_wasm_init_block(struct r4300_core* r4300, uint32_t address) {
  wasmReleaseBlock(address >> 12);
  cached_interp_init_block(r4300, address);
}

/*
static int is_compiled(uint32_t block, struct precomp_instr* instr) {

  return EM_ASM_INT({
      const instructionPointer = $0;
      const block = $1;

      //      console.log(Module.recompilingFunctionsByBlock);
      //      console.log("block: %d, instructionPointer: %d\n", block, instructionPointer);
      if (Module.recompiledFunctionsByBlock[block]
          && Module.recompiledFunctionsByBlock[block].includes(instructionPointer)) {
        //        console.log("Module already compiled!");
        return 1;
      }

      if (Module.recompilingFunctionsByBlock[block] &&
          Module.recompilingFunctionsByBlock[block].includes(instructionPointer)) {
        //        console.log("Module already being compiled!");
        return 1;
      }

      return 0;
    }, (uint32_t) &instr->ops, block);
*/
  // TODO - something more efficient
    /*
  int i;
  for (i = 0; i < numCompiledBlocks; i++) {
    if (compiledCodeBlocks[i] == instr) {
      return 1;
    }
  }
  return 0;
    */
/*}*/

struct precomp_instr* checkForJumpTarget(enum r4300_opcode opcode,
                                         struct precomp_instr* inst,
                                         const struct precomp_block* block,
                                         uint32_t iw,
                                         struct r4300_idec* idec) {

  uint32_t instructionAddress;
  uint32_t instructionOffset;
  struct precomp_instr* branchTargetInstruction;
  
  switch (opcode)
    {
    case R4300_OP_J:
    case R4300_OP_JAL:

      instructionAddress = (inst->addr & ~0xfffffff) | (idec_imm(iw, idec) & 0xfffffff);

      instructionOffset = instructionAddress - block->start;

      branchTargetInstruction = block->block + (instructionOffset / 4);
      return branchTargetInstruction;

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
      // Branch offset is added to the *incremented* PC
      branchTargetInstruction = inst + 1 + inst->f.i.immediate;

      //printf("Found branch! instAddr=%u; branchTargetInstructionAddr=%u, immediate=%d\n",
      //inst,
      //branchTargetInstruction,
      //             inst->f.i.immediate);
      return branchTargetInstruction;
  }
  
  return NULL;
}


void try_add_recomp_target(struct precomp_instr* target) {
  // TODO
  // 1. Check if we already have the target
  // 2. Check if we need to resize recompTargets
  // 3. Add target

  if (target->recomp_status < WASM_OPTIMIZED_RECOMP_STATUS) {

    if (numRecompTargets >= MAX_RECOMP_TARGETS) {
      printf("MAX_RECOMP_TARGETS (%d) reached! Skipping optimization of instruction!\n", MAX_RECOMP_TARGETS);
      return;
    }

    recompTargets[numRecompTargets++] = target;
    target->recomp_status = WASM_OPTIMIZED_RECOMP_STATUS;
  }
}


void generate_delay_slot_block_exit_check() {
  DECLARE_R4300

    I32_CONST((unsigned int) &(r4300->delay_slot));
  I32_LOAD(0);
  //generate_i32_indirect_call_no_args((uint32_t) isDelaySlot);
  // br_if
  put8(0x0d);
  // break depth (0)
  put8(0x00);
  // end
}

void generate_wasm_function_for_recompile_target(struct r4300_core* r4300,
                                                 const uint32_t* iw,
                                                 struct precomp_block* block,
                                                 uint32_t recompTargetIndex) {

  int i, length, length2, finished;
  struct precomp_instr* inst;
  enum r4300_opcode opcode;


  /* ??? not sure why we need these 2 different tests */
  int block_start_in_tlb = ((block->start & UINT32_C(0xc0000000)) != UINT32_C(0x80000000));
  int block_not_in_tlb = (block->start >= UINT32_C(0xc0000000) || block->end < UINT32_C(0x80000000));

  
  length = get_block_length(block);
  length2 = length - 2 + (length >> 2); // Something to do with jumps?


  start_wasm_code_section_function_body();
  //  generate_block_access_check(block->start + recompTargetIndex);


  int skip_next_instruction_assembly = 0;

  //  float numberOfInstructionsCompiled = 0.0f;
  //  float numberOfInterpretedInstructions = 0.0f;
  
  int pass;
  for (pass = 0; pass < 2; pass++) {
    // (func & 0xFFF) finds the byte offset for `func` within the block
    // Divide by 4 to get the instruction index
    for (i = recompTargetIndex, finished = 0; finished != 2; ++i) {

      //    printf("foo numUsedFunctions[0]=%u\n", numUsedFunctions[0]);
      inst = block->block + i;

      if (pass == 0) {
        //    opcode = inst->decodedOpcode;
        uint32_t opsBefore = (uint32_t) inst->ops;

        /* decode instruction */
        struct r4300_idec* idec = r4300_get_idec(iw[i]);
        //printf("%u: decoded opcode: %s", opcode_count++, opcode_names[idec->opcode]);
        opcode = r4300_decode(inst, r4300, idec, iw[i], iw[i+1], block);

        //printf("bar numUsedFunctions[0]=%u\n", usedFunctions[0]);
        inst->decodedOpcode = opcode;
    
        inst->ops = (void*) opsBefore;
      } else {
        opcode = inst->decodedOpcode;
      }
      
    

      /* decode ending conditions */
      if (i >= length2) { finished = 2; }
      if (i >= (length-1)
          && (block->start == UINT32_C(0xa4000000) || block_not_in_tlb)) { finished = 2; }
      if (opcode == R4300_OP_ERET || finished == 1) { finished = 2; }
      if (/*i >= length && */
          (opcode == R4300_OP_J ||
           opcode == R4300_OP_J_OUT ||
           opcode == R4300_OP_JR ||
           opcode == R4300_OP_JR_OUT) &&
          !(i >= (length-1) && block_not_in_tlb)) {
        finished = 1;
      }


      // No Idea why having this here keeps things from breaking...
      //if (usedFunctions[0] == 999) {
      //  printf("foo\n");
      //}

      //printf("baz numUsedFunctions[0]=%u\n", usedFunctions[0]);

      // Assemble wasm

      if (pass == 1) {
        if (finished != 2) {
          next_iw = iw[i+1];
          next_idec = r4300_get_idec(next_iw);
          next_opcode = next_idec->opcode;

          //            next_inst.addr = inst->addr + 4;
          next_inst = inst + 1;
          //        r4300_decode(&next_inst, r4300, next_idec, iw[i+1], iw[i+2], block);
        }

        //printf("foo\n");
        //printf("buzz numUsedFunctions[0]=%u\n", usedFunctions[0]);
        if (!skip_next_instruction_assembly) {
          //gen_table[idec->opcode]();
          gen_inst(inst, opcode, r4300_get_idec(iw[i]), iw[i]);

          //          numberOfInstructionsCompiled += 1.0f;
          //          if (instructionWasInterpreted) {
          //            numberOfInterpretedInstructions += 1.0f;
          //            instructionWasInterpreted = 0;
          //          }

          if (i == recompTargetIndex) {
            generate_delay_slot_block_exit_check();
          }

          if (isBranchInstruction(opcode)) {
            skip_next_instruction_assembly = 1;
          }
          //printf(" (generated)\n");
          // generate the wasm code for the instruction
        } else {
          skip_next_instruction_assembly = 0;
        }

      }
      //printf("buzz2 numUsedFunctions[0]=%u\n", usedFunctions[0]);
    }

  }

  //  float ratio = numberOfInterpretedInstructions / numberOfInstructionsCompiled;
  //  printf("numInstructionsGenerated=%f; numInterprettedInstructions=%f; ratio=%f\n",
  //         numberOfInstructionsCompiled,
  //         numberOfInterpretedInstructions,
  //         ratio);
         
  
  end_wasm_code_section_function_body();
}

void wasm_recompile_block(struct r4300_core* r4300, const uint32_t* iw, struct precomp_block* block, uint32_t func) {

  //printf("wasm_recompile_block! func=%u\n", func);

  //EM_ASM({ Module._lastRecompileBlockStartTime = performance.now() });
  
    int i, length, length2, finished;
    struct precomp_instr* inst;
    enum r4300_opcode opcode;

    uint32_t opcode_count = 0;

    inst = block->block + ((func & 0xFFF) / 4);
    numRecompTargets = 0;
    int shouldOptimizeJumpTargets = inst->recomp_status == (WASM_OPTIMIZED_RECOMP_STATUS - 1);
    //    printf("wasm_recompile_block! func=%u; shouldOptimizeJumpTargets=%u\n",
    //           func,
    //           shouldOptimizeJumpTargets);

    if (shouldOptimizeJumpTargets) {
      init_wasm_module_code();
    }

    /* ??? not sure why we need these 2 different tests */
    int block_start_in_tlb = ((block->start & UINT32_C(0xc0000000)) != UINT32_C(0x80000000));
    int block_not_in_tlb = (block->start >= UINT32_C(0xc0000000) || block->end < UINT32_C(0x80000000));

    length = get_block_length(block);
    length2 = length - 2 + (length >> 2); // Something to do with jumps?

    /* reset xxhash */
    block->xxhash = 0;

    try_add_recomp_target(block->block + ((func & 0xFFF) / 4));
    //recompTargets[0] = block->block + ((func & 0xFFF) / 4);
    //numRecompTargets = 1;

    
    // pass 0: decode instructions; find jump targets in the block
    //uint32_t earliestRecompileTargetInstructionIndex = (func & 0xFFF) / 4;

    uint32_t numProcessedRecompTargets = 0;
    //uint32_t currentRecompileTargetInstructionIndex = earliestRecompileTargetInstructionIndex;
    do {

      uint32_t currentRecompileTargetInstructionIndex = recompTargets[numProcessedRecompTargets] - block->block;//earliestRecompileTargetInstructionIndex;

      //printf("checking for recompile targets! numProcessedRecompTargets=%u; numRecompTargets=%u\n", numProcessedRecompTargets, numRecompTargets);
      // (func & 0xFFF) finds the byte offset for `func` within the block
      // Divide by 4 to get the instruction index
      for (i = currentRecompileTargetInstructionIndex, finished = 0; finished != 2; ++i)
        {


          inst = block->block + i;


          /* set decoded instruction address */
          inst->addr = block->start + i * 4;

          if (block_start_in_tlb)
            {
              uint32_t address2 = virtual_to_physical_address(r4300, inst->addr, 0);
              if (r4300->cached_interp.blocks[address2>>12]->block[(address2&UINT32_C(0xFFF))/4].ops == cached_interp_NOTCOMPILED) {
                r4300->cached_interp.blocks[address2>>12]->block[(address2&UINT32_C(0xFFF))/4].ops = cached_interp_NOTCOMPILED2;
              }
            }

          uint32_t opsBefore = (uint32_t) inst->ops;

          /* decode instruction */
          struct r4300_idec* idec = r4300_get_idec(iw[i]);
          //        printf("%u: decoded opcode: %s", opcode_count++, opcode_names[idec->opcode]);
          opcode = r4300_decode(inst, r4300, idec, iw[i], iw[i+1], block);
          if (inst->recomp_status == 0) {
            inst->recomp_status = 1;
          }
          
          inst->decodedOpcode = opcode;//idec->opcode;

          if (shouldOptimizeJumpTargets) {
            struct precomp_instr* maybeJumpTarget = checkForJumpTarget(opcode,
                                                                       inst,
                                                                       block,
                                                                       iw[i],
                                                                       idec);

            if (maybeJumpTarget != NULL) {
            

              try_add_recomp_target(maybeJumpTarget);

              //uint32_t instructionIndex = ((uint32_t) (maybeJumpTarget - block->block));
              //if (instructionIndex < earliestRecompileTargetInstructionIndex) {
              //earliestRecompileTargetInstructionIndex = instructionIndex;
              //}
            }
          }

          /* decode ending conditions */
          if (i >= length2) { finished = 2; }
          if (i >= (length-1)
              && (block->start == UINT32_C(0xa4000000) || block_not_in_tlb)) { finished = 2; }
          if (opcode == R4300_OP_ERET || finished == 1) { finished = 2; }
          if (/*i >= length && */
              (opcode == R4300_OP_J ||
               opcode == R4300_OP_J_OUT ||
               opcode == R4300_OP_JR ||
               opcode == R4300_OP_JR_OUT) &&
              !(i >= (length-1) && block_not_in_tlb)) {
            finished = 1;
          }
        }

        if (i >= length)
        {
          inst = block->block + i;
          inst->addr = block->start + i*4;
          inst->ops = cached_interp_FIN_BLOCK;
          ++i;
          if (i <= length2) // useful when last opcode is a jump
            {
              inst = block->block + i;
              inst->addr = block->start + i*4;
              inst->ops = cached_interp_FIN_BLOCK;
              i++;
            }
        }


      numProcessedRecompTargets++;
    } while(numProcessedRecompTargets < numRecompTargets);

    // TODO - Sort recompTargets, with > address first?
    // For each recompTarget:
    // 1. Create a new function in wasm module
    // 2. Generate code from the recompTarget to either:
    //    A) The end of the block (to start)
    //    B) The last recompTarget (Link with function call

    // pass 1: generate wasm code for jump targets

    if (shouldOptimizeJumpTargets) {
      generate_function_section();
      generate_exports_section();
      start_wasm_code_section();



      //EM_ASM({ Module._recompTargetWasmGenerationStart = performance.now()});
      //printf("numRecompTargets: %d\n", numRecompTargets);
      int k;
      for (k = 0; k < numRecompTargets; k++) {
        uint32_t recompTargetIndex = (recompTargets[k]->addr - block->start) / 4;
        generate_wasm_function_for_recompile_target(r4300, iw, block, recompTargetIndex);
      }

      //EM_ASM({ Module._recompTargetWasmGenerationEnd = performance.now()});

      end_wasm_code_section();


      inst = block->block + ((func & 0xFFF) / 4);

      uint32_t recompTargetFunctionPointers[MAX_RECOMP_TARGETS];

      compileAndPatchModule(func >> 12,
                            wasm_code,
                            wasm_code_length,
                            usedFunctions,
                            numUsedFunctions,
                            recompTargetFunctionPointers,
                            numRecompTargets,
                            sizeof(struct precomp_instr));

      for (k = 0; k < numRecompTargets; k++) {

        /*printf("Setting value=%u at address=%u; valueBefore=%u\n", recompTargetFunctionPointers[k], &recompTargets[k]->ops, recompTargets[k]->ops);*/

        recompTargets[k]->ops = (void*) recompTargetFunctionPointers[k];
      }

    }
    compileCount++;
    //printf("compiledFunction: %d\n", compiledFunction);

    //    inst->ops = (void*) compiledFunction;

    
    //    printf("AFTERCOMPILE: %u; ops=%u\n", &(*r4300_pc_struct(r4300))->ops, (*r4300_pc_struct(r4300))->ops);


    // TODO - Free buffer at some point?
    // TODO - Profit?

#ifdef DBG
      DebugMessage(M64MSG_INFO, "block recompiled (%" PRIX32 "-%" PRIX32 ")", func, block->start+i*4);
#endif

}

void recomp_wasm_DO_OPTIMIZE(void)
{
    DECLARE_R4300
    uint32_t *mem = fast_mem_access(r4300, r4300->cached_interp.blocks[*r4300_pc(r4300)>>12]->start);
#ifdef DBG
    DebugMessage(M64MSG_INFO, "NOTCOMPILED: addr = %x ops = %lx", *r4300_pc(r4300), (long) (*r4300_pc_struct(r4300))->ops);
#endif


    if (mem == NULL) {
        DebugMessage(M64MSG_ERROR, "not compiled exception");
    }
    else {
        wasm_recompile_block(r4300, mem, r4300->cached_interp.blocks[*r4300_pc(r4300) >> 12], *r4300_pc(r4300));
    }

    (*r4300_pc_struct(r4300))->ops();
}

void recomp_wasm_jump_to(struct r4300_core* r4300, uint32_t address) {
  cached_interpreter_jump_to(r4300, address);

  struct precomp_instr* instr = *r4300_pc_struct(r4300);

  if (instr->recomp_status < (WASM_OPTIMIZED_RECOMP_STATUS - 1)) {
    instr->recomp_status++;
  } else if (instr->recomp_status == (WASM_OPTIMIZED_RECOMP_STATUS - 1)) {
    //printf("Second time jumping to inst=%u! Running JIT-optimizer!\n", instr);
    instr->ops = recomp_wasm_DO_OPTIMIZE;
  }
}
