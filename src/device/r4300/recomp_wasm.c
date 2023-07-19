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

#define WASM_OPTIMIZED_RECOMP_STATUS 4

// In jslib/corelib.js
extern void initWasmRecompiler();
extern void compileAndPatchModule(uint32_t* blocks,
                                      void* moduleDataPointer,
                                      int moduleLength,
                                      void* usedFunctionsPointerArray,
                                      int numFunctionsUsed,
                                      uint32_t* recompTargetsPointer,
                                      uint32_t numRecompTargets);
extern void notifyBlockAccess(uint32_t address);
extern void wasmReleaseBlock(uint32_t block);

extern uint32_t numberOfRecompiles;
extern uint32_t numberOfRecompiledBytes;
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

struct WriteableCodeBuffer {
  unsigned char *code;
  uint32_t max_code_length;
  uint32_t code_length;
};

struct WriteableCodeBuffer wasmModuleBuffer;
struct WriteableCodeBuffer *activeOutputCodeBuffer;

//unsigned char *wasm_code;
//int max_wasm_code_length = 0;
//int wasm_code_length = 0;

  
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

const uint32_t MAX_RECOMP_TARGETS = 100;
struct precomp_instr* recompTargets[MAX_RECOMP_TARGETS];
uint32_t numRecompTargets = 0;

#define MAX_NUMBER_OF_RECOMPILED_BLOCKS_PER_VI 100

struct RecompiledWASMFunctionBlock {
  char invalidated;
  uint32_t block;
  struct precomp_instr* entryInstruction;
  struct WriteableCodeBuffer wasmCodeBuffer;
};
struct RecompiledWASMFunctionBlock recompiledWASMFunctionBlocks[MAX_NUMBER_OF_RECOMPILED_BLOCKS_PER_VI];
uint32_t numberOfRecompiledWASMFunctionBlocks = 0;


static void resizeCodeBufferIfNeeded(int wasmCodeIndex) {

  if (wasmCodeIndex > activeOutputCodeBuffer->code_length) {
    activeOutputCodeBuffer->code_length = wasmCodeIndex;

    if (activeOutputCodeBuffer->code_length == activeOutputCodeBuffer->max_code_length) {
      activeOutputCodeBuffer->code =
        (unsigned char *) realloc_exec(activeOutputCodeBuffer->code,
                                       activeOutputCodeBuffer->max_code_length,
                                       activeOutputCodeBuffer->max_code_length+8192);
      activeOutputCodeBuffer->max_code_length += 8192;
    }
  }
}

static void put8(unsigned char octet)
{

  activeOutputCodeBuffer->code[activeOutputCodeBuffer->code_length] = octet;
  activeOutputCodeBuffer->code_length++;

  if (activeOutputCodeBuffer->code_length == activeOutputCodeBuffer->max_code_length) {
    activeOutputCodeBuffer->code =
      (unsigned char *) realloc_exec(activeOutputCodeBuffer->code,
                                     activeOutputCodeBuffer->max_code_length,
                                     activeOutputCodeBuffer->max_code_length+8192);
    activeOutputCodeBuffer->max_code_length += 8192;
  }
}

static void editSLEB128(long dword, uint32_t destinationByteIndex) {
  if ((destinationByteIndex > activeOutputCodeBuffer->code_length)) {
    printf("invalid destinationByteIndex=%d provided to 'editSLEB128' with code_length=%d\n",
           destinationByteIndex,
           activeOutputCodeBuffer->code_length);
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
    //wasm_code[wasmCodeIndex++] = byte;
    activeOutputCodeBuffer->code[wasmCodeIndex++] = byte;

    resizeCodeBufferIfNeeded(wasmCodeIndex);
    
  } while (more);
}

static void editULEB128(long dword, uint32_t destinationByteIndex, int padTo) {

  if ((destinationByteIndex > activeOutputCodeBuffer->code_length)) {
    printf("invalid destinationByteIndex=%d provided to 'editULEB128' with code_length=%d\n",
           destinationByteIndex,
           activeOutputCodeBuffer->code_length);
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
    //wasm_code[wasmCodeIndex++] = byte;
    activeOutputCodeBuffer->code[wasmCodeIndex++] = byte;

    resizeCodeBufferIfNeeded(wasmCodeIndex);
    
  } while (value != 0);

  if (count < padTo) {
    for (; count < padTo - 1; ++count) {
      //wasm_code[wasmCodeIndex++] = 0x80;
      activeOutputCodeBuffer->code[wasmCodeIndex++] = 0x80;

      resizeCodeBufferIfNeeded(wasmCodeIndex);
    }
    //wasm_code[wasmCodeIndex++] = 0x00;
    activeOutputCodeBuffer->code[wasmCodeIndex++] = 0x00;

    resizeCodeBufferIfNeeded(wasmCodeIndex);
  }
}

static void putSLEB128(long dword) {
  editSLEB128(dword, activeOutputCodeBuffer->code_length);
}

static void putULEB128(unsigned long dword, int padTo) {
  editULEB128(dword, activeOutputCodeBuffer->code_length, padTo);
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

static void shiftBytesOver(uint32_t startByte, uint32_t numBytesToShift) {
  
  if (activeOutputCodeBuffer->code_length < startByte) {
    printf("Invalid startByte=%d and numBytes=%d provided to shiftBytesOver with wasm_code_length=%d\n",
           startByte,
           numBytesToShift,
           activeOutputCodeBuffer->code_length);
    return;
  }

  if ((activeOutputCodeBuffer->code_length + numBytesToShift) > activeOutputCodeBuffer->max_code_length) {
    activeOutputCodeBuffer->code =
      (unsigned char *) realloc_exec(activeOutputCodeBuffer->code,
                                     activeOutputCodeBuffer->max_code_length,
                                     activeOutputCodeBuffer->max_code_length+8192);
    activeOutputCodeBuffer->max_code_length += 8192;
  }
  
  int i;
  for (i = (activeOutputCodeBuffer->code_length + numBytesToShift); i >= (startByte + numBytesToShift); i--) {
    activeOutputCodeBuffer->code[i] = activeOutputCodeBuffer->code[i - numBytesToShift];
    //wasm_code[i] = wasm_code[i - numBytesToShift];
  }

  //wasm_code_length += numBytesToShift;
  activeOutputCodeBuffer->code_length += numBytesToShift;
}


static void generate_types_section() {
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
  
  // section code
  put8(0x03);

  uint32_t sectionSize = MAX_BYTES_FOR_32ULEB128 + numberOfRecompiledWASMFunctionBlocks;
  // section size
  putULEB128(sectionSize, MAX_BYTES_FOR_32ULEB128);

  // num functions
  putULEB128(numberOfRecompiledWASMFunctionBlocks, MAX_BYTES_FOR_32ULEB128);

  int i;
  for (i = 0; i < numberOfRecompiledWASMFunctionBlocks; i++) {
    // function i signature index
    put8(0x00);
  }
}

static void generate_imports_section() {

  // section code
  put8(0x02);
  // section size
  // TODO - compute dynamically
  putULEB128(0x1c, 0); // 28
  // num imports
  put8(0x02);

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

  // import header 1 (mem)
  // string length
  putULEB128(0x03, 0);
  // "env"
  put8(0x65); put8(0x6e); put8(0x76);
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

  uint32_t sectionStart = activeOutputCodeBuffer->code_length;
  // section size (guess)
  putULEB128(0x00, MAX_BYTES_FOR_32ULEB128);

  // num exports
  putULEB128(numberOfRecompiledWASMFunctionBlocks, 0);
  
  int i;
  for (i = 0; i < numberOfRecompiledWASMFunctionBlocks; i++) {

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

  uint32_t sectionEnd = activeOutputCodeBuffer->code_length;

  uint32_t sectionSize = sectionEnd - sectionStart - MAX_BYTES_FOR_32ULEB128;
  editULEB128(sectionSize, sectionStart, MAX_BYTES_FOR_32ULEB128);
}

static void start_wasm_code_section() {

  code_section_start = activeOutputCodeBuffer->code_length;
  
  // section code
  put8(0x0a); // 3a
  // section size (guess)
  putULEB128(0x00, MAX_BYTES_FOR_32ULEB128); // 3b
  // num functions
  putULEB128(numberOfRecompiledWASMFunctionBlocks, 0); // 3c
}

static void end_wasm_code_section() {
  // FIXUP code section size
  uint32_t codeSectionByteLength = activeOutputCodeBuffer->code_length - (code_section_start + 1 + MAX_BYTES_FOR_32ULEB128);

  editULEB128(codeSectionByteLength, code_section_start + 1, MAX_BYTES_FOR_32ULEB128);
}

static int claim_i32_local() {

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
  }

  return totalNumberOfLocals++;
}

static int claim_i64_local() {

  // Check for existing declarations with unclaimed locals
  int i;
  int localIndex = 1;
  for (i = 0; i < customLocalDeclarationCount; i++) {
    if (localDeclarations[i].type == 1) {
      if (localDeclarations[i].numClaimedLocals
          < localDeclarations[i].numDeclaredLocals) {

        // "claim" an existing local
        return localIndex + localDeclarations[i].numClaimedLocals++;

      }
    }
    localIndex += localDeclarations[i].numDeclaredLocals;
  }

  if (customLocalDeclarationCount > 0 && localDeclarations[customLocalDeclarationCount - 1].type == 1) {
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

}

static void release_locals() {

  int i;
  for (i = 0; i < customLocalDeclarationCount; i++) {
    localDeclarations[i].numClaimedLocals = 0;
  }
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
    activeOutputCodeBuffer->code[currentEditIndex++] = getWasmLocalType(localDeclarations[i].type);
  }
}

static void start_wasm_code_section_function_body() {
  //  printf("start_wasm_code_section_function_body\n");

  // Used to calculate the function body size later
  last_function_body_start = activeOutputCodeBuffer->code_length;

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
  custom_local_declaration_end_index = activeOutputCodeBuffer->code_length;

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
  uint32_t functionBodyByteLength = activeOutputCodeBuffer->code_length - (last_function_body_start + MAX_BYTES_FOR_32ULEB128);
  editULEB128(functionBodyByteLength, last_function_body_start, MAX_BYTES_FOR_32ULEB128);
}

static void generate_reusable_wasm_module_boilerplate() {
  // WASM BINARY MAGIC HEADER
  put8(0x00); put8(0x61); put8(0x73); put8(0x6d);
  // WASM BINARY VERSION
  put8(0x01); put8(0x00); put8(0x00); put8(0x00);
  
  generate_types_section();
  generate_imports_section();
}

static void init_wasm_module_code() {

  /*
  if (activeOutputCodeBuffer->max_code_length == 0) {

    // Arbitrary. Can be increased
    activeOutputCodeBuffer->max_code_length = 32768; 
    activeOutputCodeBuffer->code = malloc(activeOutputCodeBuffer->max_code_length);

    generate_reusable_wasm_module_boilerplate();
    reusable_wasm_boilerplate_length = activeOutputCodeBuffer->code_length;
  } else {
    activeOutputCodeBuffer->code_length = reusable_wasm_boilerplate_length;
  }
  */

  /*
  customLocalDeclarationCount = 0;
  totalNumberOfLocals = NUM_RESERVED_I32_LOCALS;
  */
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

    gen_table[opcode](inst);
    break;
  }
  }
}

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

void recomp_wasm_init_block(struct r4300_core* r4300, uint32_t address) {
  wasmReleaseBlock(address >> 12);
  //printf("cached_interp_init_block: %u\n", address);

  int i;
  for (i = 0; i < numberOfRecompiledWASMFunctionBlocks; i++) {
    if ((address >> 12) == recompiledWASMFunctionBlocks[i].block) {
      printf("A block we're recompiling has been invalidated! blockId=%u\n", address >> 12);
    }
  }

  
  cached_interp_init_block(r4300, address);
}

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

      return branchTargetInstruction;
  }
  
  return NULL;
}


void try_add_recomp_target(struct precomp_instr* target) {
  // 1. Check if we already have the target
  // 2. Check if we need to resize recompTargets
  // 3. Add target

  //printf("try_add_recomp_target\n");
  
  if (target->recomp_status < WASM_OPTIMIZED_RECOMP_STATUS) {

    if (numberOfRecompiledWASMFunctionBlocks + numRecompTargets >= MAX_RECOMP_TARGETS) {
      printf("MAX_RECOMP_TARGETS (%d) reached! Skipping optimization of instruction!\n", MAX_NUMBER_OF_RECOMPILED_BLOCKS_PER_VI);
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
  // br_if
  put8(0x0d);
  // break depth (0)
  put8(0x00);
  // end
}

void generate_wasm_function_for_recompile_target(struct r4300_core* r4300,
                                                 const uint32_t* iw,
                                                 uint32_t blockId,
                                                 struct precomp_block* block,
                                                 uint32_t recompTargetIndex) {

  //printf("generate_wasm_function_for_recompiled_target; numberOfRecompiledWASMFunctionBlocks=%u\n", numberOfRecompiledWASMFunctionBlocks);
  int i, length, length2, finished;
  struct precomp_instr* inst;
  enum r4300_opcode opcode;
  
  // claim a new output buffer
  recompiledWASMFunctionBlocks[numberOfRecompiledWASMFunctionBlocks].entryInstruction = block->block + recompTargetIndex;
  recompiledWASMFunctionBlocks[numberOfRecompiledWASMFunctionBlocks].block = blockId;
  recompiledWASMFunctionBlocks[numberOfRecompiledWASMFunctionBlocks].invalidated = 0;
  activeOutputCodeBuffer = &recompiledWASMFunctionBlocks[numberOfRecompiledWASMFunctionBlocks].wasmCodeBuffer;
  numberOfRecompiledWASMFunctionBlocks++;

  /* ??? not sure why we need these 2 different tests */
  int block_start_in_tlb = ((block->start & UINT32_C(0xc0000000)) != UINT32_C(0x80000000));
  int block_not_in_tlb = (block->start >= UINT32_C(0xc0000000) || block->end < UINT32_C(0x80000000));

  
  length = get_block_length(block);
  length2 = length - 2 + (length >> 2); // Something to do with jumps?

  start_wasm_code_section_function_body();

  int skip_next_instruction_assembly = 0;

  int pass;
  for (pass = 0; pass < 2; pass++) {
    // (func & 0xFFF) finds the byte offset for `func` within the block
    // Divide by 4 to get the instruction index
    for (i = recompTargetIndex, finished = 0; finished != 2; ++i) {

      inst = block->block + i;

      if (pass == 0) {
        uint32_t opsBefore = (uint32_t) inst->ops;

        /* decode instruction */
        struct r4300_idec* idec = r4300_get_idec(iw[i]);
        
        opcode = r4300_decode(inst, r4300, idec, iw[i], iw[i+1], block);

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

      // Assemble wasm
      if (pass == 1) {
        if (finished != 2) {
          next_iw = iw[i+1];
          next_idec = r4300_get_idec(next_iw);
          next_opcode = next_idec->opcode;

          next_inst = inst + 1;
        }

        if (!skip_next_instruction_assembly) {
          gen_inst(inst, opcode, r4300_get_idec(iw[i]), iw[i]);

          if (i == recompTargetIndex) {
            generate_delay_slot_block_exit_check();
          }

          if (isBranchInstruction(opcode)) {
            skip_next_instruction_assembly = 1;
          }
        } else {
          skip_next_instruction_assembly = 0;
        }

      }
    }

  }

  numberOfRecompiles++;
  end_wasm_code_section_function_body();

  //  printf("Finished generating function body\n");
}

void init_wasm_recompiler() {

  initWasmRecompiler();
  
  int i;
  struct RecompiledWASMFunctionBlock *currentRecompiledBlock;
  for (i = 0; i < MAX_NUMBER_OF_RECOMPILED_BLOCKS_PER_VI; i++) {

    currentRecompiledBlock = &recompiledWASMFunctionBlocks[i];

    currentRecompiledBlock->invalidated = 0;
    currentRecompiledBlock->entryInstruction = NULL;
        
    currentRecompiledBlock->wasmCodeBuffer.code_length = 0;
    currentRecompiledBlock->wasmCodeBuffer.max_code_length = 8192;
    currentRecompiledBlock->wasmCodeBuffer.code =
      malloc(currentRecompiledBlock->wasmCodeBuffer.max_code_length);
  }

  wasmModuleBuffer.code_length = 0;
  // arbitrary; can be increased
  wasmModuleBuffer.max_code_length = 32768;
  wasmModuleBuffer.code = malloc(wasmModuleBuffer.max_code_length);

  activeOutputCodeBuffer = &recompiledWASMFunctionBlocks[0].wasmCodeBuffer;
  customLocalDeclarationCount = 0;
  totalNumberOfLocals = NUM_RESERVED_I32_LOCALS;
}

static void reset_wasm_recompiler() {

  int i;
  struct RecompiledWASMFunctionBlock *currentRecompiledBlock;
  for (i = 0; i < numberOfRecompiledWASMFunctionBlocks; i++) {
    currentRecompiledBlock = &recompiledWASMFunctionBlocks[i];
    currentRecompiledBlock->wasmCodeBuffer.code_length = 0;
  }

  wasmModuleBuffer.code_length = 0;

  activeOutputCodeBuffer = &recompiledWASMFunctionBlocks[0].wasmCodeBuffer;
  customLocalDeclarationCount = 0;
  numUsedFunctions = 0;
  numberOfRecompiledWASMFunctionBlocks = 0;
  totalNumberOfLocals = NUM_RESERVED_I32_LOCALS;
}

void recomp_wasm_build_and_patch_module() {

  if (numberOfRecompiledWASMFunctionBlocks == 0) {
    return;
  }
  
  // TODO - Initialize code buffer?
  
  activeOutputCodeBuffer = &wasmModuleBuffer;
  
  // WASM BINARY MAGIC HEADER
  put8(0x00); put8(0x61); put8(0x73); put8(0x6d);
  // WASM BINARY VERSION
  put8(0x01); put8(0x00); put8(0x00); put8(0x00);
  
  generate_types_section();
  generate_imports_section();
  generate_function_section();
  generate_exports_section();
  start_wasm_code_section();

  int i;
  struct RecompiledWASMFunctionBlock *currentRecompiledBlock;
  for (i = 0; i < numberOfRecompiledWASMFunctionBlocks; i++) {

    currentRecompiledBlock = &recompiledWASMFunctionBlocks[i];

    if (activeOutputCodeBuffer->code_length + currentRecompiledBlock->wasmCodeBuffer.code_length > activeOutputCodeBuffer->max_code_length) {

      printf("resizing....\n");
      uint32_t growSize = ((activeOutputCodeBuffer->code_length + currentRecompiledBlock->wasmCodeBuffer.code_length)
        - activeOutputCodeBuffer->max_code_length) + 8192;
      activeOutputCodeBuffer->code =
        (unsigned char *) realloc_exec(activeOutputCodeBuffer->code,
                                       activeOutputCodeBuffer->max_code_length,
                                       activeOutputCodeBuffer->max_code_length+growSize);
      activeOutputCodeBuffer->max_code_length += growSize;
    }
    
    memcpy(&activeOutputCodeBuffer->code[activeOutputCodeBuffer->code_length],
           currentRecompiledBlock->wasmCodeBuffer.code,
           currentRecompiledBlock->wasmCodeBuffer.code_length);

    activeOutputCodeBuffer->code_length += currentRecompiledBlock->wasmCodeBuffer.code_length;
  } 
  
  end_wasm_code_section();

  uint32_t recompTargetFunctionPointers[MAX_RECOMP_TARGETS];
  uint32_t blocks[MAX_RECOMP_TARGETS];

  for (i = 0; i < numberOfRecompiledWASMFunctionBlocks; i++) {
    blocks[i] = recompiledWASMFunctionBlocks[i].block;
  }

  
  compileAndPatchModule(blocks,
                        activeOutputCodeBuffer->code,
                        activeOutputCodeBuffer->code_length,
                        usedFunctions,
                        numUsedFunctions,
                        recompTargetFunctionPointers,
                        numberOfRecompiledWASMFunctionBlocks);

  //printf("After compiledAndPatchModule\n");
  int k;
  for (k = 0; k < numberOfRecompiledWASMFunctionBlocks; k++) {
    // TODO - This is the issue idiot
    //recompTargets[k]->ops = (void*) recompTargetFunctionPointers[k];
    recompiledWASMFunctionBlocks[k].entryInstruction->ops = (void*) recompTargetFunctionPointers[k];
  }

  numberOfRecompiledBytes = activeOutputCodeBuffer->code_length;
    
  reset_wasm_recompiler();
}

void wasm_recompile_block(struct r4300_core* r4300, const uint32_t* iw, struct precomp_block* block, uint32_t func) {
  
    int i, length, length2, finished;
    struct precomp_instr* inst;
    enum r4300_opcode opcode;

    uint32_t opcode_count = 0;

    inst = block->block + ((func & 0xFFF) / 4);
    numRecompTargets = 0;
    int shouldOptimizeJumpTargets = (inst->recomp_status == (WASM_OPTIMIZED_RECOMP_STATUS - 1))
      && (numberOfRecompiledWASMFunctionBlocks < MAX_RECOMP_TARGETS);

    //if (shouldOptimizeJumpTargets) {
      //      init_wasm_module_code();
    //}
    
    /* ??? not sure why we need these 2 different tests */
    int block_start_in_tlb = ((block->start & UINT32_C(0xc0000000)) != UINT32_C(0x80000000));
    int block_not_in_tlb = (block->start >= UINT32_C(0xc0000000) || block->end < UINT32_C(0x80000000));

    length = get_block_length(block);
    length2 = length - 2 + (length >> 2);

    /* reset xxhash */
    block->xxhash = 0;

    if (shouldOptimizeJumpTargets) {
      try_add_recomp_target(block->block + ((func & 0xFFF) / 4));
    } else {
      // Needed for "compiling" cached instructions
      recompTargets[numRecompTargets++] = block->block + ((func & 0xFFF) / 4);
    }
    
    // pass 0: decode instructions; find jump targets in the block

    uint32_t numProcessedRecompTargets = 0;
    do {

      uint32_t currentRecompileTargetInstructionIndex = recompTargets[numProcessedRecompTargets] - block->block;

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
          
          opcode = r4300_decode(inst, r4300, idec, iw[i], iw[i+1], block);
          
          if (inst->recomp_status == 0) {
            inst->recomp_status = 1;
          }
          
          inst->decodedOpcode = opcode;

          if (shouldOptimizeJumpTargets) {
            struct precomp_instr* maybeJumpTarget = checkForJumpTarget(opcode,
                                                                       inst,
                                                                       block,
                                                                       iw[i],
                                                                       idec);
            
            if (maybeJumpTarget != NULL) {
            
              try_add_recomp_target(maybeJumpTarget);

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

    if (shouldOptimizeJumpTargets) {
      //generate_function_section();
      // TODO - Move
      //generate_exports_section();
      //start_wasm_code_section();

      int k;
      for (k = 0; k < numRecompTargets; k++) {
        uint32_t recompTargetIndex = (recompTargets[k]->addr - block->start) / 4;
        generate_wasm_function_for_recompile_target(r4300, iw, func >> 12, block, recompTargetIndex);
      }

      //end_wasm_code_section();


      //inst = block->block + ((func & 0xFFF) / 4);

      /*
      uint32_t recompTargetFunctionPointers[MAX_RECOMP_TARGETS];

      compileAndPatchModule(func >> 12,
                            activeOutputCodeBuffer->code,
                            activeOutputCodeBuffer->code_length,
                            usedFunctions,
                            numUsedFunctions,
                            recompTargetFunctionPointers,
                            numRecompTargets);
      numberOfRecompiles++;

      for (k = 0; k < numRecompTargets; k++) {

        recompTargets[k]->ops = (void*) recompTargetFunctionPointers[k];
      }
      */
    }
    compileCount++;

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
  } else if (instr->recomp_status == (WASM_OPTIMIZED_RECOMP_STATUS - 1)
             && (numberOfRecompiledWASMFunctionBlocks < MAX_RECOMP_TARGETS)) {
    //printf("Second time jumping to inst=%u! Running JIT-optimizer!\n", instr);
    instr->ops = recomp_wasm_DO_OPTIMIZE;
  }
}
