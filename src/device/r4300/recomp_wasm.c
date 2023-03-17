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

// In jslib/corelib.js
extern uint32_t compileAndPatchModule(int block, void* moduleDataPointer,
                                      int moduleLength,
                                      void* usedFunctionsPointerArray,
                                      int numFunctionsUsed);
extern void releaseWasmFunction(uint32_t functionIndex);
extern void wasmFreeBlocks(int startBlockInclusive, int endBlockExclusive);
extern void notifyBlockAccess(uint32_t address);


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
      printf("cached_interp_%s, destination=%u, pc=%u\n", #name, (#destination), (*r4300_pc_struct(r4300))); \
    const int take_jump = (condition); \
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
        printf("pc_before_ds=%d\n", (*r4300_pc_struct(r4300))); \
        r4300->delay_slot=1; \
        UPDATE_DEBUGGER(); \
        (*r4300_pc_struct(r4300))->ops(); \
        cp0_update_count(r4300); \
        r4300->delay_slot=0; \
        printf("pc_after_ds=%d\n", (*r4300_pc_struct(r4300))); \
        if (take_jump && !r4300->skip_jump) \
        { \
         printf("taking jump\n"); \
            (*r4300_pc_struct(r4300))=r4300->cached_interp.actual->block+((jump_target-r4300->cached_interp.actual->start)>>2); \
            printf("pc_after_jump=%d\n", (*r4300_pc_struct(r4300)));      \
        } \
    } \
    else \
    { \
        (*r4300_pc_struct(r4300)) += 2; \
        cp0_update_count(r4300); \
    } \
    r4300->cp0.last_addr = *r4300_pc(r4300); \
    if (*r4300_cp0_cycle_count(&r4300->cp0) >= 0) gen_interrupt(r4300); \
    return 0; \
} \
 \
int recomp_wasm_interp_##name##_OUT(void) \
{ \
    DECLARE_R4300 \
        printf("cached_interp_%s_OUT, destination=%d, pc=%d\n", #name, (#destination), (*r4300_pc_struct(r4300))); \
    const int take_jump = (condition); \
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
        } \
    } \
    else \
    { \
        (*r4300_pc_struct(r4300)) += 2; \
        cp0_update_count(r4300); \
    } \
    r4300->cp0.last_addr = *r4300_pc(r4300); \
    if (*r4300_cp0_cycle_count(&r4300->cp0) >= 0) gen_interrupt(r4300); \
    return 0; \
} \
  \
int recomp_wasm_interp_##name##_IDLE(void)    \
{ \
    DECLARE_R4300 \
      printf("cached_interp_%s_IDLE, destination=%d, pc=%d\n", #name, (#destination), (*r4300_pc_struct(r4300))); \
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
    cached_interp_##name(); \
    return 0; \
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

const uint32_t NUM_LOCALS = 1;
const unsigned char JUMP_TAKEN_DECISION_LOCAL_INDEX = 0;

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

int skip_next_instruction_assembly = 0;
enum r4300_opcode next_opcode;
struct r4300_idec* next_idec;
uint32_t next_iw;


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

static void edit32SLEB128(int dword, uint32_t destinationByteIndex) {
  if ((destinationByteIndex > wasm_code_length)) {
    printf("invalid destinationByteIndex=%d provided to 'edit32ULEB128' with code_length=%d\n",
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
    
    if (wasmCodeIndex > wasm_code_length) {
      wasm_code_length = wasmCodeIndex;

      if (wasm_code_length == max_wasm_code_length) {
        wasm_code = (unsigned char *) realloc_exec(wasm_code,
                                                   max_wasm_code_length,
                                                   max_wasm_code_length+8192);
        max_wasm_code_length += 8192;
      }
    }
    
  } while (more);  
}

static void edit32ULEB128(uint32_t dword, uint32_t destinationByteIndex) {

  if ((destinationByteIndex > wasm_code_length)) {
    printf("invalid destinationByteIndex=%d provided to 'edit32ULEB128' with code_length=%d\n",
           destinationByteIndex,
           wasm_code_length);
    return;
  }

  int value = dword;
  uint32_t wasmCodeIndex = destinationByteIndex;

  do {
    // Get the lowest 7 bits
    unsigned char byte = (value & 0x7F);
    value = value >> 7;

    if (value != 0) {
      // Set high order bit to 1
      byte = byte | 0x80;
    }

    //    printf("writing byte %x at position %x\n", byte, wasmCodeIndex);
    wasm_code[wasmCodeIndex++] = byte;
    
    if (wasmCodeIndex > wasm_code_length) {
      wasm_code_length = wasmCodeIndex;

      if (wasm_code_length == max_wasm_code_length) {
        wasm_code = (unsigned char *) realloc_exec(wasm_code,
                                                   max_wasm_code_length,
                                                   max_wasm_code_length+8192);
        max_wasm_code_length += 8192;
      }
    }
    
  } while (value != 0);
}

static void put32SLEB128(int dword) {
  edit32SLEB128(dword, wasm_code_length);
}

static void put32ULEB128(unsigned int dword) {
  edit32ULEB128(dword, wasm_code_length);
}


uint32_t numCompiledBlocks = 0;
const uint32_t MAX_COMPILED_BLOCKS = 500;

struct precomp_instr* compiledCodeBlocks[MAX_COMPILED_BLOCKS];

static void generate_void_indirect_call_no_args(uint32_t func);
static void generate_void_indirect_call_i32_arg(uint32_t func, int arg);
static void generate_i32_indirect_call_u32_arg(uint32_t func, uint32_t arg);
static void generate_i32_indirect_call_no_args(uint32_t func);

static void gen_inst(enum r4300_opcode opcode, struct r4300_idec* idec, uint32_t iw);

#include "./wasm_assemble.c"

#define X(op) wasm_gen_##op
static void (*const gen_table[R4300_OPCODES_COUNT])(void) =
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

  //  printf("shifting bytes over %d bytes starting at %x\n", numBytesToShift, startByte);
  
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
  put32ULEB128(0x11);
  // num types
  put32ULEB128(0x04);

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
}

static void generate_function_section() {

  numUsedFunctions = 0;
  
  //  printf("generate_function_section\n");
  // section code
  put8(0x03);
  //  printf("generate_function_section\n");
  // section size
  put32ULEB128(0x02);
  // num functions
  put8(0x01);
  // function 0 signature index
  put8(0x00);
}

static void generate_imports_section() {

  // section code
  put8(0x02);
  // section size
  // TODO - generate dynamically
  put32ULEB128(0x1c); // 28
  // num imports
  put8(0x02);

  //  printf("generate_imports_section 1\n");
  // import header 0 (__indirect_function_table)
  // string length
  put32ULEB128(0x03);
  // "env"
  put8(0x65); put8(0x6e); put8(0x76);
  // string length for import field name
  put32ULEB128(0x07);
  // "funcref"
  put8(0x66); put8(0x75); put8(0x6e); put8(0x63); put8(0x72); put8(0x65); put8(0x66);
  // import kind
  put8(0x01);
  // funcref
  put8(0x70);
  // limits: flags
  put8(0x00);
  // limit: initial
  put8(0x01);

  //  printf("generate_imports_section 2\n");
  // import header 1 (mem)
  // string length
  put32ULEB128(0x03);
  // "env"
  put8(0x65); put8(0x6e); put8(0x76);
  //  printf("generate_imports_section 2\n");
  // string length
  put32ULEB128(0x03);
  // "mem"
  put8(0x6d); put8(0x65); put8(0x6d);
  // import kind
  put8(0x02);
  // limits: flags
  put8(0x00);
  // limits: initial
  put8(0x01);
}

static void generate_exports_section() {
  // section code
  put8(0x07);
  // section size
  put32ULEB128(0x08);
  // num exports
  put8(0x01);
  //  printf("generate_exports_section\n");
  // string length
  put32ULEB128(0x04);
  // "func"
  put8(0x66); put8(0x75); put8(0x6e); put8(0x63);
  // export kind
  put8(0x00);
  // export func index
  put8(0x00);
}

static void start_wasm_code_section() {
  //printf("start_wasm_code_section\n");

  code_section_start = wasm_code_length;
  
  // section code
  put8(0x0a); // 3a
  // section size (guess)
  put8(0x00); // 3b
  // num functions 
  put8(0x01); // 3c
}

static void end_wasm_code_section() {
  // FIXUP code section size
  uint32_t codeSectionByteLength = wasm_code_length - (code_section_start + 2);

  uint32_t numBytesForULEBLength = num_bytes_needed_for_32ULEB128(codeSectionByteLength);

  
  //  printf("end_wasm_code_section numBytesForULEBLength: %d\n", numBytesForULEBLength);
  if (numBytesForULEBLength > 1) {
    shiftBytesOver(code_section_start + 2, numBytesForULEBLength - 1);
  }

  //  printf("codeSectionByteLength: %d\n", codeSectionByteLength);
  edit32ULEB128(codeSectionByteLength, code_section_start + 1);
}


static void start_wasm_code_section_function_body() {
  //  printf("start_wasm_code_section_function_body\n");

  // Used to calculate the function body size later
  last_function_body_start = wasm_code_length;

  // func body size (placeholder)
  put8(0x00); // 3d
  // local decl count
  put32ULEB128(NUM_LOCALS);
  // local type count
  put8(0x01);
  // i32
  put8(0x7f);

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

  // FIXUP function body size
  uint32_t functionBodyByteLength = wasm_code_length - (last_function_body_start + 1);

  uint32_t numBytesForULEBLength = num_bytes_needed_for_32ULEB128(functionBodyByteLength);
  //  printf("end_wasm_code_section_function_body numBytesForULEBLength: %d\n", numBytesForULEBLength);
  if (numBytesForULEBLength > 1) {
    shiftBytesOver(last_function_body_start + 1, numBytesForULEBLength - 1);
  }

  //  printf("functionBodyByteLength: %d\n", functionBodyByteLength);
  edit32ULEB128(functionBodyByteLength, last_function_body_start);
}

static void generate_reusable_wasm_module_boilerplate() {
  // WASM BINARY MAGIC HEADER
  put8(0x00); put8(0x61); put8(0x73); put8(0x6d);
  // WASM BINARY VERSION
  put8(0x01); put8(0x00); put8(0x00); put8(0x00);
  
  generate_types_section();
  generate_imports_section();
  generate_function_section();
  generate_exports_section();
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
}

// idec->opcode may not necessarily be == opcode
static void gen_inst(enum r4300_opcode opcode, struct r4300_idec* idec, uint32_t iw) {

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
    default: wasm_gen_CP1_CVT_D();
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
      wasm_gen_CP1_CVT_S();
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
        default: wasm_gen_CP1_##op();                                         \
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
    gen_table[opcode]();
    break;
  }
  }
}

static void block_access_check(uint32_t addr) {
  int i;
  int j;
  struct precomp_instr* curr;
  struct precomp_instr* tmp;

  notifyBlockAccess(addr);
  
  for (i = 0; i < numCompiledBlocks; i++) {
    curr = compiledCodeBlocks[i];
    if (curr->addr == addr) {

      if (i == (numCompiledBlocks - 1)) {
        return;
      }

      // Move the match to the last index and shift
      // everything else
      for (j = numCompiledBlocks - 1; j >= i; j--) {
        tmp = compiledCodeBlocks[j];
        compiledCodeBlocks[j] = curr;
        curr = tmp;
      }
      return;
    }
  }
}

static uint32_t getTranslatedFunctionIndex(uint32_t func) {
  int i;
  for (i = 0; i < numUsedFunctions; i++) {
    if (usedFunctions[i] == func) {
      return i;
    }
  }

  usedFunctions[numUsedFunctions] = func;
  return numUsedFunctions++;
}

static void generate_void_indirect_call_no_args(uint32_t func) {

  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32SLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  put32ULEB128(0x00); // TODO
  // table index (always 0)
  put8(0x00);
}

static void generate_void_indirect_call_i32_arg(uint32_t func, int arg) {
  // instruction i32.const
  put8(0x41);
  // i32 literal (arg)
  put32SLEB128(arg);
  
  // instruction i32.const
  put8(0x41);  
  // i32 literal (func)
  put32SLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 params and 0 results
  put32ULEB128(0x02);
  // table index (always 0)
  put8(0x00);
}

// TODO - Combine with above?
static void generate_i32_indirect_call_u32_arg(uint32_t func, uint32_t arg) {
  // instruction i32.const
  put8(0x41);
  // i32 literal (func)
  put32SLEB128(arg);

  // instruction i32.const
  put8(0x41);
  // i32 literal (func)
  put32SLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int arg and int return type in the types section
  put32ULEB128(0x03); //TODO
  // table index (always 0)
  put8(0x00);
}

static void generate_i32_indirect_call_no_args(uint32_t func) {
  // instruction i32.const
  put8(0x41);
  // i32 literal (func)
  put32SLEB128((int) getTranslatedFunctionIndex(func));

  // call_indirect
  put8(0x11);
  // signature index
  // references the function signature with 1 int arg and int return type in the types section
  put32ULEB128(0x01); //TODO
  // table index (always 0)
  put8(0x00);
}


void generate_block_access_check(uint32_t codeBlockAddress) {
  generate_void_indirect_call_i32_arg((uint32_t) block_access_check, codeBlockAddress);
}


static void wasm_release_block(uint32_t block) {
  int i = 0;
  int k;
  struct precomp_instr* instr;

  while(i < numCompiledBlocks) {

    instr = compiledCodeBlocks[i];
    int shouldRelease = (instr->addr >> 12 == block);
    
    if (shouldRelease) {

      releaseWasmFunction((uint32_t) instr->ops);

      for (k = i; k < numCompiledBlocks; k++) {
        compiledCodeBlocks[k] = compiledCodeBlocks[k + 1];
      }
      numCompiledBlocks -= 1;

      // 'i' now points to the next instruction
    } else {
      i++;
    }
  }
}

static void releaseLRUBlock() {
  struct precomp_instr* blockToRelease = compiledCodeBlocks[0];

  if (blockToRelease->ops != cached_interp_NOTCOMPILED) {
    //printf("Releasing function %u for instruction %u\n", blockToRelease->ops,
    //blockToRelease->addr);
    releaseWasmFunction((uint32_t) blockToRelease->ops);
    blockToRelease->ops = cached_interp_NOTCOMPILED;
    viArrived++;
  }

  numCompiledBlocks -= 1;

  
  int i;
  for (i = 0; i < numCompiledBlocks; i++) {
    compiledCodeBlocks[i] = compiledCodeBlocks[i + 1];
  }

}

void recomp_wasm_init_block(struct r4300_core* r4300, uint32_t address) {
  wasm_release_block(address >> 12);  
  cached_interp_init_block(r4300, address);
}

void wasm_recompile_block(struct r4300_core* r4300, const uint32_t* iw, struct precomp_block* block, uint32_t func) {


  if (numCompiledBlocks >= MAX_COMPILED_BLOCKS) {
    releaseLRUBlock();
  }
  
  int i, length, length2, finished;
    struct precomp_instr* inst;
    enum r4300_opcode opcode;

    uint32_t opcode_count = 0;

    init_wasm_module_code();    

    /* ??? not sure why we need these 2 different tests */
    int block_start_in_tlb = ((block->start & UINT32_C(0xc0000000)) != UINT32_C(0x80000000));
    int block_not_in_tlb = (block->start >= UINT32_C(0xc0000000) || block->end < UINT32_C(0x80000000));

    length = get_block_length(block);
    length2 = length - 2 + (length >> 2); // Something to do with jumps?

    /* reset xxhash */
    block->xxhash = 0;


    start_wasm_code_section();
    start_wasm_code_section_function_body();

    generate_block_access_check(block->start + (func & 0xFFF));
    
    // (func & 0xFFF) finds the byte offset for `func` within the block
    // Divide by 4 to get the instruction index
    for (i = (func & 0xFFF) / 4, finished = 0; finished != 2; ++i)
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
        inst->decodedOpcode = idec->opcode;

        //printf("decoded %s (%u)\n", opcode_names[opcode], opcode);
        
        // r4300_decode sets ops to an interpretive function, which we undo here
        inst->ops = (void*) opsBefore;

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


        if (finished != 2) {
          next_iw = iw[i+1];
          next_idec = r4300_get_idec(next_iw);
          next_opcode = next_idec->opcode;
        }

        if (!skip_next_instruction_assembly) {
          //gen_table[idec->opcode]();
          gen_inst(opcode, idec, iw[i]);
          //printf(" (generated)\n");
          // generate the wasm code for the instruction
        } else {
          //printf(" (not generated)\n");
          skip_next_instruction_assembly = 0;
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

    end_wasm_code_section_function_body();
    end_wasm_code_section();

    // TODO - Include this
    uint32_t compiledFunction = compileAndPatchModule(func >> 12,
                                                      wasm_code,
                                                      wasm_code_length,
                                                      usedFunctions,
                                                      numUsedFunctions);

    compileCount++;
    if (compileCount >= 3000) {
      //afterCondition = 1;
    }
    
    //printf("compiledFunction: %d\n", compiledFunction);
    inst = block->block + ((func & 0xFFF) / 4);
    inst->ops = (void*) compiledFunction;

    compiledCodeBlocks[numCompiledBlocks++] = inst;

    
    // TODO - Free buffer at some point?
    // TODO - Profit?
    
#ifdef DBG
    DebugMessage(M64MSG_INFO, "block recompiled (%" PRIX32 "-%" PRIX32 ")", func, block->start+i*4);
#endif

}
