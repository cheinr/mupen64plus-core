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

// In jslib/corelib.js
extern uint32_t compileAndPatchModule(void* moduleDataPointer, int moduleLength);

/* TODO: implement them properly */
#define cached_interp_BC0F        cached_interp_NI
#define cached_interp_BC0F_IDLE   cached_interp_NI
#define cached_interp_BC0F_OUT    cached_interp_NI
#define cached_interp_BC0FL       cached_interp_NI
#define cached_interp_BC0FL_IDLE  cached_interp_NI
#define cached_interp_BC0FL_OUT   cached_interp_NI
#define cached_interp_BC0T        cached_interp_NI
#define cached_interp_BC0T_IDLE   cached_interp_NI
#define cached_interp_BC0T_OUT    cached_interp_NI
#define cached_interp_BC0TL       cached_interp_NI
#define cached_interp_BC0TL_IDLE  cached_interp_NI
#define cached_interp_BC0TL_OUT   cached_interp_NI
#define cached_interp_BC2F        cached_interp_NI
#define cached_interp_BC2F_IDLE   cached_interp_NI
#define cached_interp_BC2F_OUT    cached_interp_NI
#define cached_interp_BC2FL       cached_interp_NI
#define cached_interp_BC2FL_IDLE  cached_interp_NI
#define cached_interp_BC2FL_OUT   cached_interp_NI
#define cached_interp_BC2T        cached_interp_NI
#define cached_interp_BC2T_IDLE   cached_interp_NI
#define cached_interp_BC2T_OUT    cached_interp_NI
#define cached_interp_BC2TL       cached_interp_NI
#define cached_interp_BC2TL_IDLE  cached_interp_NI
#define cached_interp_BC2TL_OUT   cached_interp_NI
#define cached_interp_BREAK       cached_interp_NI
#define cached_interp_CFC0        cached_interp_NI
#define cached_interp_CFC2        cached_interp_NI
#define cached_interp_CTC0        cached_interp_NI
#define cached_interp_CTC2        cached_interp_NI
#define cached_interp_DMFC0       cached_interp_NI
#define cached_interp_DMFC2       cached_interp_NI
#define cached_interp_DMTC0       cached_interp_NI
#define cached_interp_DMTC2       cached_interp_NI
#define cached_interp_LDC2        cached_interp_NI
#define cached_interp_LWC2        cached_interp_NI
#define cached_interp_LLD         cached_interp_NI
#define cached_interp_MFC2        cached_interp_NI
#define cached_interp_MTC2        cached_interp_NI
#define cached_interp_SCD         cached_interp_NI
#define cached_interp_SDC2        cached_interp_NI
#define cached_interp_SWC2        cached_interp_NI
#define cached_interp_JR_IDLE     cached_interp_NI
#define cached_interp_JALR_IDLE   cached_interp_NI
#define cached_interp_CP1_ABS     cached_interp_RESERVED
#define cached_interp_CP1_ADD     cached_interp_RESERVED
#define cached_interp_CP1_CEIL_L  cached_interp_RESERVED
#define cached_interp_CP1_CEIL_W  cached_interp_RESERVED
#define cached_interp_CP1_C_EQ    cached_interp_RESERVED
#define cached_interp_CP1_C_F     cached_interp_RESERVED
#define cached_interp_CP1_C_LE    cached_interp_RESERVED
#define cached_interp_CP1_C_LT    cached_interp_RESERVED
#define cached_interp_CP1_C_NGE   cached_interp_RESERVED
#define cached_interp_CP1_C_NGL   cached_interp_RESERVED
#define cached_interp_CP1_C_NGLE  cached_interp_RESERVED
#define cached_interp_CP1_C_NGT   cached_interp_RESERVED
#define cached_interp_CP1_C_OLE   cached_interp_RESERVED
#define cached_interp_CP1_C_OLT   cached_interp_RESERVED
#define cached_interp_CP1_C_SEQ   cached_interp_RESERVED
#define cached_interp_CP1_C_SF    cached_interp_RESERVED
#define cached_interp_CP1_C_UEQ   cached_interp_RESERVED
#define cached_interp_CP1_C_ULE   cached_interp_RESERVED
#define cached_interp_CP1_C_ULT   cached_interp_RESERVED
#define cached_interp_CP1_C_UN    cached_interp_RESERVED
#define cached_interp_CP1_CVT_D   cached_interp_RESERVED
#define cached_interp_CP1_CVT_L   cached_interp_RESERVED
#define cached_interp_CP1_CVT_S   cached_interp_RESERVED
#define cached_interp_CP1_CVT_W   cached_interp_RESERVED
#define cached_interp_CP1_DIV     cached_interp_RESERVED
#define cached_interp_CP1_FLOOR_L cached_interp_RESERVED
#define cached_interp_CP1_FLOOR_W cached_interp_RESERVED
#define cached_interp_CP1_MOV     cached_interp_RESERVED
#define cached_interp_CP1_MUL     cached_interp_RESERVED
#define cached_interp_CP1_NEG     cached_interp_RESERVED
#define cached_interp_CP1_ROUND_L cached_interp_RESERVED
#define cached_interp_CP1_ROUND_W cached_interp_RESERVED
#define cached_interp_CP1_SQRT    cached_interp_RESERVED
#define cached_interp_CP1_SUB     cached_interp_RESERVED
#define cached_interp_CP1_TRUNC_L cached_interp_RESERVED
#define cached_interp_CP1_TRUNC_W cached_interp_RESERVED

#define X(op) cached_interp_##op
static void (*const ci_table[R4300_OPCODES_COUNT])(void) =
{
    #include "opcodes.md"
};
#undef X

// TODO - Add as a new struct in r4300?
unsigned char *wasm_code;
int max_wasm_code_length = 0;
int wasm_code_length = 0;
int reusable_wasm_boilerplate_length = 0;

int code_section_start = 0;
int last_function_body_start = 0;

static void put8(unsigned char octet)
{

  printf("writing byte %x at position %x\n", octet, wasm_code_length);
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

static void edit32ULEB128(unsigned int dword, uint32_t destinationByteIndex) {

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
    unsigned char byte = (value & 0x0000007F);
    value = value >> 7;

    if (value != 0) {
      // Set high order bit to 1
      byte = byte | 0x80;
    }

    printf("writing byte %x at position %x\n", byte, wasmCodeIndex);
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

static void put32ULEB128(unsigned int dword) {
  edit32ULEB128(dword, wasm_code_length);
}

static uint32_t num_bytes_needed_for_32ULEB128(unsigned int dword) {

  int value = dword;
  uint32_t numBytesNeeded = 0;

  do {
    value = value >> 7;
    numBytesNeeded++;
  } while (value != 0);  

  return numBytesNeeded;
}

static void shiftBytesOver(uint32_t startByte, uint32_t numBytesToShift) {

  printf("shifting bytes over %d bytes starting at %x\n", numBytesToShift, startByte);
  
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
      printf("shifting 0x%x\n", i - numBytesToShift);
    }
    wasm_code[i] = wasm_code[i - numBytesToShift];
  }

  wasm_code_length += numBytesToShift;
}
 
static void generate_interpretter_function_call(uint32_t func) {

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
}

static void generate_types_section() {
  printf("generate_types_section\n");
  // section code
  put8(0x01);
  // section size
  put32ULEB128(0x04);
  // num types
  put32ULEB128(0x01);

  // func type 0
  put8(0x60);
  // num params
  put8(0x00);
  // num results
  put8(0x00);
}

static void generate_function_section() {
  printf("generate_function_section\n");
  // section code
  put8(0x03);
  printf("generate_function_section\n");
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

  printf("generate_imports_section 1\n");
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

  printf("generate_imports_section 2\n");
  // import header 1 (mem)
  // string length
  put32ULEB128(0x03);
  // "env"
  put8(0x65); put8(0x6e); put8(0x76);
  printf("generate_imports_section 2\n");
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
  printf("generate_exports_section\n");
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
  printf("start_wasm_code_section\n");

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

  
  printf("end_wasm_code_section numBytesForULEBLength: %d\n", numBytesForULEBLength);
  if (numBytesForULEBLength > 1) {
    shiftBytesOver(code_section_start + 2, numBytesForULEBLength - 1);
  }

  printf("codeSectionByteLength: %d\n", codeSectionByteLength);
  edit32ULEB128(codeSectionByteLength, code_section_start + 1);
}


static void start_wasm_code_section_function_body() {
  printf("start_wasm_code_section_function_body\n");

  // Used to calculate the function body size later
  last_function_body_start = wasm_code_length;

  // func body size (placeholder)
  put8(0x00); // 3d
  // local decl count
  put8(0x00); // 3e + 1
}

static void end_wasm_code_section_function_body() {

  // function end
  put8(0x0b);

  // FIXUP function body size
  uint32_t functionBodyByteLength = wasm_code_length - (last_function_body_start + 1);

  uint32_t numBytesForULEBLength = num_bytes_needed_for_32ULEB128(functionBodyByteLength);
  printf("end_wasm_code_section_function_body numBytesForULEBLength: %d\n", numBytesForULEBLength);
  if (numBytesForULEBLength > 1) {
    shiftBytesOver(last_function_body_start + 1, numBytesForULEBLength - 1);
  }

  printf("functionBodyByteLength: %d\n", functionBodyByteLength);
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

void wasm_recompile_block(struct r4300_core* r4300, const uint32_t* iw, struct precomp_block* block, uint32_t func) {

  printf("wasm_recompile_block!\n");
  
    int i, length, length2, finished;
    struct precomp_instr* inst;
    enum r4300_opcode opcode;

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

        /* decode instruction */
        opcode = r4300_decode(inst, r4300, r4300_get_idec(iw[i]), iw[i], iw[i+1], block);

        generate_interpretter_function_call((uint32_t) ci_table[opcode]);

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

    end_wasm_code_section_function_body();
    end_wasm_code_section();

    // TODO - Include this
    uint32_t compiledFunction = compileAndPatchModule(wasm_code,
                                                      wasm_code_length);

    inst = block->block + ((func & 0xFFF) / 4);
    inst->ops = (void*) compiledFunction;
    
    // TODO - Free buffer at some point?
    // TODO - Profit?
    
#ifdef DBG
    DebugMessage(M64MSG_INFO, "block recompiled (%" PRIX32 "-%" PRIX32 ")", func, block->start+i*4);
#endif

}
