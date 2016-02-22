/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - pure_interp.c                                           *
 *   Mupen64Plus homepage: http://code.google.com/p/mupen64plus/           *
 *   Copyright (C) 2015 Nebuleon <nebuleon.fumika@gmail.com>               *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.          *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdint.h>
#include <stdio.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include "api/callbacks.h"
#include "api/debugger.h"
#include "api/m64p_types.h"
/* TLBWrite requires invalid_code and blocks from cached_interp.h, but only if
 * (at run time) the active core is not the Pure Interpreter. */
#include "cached_interp.h"
#include "cp0_private.h"
#include "cp1_private.h"
#include "exception.h"
#include "interupt.h"
#include "main/main.h"
#include "memory/memory.h"
#include "osal/preproc.h"
#include "r4300.h"
#include "tlb.h"

#ifdef DBG
#include "debugger/dbg_types.h"
#endif

#ifdef EMSCRIPTEN
#include "vi/vi_controller.h"
#include "emscripten.h"

//#include <iostream>

// global that we use to indicate a vi has arrived
int viArrived = 0;

#endif

static precomp_instr interp_PC;

static void InterpretOpcode(void);

#define PCADDR interp_PC.addr
#define ADD_TO_PC(x) interp_PC.addr += x*4;
#define DECLARE_INSTRUCTION(name) static void name(uint32_t op)
#define DECLARE_JUMP(name, destination, condition, link, likely, cop1) \
   static void name(uint32_t op) \
   { \
      const int take_jump = (condition); \
      const uint32_t jump_target = (destination); \
      int64_t *link_register = (link); \
      if (cop1 && check_cop1_unusable()) return; \
      if (link_register != &reg[0]) \
      { \
          *link_register = SE32(interp_PC.addr + 8); \
      } \
      if (!likely || take_jump) \
      { \
        interp_PC.addr += 4; \
        delay_slot=1; \
        InterpretOpcode(); \
        cp0_update_count(); \
        delay_slot=0; \
        if (take_jump && !skip_jump) \
        { \
          interp_PC.addr = jump_target; \
        } \
      } \
      else \
      { \
         interp_PC.addr += 8; \
         cp0_update_count(); \
      } \
      last_addr = interp_PC.addr; \
      if (next_interupt <= g_cp0_regs[CP0_COUNT_REG]) gen_interupt(); \
   } \
   static void name##_IDLE(uint32_t op) \
   { \
      const int take_jump = (condition); \
      int skip; \
      if (cop1 && check_cop1_unusable()) return; \
      if (take_jump) \
      { \
         cp0_update_count(); \
         skip = next_interupt - g_cp0_regs[CP0_COUNT_REG]; \
         if (skip > 3) g_cp0_regs[CP0_COUNT_REG] += (skip & UINT32_C(0xFFFFFFFC)); \
         else name(op); \
      } \
      else name(op); \
   }
#define CHECK_MEMORY()

#define RD_OF(op)      (((op) >> 11) & 0x1F)
#define RS_OF(op)      (((op) >> 21) & 0x1F)
#define RT_OF(op)      (((op) >> 16) & 0x1F)
#define SA_OF(op)      (((op) >>  6) & 0x1F)
#define IMM16S_OF(op)  ((int16_t) (op))
#define IMM16U_OF(op)  ((uint16_t) (op))
#define FD_OF(op)      (((op) >>  6) & 0x1F)
#define FS_OF(op)      (((op) >> 11) & 0x1F)
#define FT_OF(op)      (((op) >> 16) & 0x1F)
#define JUMP_OF(op)    ((op) & UINT32_C(0x3FFFFFF))

/* Determines whether a relative jump in a 16-bit immediate goes back to the
 * same instruction without doing any work in its delay slot. The jump is
 * relative to the instruction in the delay slot, so 1 instruction backwards
 * (-1) goes back to the jump. */
#define IS_RELATIVE_IDLE_LOOP(op, addr) \
	(IMM16S_OF(op) == -1 && *fast_mem_access((addr) + 4) == 0)

/* Determines whether an absolute jump in a 26-bit immediate goes back to the
 * same instruction without doing any work in its delay slot. The jump is
 * in the same 256 MiB segment as the delay slot, so if the jump instruction
 * is at the last address in its segment, it does not jump back to itself. */
#define IS_ABSOLUTE_IDLE_LOOP(op, addr) \
	(JUMP_OF(op) == ((addr) & UINT32_C(0x0FFFFFFF)) >> 2 \
	 && ((addr) & UINT32_C(0x0FFFFFFF)) != UINT32_C(0x0FFFFFFC) \
	 && *fast_mem_access((addr) + 4) == 0)

#define SE8(a) ((int64_t) ((int8_t) (a)))
#define SE16(a) ((int64_t) ((int16_t) (a)))
#define SE32(a) ((int64_t) ((int32_t) (a)))

/* These macros are like those in macros.h, but they parse opcode fields. */
#define rrt reg[RT_OF(op)]
#define rrd reg[RD_OF(op)]
#define rfs FS_OF(op)
#define rrs reg[RS_OF(op)]
#define rsa SA_OF(op)
#define irt reg[RT_OF(op)]
#define ioffset IMM16S_OF(op)
#define iimmediate IMM16S_OF(op)
#define irs reg[RS_OF(op)]
#define ibase reg[RS_OF(op)]
#define jinst_index JUMP_OF(op)
#define lfbase RS_OF(op)
#define lfft FT_OF(op)
#define lfoffset IMM16S_OF(op)
#define cfft FT_OF(op)
#define cffs FS_OF(op)
#define cffd FD_OF(op)

// 32 bits macros
#ifndef M64P_BIG_ENDIAN
#define rrt32 *((int32_t*) &reg[RT_OF(op)])
#define rrd32 *((int32_t*) &reg[RD_OF(op)])
#define rrs32 *((int32_t*) &reg[RS_OF(op)])
#define irs32 *((int32_t*) &reg[RS_OF(op)])
#define irt32 *((int32_t*) &reg[RT_OF(op)])
#else
#define rrt32 *((int32_t*) &reg[RT_OF(op)] + 1)
#define rrd32 *((int32_t*) &reg[RD_OF(op)] + 1)
#define rrs32 *((int32_t*) &reg[RS_OF(op)] + 1)
#define irs32 *((int32_t*) &reg[RS_OF(op)] + 1)
#define irt32 *((int32_t*) &reg[RT_OF(op)] + 1)
#endif

// two functions are defined from the macros above but never used
// these prototype declarations will prevent a warning
#if defined(__GNUC__)
  static void JR_IDLE(uint32_t) __attribute__((used));
  static void JALR_IDLE(uint32_t) __attribute__((used));
#endif

#include "interpreter.def"

// Array based access interpreter loop on emscripten
// because of degenerate V8 switch statement performance.
#if EMSCRIPTEN

void BNEHandler(uint32_t op) // MAJOR OPCODE 5
{
  if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BNE_IDLE(op);
  else                                     BNE(op);
}

void ADDIHandler(uint32_t op) /* Major opcode 8: ADDI */
{
  if (RT_OF(op) != 0) ADDI(op);
  else                NOP(0);
}

void ADDIUHandler(uint32_t op) /* Major opcode 9: ADDIU */
{
  if (RT_OF(op) != 0) ADDIU(op);
  else                NOP(0);
}

void ANDIHandler(uint32_t op)/* Major opcode 12: ANDI */
{
  if (RT_OF(op) != 0) ANDI(op);
  else                NOP(0);
}

void LUIHandler(uint32_t op) // MAJOR OPCODE 15
{
  if (RT_OF(op) != 0) LUI(op);
	else                NOP(0);
}

void LWHandler(uint32_t op) /* Major opcode 35: LW */
{
  if (RT_OF(op) != 0) LW(op);
  else                NOP(0);
}

void SpecialPrefixHandler(uint32_t op)
{
  switch (op & 0x3F) {
  case 0: /* SPECIAL opcode 0: SLL */
    if (RD_OF(op) != 0) SLL(op);
    else                NOP(0);
    break;
  case 2: /* SPECIAL opcode 2: SRL */
    if (RD_OF(op) != 0) SRL(op);
    else                NOP(0);
    break;
  case 3: /* SPECIAL opcode 3: SRA */
    if (RD_OF(op) != 0) SRA(op);
    else                NOP(0);
    break;
  case 4: /* SPECIAL opcode 4: SLLV */
    if (RD_OF(op) != 0) SLLV(op);
    else                NOP(0);
    break;
  case 6: /* SPECIAL opcode 6: SRLV */
    if (RD_OF(op) != 0) SRLV(op);
    else                NOP(0);
    break;
  case 7: /* SPECIAL opcode 7: SRAV */
    if (RD_OF(op) != 0) SRAV(op);
    else                NOP(0);
    break;
  case 8: JR(op); break;
  case 9: /* SPECIAL opcode 9: JALR */
    /* Note: This can omit the check for Rd == 0 because the JALR
     * function checks for link_register != &reg[0]. If you're
     * using this as a reference for a JIT, do check Rd == 0 in it. */
    JALR(op);
    break;
  case 12: SYSCALL(op); break;
  case 13: /* SPECIAL opcode 13: BREAK (Not implemented) */
    NI(op);
    break;
  case 15: SYNC(op); break;
  case 16: /* SPECIAL opcode 16: MFHI */
    if (RD_OF(op) != 0) MFHI(op);
    else                NOP(0);
    break;
  case 17: MTHI(op); break;
  case 18: /* SPECIAL opcode 18: MFLO */
    if (RD_OF(op) != 0) MFLO(op);
    else                NOP(0);
    break;
  case 19: MTLO(op); break;
  case 20: /* SPECIAL opcode 20: DSLLV */
    if (RD_OF(op) != 0) DSLLV(op);
    else                NOP(0);
    break;
  case 22: /* SPECIAL opcode 22: DSRLV */
    if (RD_OF(op) != 0) DSRLV(op);
    else                NOP(0);
    break;
  case 23: /* SPECIAL opcode 23: DSRAV */
    if (RD_OF(op) != 0) DSRAV(op);
    else                NOP(0);
    break;
  case 24: MULT(op); break;
  case 25: MULTU(op); break;
  case 26: DIV(op); break;
  case 27: DIVU(op); break;
  case 28: DMULT(op); break;
  case 29: DMULTU(op); break;
  case 30: DDIV(op); break;
  case 31: DDIVU(op); break;
  case 32: /* SPECIAL opcode 32: ADD */
    if (RD_OF(op) != 0) ADD(op);
    else                NOP(0);
    break;
  case 33: /* SPECIAL opcode 33: ADDU */
    if (RD_OF(op) != 0) ADDU(op);
    else                NOP(0);
    break;
  case 34: /* SPECIAL opcode 34: SUB */
    if (RD_OF(op) != 0) SUB(op);
    else                NOP(0);
    break;
  case 35: /* SPECIAL opcode 35: SUBU */
    if (RD_OF(op) != 0) SUBU(op);
    else                NOP(0);
    break;
  case 36: /* SPECIAL opcode 36: AND */
    if (RD_OF(op) != 0) AND(op);
    else                NOP(0);
    break;
  case 37: /* SPECIAL opcode 37: OR */
    if (RD_OF(op) != 0) OR(op);
    else                NOP(0);
    break;
  case 38: /* SPECIAL opcode 38: XOR */
    if (RD_OF(op) != 0) XOR(op);
    else                NOP(0);
    break;
  case 39: /* SPECIAL opcode 39: NOR */
    if (RD_OF(op) != 0) NOR(op);
    else                NOP(0);
    break;
  case 42: /* SPECIAL opcode 42: SLT */
    if (RD_OF(op) != 0) SLT(op);
    else                NOP(0);
    break;
  case 43: /* SPECIAL opcode 43: SLTU */
    if (RD_OF(op) != 0) SLTU(op);
    else                NOP(0);
    break;
  case 44: /* SPECIAL opcode 44: DADD */
    if (RD_OF(op) != 0) DADD(op);
    else                NOP(0);
    break;
  case 45: /* SPECIAL opcode 45: DADDU */
    if (RD_OF(op) != 0) DADDU(op);
    else                NOP(0);
    break;
  case 46: /* SPECIAL opcode 46: DSUB */
    if (RD_OF(op) != 0) DSUB(op);
    else                NOP(0);
    break;
  case 47: /* SPECIAL opcode 47: DSUBU */
    if (RD_OF(op) != 0) DSUBU(op);
    else                NOP(0);
    break;
  case 48: /* SPECIAL opcode 48: TGE (Not implemented) */
  case 49: /* SPECIAL opcode 49: TGEU (Not implemented) */
  case 50: /* SPECIAL opcode 50: TLT (Not implemented) */
  case 51: /* SPECIAL opcode 51: TLTU (Not implemented) */
    NI(op);
    break;
  case 52: TEQ(op); break;
  case 54: /* SPECIAL opcode 54: TNE (Not implemented) */
    NI(op);
    break;
  case 56: /* SPECIAL opcode 56: DSLL */
    if (RD_OF(op) != 0) DSLL(op);
    else                NOP(0);
    break;
  case 58: /* SPECIAL opcode 58: DSRL */
    if (RD_OF(op) != 0) DSRL(op);
    else                NOP(0);
    break;
  case 59: /* SPECIAL opcode 59: DSRA */
    if (RD_OF(op) != 0) DSRA(op);
    else                NOP(0);
    break;
  case 60: /* SPECIAL opcode 60: DSLL32 */
    if (RD_OF(op) != 0) DSLL32(op);
    else                NOP(0);
    break;
  case 62: /* SPECIAL opcode 62: DSRL32 */
    if (RD_OF(op) != 0) DSRL32(op);
    else                NOP(0);
    break;
  case 63: /* SPECIAL opcode 63: DSRA32 */
    if (RD_OF(op) != 0) DSRA32(op);
    else                NOP(0);
    break;
  default: /* SPECIAL opcodes 1, 5, 10, 11, 14, 21, 40, 41, 53, 55, 57,
              61: Reserved Instructions */
    RESERVED(op);
    break;
  } /* switch (op & 0x3F) for the SPECIAL prefix */
}

void REGIMMPrefixHandler(uint32_t op)
{
  switch ((op >> 16) & 0x1F) {
  case 0: /* REGIMM opcode 0: BLTZ */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZ_IDLE(op);
    else                                     BLTZ(op);
    break;
  case 1: /* REGIMM opcode 1: BGEZ */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZ_IDLE(op);
    else                                     BGEZ(op);
    break;
  case 2: /* REGIMM opcode 2: BLTZL */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZL_IDLE(op);
    else                                     BLTZL(op);
    break;
  case 3: /* REGIMM opcode 3: BGEZL */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZL_IDLE(op);
    else                                     BGEZL(op);
    break;
  case 8: /* REGIMM opcode 8: TGEI (Not implemented) */
  case 9: /* REGIMM opcode 9: TGEIU (Not implemented) */
  case 10: /* REGIMM opcode 10: TLTI (Not implemented) */
  case 11: /* REGIMM opcode 11: TLTIU (Not implemented) */
  case 12: /* REGIMM opcode 12: TEQI (Not implemented) */
  case 14: /* REGIMM opcode 14: TNEI (Not implemented) */
    NI(op);
    break;
  case 16: /* REGIMM opcode 16: BLTZAL */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZAL_IDLE(op);
    else                                     BLTZAL(op);
    break;
  case 17: /* REGIMM opcode 17: BGEZAL */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZAL_IDLE(op);
    else                                     BGEZAL(op);
    break;
  case 18: /* REGIMM opcode 18: BLTZALL */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZALL_IDLE(op);
    else                                     BLTZALL(op);
    break;
  case 19: /* REGIMM opcode 19: BGEZALL */
    if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZALL_IDLE(op);
    else                                     BGEZALL(op);
    break;
  default: /* REGIMM opcodes 4..7, 13, 15, 20..31:
              Reserved Instructions */
    RESERVED(op);
    break;
  } /* switch ((op >> 16) & 0x1F) for the REGIMM prefix */
}

void Coprocessor0Handler(uint32_t op)
{
  switch ((op >> 21) & 0x1F) {
  case 0: /* Coprocessor 0 opcode 0: MFC0 */
    if (RT_OF(op) != 0) MFC0(op);
    else                NOP(0);
    break;
  case 4: MTC0(op); break;
  case 16: /* Coprocessor 0 opcode 16: TLB */
    switch (op & 0x3F) {
    case 1: TLBR(op); break;
    case 2: TLBWI(op); break;
    case 6: TLBWR(op); break;
    case 8: TLBP(op); break;
    case 24: ERET(op); break;
    default: /* TLB sub-opcodes 0, 3..5, 7, 9..23, 25..63:
                Reserved Instructions */
      RESERVED(op);
      break;
    } /* switch (op & 0x3F) for Coprocessor 0 TLB opcodes */
    break;
  default: /* Coprocessor 0 opcodes 1..3, 4..15, 17..31:
              Reserved Instructions */
    RESERVED(op);
    break;
  } /* switch ((op >> 21) & 0x1F) for the Coprocessor 0 prefix */
}

void (*COPROCESSOR_1S_OPCODE_ARRAY[])( uint32_t ) = {
  ADD_S, //case 0: ADD_S(op); break;
  SUB_S, //case 1: SUB_S(op); break;
  MUL_S, //case 2: MUL_S(op); break;
  DIV_S, //case 3: DIV_S(op); break;
  SQRT_S, //case 4: SQRT_S(op); break;
  ABS_S, //case 5: ABS_S(op); break;
  MOV_S, //case 6: MOV_S(op); break;
  NEG_S, //case 7: NEG_S(op); break;
  ROUND_L_S, //case 8: ROUND_L_S(op); break;
  TRUNC_L_S, //case 9: TRUNC_L_S(op); break;
  CEIL_L_S, //case 10: CEIL_L_S(op); break;
  FLOOR_L_S, //case 11: FLOOR_L_S(op); break;
  ROUND_W_S, //case 12: ROUND_W_S(op); break;
  TRUNC_W_S, //case 13: TRUNC_W_S(op); break;
  CEIL_W_S, //case 14: CEIL_W_S(op); break;
  FLOOR_W_S, //case 15: FLOOR_W_S(op); break;
  NOP, // 16
  NOP, // 17
  NOP, // 18
  NOP, // 19
  NOP, // 20
  NOP, // 21
  NOP, // 22
  NOP, // 23
  NOP, // 24
  NOP, // 25
  NOP, // 26
  NOP, // 27
  NOP, // 28
  NOP, // 29
  NOP, // 30
  NOP, // 31
  NOP, // 32
  CVT_D_S, //case 33: CVT_D_S(op); break;
  NOP, // 34
  NOP, // 35
  CVT_W_S, //case 36: CVT_W_S(op); break;
  CVT_L_S, //case 37: CVT_L_S(op); break;
  NOP, // 38
  NOP, // 39
  NOP, // 40
  NOP, // 41
  NOP, // 42
  NOP, // 43
  NOP, // 44
  NOP, // 45
  NOP, // 46
  NOP, // 47
  C_F_S, //case 48: C_F_S(op); break;
  C_UN_S, //case 49: C_UN_S(op); break;
  C_EQ_S, //case 50: C_EQ_S(op); break;
  C_UEQ_S, //case 51: C_UEQ_S(op); break;
  C_OLT_S, //case 52: C_OLT_S(op); break;
  C_ULT_S, //case 53: C_ULT_S(op); break;
  C_OLE_S, //case 54: C_OLE_S(op); break;
  C_ULE_S, //case 55: C_ULE_S(op); break;
  C_SF_S, //case 56: C_SF_S(op); break;
  C_NGLE_S, //case 57: C_NGLE_S(op); break;
  C_SEQ_S, //case 58: C_SEQ_S(op); break;
  C_NGL_S, //case 59: C_NGL_S(op); break;
  C_LT_S, //case 60: C_LT_S(op); break;
  C_NGE_S, //case 61: C_NGE_S(op); break;
  C_LE_S, //case 62: C_LE_S(op); break;
  C_NGL_S, //case 63: C_NGT_S(op); break;
  //default: /* Coprocessor 1 S-format opcodes 16..32, 34..35, 38..47:
};

void Coprocessor1Handler(uint32_t op)
{
  switch ((op >> 21) & 0x1F) {
  case 0: /* Coprocessor 1 opcode 0: MFC1 */
    if (RT_OF(op) != 0) MFC1(op);
    else                NOP(0);
    break;
  case 1: /* Coprocessor 1 opcode 1: DMFC1 */
    if (RT_OF(op) != 0) DMFC1(op);
    else                NOP(0);
    break;
  case 2: /* Coprocessor 1 opcode 2: CFC1 */
    if (RT_OF(op) != 0) CFC1(op);
    else                NOP(0);
    break;
  case 4: MTC1(op); break;
  case 5: DMTC1(op); break;
  case 6: CTC1(op); break;
  case 8: /* Coprocessor 1 opcode 8: Branch on C1 condition... */
    switch ((op >> 16) & 0x3) {
    case 0: /* opcode 0: BC1F */
      if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1F_IDLE(op);
      else                                     BC1F(op);
      break;
    case 1: /* opcode 1: BC1T */
      if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1T_IDLE(op);
      else                                     BC1T(op);
      break;
    case 2: /* opcode 2: BC1FL */
      if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1FL_IDLE(op);
      else                                     BC1FL(op);
      break;
    case 3: /* opcode 3: BC1TL */
      if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1TL_IDLE(op);
      else                                     BC1TL(op);
      break;
    } /* switch ((op >> 16) & 0x3) for branches on C1 condition */
    break;
  case 16: /* Coprocessor 1 S-format opcodes */
#if 1
    {
      uint32_t c = op & 0x3F;
      COPROCESSOR_1S_OPCODE_ARRAY[c](op);
    }
    break;
#else
    switch (op & 0x3F) {
    case 0: ADD_S(op); break;
    case 1: SUB_S(op); break;
    case 2: MUL_S(op); break;
    case 3: DIV_S(op); break;
    case 4: SQRT_S(op); break;
    case 5: ABS_S(op); break;
    case 6: MOV_S(op); break;
    case 7: NEG_S(op); break;
    case 8: ROUND_L_S(op); break;
    case 9: TRUNC_L_S(op); break;
    case 10: CEIL_L_S(op); break;
    case 11: FLOOR_L_S(op); break;
    case 12: ROUND_W_S(op); break;
    case 13: TRUNC_W_S(op); break;
    case 14: CEIL_W_S(op); break;
    case 15: FLOOR_W_S(op); break;
    case 33: CVT_D_S(op); break;
    case 36: CVT_W_S(op); break;
    case 37: CVT_L_S(op); break;
    case 48: C_F_S(op); break;
    case 49: C_UN_S(op); break;
    case 50: C_EQ_S(op); break;
    case 51: C_UEQ_S(op); break;
    case 52: C_OLT_S(op); break;
    case 53: C_ULT_S(op); break;
    case 54: C_OLE_S(op); break;
    case 55: C_ULE_S(op); break;
    case 56: C_SF_S(op); break;
    case 57: C_NGLE_S(op); break;
    case 58: C_SEQ_S(op); break;
    case 59: C_NGL_S(op); break;
    case 60: C_LT_S(op); break;
    case 61: C_NGE_S(op); break;
    case 62: C_LE_S(op); break;
    case 63: C_NGT_S(op); break;
    default: /* Coprocessor 1 S-format opcodes 16..32, 34..35, 38..47:
                Reserved Instructions */
      RESERVED(op);
      break;
    } /* switch (op & 0x3F) for Coprocessor 1 S-format opcodes */
    break;
#endif
  case 17: /* Coprocessor 1 D-format opcodes */
    switch (op & 0x3F) {
    case 0: ADD_D(op); break;
    case 1: SUB_D(op); break;
    case 2: MUL_D(op); break;
    case 3: DIV_D(op); break;
    case 4: SQRT_D(op); break;
    case 5: ABS_D(op); break;
    case 6: MOV_D(op); break;
    case 7: NEG_D(op); break;
    case 8: ROUND_L_D(op); break;
    case 9: TRUNC_L_D(op); break;
    case 10: CEIL_L_D(op); break;
    case 11: FLOOR_L_D(op); break;
    case 12: ROUND_W_D(op); break;
    case 13: TRUNC_W_D(op); break;
    case 14: CEIL_W_D(op); break;
    case 15: FLOOR_W_D(op); break;
    case 32: CVT_S_D(op); break;
    case 36: CVT_W_D(op); break;
    case 37: CVT_L_D(op); break;
    case 48: C_F_D(op); break;
    case 49: C_UN_D(op); break;
    case 50: C_EQ_D(op); break;
    case 51: C_UEQ_D(op); break;
    case 52: C_OLT_D(op); break;
    case 53: C_ULT_D(op); break;
    case 54: C_OLE_D(op); break;
    case 55: C_ULE_D(op); break;
    case 56: C_SF_D(op); break;
    case 57: C_NGLE_D(op); break;
    case 58: C_SEQ_D(op); break;
    case 59: C_NGL_D(op); break;
    case 60: C_LT_D(op); break;
    case 61: C_NGE_D(op); break;
    case 62: C_LE_D(op); break;
    case 63: C_NGT_D(op); break;
    default: /* Coprocessor 1 D-format opcodes 16..31, 33..35, 38..47:
                Reserved Instructions */
      RESERVED(op);
      break;
    } /* switch (op & 0x3F) for Coprocessor 1 D-format opcodes */
    break;
  case 20: /* Coprocessor 1 W-format opcodes */
    switch (op & 0x3F) {
    case 32: CVT_S_W(op); break;
    case 33: CVT_D_W(op); break;
    default: /* Coprocessor 1 W-format opcodes 0..31, 34..63:
                Reserved Instructions */
      RESERVED(op);
      break;
    }
    break;
  case 21: /* Coprocessor 1 L-format opcodes */
    switch (op & 0x3F) {
    case 32: CVT_S_L(op); break;
    case 33: CVT_D_L(op); break;
    default: /* Coprocessor 1 L-format opcodes 0..31, 34..63:
                Reserved Instructions */
      RESERVED(op);
      break;
    }
    break;
  default: /* Coprocessor 1 opcodes 3, 7, 9..15, 18..19, 22..31:
              Reserved Instructions */
    RESERVED(op);
    break;
  } /* switch ((op >> 21) & 0x1F) for the Coprocessor 1 prefix */
}

//typedef void (*)(int)

void (*OPCODE_ARRAY[])( uint32_t ) = {
  SpecialPrefixHandler, // 0
  REGIMMPrefixHandler, // 1
  J, //2
	JAL, /* Major opcode 3: JAL */
	BEQ, /* Major opcode 4: BEQ */
	BNE, //BNE,/* Major opcode 5: BNE */
	BLEZ, /* Major opcode 6: BLEZ */
	BGTZ, /* Major opcode 7: BGTZ */
	ADDI, /* Major opcode 8: ADDI */
	ADDIU, /* Major opcode 9: ADDIU */
	SLTI, /* Major opcode 10: SLTI */
	SLTIU,/* Major opcode 11: SLTIU */
	ANDI, /* Major opcode 12: ANDI */
  ORI, /* Major opcode 13: ORI */
  XORI, /* Major opcode 14: XORI */
  LUI, /* Major opcode 15: LUI */
  Coprocessor0Handler, /*16 Coprocessor 1 S-format opcodes */
  Coprocessor1Handler, /*17 Coprocessor 1 D-format opcodes */
  NOP, // 18 RESERVERD
  NOP, // 19 RESERVED
  BEQL, /* Major opcode 20: BEQL */
	BNEL,/* Major opcode 21: BNEL */
  BLEZL, /* Major opcode 22: BLEZL */
	BGTZL,/* Major opcode 23: BGTZL */
	DADDI,/* Major opcode 24: DADDI */
	DADDIU, /* Major opcode 25: DADDIU */
	LDL,/* Major opcode 26: LDL */
	LDR, /* Major opcode 27: LDR */
  NOP, // 28 RESERVED
  NOP, // 29 RESERVED
  NOP, // 30
  NOP, // 31
	LB,/* Major opcode 32: LB */
	LH, /* Major opcode 33: LH */
	LWL, /* Major opcode 34: LWL */
	LW, /* Major opcode 35: LW */
	LBU, /* Major opcode 36: LBU */
	LHU, /* Major opcode 37: LHU */
	LWR, /* Major opcode 38: LWR */
	LWU, /* Major opcode 39: LWU */
	SB, //case 40: SB(op); break;
	SH, //case 41: SH(op); break;
	SWL, //case 42: SWL(op); break;
	SW, //case 43: SW(op); break;
	SDL, //case 44: SDL(op); break;
	SDR, //case 45: SDR(op); break;
	SWR, //case 46: SWR(op); break;
	CACHE, //case 47: CACHE(op); break;
	LL, /* Major opcode 48: LL */
	LWC1, //case 49: LWC1(op); break;
  NOP, // 50 RESERVED
  NOP, // 51 RESERVED
	NI,/* Major opcode 52: LLD (Not implemented) */
	LDC1, //case 53: LDC1(op); break;
  NOP,//54 RESERVED
	LD, /* Major opcode 55: LD */
	SC, /* Major opcode 56: SC */
	SWC1, //case 57: SWC1(op); break;
  NOP,// 58 RESERVED
  NOP,// 59 RESERVED
	NI, //case 60: /* Major opcode 60: SCD (Not implemented) */
	SDC1, //case 61: SDC1(op); break;
  NOP,//62 RESERVED
	SD //case 63: SD(op); break;
};


#endif

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

void InterpretOpcode()
{
	uint32_t op = *fast_mem_access(PC->addr);

#if 0 //EMSCRIPTEN

    // Emscripten generated javascript does not like nested switch
    // statements. Thus we change the primary opcode switch to a much
    // faster function pointer array.
    int code = ((op >> 26) & 0x3F);

#if USE_INLINE_JAVASCRIPT_OPCODES
  EM_ASM_INT({
    var op = $0|0;
    var code = $1|0;

    if(!Module.OPCODE_TABLE)
    {
      Module.OPCODE_TABLE = [];
      Module.OPCODE_TABLE[0] = _SpecialPrefixHandler; // 0
      Module.OPCODE_TABLE[1] = _REGIMMPrefixHandler; // 1
      Module.OPCODE_TABLE[2] = _J; //2
    	Module.OPCODE_TABLE[3] = _JAL; /* Major opcode 3: JAL */
    	Module.OPCODE_TABLE[4] = _BEQ; /* Major opcode 4: BEQ */
    	Module.OPCODE_TABLE[5] = _BNE; //BNE,/* Major opcode 5: BNE */
    	Module.OPCODE_TABLE[6] = _BLEZ; /* Major opcode 6: BLEZ */
    	Module.OPCODE_TABLE[7] = _BGTZ; /* Major opcode 7: BGTZ */
    	Module.OPCODE_TABLE[8] = _ADDI; /* Major opcode 8: ADDI */
    	Module.OPCODE_TABLE[9] = _ADDIU; /* Major opcode 9: ADDIU */
    	Module.OPCODE_TABLE[10] = _SLTI; /* Major opcode 10: SLTI */
    	Module.OPCODE_TABLE[11] = _SLTIU;/* Major opcode 11: SLTIU */
    	Module.OPCODE_TABLE[12] = _ANDI; /* Major opcode 12: ANDI */
      Module.OPCODE_TABLE[13] = _ORI; /* Major opcode 13: ORI */
      Module.OPCODE_TABLE[14] = _XORI; /* Major opcode 14: XORI */
      Module.OPCODE_TABLE[15] = _LUI; /* Major opcode 15: LUI */
      Module.OPCODE_TABLE[16] = _Coprocessor0Handler; /*16 Coprocessor 1 S-format opcodes */
      Module.OPCODE_TABLE[17] = _Coprocessor1Handler; /*17 Coprocessor 1 D-format opcodes */
      Module.OPCODE_TABLE[18] = _NOP; // 18 RESERVERD
      Module.OPCODE_TABLE[19] = _NOP; // 19 RESERVED
      Module.OPCODE_TABLE[20] = _BEQL; /* Major opcode 20: BEQL */
    	Module.OPCODE_TABLE[21] = _BNEL;/* Major opcode 21: BNEL */
      Module.OPCODE_TABLE[22] = _BLEZL; /* Major opcode 22: BLEZL */
    	Module.OPCODE_TABLE[23] = _BGTZL;/* Major opcode 23: BGTZL */
    	Module.OPCODE_TABLE[24] = _DADDI;/* Major opcode 24: DADDI */
    	Module.OPCODE_TABLE[25] = _DADDIU; /* Major opcode 25: DADDIU */
    	Module.OPCODE_TABLE[26] = _LDL;/* Major opcode 26: LDL */
    	Module.OPCODE_TABLE[27] = _LDR; /* Major opcode 27: LDR */
      Module.OPCODE_TABLE[28] = _NOP; // 28 RESERVED
      Module.OPCODE_TABLE[29] = _NOP; // 29 RESERVED
      Module.OPCODE_TABLE[30] = _NOP; // 30
      Module.OPCODE_TABLE[31] = _NOP; // 31
    	Module.OPCODE_TABLE[32] = _LB;/* Major opcode 32: LB */
    	Module.OPCODE_TABLE[33] = _LH; /* Major opcode 33: LH */
    	Module.OPCODE_TABLE[34] = _LWL; /* Major opcode 34: LWL */
    	Module.OPCODE_TABLE[35] = _LW; /* Major opcode 35: LW */
    	Module.OPCODE_TABLE[36] = _LBU; /* Major opcode 36: LBU */
    	Module.OPCODE_TABLE[37] = _LHU; /* Major opcode 37: LHU */
    	Module.OPCODE_TABLE[38] = _LWR; /* Major opcode 38: LWR */
    	Module.OPCODE_TABLE[39] = _LWU; /* Major opcode 39: LWU */
    	Module.OPCODE_TABLE[40] = _SB; //case 40: SB(op); break;
    	Module.OPCODE_TABLE[41] = _SH; //case 41: SH(op); break;
    	Module.OPCODE_TABLE[42] = _SWL; //case 42: SWL(op); break;
    	Module.OPCODE_TABLE[43] = _SW; //case 43: SW(op); break;
    	Module.OPCODE_TABLE[44] = _SDL; //case 44: SDL(op); break;
    	Module.OPCODE_TABLE[45] = _SDR; //case 45: SDR(op); break;
    	Module.OPCODE_TABLE[46] = _SWR; //case 46: SWR(op); break;
    	Module.OPCODE_TABLE[47] = _CACHE; //case 47: CACHE(op); break;
    	Module.OPCODE_TABLE[48] = _LL; /* Major opcode 48: LL */
    	Module.OPCODE_TABLE[49] = _LWC1; //case 49: LWC1(op); break;
      Module.OPCODE_TABLE[50] = _NOP; // 50 RESERVED
      Module.OPCODE_TABLE[51] = _NOP; // 51 RESERVED
    	Module.OPCODE_TABLE[52] = _NI;/* Major opcode 52: LLD (Not implemented) */
    	Module.OPCODE_TABLE[53] = _LDC1; //case 53: LDC1(op); break;
      Module.OPCODE_TABLE[54] = _NOP;//54 RESERVED
    	Module.OPCODE_TABLE[55] = _LD; /* Major opcode 55: LD */
    	Module.OPCODE_TABLE[56] = _SC; /* Major opcode 56: SC */
    	Module.OPCODE_TABLE[57] = _SWC1; //case 57: SWC1(op); break;
      Module.OPCODE_TABLE[58] = _NOP;// 58 RESERVED
      Module.OPCODE_TABLE[59] = _NOP;// 59 RESERVED
    	Module.OPCODE_TABLE[60] = _NI; //case 60: /* Major opcode 60: SCD (Not implemented) */
    	Module.OPCODE_TABLE[61] = _SDC1; //case 61: SDC1(op); break;
      Module.OPCODE_TABLE[62] = _NOP;//62 RESERVED
    	Module.OPCODE_TABLE[63] = _SD; //case 63: SD(op); break;
    }

    Module.OPCODE_TABLE[code](op);

  }
  ,op
  ,code);
#else
      OPCODE_ARRAY[code](op);
#endif

#else //EMSCRIPTEN

	switch ((op >> 26) & 0x3F) {
	case 0: /* SPECIAL prefix */
		switch (op & 0x3F) {
		case 0: /* SPECIAL opcode 0: SLL */
			if (RD_OF(op) != 0) SLL(op);
			else                NOP(0);
			break;
		case 2: /* SPECIAL opcode 2: SRL */
			if (RD_OF(op) != 0) SRL(op);
			else                NOP(0);
			break;
		case 3: /* SPECIAL opcode 3: SRA */
			if (RD_OF(op) != 0) SRA(op);
			else                NOP(0);
			break;
		case 4: /* SPECIAL opcode 4: SLLV */
			if (RD_OF(op) != 0) SLLV(op);
			else                NOP(0);
			break;
		case 6: /* SPECIAL opcode 6: SRLV */
			if (RD_OF(op) != 0) SRLV(op);
			else                NOP(0);
			break;
		case 7: /* SPECIAL opcode 7: SRAV */
			if (RD_OF(op) != 0) SRAV(op);
			else                NOP(0);
			break;
		case 8: JR(op); break;
		case 9: /* SPECIAL opcode 9: JALR */
			/* Note: This can omit the check for Rd == 0 because the JALR
			 * function checks for link_register != &reg[0]. If you're
			 * using this as a reference for a JIT, do check Rd == 0 in it. */
			JALR(op);
			break;
		case 12: SYSCALL(op); break;
		case 13: /* SPECIAL opcode 13: BREAK (Not implemented) */
			NI(op);
			break;
		case 15: SYNC(op); break;
		case 16: /* SPECIAL opcode 16: MFHI */
			if (RD_OF(op) != 0) MFHI(op);
			else                NOP(0);
			break;
		case 17: MTHI(op); break;
		case 18: /* SPECIAL opcode 18: MFLO */
			if (RD_OF(op) != 0) MFLO(op);
			else                NOP(0);
			break;
		case 19: MTLO(op); break;
		case 20: /* SPECIAL opcode 20: DSLLV */
			if (RD_OF(op) != 0) DSLLV(op);
			else                NOP(0);
			break;
		case 22: /* SPECIAL opcode 22: DSRLV */
			if (RD_OF(op) != 0) DSRLV(op);
			else                NOP(0);
			break;
		case 23: /* SPECIAL opcode 23: DSRAV */
			if (RD_OF(op) != 0) DSRAV(op);
			else                NOP(0);
			break;
		case 24: MULT(op); break;
		case 25: MULTU(op); break;
		case 26: DIV(op); break;
		case 27: DIVU(op); break;
		case 28: DMULT(op); break;
		case 29: DMULTU(op); break;
		case 30: DDIV(op); break;
		case 31: DDIVU(op); break;
		case 32: /* SPECIAL opcode 32: ADD */
			if (RD_OF(op) != 0) ADD(op);
			else                NOP(0);
			break;
		case 33: /* SPECIAL opcode 33: ADDU */
			if (RD_OF(op) != 0) ADDU(op);
			else                NOP(0);
			break;
		case 34: /* SPECIAL opcode 34: SUB */
			if (RD_OF(op) != 0) SUB(op);
			else                NOP(0);
			break;
		case 35: /* SPECIAL opcode 35: SUBU */
			if (RD_OF(op) != 0) SUBU(op);
			else                NOP(0);
			break;
		case 36: /* SPECIAL opcode 36: AND */
			if (RD_OF(op) != 0) AND(op);
			else                NOP(0);
			break;
		case 37: /* SPECIAL opcode 37: OR */
			if (RD_OF(op) != 0) OR(op);
			else                NOP(0);
			break;
		case 38: /* SPECIAL opcode 38: XOR */
			if (RD_OF(op) != 0) XOR(op);
			else                NOP(0);
			break;
		case 39: /* SPECIAL opcode 39: NOR */
			if (RD_OF(op) != 0) NOR(op);
			else                NOP(0);
			break;
		case 42: /* SPECIAL opcode 42: SLT */
			if (RD_OF(op) != 0) SLT(op);
			else                NOP(0);
			break;
		case 43: /* SPECIAL opcode 43: SLTU */
			if (RD_OF(op) != 0) SLTU(op);
			else                NOP(0);
			break;
		case 44: /* SPECIAL opcode 44: DADD */
			if (RD_OF(op) != 0) DADD(op);
			else                NOP(0);
			break;
		case 45: /* SPECIAL opcode 45: DADDU */
			if (RD_OF(op) != 0) DADDU(op);
			else                NOP(0);
			break;
		case 46: /* SPECIAL opcode 46: DSUB */
			if (RD_OF(op) != 0) DSUB(op);
			else                NOP(0);
			break;
		case 47: /* SPECIAL opcode 47: DSUBU */
			if (RD_OF(op) != 0) DSUBU(op);
			else                NOP(0);
			break;
		case 48: /* SPECIAL opcode 48: TGE (Not implemented) */
		case 49: /* SPECIAL opcode 49: TGEU (Not implemented) */
		case 50: /* SPECIAL opcode 50: TLT (Not implemented) */
		case 51: /* SPECIAL opcode 51: TLTU (Not implemented) */
			NI(op);
			break;
		case 52: TEQ(op); break;
		case 54: /* SPECIAL opcode 54: TNE (Not implemented) */
			NI(op);
			break;
		case 56: /* SPECIAL opcode 56: DSLL */
			if (RD_OF(op) != 0) DSLL(op);
			else                NOP(0);
			break;
		case 58: /* SPECIAL opcode 58: DSRL */
			if (RD_OF(op) != 0) DSRL(op);
			else                NOP(0);
			break;
		case 59: /* SPECIAL opcode 59: DSRA */
			if (RD_OF(op) != 0) DSRA(op);
			else                NOP(0);
			break;
		case 60: /* SPECIAL opcode 60: DSLL32 */
			if (RD_OF(op) != 0) DSLL32(op);
			else                NOP(0);
			break;
		case 62: /* SPECIAL opcode 62: DSRL32 */
			if (RD_OF(op) != 0) DSRL32(op);
			else                NOP(0);
			break;
		case 63: /* SPECIAL opcode 63: DSRA32 */
			if (RD_OF(op) != 0) DSRA32(op);
			else                NOP(0);
			break;
		default: /* SPECIAL opcodes 1, 5, 10, 11, 14, 21, 40, 41, 53, 55, 57,
		            61: Reserved Instructions */
			RESERVED(op);
			break;
		} /* switch (op & 0x3F) for the SPECIAL prefix */
		break;
	case 1: /* REGIMM prefix */
		switch ((op >> 16) & 0x1F) {
		case 0: /* REGIMM opcode 0: BLTZ */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZ_IDLE(op);
			else                                     BLTZ(op);
			break;
		case 1: /* REGIMM opcode 1: BGEZ */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZ_IDLE(op);
			else                                     BGEZ(op);
			break;
		case 2: /* REGIMM opcode 2: BLTZL */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZL_IDLE(op);
			else                                     BLTZL(op);
			break;
		case 3: /* REGIMM opcode 3: BGEZL */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZL_IDLE(op);
			else                                     BGEZL(op);
			break;
		case 8: /* REGIMM opcode 8: TGEI (Not implemented) */
		case 9: /* REGIMM opcode 9: TGEIU (Not implemented) */
		case 10: /* REGIMM opcode 10: TLTI (Not implemented) */
		case 11: /* REGIMM opcode 11: TLTIU (Not implemented) */
		case 12: /* REGIMM opcode 12: TEQI (Not implemented) */
		case 14: /* REGIMM opcode 14: TNEI (Not implemented) */
			NI(op);
			break;
		case 16: /* REGIMM opcode 16: BLTZAL */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZAL_IDLE(op);
			else                                     BLTZAL(op);
			break;
		case 17: /* REGIMM opcode 17: BGEZAL */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZAL_IDLE(op);
			else                                     BGEZAL(op);
			break;
		case 18: /* REGIMM opcode 18: BLTZALL */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLTZALL_IDLE(op);
			else                                     BLTZALL(op);
			break;
		case 19: /* REGIMM opcode 19: BGEZALL */
			if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGEZALL_IDLE(op);
			else                                     BGEZALL(op);
			break;
		default: /* REGIMM opcodes 4..7, 13, 15, 20..31:
		            Reserved Instructions */
			RESERVED(op);
			break;
		} /* switch ((op >> 16) & 0x1F) for the REGIMM prefix */
		break;
	case 2: /* Major opcode 2: J */
		if (IS_ABSOLUTE_IDLE_LOOP(op, PC->addr)) J_IDLE(op);
		else                                     J(op);
		break;
	case 3: /* Major opcode 3: JAL */
		if (IS_ABSOLUTE_IDLE_LOOP(op, PC->addr)) JAL_IDLE(op);
		else                                     JAL(op);
		break;
	case 4: /* Major opcode 4: BEQ */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BEQ_IDLE(op);
		else                                     BEQ(op);
		break;
	case 5: /* Major opcode 5: BNE */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BNE_IDLE(op);
		else                                     BNE(op);
		break;
	case 6: /* Major opcode 6: BLEZ */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLEZ_IDLE(op);
		else                                     BLEZ(op);
		break;
	case 7: /* Major opcode 7: BGTZ */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGTZ_IDLE(op);
		else                                     BGTZ(op);
		break;
	case 8: /* Major opcode 8: ADDI */
		if (RT_OF(op) != 0) ADDI(op);
		else                NOP(0);
		break;
	case 9: /* Major opcode 9: ADDIU */
		if (RT_OF(op) != 0) ADDIU(op);
		else                NOP(0);
		break;
	case 10: /* Major opcode 10: SLTI */
		if (RT_OF(op) != 0) SLTI(op);
		else                NOP(0);
		break;
	case 11: /* Major opcode 11: SLTIU */
		if (RT_OF(op) != 0) SLTIU(op);
		else                NOP(0);
		break;
	case 12: /* Major opcode 12: ANDI */
		if (RT_OF(op) != 0) ANDI(op);
		else                NOP(0);
		break;
	case 13: /* Major opcode 13: ORI */
		if (RT_OF(op) != 0) ORI(op);
		else                NOP(0);
		break;
	case 14: /* Major opcode 14: XORI */
		if (RT_OF(op) != 0) XORI(op);
		else                NOP(0);
		break;
	case 15: /* Major opcode 15: LUI */
		if (RT_OF(op) != 0) LUI(op);
		else                NOP(0);
		break;
	case 16: /* Coprocessor 0 prefix */
		switch ((op >> 21) & 0x1F) {
		case 0: /* Coprocessor 0 opcode 0: MFC0 */
			if (RT_OF(op) != 0) MFC0(op);
			else                NOP(0);
			break;
		case 4: MTC0(op); break;
		case 16: /* Coprocessor 0 opcode 16: TLB */
			switch (op & 0x3F) {
			case 1: TLBR(op); break;
			case 2: TLBWI(op); break;
			case 6: TLBWR(op); break;
			case 8: TLBP(op); break;
			case 24: ERET(op); break;
			default: /* TLB sub-opcodes 0, 3..5, 7, 9..23, 25..63:
			            Reserved Instructions */
				RESERVED(op);
				break;
			} /* switch (op & 0x3F) for Coprocessor 0 TLB opcodes */
			break;
		default: /* Coprocessor 0 opcodes 1..3, 4..15, 17..31:
		            Reserved Instructions */
			RESERVED(op);
			break;
		} /* switch ((op >> 21) & 0x1F) for the Coprocessor 0 prefix */
		break;
	case 17: /* Coprocessor 1 prefix */
		switch ((op >> 21) & 0x1F) {
		case 0: /* Coprocessor 1 opcode 0: MFC1 */
			if (RT_OF(op) != 0) MFC1(op);
			else                NOP(0);
			break;
		case 1: /* Coprocessor 1 opcode 1: DMFC1 */
			if (RT_OF(op) != 0) DMFC1(op);
			else                NOP(0);
			break;
		case 2: /* Coprocessor 1 opcode 2: CFC1 */
			if (RT_OF(op) != 0) CFC1(op);
			else                NOP(0);
			break;
		case 4: MTC1(op); break;
		case 5: DMTC1(op); break;
		case 6: CTC1(op); break;
		case 8: /* Coprocessor 1 opcode 8: Branch on C1 condition... */
			switch ((op >> 16) & 0x3) {
			case 0: /* opcode 0: BC1F */
				if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1F_IDLE(op);
				else                                     BC1F(op);
				break;
			case 1: /* opcode 1: BC1T */
				if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1T_IDLE(op);
				else                                     BC1T(op);
				break;
			case 2: /* opcode 2: BC1FL */
				if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1FL_IDLE(op);
				else                                     BC1FL(op);
				break;
			case 3: /* opcode 3: BC1TL */
				if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BC1TL_IDLE(op);
				else                                     BC1TL(op);
				break;
			} /* switch ((op >> 16) & 0x3) for branches on C1 condition */
			break;
		case 16: /* Coprocessor 1 S-format opcodes */
			switch (op & 0x3F) {
			case 0: ADD_S(op); break;
			case 1: SUB_S(op); break;
			case 2: MUL_S(op); break;
			case 3: DIV_S(op); break;
			case 4: SQRT_S(op); break;
			case 5: ABS_S(op); break;
			case 6: MOV_S(op); break;
			case 7: NEG_S(op); break;
			case 8: ROUND_L_S(op); break;
			case 9: TRUNC_L_S(op); break;
			case 10: CEIL_L_S(op); break;
			case 11: FLOOR_L_S(op); break;
			case 12: ROUND_W_S(op); break;
			case 13: TRUNC_W_S(op); break;
			case 14: CEIL_W_S(op); break;
			case 15: FLOOR_W_S(op); break;
			case 33: CVT_D_S(op); break;
			case 36: CVT_W_S(op); break;
			case 37: CVT_L_S(op); break;
			case 48: C_F_S(op); break;
			case 49: C_UN_S(op); break;
			case 50: C_EQ_S(op); break;
			case 51: C_UEQ_S(op); break;
			case 52: C_OLT_S(op); break;
			case 53: C_ULT_S(op); break;
			case 54: C_OLE_S(op); break;
			case 55: C_ULE_S(op); break;
			case 56: C_SF_S(op); break;
			case 57: C_NGLE_S(op); break;
			case 58: C_SEQ_S(op); break;
			case 59: C_NGL_S(op); break;
			case 60: C_LT_S(op); break;
			case 61: C_NGE_S(op); break;
			case 62: C_LE_S(op); break;
			case 63: C_NGT_S(op); break;
			default: /* Coprocessor 1 S-format opcodes 16..32, 34..35, 38..47:
			            Reserved Instructions */
				RESERVED(op);
				break;
			} /* switch (op & 0x3F) for Coprocessor 1 S-format opcodes */
			break;
		case 17: /* Coprocessor 1 D-format opcodes */
			switch (op & 0x3F) {
			case 0: ADD_D(op); break;
			case 1: SUB_D(op); break;
			case 2: MUL_D(op); break;
			case 3: DIV_D(op); break;
			case 4: SQRT_D(op); break;
			case 5: ABS_D(op); break;
			case 6: MOV_D(op); break;
			case 7: NEG_D(op); break;
			case 8: ROUND_L_D(op); break;
			case 9: TRUNC_L_D(op); break;
			case 10: CEIL_L_D(op); break;
			case 11: FLOOR_L_D(op); break;
			case 12: ROUND_W_D(op); break;
			case 13: TRUNC_W_D(op); break;
			case 14: CEIL_W_D(op); break;
			case 15: FLOOR_W_D(op); break;
			case 32: CVT_S_D(op); break;
			case 36: CVT_W_D(op); break;
			case 37: CVT_L_D(op); break;
			case 48: C_F_D(op); break;
			case 49: C_UN_D(op); break;
			case 50: C_EQ_D(op); break;
			case 51: C_UEQ_D(op); break;
			case 52: C_OLT_D(op); break;
			case 53: C_ULT_D(op); break;
			case 54: C_OLE_D(op); break;
			case 55: C_ULE_D(op); break;
			case 56: C_SF_D(op); break;
			case 57: C_NGLE_D(op); break;
			case 58: C_SEQ_D(op); break;
			case 59: C_NGL_D(op); break;
			case 60: C_LT_D(op); break;
			case 61: C_NGE_D(op); break;
			case 62: C_LE_D(op); break;
			case 63: C_NGT_D(op); break;
			default: /* Coprocessor 1 D-format opcodes 16..31, 33..35, 38..47:
			            Reserved Instructions */
				RESERVED(op);
				break;
			} /* switch (op & 0x3F) for Coprocessor 1 D-format opcodes */
			break;
		case 20: /* Coprocessor 1 W-format opcodes */
			switch (op & 0x3F) {
			case 32: CVT_S_W(op); break;
			case 33: CVT_D_W(op); break;
			default: /* Coprocessor 1 W-format opcodes 0..31, 34..63:
			            Reserved Instructions */
				RESERVED(op);
				break;
			}
			break;
		case 21: /* Coprocessor 1 L-format opcodes */
			switch (op & 0x3F) {
			case 32: CVT_S_L(op); break;
			case 33: CVT_D_L(op); break;
			default: /* Coprocessor 1 L-format opcodes 0..31, 34..63:
			            Reserved Instructions */
				RESERVED(op);
				break;
			}
			break;
		default: /* Coprocessor 1 opcodes 3, 7, 9..15, 18..19, 22..31:
		            Reserved Instructions */
			RESERVED(op);
			break;
		} /* switch ((op >> 21) & 0x1F) for the Coprocessor 1 prefix */
		break;
	case 20: /* Major opcode 20: BEQL */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BEQL_IDLE(op);
		else                                     BEQL(op);
		break;
	case 21: /* Major opcode 21: BNEL */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BNEL_IDLE(op);
		else                                     BNEL(op);
		break;
	case 22: /* Major opcode 22: BLEZL */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BLEZL_IDLE(op);
		else                                     BLEZL(op);
		break;
	case 23: /* Major opcode 23: BGTZL */
		if (IS_RELATIVE_IDLE_LOOP(op, PC->addr)) BGTZL_IDLE(op);
		else                                     BGTZL(op);
		break;
	case 24: /* Major opcode 24: DADDI */
		if (RT_OF(op) != 0) DADDI(op);
		else                NOP(0);
		break;
	case 25: /* Major opcode 25: DADDIU */
		if (RT_OF(op) != 0) DADDIU(op);
		else                NOP(0);
		break;
	case 26: /* Major opcode 26: LDL */
		if (RT_OF(op) != 0) LDL(op);
		else                NOP(0);
		break;
	case 27: /* Major opcode 27: LDR */
		if (RT_OF(op) != 0) LDR(op);
		else                NOP(0);
		break;
	case 32: /* Major opcode 32: LB */
		if (RT_OF(op) != 0) LB(op);
		else                NOP(0);
		break;
	case 33: /* Major opcode 33: LH */
		if (RT_OF(op) != 0) LH(op);
		else                NOP(0);
		break;
	case 34: /* Major opcode 34: LWL */
		if (RT_OF(op) != 0) LWL(op);
		else                NOP(0);
		break;
	case 35: /* Major opcode 35: LW */
		if (RT_OF(op) != 0) LW(op);
		else                NOP(0);
		break;
	case 36: /* Major opcode 36: LBU */
		if (RT_OF(op) != 0) LBU(op);
		else                NOP(0);
		break;
	case 37: /* Major opcode 37: LHU */
		if (RT_OF(op) != 0) LHU(op);
		else                NOP(0);
		break;
	case 38: /* Major opcode 38: LWR */
		if (RT_OF(op) != 0) LWR(op);
		else                NOP(0);
		break;
	case 39: /* Major opcode 39: LWU */
		if (RT_OF(op) != 0) LWU(op);
		else                NOP(0);
		break;
	case 40: SB(op); break;
	case 41: SH(op); break;
	case 42: SWL(op); break;
	case 43: SW(op); break;
	case 44: SDL(op); break;
	case 45: SDR(op); break;
	case 46: SWR(op); break;
	case 47: CACHE(op); break;
	case 48: /* Major opcode 48: LL */
		if (RT_OF(op) != 0) LL(op);
		else                NOP(0);
		break;
	case 49: LWC1(op); break;
	case 52: /* Major opcode 52: LLD (Not implemented) */
		NI(op);
		break;
	case 53: LDC1(op); break;
	case 55: /* Major opcode 55: LD */
		if (RT_OF(op) != 0) LD(op);
		else                NOP(0);
		break;
	case 56: /* Major opcode 56: SC */
		if (RT_OF(op) != 0) SC(op);
		else                NOP(0);
		break;
	case 57: SWC1(op); break;
	case 60: /* Major opcode 60: SCD (Not implemented) */
		NI(op);
		break;
	case 61: SDC1(op); break;
	case 63: SD(op); break;
	default: /* Major opcodes 18..19, 28..31, 50..51, 54, 58..59, 62:
	            Reserved Instructions */
		RESERVED(op);
		break;
	} /* switch ((op >> 26) & 0x3F) */
#endif
}

#if EMSCRIPTEN
static void  pure_interpreter_loop()
{
  //EM_ASM({Module.viArrived = 0;});
  viArrived = 0;

#if ONSCREEN_FPS
  EM_ASM({Module.stats.begin();});
#endif

  while(viArrived<1)
  {
#if 0 //def 0 //COMPARE_CORE
    CoreCompareCallback();
#endif
#if 0 //def 0 //DBG
    if (g_DebuggerActive) update_debugger(PC->addr);
#endif
    InterpretOpcode();
  }

  #if ONSCREEN_FPS
    EM_ASM({Module.stats.end();});

    //EM_ASM({console.error("END_LOOP");});
  #endif
}
#endif

void pure_interpreter(void)
{
   stop = 0;
   PC = &interp_PC;
   PC->addr = last_addr = 0xa4000040;

#if (!EMSCRIPTEN)
   while (!stop)
   {
#ifdef COMPARE_CORE
     CoreCompareCallback();
#endif
#ifdef DBG
     if (g_DebuggerActive) update_debugger(PC->addr);
#endif
     InterpretOpcode();
   }
#else

#if ONSCREEN_FPS
    EM_ASM({
      Module.stats = new Stats();
      Module.stats.setMode( 0 );
      var canvas = document.getElementById('canvas');
      Module.stats.domElement.style.position = "absolute";
      Module.stats.domElement.style.x = 0;
      Module.stats.domElement.style.y = 0;
      document.body.insertBefore(Module.stats.domElement, canvas);
    });
#endif


  emscripten_set_main_loop(pure_interpreter_loop, 0, 1);
#endif //EMSCRIPTEN
}
