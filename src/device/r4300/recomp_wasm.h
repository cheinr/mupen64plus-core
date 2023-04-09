/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - recomp_wasm.h                                           *
 *   Mupen64Plus homepage: https://mupen64plus.org/                        *
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

#ifndef M64P_DEVICE_R4300_RECOMP_WASM_H
#define M64P_DEVICE_R4300_RECOMP_WASM_H

#include <stddef.h>
#include <stdint.h>

void recomp_wasm_init_block(struct r4300_core* r4300, uint32_t address);
void wasm_recompile_block(struct r4300_core* r4300, const uint32_t* iw, struct precomp_block* block, uint32_t func);
void recomp_wasm_jump_to(struct r4300_core* r4300, uint32_t address);

#endif /* M64P_DEVICE_R4300_RECOMP_WASM_H */



