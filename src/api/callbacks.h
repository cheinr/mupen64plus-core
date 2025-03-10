/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus-core - api/callbacks.h                                    *
 *   Mupen64Plus homepage: https://mupen64plus.org/                        *
 *   Copyright (C) 2009 Richard Goedeken                                   *
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

/* This file contains the definitions for callback functions which will be
 * called from the other Core modules
 */

#if !defined(API_CALLBACKS_H)
#define API_CALLBACKS_H

#include "m64p_frontend.h"
#include "m64p_types.h"

#if M64P_STATIC_PLUGINS
#define DebugMessage(...) DebugMessageCore(__VA_ARGS__)
#endif


#if defined(__GNUC__)
#define ATTR_FMT(fmtpos, attrpos) __attribute__ ((format (printf, fmtpos, attrpos)))
#else
#define ATTR_FMT(fmtpos, attrpos)
#endif

/* Functions for use by the Core, to send information back to the front-end app */
extern m64p_error SetDebugCallback(ptr_DebugCallback pFunc, void *Context);
extern m64p_error SetStateCallback(ptr_StateCallback pFunc, void *Context);
#if M64P_STATIC_PLUGINS
extern void       DebugMessageCore(int level, const char *message, ...) ATTR_FMT(2,3);
#else
extern void       DebugMessage(int level, const char *message, ...) ATTR_FMT(2,3);
#endif
extern void       StateChanged(m64p_core_param param_type, int new_value);

#endif /* API_CALLBACKS_H */

