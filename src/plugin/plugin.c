/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - plugin.c                                                *
 *   Mupen64Plus homepage: https://mupen64plus.org/                        *
 *   Copyright (C) 2002 Hacktarux                                          *
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

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if M64P_STATIC_PLUGINS
#define M64P_PLUGIN_PROTOTYPES 1
#define M64P_CORE_PROTOTYPES 1
#include "../../../mupen64plus-video-rice-web-netplay/src/Video.h"
#include "../../../mupen64plus-rsp-hle/src/rsp-hle_static.h"
#include "../../../mupen64plus-audio-sdl/src/audio_static.h"
#include "../../../mupen64plus-input-sdl/src/input_static.h"
#endif

#include "api/callbacks.h"
#include "api/m64p_common.h"
#include "api/m64p_plugin.h"
#include "api/m64p_types.h"
#include "device/memory/memory.h"
#include "device/rcp/ai/ai_controller.h"
#include "device/rcp/mi/mi_controller.h"
#include "device/rcp/rdp/rdp_core.h"
#include "device/rcp/rsp/rsp_core.h"
#include "device/rcp/vi/vi_controller.h"
#include "dummy_audio.h"
#include "dummy_input.h"
#include "dummy_rsp.h"
#include "dummy_video.h"
#include "main/main.h"
#include "main/rom.h"
#include "main/version.h"
#include "osal/dynamiclib.h"
#include "plugin.h"

CONTROL Controls[4];

/* global function pointers - initialized on core startup */
gfx_plugin_functions gfx;
audio_plugin_functions audio;
input_plugin_functions input;
rsp_plugin_functions rsp;

/* local data structures and functions */
static const gfx_plugin_functions dummy_gfx = {
    dummyvideo_PluginGetVersion,
    dummyvideo_ChangeWindow,
    dummyvideo_InitiateGFX,
    dummyvideo_MoveScreen,
    dummyvideo_ProcessDList,
    dummyvideo_ProcessRDPList,
    dummyvideo_RomClosed,
    dummyvideo_RomOpen,
    dummyvideo_ShowCFB,
    dummyvideo_UpdateScreen,
    dummyvideo_ViStatusChanged,
    dummyvideo_ViWidthChanged,
    dummyvideo_ReadScreen2,
    dummyvideo_SetRenderingCallback,
    dummyvideo_ResizeVideoOutput,
    dummyvideo_FBRead,
    dummyvideo_FBWrite,
    dummyvideo_FBGetFrameBufferInfo
};

static const audio_plugin_functions dummy_audio = {
    dummyaudio_PluginGetVersion,
    dummyaudio_AiDacrateChanged,
    dummyaudio_AiLenChanged,
    dummyaudio_InitiateAudio,
    dummyaudio_ProcessAList,
    dummyaudio_RomClosed,
    dummyaudio_RomOpen,
    dummyaudio_SetSpeedFactor,
    dummyaudio_VolumeUp,
    dummyaudio_VolumeDown,
    dummyaudio_VolumeGetLevel,
    dummyaudio_VolumeSetLevel,
    dummyaudio_VolumeMute,
    dummyaudio_VolumeGetString
};

static const input_plugin_functions dummy_input = {
    dummyinput_PluginGetVersion,
    dummyinput_ControllerCommand,
    dummyinput_GetKeys,
    dummyinput_InitiateControllers,
    dummyinput_ReadController,
    dummyinput_RomClosed,
    dummyinput_RomOpen,
    dummyinput_SDL_KeyDown,
    dummyinput_SDL_KeyUp,
    dummyinput_RenderCallback
};

static const rsp_plugin_functions dummy_rsp = {
    dummyrsp_PluginGetVersion,
    dummyrsp_DoRspCycles,
    dummyrsp_InitiateRSP,
    dummyrsp_RomClosed
};

static GFX_INFO gfx_info;
static AUDIO_INFO audio_info;
static CONTROL_INFO control_info;
static RSP_INFO rsp_info;

static int l_RspAttached = 0;
static int l_InputAttached = 0;
static int l_AudioAttached = 0;
static int l_GfxAttached = 0;

static unsigned int dummy;

/* local functions */
static void EmptyFunc(void)
{
}

// Handy macro to avoid code bloat when loading symbols
#define GET_FUNC(type, field, name) \
    ((field = (type)osal_dynlib_getproc(plugin_handle, name)) != NULL)

// code to handle backwards-compatibility to video plugins with API_VERSION < 02.1.0.  This API version introduced a boolean
// flag in the rendering callback, which told the core whether or not the current screen has been freshly redrawn since the
// last time the callback was called.
static void                     (*l_mainRenderCallback)(int) = NULL;
static ptr_SetRenderingCallback   l_old1SetRenderingCallback = NULL;

static void backcompat_videoRenderCallback(int unused)  // this function will be called by the video plugin as the render callback
{
    if (l_mainRenderCallback != NULL)
        l_mainRenderCallback(1);  // assume screen is always freshly redrawn (otherwise screenshots won't work w/ OSD enabled)
}

static void backcompat_setRenderCallbackIntercept(void (*callback)(int))
{
    l_mainRenderCallback = callback;
}

static void plugin_disconnect_gfx(void)
{
    gfx = dummy_gfx;
    l_GfxAttached = 0;
    l_mainRenderCallback = NULL;
}

static m64p_error plugin_connect_gfx(m64p_dynlib_handle plugin_handle)
{
    /* attach the Video plugin function pointers */
    if (plugin_handle != NULL)
    {
        m64p_plugin_type PluginType;
        int PluginVersion, APIVersion;

        if (l_GfxAttached)
            return M64ERR_INVALID_STATE;

#if (!M64P_STATIC_PLUGINS)
        /* set function pointers for required functions */
        if (!GET_FUNC(ptr_PluginGetVersion, gfx.getVersion, "PluginGetVersion") ||
            !GET_FUNC(ptr_ChangeWindow, gfx.changeWindow, "ChangeWindow") ||
            !GET_FUNC(ptr_InitiateGFX, gfx.initiateGFX, "InitiateGFX") ||
            !GET_FUNC(ptr_MoveScreen, gfx.moveScreen, "MoveScreen") ||
            !GET_FUNC(ptr_ProcessDList, gfx.processDList, "ProcessDList") ||
            !GET_FUNC(ptr_ProcessRDPList, gfx.processRDPList, "ProcessRDPList") ||
            !GET_FUNC(ptr_RomClosed, gfx.romClosed, "RomClosed") ||
            !GET_FUNC(ptr_RomOpen, gfx.romOpen, "RomOpen") ||
            !GET_FUNC(ptr_ShowCFB, gfx.showCFB, "ShowCFB") ||
            !GET_FUNC(ptr_UpdateScreen, gfx.updateScreen, "UpdateScreen") ||
            !GET_FUNC(ptr_ViStatusChanged, gfx.viStatusChanged, "ViStatusChanged") ||
            !GET_FUNC(ptr_ViWidthChanged, gfx.viWidthChanged, "ViWidthChanged") ||
            !GET_FUNC(ptr_ReadScreen2, gfx.readScreen, "ReadScreen2") ||
            !GET_FUNC(ptr_SetRenderingCallback, gfx.setRenderingCallback, "SetRenderingCallback") ||
            !GET_FUNC(ptr_FBRead, gfx.fBRead, "FBRead") ||
            !GET_FUNC(ptr_FBWrite, gfx.fBWrite, "FBWrite") ||
            !GET_FUNC(ptr_FBGetFrameBufferInfo, gfx.fBGetFrameBufferInfo, "FBGetFrameBufferInfo"))
        {
            DebugMessage(M64MSG_ERROR, "broken Video plugin; function(s) not found.");
            plugin_disconnect_gfx();
            return M64ERR_INPUT_INVALID;
        }

        /* set function pointers for optional functions */
        gfx.resizeVideoOutput = (ptr_ResizeVideoOutput)core_osal_dynlib_getproc(plugin_handle, "ResizeVideoOutput");

#else // M64P_STATIC_PLUGINS
        
        gfx.getVersion = &PluginGetVersionVideo;
        gfx.changeWindow = &ChangeWindow;
        gfx.initiateGFX = &InitiateGFX;
        gfx.moveScreen =  &MoveScreen;
        gfx.processDList =  &ProcessDList;
        gfx.processRDPList = ProcessRDPList;
        gfx.romClosed = &RomClosedVideo;
        gfx.romOpen = &RomOpenVideo;
        gfx.showCFB = &ShowCFB;
        gfx.updateScreen = &UpdateScreen;
        gfx.viStatusChanged = &ViStatusChanged;
        gfx.viWidthChanged = &ViWidthChanged;
        gfx.readScreen = &ReadScreen2;
        gfx.setRenderingCallback = &SetRenderingCallback;
        /*gfx.fBRead = &FBRead;
          gfx.fBWrite = &FBWrite;
          gfx.fBGetFrameBufferInfo = &FBGetFrameBufferInfo;
        */
        gfx.resizeVideoOutput = &ResizeVideoOutput;
        gfx.fBRead = NULL;
        gfx.fBWrite = NULL;
        gfx.fBGetFrameBufferInfo = NULL;
#endif

        /* check the version info */
        (*gfx.getVersion)(&PluginType, &PluginVersion, &APIVersion, NULL, NULL);
        if (PluginType != M64PLUGIN_GFX || (APIVersion & 0xffff0000) != (GFX_API_VERSION & 0xffff0000))
        {
            DebugMessage(M64MSG_ERROR, "incompatible Video plugin");
            plugin_disconnect_gfx();
            return M64ERR_INCOMPATIBLE;
        }

        /* handle backwards-compatibility */
        if (APIVersion < 0x020100)
        {
            DebugMessage(M64MSG_WARNING, "Fallback for Video plugin API (%02i.%02i.%02i) < 2.1.0. Screenshots may contain On Screen Display text", VERSION_PRINTF_SPLIT(APIVersion));
            // tell the video plugin to make its rendering callback to me (it's old, and doesn't have the bScreenRedrawn flag)
            gfx.setRenderingCallback(backcompat_videoRenderCallback);
            l_old1SetRenderingCallback = gfx.setRenderingCallback; // save this just for future use
            gfx.setRenderingCallback = (ptr_SetRenderingCallback) backcompat_setRenderCallbackIntercept;
        }
        if (APIVersion < 0x20200 || gfx.resizeVideoOutput == NULL)
        {
            DebugMessage(M64MSG_WARNING, "Fallback for Video plugin API (%02i.%02i.%02i) < 2.2.0. Resizable video will not work", VERSION_PRINTF_SPLIT(APIVersion));
            gfx.resizeVideoOutput = dummyvideo_ResizeVideoOutput;
        }

        l_GfxAttached = 1;
    }
    else
        plugin_disconnect_gfx();

    return M64ERR_SUCCESS;
}

static m64p_error plugin_start_gfx(void)
{
    uint8_t media = *((uint8_t*)mem_base_u32(g_mem_base, MM_CART_ROM) + (0x3b ^ S8));

    /* Here we feed 64DD IPL ROM header to GFX plugin if 64DD is present.
     * We use g_media_loader.get_dd_rom to detect 64DD presence
     * instead of g_dev because the latter is not yet initialized at plugin_start time */
    /* XXX: Not sure it is the best way to convey which game is being played to the GFX plugin
     * as 64DD IPL is the same for all 64DD games... */
    char* dd_ipl_rom_filename = (g_media_loader.get_dd_rom == NULL)
        ? NULL
        : g_media_loader.get_dd_rom(g_media_loader.cb_data);

    uint32_t rom_base = (dd_ipl_rom_filename != NULL && strlen(dd_ipl_rom_filename) != 0 && media != 'C')
        ? MM_DD_ROM
        : MM_CART_ROM;

    free(dd_ipl_rom_filename);

    /* fill in the GFX_INFO data structure */
    gfx_info.HEADER = (unsigned char *)mem_base_u32(g_mem_base, rom_base);
    gfx_info.RDRAM = (unsigned char *)mem_base_u32(g_mem_base, MM_RDRAM_DRAM);
    gfx_info.DMEM = (unsigned char *)mem_base_u32(g_mem_base, MM_RSP_MEM);
    gfx_info.IMEM = (unsigned char *)mem_base_u32(g_mem_base, MM_RSP_MEM + 0x1000);
    gfx_info.MI_INTR_REG = &(g_dev.mi.regs[MI_INTR_REG]);
    gfx_info.DPC_START_REG = &(g_dev.dp.dpc_regs[DPC_START_REG]);
    gfx_info.DPC_END_REG = &(g_dev.dp.dpc_regs[DPC_END_REG]);
    gfx_info.DPC_CURRENT_REG = &(g_dev.dp.dpc_regs[DPC_CURRENT_REG]);
    gfx_info.DPC_STATUS_REG = &(g_dev.dp.dpc_regs[DPC_STATUS_REG]);
    gfx_info.DPC_CLOCK_REG = &(g_dev.dp.dpc_regs[DPC_CLOCK_REG]);
    gfx_info.DPC_BUFBUSY_REG = &(g_dev.dp.dpc_regs[DPC_BUFBUSY_REG]);
    gfx_info.DPC_PIPEBUSY_REG = &(g_dev.dp.dpc_regs[DPC_PIPEBUSY_REG]);
    gfx_info.DPC_TMEM_REG = &(g_dev.dp.dpc_regs[DPC_TMEM_REG]);
    gfx_info.VI_STATUS_REG = &(g_dev.vi.regs[VI_STATUS_REG]);
    gfx_info.VI_ORIGIN_REG = &(g_dev.vi.regs[VI_ORIGIN_REG]);
    gfx_info.VI_WIDTH_REG = &(g_dev.vi.regs[VI_WIDTH_REG]);
    gfx_info.VI_INTR_REG = &(g_dev.vi.regs[VI_V_INTR_REG]);
    gfx_info.VI_V_CURRENT_LINE_REG = &(g_dev.vi.regs[VI_CURRENT_REG]);
    gfx_info.VI_TIMING_REG = &(g_dev.vi.regs[VI_BURST_REG]);
    gfx_info.VI_V_SYNC_REG = &(g_dev.vi.regs[VI_V_SYNC_REG]);
    gfx_info.VI_H_SYNC_REG = &(g_dev.vi.regs[VI_H_SYNC_REG]);
    gfx_info.VI_LEAP_REG = &(g_dev.vi.regs[VI_LEAP_REG]);
    gfx_info.VI_H_START_REG = &(g_dev.vi.regs[VI_H_START_REG]);
    gfx_info.VI_V_START_REG = &(g_dev.vi.regs[VI_V_START_REG]);
    gfx_info.VI_V_BURST_REG = &(g_dev.vi.regs[VI_V_BURST_REG]);
    gfx_info.VI_X_SCALE_REG = &(g_dev.vi.regs[VI_X_SCALE_REG]);
    gfx_info.VI_Y_SCALE_REG = &(g_dev.vi.regs[VI_Y_SCALE_REG]);
    gfx_info.CheckInterrupts = EmptyFunc;

    gfx_info.version = 2; //Version 2 added SP_STATUS_REG and RDRAM_SIZE
    gfx_info.SP_STATUS_REG = &g_dev.sp.regs[SP_STATUS_REG];
    gfx_info.RDRAM_SIZE = (unsigned int*) &g_dev.rdram.dram_size;

    /* call the audio plugin */
    if (!gfx.initiateGFX(gfx_info))
        return M64ERR_PLUGIN_FAIL;

    return M64ERR_SUCCESS;
}

static void plugin_disconnect_audio(void)
{
    audio = dummy_audio;
    l_AudioAttached = 0;
}

static m64p_error plugin_connect_audio(m64p_dynlib_handle plugin_handle)
{
    /* attach the Audio plugin function pointers */
    if (plugin_handle != NULL)
    {
        m64p_plugin_type PluginType;
        int PluginVersion, APIVersion;

        if (l_AudioAttached)
            return M64ERR_INVALID_STATE;

#if (!M64P_STATIC_PLUGINS) 
        if (!GET_FUNC(ptr_PluginGetVersion, audio.getVersion, "PluginGetVersion") ||
            !GET_FUNC(ptr_AiDacrateChanged, audio.aiDacrateChanged, "AiDacrateChanged") ||
            !GET_FUNC(ptr_AiLenChanged, audio.aiLenChanged, "AiLenChanged") ||
            !GET_FUNC(ptr_InitiateAudio, audio.initiateAudio, "InitiateAudio") ||
            !GET_FUNC(ptr_ProcessAList, audio.processAList, "ProcessAList") ||
            !GET_FUNC(ptr_RomOpen, audio.romOpen, "RomOpen") ||
            !GET_FUNC(ptr_RomClosed, audio.romClosed, "RomClosed") ||
            !GET_FUNC(ptr_SetSpeedFactor, audio.setSpeedFactor, "SetSpeedFactor") ||
            !GET_FUNC(ptr_VolumeUp, audio.volumeUp, "VolumeUp") ||
            !GET_FUNC(ptr_VolumeDown, audio.volumeDown, "VolumeDown") ||
            !GET_FUNC(ptr_VolumeGetLevel, audio.volumeGetLevel, "VolumeGetLevel") ||
            !GET_FUNC(ptr_VolumeSetLevel, audio.volumeSetLevel, "VolumeSetLevel") ||
            !GET_FUNC(ptr_VolumeMute, audio.volumeMute, "VolumeMute") ||
            !GET_FUNC(ptr_VolumeGetString, audio.volumeGetString, "VolumeGetString"))
        {
            DebugMessage(M64MSG_ERROR, "broken Audio plugin; function(s) not found.");
            plugin_disconnect_audio();
            return M64ERR_INPUT_INVALID;
        }

#else
	audio.getVersion = &PluginGetVersionAudio;
        audio.aiDacrateChanged = &AiDacrateChanged;
        audio.aiLenChanged = &AiLenChanged;
        audio.initiateAudio = &InitiateAudio;
        audio.processAList = &ProcessAList;
        audio.romOpen = &RomOpenAudio;
        audio.romClosed = &RomClosedAudio;
        audio.setSpeedFactor = &SetSpeedFactor;
        audio.volumeUp = &VolumeUp;
        audio.volumeDown = &VolumeDown;
        audio.volumeGetLevel = &VolumeGetLevel;
        audio.volumeSetLevel = &VolumeSetLevel;
        audio.volumeMute = &VolumeMute;
        audio.volumeGetString = &VolumeGetString;
#endif
        /* check the version info */
        (*audio.getVersion)(&PluginType, &PluginVersion, &APIVersion, NULL, NULL);
        if (PluginType != M64PLUGIN_AUDIO || (APIVersion & 0xffff0000) != (AUDIO_API_VERSION & 0xffff0000))
        {
            DebugMessage(M64MSG_ERROR, "incompatible Audio plugin");
            plugin_disconnect_audio();
            return M64ERR_INCOMPATIBLE;
        }

        l_AudioAttached = 1;
    }
    else
        plugin_disconnect_audio();

    return M64ERR_SUCCESS;
}

static m64p_error plugin_start_audio(void)
{
    /* fill in the AUDIO_INFO data structure */
    audio_info.RDRAM = (unsigned char *)mem_base_u32(g_mem_base, MM_RDRAM_DRAM);
    audio_info.DMEM = (unsigned char *)mem_base_u32(g_mem_base, MM_RSP_MEM);
    audio_info.IMEM = (unsigned char *)mem_base_u32(g_mem_base, MM_RSP_MEM + 0x1000);
    audio_info.MI_INTR_REG = &(g_dev.mi.regs[MI_INTR_REG]);
    audio_info.AI_DRAM_ADDR_REG = &(g_dev.ai.regs[AI_DRAM_ADDR_REG]);
    audio_info.AI_LEN_REG = &(g_dev.ai.regs[AI_LEN_REG]);
    audio_info.AI_CONTROL_REG = &(g_dev.ai.regs[AI_CONTROL_REG]);
    audio_info.AI_STATUS_REG = &dummy;
    audio_info.AI_DACRATE_REG = &(g_dev.ai.regs[AI_DACRATE_REG]);
    audio_info.AI_BITRATE_REG = &(g_dev.ai.regs[AI_BITRATE_REG]);
    audio_info.CheckInterrupts = EmptyFunc;

    /* call the audio plugin */
    if (!audio.initiateAudio(audio_info))
        return M64ERR_PLUGIN_FAIL;

    return M64ERR_SUCCESS;
}

static void plugin_disconnect_input(void)
{
    input = dummy_input;
    l_InputAttached = 0;
}

static m64p_error plugin_connect_input(m64p_dynlib_handle plugin_handle)
{
    /* attach the Input plugin function pointers */
    if (plugin_handle != NULL)
    {
        m64p_plugin_type PluginType;
        int PluginVersion, APIVersion;

        if (l_InputAttached)
            return M64ERR_INVALID_STATE;

#if (!M64P_STATIC_PLUGINS)
        if (!GET_FUNC(ptr_PluginGetVersion, input.getVersion, "PluginGetVersion") ||
            !GET_FUNC(ptr_ControllerCommand, input.controllerCommand, "ControllerCommand") ||
            !GET_FUNC(ptr_GetKeys, input.getKeys, "GetKeys") ||
            !GET_FUNC(ptr_InitiateControllers, input.initiateControllers, "InitiateControllers") ||
            !GET_FUNC(ptr_ReadController, input.readController, "ReadController") ||
            !GET_FUNC(ptr_RomOpen, input.romOpen, "RomOpen") ||
            !GET_FUNC(ptr_RomClosed, input.romClosed, "RomClosed") ||
            !GET_FUNC(ptr_SDL_KeyDown, input.keyDown, "SDL_KeyDown") ||
            !GET_FUNC(ptr_SDL_KeyUp, input.keyUp, "SDL_KeyUp"))
        {
            DebugMessage(M64MSG_ERROR, "broken Input plugin; function(s) not found.");
            plugin_disconnect_input();
            return M64ERR_INPUT_INVALID;
        }
#else
        input.getVersion = &PluginGetVersionInput;
        input.controllerCommand = &ControllerCommand;
        input.getKeys = &GetKeys;
        input.initiateControllers = &InitiateControllers;
        input.readController = &ReadController;
        input.romOpen = &RomOpenInput;
        input.romClosed = &RomClosedInput;
        input.keyDown = &SDL_KeyDown;
        input.keyUp = &SDL_KeyUp;
#endif

        if (!GET_FUNC(ptr_SendVRUWord, input.sendVRUWord, "SendVRUWord") ||
            !GET_FUNC(ptr_SetMicState, input.setMicState, "SetMicState") ||
            !GET_FUNC(ptr_ReadVRUResults, input.readVRUResults, "ReadVRUResults") ||
            !GET_FUNC(ptr_ClearVRUWords, input.clearVRUWords, "ClearVRUWords") ||
            !GET_FUNC(ptr_SetVRUWordMask, input.setVRUWordMask, "SetVRUWordMask"))
        {
            DebugMessage(M64MSG_WARNING, "Input plugin does not contain VRU support.");
        }

        /* check the version info */
        (*input.getVersion)(&PluginType, &PluginVersion, &APIVersion, NULL, NULL);
        if (PluginType != M64PLUGIN_INPUT || (APIVersion & 0xffff0000) != (INPUT_API_VERSION & 0xffff0000) || APIVersion < 0x020100)
        {
            DebugMessage(M64MSG_ERROR, "incompatible Input plugin");
            plugin_disconnect_input();
            return M64ERR_INCOMPATIBLE;
        }

#if (!M64P_STATIC_PLUGINS)
        if (!GET_FUNC(ptr_RenderCallback, input.renderCallback, "RenderCallback"))
        {
            DebugMessage(M64MSG_INFO, "input plugin did not specify a render callback; there will be no on screen display by the input plugin.");
        }
#endif

        l_InputAttached = 1;
    }
    else
        plugin_disconnect_input();

    return M64ERR_SUCCESS;
}

static m64p_error plugin_start_input(void)
{
    int i;

    /* fill in the CONTROL_INFO data structure */
    control_info.Controls = Controls;
    for (i=0; i<4; i++)
      {
         Controls[i].Present = 0;
         Controls[i].RawData = 0;
         Controls[i].Plugin = PLUGIN_NONE;
         Controls[i].Type = CONT_TYPE_STANDARD;
      }

    /* call the input plugin */
    input.initiateControllers(control_info);

    return M64ERR_SUCCESS;
}

static void plugin_disconnect_rsp(void)
{
    rsp = dummy_rsp;
    l_RspAttached = 0;
}

static m64p_error plugin_connect_rsp(m64p_dynlib_handle plugin_handle)
{
    /* attach the RSP plugin function pointers */
    if (plugin_handle != NULL)
    {
        m64p_plugin_type PluginType;
        int PluginVersion, APIVersion;

        if (l_RspAttached)
            return M64ERR_INVALID_STATE;

#if (!M64P_STATIC_PLUGINS)
        if (!GET_FUNC(ptr_PluginGetVersion, rsp.getVersion, "PluginGetVersion") ||
            !GET_FUNC(ptr_DoRspCycles, rsp.doRspCycles, "DoRspCycles") ||
            !GET_FUNC(ptr_InitiateRSP, rsp.initiateRSP, "InitiateRSP") ||
            !GET_FUNC(ptr_RomClosed, rsp.romClosed, "RomClosed"))
        {
            DebugMessage(M64MSG_ERROR, "broken RSP plugin; function(s) not found.");
            plugin_disconnect_rsp();
            return M64ERR_INPUT_INVALID;
        }

#else
	rsp.getVersion = &PluginGetVersionRSP;
        rsp.doRspCycles = &DoRspCycles;
        rsp.initiateRSP = &InitiateRSP;
        rsp.romClosed = &RomClosedRSP;
#endif

        /* check the version info */
        (*rsp.getVersion)(&PluginType, &PluginVersion, &APIVersion, NULL, NULL);
        if (PluginType != M64PLUGIN_RSP || (APIVersion & 0xffff0000) != (RSP_API_VERSION & 0xffff0000))
        {
            DebugMessage(M64MSG_ERROR, "incompatible RSP plugin");
            plugin_disconnect_rsp();
            return M64ERR_INCOMPATIBLE;
        }

        l_RspAttached = 1;
    }
    else
        plugin_disconnect_rsp();

    return M64ERR_SUCCESS;
}

static m64p_error plugin_start_rsp(void)
{
    /* fill in the RSP_INFO data structure */
    rsp_info.RDRAM = (unsigned char *)mem_base_u32(g_mem_base, MM_RDRAM_DRAM);
    rsp_info.DMEM = (unsigned char *)mem_base_u32(g_mem_base, MM_RSP_MEM);
    rsp_info.IMEM = (unsigned char *)mem_base_u32(g_mem_base, MM_RSP_MEM + 0x1000);
    rsp_info.MI_INTR_REG = &g_dev.mi.regs[MI_INTR_REG];
    rsp_info.SP_MEM_ADDR_REG = &g_dev.sp.regs[SP_MEM_ADDR_REG];
    rsp_info.SP_DRAM_ADDR_REG = &g_dev.sp.regs[SP_DRAM_ADDR_REG];
    rsp_info.SP_RD_LEN_REG = &g_dev.sp.regs[SP_RD_LEN_REG];
    rsp_info.SP_WR_LEN_REG = &g_dev.sp.regs[SP_WR_LEN_REG];
    rsp_info.SP_STATUS_REG = &g_dev.sp.regs[SP_STATUS_REG];
    rsp_info.SP_DMA_FULL_REG = &g_dev.sp.regs[SP_DMA_FULL_REG];
    rsp_info.SP_DMA_BUSY_REG = &g_dev.sp.regs[SP_DMA_BUSY_REG];
    rsp_info.SP_PC_REG = &g_dev.sp.regs2[SP_PC_REG];
    rsp_info.SP_SEMAPHORE_REG = &g_dev.sp.regs[SP_SEMAPHORE_REG];
    rsp_info.DPC_START_REG = &g_dev.dp.dpc_regs[DPC_START_REG];
    rsp_info.DPC_END_REG = &g_dev.dp.dpc_regs[DPC_END_REG];
    rsp_info.DPC_CURRENT_REG = &g_dev.dp.dpc_regs[DPC_CURRENT_REG];
    rsp_info.DPC_STATUS_REG = &g_dev.dp.dpc_regs[DPC_STATUS_REG];
    rsp_info.DPC_CLOCK_REG = &g_dev.dp.dpc_regs[DPC_CLOCK_REG];
    rsp_info.DPC_BUFBUSY_REG = &g_dev.dp.dpc_regs[DPC_BUFBUSY_REG];
    rsp_info.DPC_PIPEBUSY_REG = &g_dev.dp.dpc_regs[DPC_PIPEBUSY_REG];
    rsp_info.DPC_TMEM_REG = &g_dev.dp.dpc_regs[DPC_TMEM_REG];
    rsp_info.CheckInterrupts = EmptyFunc;
    rsp_info.ProcessDlistList = gfx.processDList;
    rsp_info.ProcessAlistList = audio.processAList;
    rsp_info.ProcessRdpList = gfx.processRDPList;
    rsp_info.ShowCFB = gfx.showCFB;

    /* call the RSP plugin  */
    rsp.initiateRSP(rsp_info, NULL);

    return M64ERR_SUCCESS;
}

/* global functions */
m64p_error plugin_connect(m64p_plugin_type type, m64p_dynlib_handle plugin_handle)
{
    switch(type)
    {
        case M64PLUGIN_GFX:
            if (plugin_handle != NULL && (l_AudioAttached || l_InputAttached || l_RspAttached))
                DebugMessage(M64MSG_WARNING, "Front-end bug: plugins are attached in wrong order.");
            return plugin_connect_gfx(plugin_handle);
        case M64PLUGIN_AUDIO:
            if (plugin_handle != NULL && (l_InputAttached || l_RspAttached))
                DebugMessage(M64MSG_WARNING, "Front-end bug: plugins are attached in wrong order.");
            return plugin_connect_audio(plugin_handle);
        case M64PLUGIN_INPUT:
            if (plugin_handle != NULL && (l_RspAttached))
                DebugMessage(M64MSG_WARNING, "Front-end bug: plugins are attached in wrong order.");
            return plugin_connect_input(plugin_handle);
        case M64PLUGIN_RSP:
            return plugin_connect_rsp(plugin_handle);
        default:
            return M64ERR_INPUT_INVALID;
    }

    return M64ERR_INTERNAL;
}

m64p_error plugin_start(m64p_plugin_type type)
{
    switch(type)
    {
        case M64PLUGIN_RSP:
            return plugin_start_rsp();
        case M64PLUGIN_GFX:
            return plugin_start_gfx();
        case M64PLUGIN_AUDIO:
            return plugin_start_audio();
        case M64PLUGIN_INPUT:
            return plugin_start_input();
        default:
            return M64ERR_INPUT_INVALID;
    }

    return M64ERR_INTERNAL;
}

m64p_error plugin_check(void)
{
    if (!l_GfxAttached)
        DebugMessage(M64MSG_WARNING, "No video plugin attached.  There will be no video output.");
    if (!l_RspAttached)
        DebugMessage(M64MSG_WARNING, "No RSP plugin attached.  The video output will be corrupted.");
    if (!l_AudioAttached)
        DebugMessage(M64MSG_WARNING, "No audio plugin attached.  There will be no sound output.");
    if (!l_InputAttached)
        DebugMessage(M64MSG_WARNING, "No input plugin attached.  You won't be able to control the game.");

    return M64ERR_SUCCESS;
}

