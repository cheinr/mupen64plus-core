/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *   Mupen64plus - netplay.c                                               *
 *   Mupen64Plus homepage: https://mupen64plus.org/                        *
 *   Copyright (C) 2020 loganmc10                                          *
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

#include <stdlib.h>

#define M64P_CORE_PROTOTYPES 1
#include "api/callbacks.h"
#include "main.h"
#include "util.h"
#include "plugin/plugin.h"
#include "backends/plugins_compat/plugins_compat.h"
#include "netplay.h"
#include <SDL.h>

#if (!EMSCRIPTEN)
#include <SDL_net.h>

#if !defined(WIN32)
#include <sys/socket.h>
#include <netinet/ip.h>
#endif

#else

#include <emscripten.h>
#include <arpa/inet.h>

#define SDLNet_Write32(VALUE, BUFFER_POINTER) {\
    uint32_t value = htonl(VALUE);             \
    memcpy(BUFFER_POINTER, &value, 4);   \
  }
#define SDLNet_Read32(BUFFER_POINTER) ntohl(*((uint32_t*) BUFFER_POINTER));

extern uint32_t netplayPaused;

/* jslib functions */
extern void netplayInit();
extern void checkForUnreliableMessages(void* responseBufferPointer,
                                       int maxNumberOfMessages,
                                       void* numberOfMessagesPresentPointer);
extern void sendUnreliableMessage(void* messageDataPointer, int messageLength);
extern void sendReliableMessage(void* messageDataPointer, int messageLength);
extern void waitForReliableMessage(void* messageDataPointer);

#endif

static int l_canFF;
static int l_netplay_controller;
static int l_netplay_control[4];
static int l_player_is_host;

#if (!EMSCRIPTEN)
static UDPsocket l_udpSocket;
static TCPsocket l_tcpSocket;
static int l_udpChannel;

#else

static int l_connected;
static int l_pauseRequested;
static uint32_t l_pauseTargets[4];
#endif

static int l_spectator;


static int l_netplay_is_init = 0;
static uint32_t l_vi_counter;
static uint8_t l_status;
static uint32_t l_reg_id;
static struct controller_input_compat *l_cin_compats;
static uint8_t l_plugin[4];
static uint8_t l_buffer_target;
static uint8_t l_player_lag[4];

//UDP packet formats
#define UDP_SEND_KEY_INFO 0
#define UDP_RECEIVE_KEY_INFO 1
#define UDP_REQUEST_KEY_INFO 2
#define UDP_SYNC_DATA 4

//TCP packet formats
#define TCP_SEND_SAVE 1
#define TCP_RECEIVE_SAVE 2
#define TCP_SEND_SETTINGS 3
#define TCP_RECEIVE_SETTINGS 4
#define TCP_REGISTER_PLAYER 5
#define TCP_GET_REGISTRATION 6
#define TCP_DISCONNECT_NOTICE 7

struct __UDPSocket {
    int ready;
    int channel;
};

#define EF 46

m64p_error netplay_start(const char* host, int port)
{

#if (!EMSCRIPTEN)
    if (SDLNet_Init() < 0)
    {
        DebugMessage(M64MSG_ERROR, "Netplay: Could not initialize SDL Net library");
        return M64ERR_SYSTEM_FAIL;
    }

    l_udpSocket = SDLNet_UDP_Open(0);
    if (l_udpSocket == NULL)
    {
        DebugMessage(M64MSG_ERROR, "Netplay: UDP socket creation failed");
        return M64ERR_SYSTEM_FAIL;
    }

#if !defined(WIN32)
    const char tos_local = EF << 2;
    struct __UDPSocket* socket = (struct __UDPSocket*) l_udpSocket;
    setsockopt(socket->channel, IPPROTO_IP, IP_TOS, &tos_local, sizeof(tos_local));
#endif

    IPaddress dest;
    SDLNet_ResolveHost(&dest, host, port);

    l_udpChannel = SDLNet_UDP_Bind(l_udpSocket, -1, &dest);
    if (l_udpChannel < 0)
    {
        DebugMessage(M64MSG_ERROR, "Netplay: could not bind to UDP socket");
        SDLNet_UDP_Close(l_udpSocket);
        l_udpSocket = NULL;
        return M64ERR_SYSTEM_FAIL;
    }

    l_tcpSocket = SDLNet_TCP_Open(&dest);
    if (l_tcpSocket == NULL)
    {
        DebugMessage(M64MSG_ERROR, "Netplay: could not bind to TCP socket");
        SDLNet_UDP_Close(l_udpSocket);
        l_udpSocket = NULL;
        return M64ERR_SYSTEM_FAIL;
    }

# else // EMSCRIPTEN

    netplayInit();

    l_connected = 1;

# endif

    for (int i = 0; i < 4; ++i)
    {
        l_netplay_control[i] = -1;
        l_plugin[i] = 0;
        l_player_lag[i] = 0;
    }

    l_canFF = 0;
    l_player_is_host = 0;
    l_netplay_controller = 0;
    l_netplay_is_init = 1;
    l_spectator = 1;
    l_vi_counter = 0;
    l_status = 0;
    l_reg_id = 0;

#if EMSCRIPTEN
    // Used for cases where the player starts out as a spectator
    l_reg_id = EM_ASM_INT({ return Module.netplayConfig.registrationId; });
#endif

    return M64ERR_SUCCESS;
}

m64p_error netplay_stop()
{

#if (!EMSCRIPTEN)
    if (l_udpSocket == NULL)
#else
    if (0)
#endif
        return M64ERR_INVALID_STATE;
    else
    {
        if (l_cin_compats != NULL)
        {
            for (int i = 0; i < 4; ++i)
            {
                struct netplay_event* current = l_cin_compats[i].event_first;
                struct netplay_event* next;
                while (current != NULL)
                {
                    next = current->next;
                    free(current);
                    current = next;
                }
            }
        }

        char output_data[5];
        output_data[0] = TCP_DISCONNECT_NOTICE;

        SDLNet_Write32(l_reg_id, &output_data[1]);
        
#if (!EMSCRIPTEN)
        SDLNet_TCP_Send(l_tcpSocket, &output_data[0], 5);

        SDLNet_UDP_Unbind(l_udpSocket, l_udpChannel);
        SDLNet_UDP_Close(l_udpSocket);
        SDLNet_TCP_Close(l_tcpSocket);
        
        l_tcpSocket = NULL;
        l_udpSocket = NULL;
        l_udpChannel = -1;
        l_netplay_is_init = 0;
        SDLNet_Quit();

#else // EMSCRIPTEN

        sendUnreliableMessage(&output_data[0], 5);
#endif

        return M64ERR_SUCCESS;
    }
}

int netplay_is_init()
{
    return l_netplay_is_init;
}

static uint8_t buffer_size(uint8_t control_id)
{
    //This function returns the size of the local input buffer
    uint8_t counter = 0;
    struct netplay_event* current = l_cin_compats[control_id].event_first;
    while (current != NULL)
    {
        current = current->next;
        ++counter;
    }
    return counter;
}

#if (!EMSCRIPTEN)
static void netplay_request_input(uint8_t control_id)
#else
EMSCRIPTEN_KEEPALIVE void netplay_request_input(uint8_t control_id)
#endif
{
  
#if (!EMSCRIPTEN)
    UDPpacket *packet = SDLNet_AllocPacket(12);
    packet->data[0] = UDP_REQUEST_KEY_INFO;
    packet->data[1] = control_id; //The player we need input for
    SDLNet_Write32(l_reg_id, &packet->data[2]); //our registration ID
    SDLNet_Write32(l_cin_compats[control_id].netplay_count, &packet->data[6]); //the current event count
    packet->data[10] = l_spectator; //whether we are a spectator
    packet->data[11] = buffer_size(control_id); //our local buffer size
    packet->len = 12;


    SDLNet_UDP_Send(l_udpSocket, l_udpChannel, packet);
    SDLNet_FreePacket(packet);
#else

    char packet[12];
    packet[0] = UDP_REQUEST_KEY_INFO;
    packet[1] = control_id;
    SDLNet_Write32(l_reg_id, &packet[2]);
    SDLNet_Write32(l_cin_compats[control_id].netplay_count, &packet[6]); //the current event count
    packet[10] = l_spectator; //whether we are a spectator
    packet[11] = buffer_size(control_id); //our local buffer size

    sendUnreliableMessage(&packet[0], 12);
#endif
}

#if (!EMSCRIPTEN)
static int check_valid(uint8_t control_id, uint32_t count)
#else
EMSCRIPTEN_KEEPALIVE int check_valid(uint8_t control_id, uint32_t count)
#endif
{
    //Check if we already have this event recorded locally, returns 1 if we do
    struct netplay_event* current = l_cin_compats[control_id].event_first;
    while (current != NULL)
    {
        if (current->count == count) //event already recorded
            return 1;
        current = current->next;
    }
    return 0;
}

#if (!EMSCRIPTEN)
static void process_udp_packet(char* data) {
#else
EMSCRIPTEN_KEEPALIVE void process_udp_packet(char* data) {
#endif
  
  uint32_t curr, count, keys;
  uint8_t plugin, player, current_status;

  switch (data[0]) {
    case UDP_RECEIVE_KEY_INFO:
      player = data[1];
      //current_status is a status update from the server
      //it will let us know if another player has disconnected, or the games have desynced

      uint32_t first_count = SDLNet_Read32(&data[5]);
      
      current_status = data[2];
      l_player_lag[player] = data[3];
      if (current_status != l_status)
        {
          if (((current_status & 0x1) ^ (l_status & 0x1)) != 0)
            DebugMessage(M64MSG_ERROR, "Netplay: players have de-synced at VI %u", l_vi_counter);
          for (int dis = 1; dis < 5; ++dis)
            {
              if (((current_status & (0x1 << dis)) ^ (l_status & (0x1 << dis))) != 0)
                DebugMessage(M64MSG_ERROR, "Netplay: player %u has disconnected", dis);
            }
          l_status = current_status;
        }
      curr = 5;
      //this loop processes input data from the server, inserting new events into the linked list for each player
      //it skips events that we have already recorded, or if we receive data for an event that has already happened
      for (uint8_t i = 0; i < data[4]; ++i)
        {
          count = SDLNet_Read32(&data[curr]);
          curr += 4;

          if (((count - l_cin_compats[player].netplay_count) > (UINT32_MAX / 2)) || (check_valid(player, count))) //event doesn't need to be recorded
            {
              curr += 5;
              continue;
            }

          keys = SDLNet_Read32(&data[curr]);
          curr += 4;
          plugin = data[curr];
          curr += 1;

          //insert new event at beginning of linked list
          struct netplay_event* new_event = (struct netplay_event*)malloc(sizeof(struct netplay_event));
          new_event->count = count;
          new_event->buttons = keys;
          new_event->plugin = plugin;
          new_event->next = l_cin_compats[player].event_first;
          l_cin_compats[player].event_first = new_event;
        }
      break;
    default:
      DebugMessage(M64MSG_ERROR, "Netplay: received unknown message from server");
      break;
    }
}


#if EMSCRIPTEN

static int check_for_unreliable_messages() {
  int numberOfMessagesPresent = 0;
  char messageBuffer[65536];
  int maxNumberOfMessages = 128;

  checkForUnreliableMessages(messageBuffer,
                            maxNumberOfMessages,
                            &numberOfMessagesPresent);

  int messageNumber;
  for (messageNumber = 0; messageNumber < numberOfMessagesPresent; messageNumber++) {

    int offset = 512 * messageNumber;

    char message[512];

    memcpy(&message[0], &messageBuffer[offset], 512);

    process_udp_packet(message);
  }

  return numberOfMessagesPresent > 0;
}

#endif

static int netplay_require_response(void* opaque)
{
  
    //This function runs inside a thread.
    //It runs if our local buffer size is 0 (we need to execute a key event, but we don't have the data we need).
  //We basically beg the server for input data.
  //After 10 seconds a timeout occurs, we assume we have lost connection to the server.
    uint8_t control_id = *(uint8_t*)opaque;
    uint32_t timeout = SDL_GetTicks() + 10000;
    uint32_t counter = 0;
#if EMSCRIPTEN
    uint32_t sleepTimeMillis = 5;
#endif

    while (!check_valid(control_id, l_cin_compats[control_id].netplay_count))
    {

#if EMSCRIPTEN
      if (check_for_unreliable_messages()) {
        continue;
      }
#endif
      
      counter++;
      netplay_request_input(control_id);
      
      if (SDL_GetTicks() > timeout) {
          
        printf("We've timed out! controller=%d, count=%d\n", control_id, l_cin_compats[control_id].netplay_count);
#if (!EMSCRIPTEN)
        l_udpChannel = -1;
        return 0;
#else
        l_connected = -1;
#endif
      }

#if (!EMSCRIPTEN)
        SDL_Delay(5);
#else
        // yield to the event loop so we can receive new messages
        emscripten_sleep(sleepTimeMillis);
        sleepTimeMillis *= 2;
#endif
    }

    return 1;
}


#if EMSCRIPTEN
void processUDPPacket(char* data) {
  process_udp_packet(data);
}
#endif


#if (!EMSCRIPTEN)
static void netplay_process()
{

  printf("netplay_process\n");

    //In this function we process data we have received from the server
    UDPpacket *packet = SDLNet_AllocPacket(512);

    while (SDLNet_UDP_Recv(l_udpSocket, packet) == 1)
    {

      process_udp_packet(packet->data);
      
    }
    SDLNet_FreePacket(packet);

}

#else // EMSCRIPTEN
    // Since packets are received and processed asynchronously this shouldn't be needed?
#endif


static int netplay_ensure_valid(uint8_t control_id)
{
  
    //This function makes sure we have data for a certain event
    //If we don't have the data, it will create a new thread that will request the data
    if (check_valid(control_id, l_cin_compats[control_id].netplay_count))
        return 1;

#if (!EMSCRIPTEN)
    if (l_udpChannel == -1)
        return 0;

#if SDL_VERSION_ATLEAST(2,0,0)
    SDL_Thread* thread = SDL_CreateThread(netplay_require_response, "Netplay key request", &control_id);
#else
    SDL_Thread* thread = SDL_CreateThread(netplay_require_response, &control_id);
#endif
   
#endif


    int success = 0;
#if (!EMSCRIPTEN)    
    while (!check_valid(control_id, l_cin_compats[control_id].netplay_count) && l_udpChannel != -1) {
      netplay_process();
    }

    SDL_WaitThread(thread, &success);
#else

    success = netplay_require_response(&control_id);

#endif

    return success;
}

static void netplay_delete_event(struct netplay_event* current, uint8_t control_id)
{
    //This function deletes an event from the linked list
    struct netplay_event* find = l_cin_compats[control_id].event_first;
    while (find != NULL)
    {
        if (find->next == current)
        {
            find->next = current->next;
            break;
        }
        find = find->next;
    }
    if (current == l_cin_compats[control_id].event_first)
        l_cin_compats[control_id].event_first = l_cin_compats[control_id].event_first->next;
    free(current);
}

static uint32_t netplay_get_input(uint8_t control_id)
{

  
    uint32_t keys;

#if (!EMSCRIPTEN)
    netplay_process();
#endif

    netplay_request_input(control_id);


    //l_buffer_target is set by the server upon registration
    //l_player_lag is how far behind we are from the lead player
    //buffer_size is the local buffer size
    if (l_player_lag[control_id] > 0 && buffer_size(control_id) > l_buffer_target)
    {
        l_canFF = 1;
        main_core_state_set(M64CORE_SPEED_LIMITER, 0);
    }
    else
    {
      // This first line needs to come first, as it ultimately
      // depends on l_canFF
      main_core_state_set(M64CORE_SPEED_LIMITER, 1);
      l_canFF = 0;
    }

    if (netplay_ensure_valid(control_id))
    {
        //We grab the event from the linked list, the delete it once it has been used
        //Finally we increment the event counter
        struct netplay_event* current = l_cin_compats[control_id].event_first;
        while (current->count != l_cin_compats[control_id].netplay_count)
            current = current->next;
        keys = current->buttons;
        Controls[control_id].Plugin = current->plugin;
        netplay_delete_event(current, control_id);
        ++l_cin_compats[control_id].netplay_count;
    }
    else
    {
        DebugMessage(M64MSG_ERROR, "Netplay: lost connection to server");
        main_core_state_set(M64CORE_EMU_STATE, M64EMU_STOPPED);
        keys = 0;
    }

    return keys;
}

static void netplay_send_input(uint8_t control_id, uint32_t keys)
{
#if (!EMSCRIPTEN)
    UDPpacket *packet = SDLNet_AllocPacket(11);
    packet->data[0] = UDP_SEND_KEY_INFO;
    packet->data[1] = control_id; //player number
    SDLNet_Write32(l_cin_compats[control_id].netplay_count, &packet->data[2]); // current event count
    SDLNet_Write32(keys, &packet->data[6]); //key data
    packet->data[10] = l_plugin[control_id]; //current plugin
    packet->len = 11;
    SDLNet_UDP_Send(l_udpSocket, l_udpChannel, packet);
    SDLNet_FreePacket(packet);
#else // EMSCRIPTEN

    char packet[11];
    packet[0] = UDP_SEND_KEY_INFO;
    packet[1] = control_id; //player number
    SDLNet_Write32(l_cin_compats[control_id].netplay_count, &packet[2]); // current event count
    SDLNet_Write32(keys, &packet[6]); //key data
    packet[10] = l_plugin[control_id]; //current plugin

    sendUnreliableMessage(packet, 11);
#endif
}

uint8_t netplay_register_player(uint8_t player, uint8_t plugin, uint8_t rawdata, uint32_t reg_id)
{
  printf("Registering player: %d with plugin %d and useRawData: %d and registrationId: %d\n", player, plugin, rawdata, reg_id);
  
    l_reg_id = reg_id;
    char output_data[8];
    output_data[0] = TCP_REGISTER_PLAYER;
    output_data[1] = player; //player number we'd like to register
    output_data[2] = plugin; //current plugin
    output_data[3] = rawdata; //whether we are using a RawData input plugin

    SDLNet_Write32(l_reg_id, &output_data[4]);

#if (!EMSCRIPTEN)
    SDLNet_TCP_Send(l_tcpSocket, &output_data[0], 8);

    uint8_t response[2];
    size_t recv = 0;
    while (recv < 2)
        recv += SDLNet_TCP_Recv(l_tcpSocket, &response[recv], 2 - recv);
    l_buffer_target = response[1]; //local buffer size target

    return response[0];
#else // EMSCRIPTEN

    uint8_t response[2];
    
    sendReliableMessage(output_data, 8);
    
    waitForReliableMessage(response);

    l_buffer_target = response[1];
    
    return response[0];
#endif
}

int EMSCRIPTEN_KEEPALIVE netplay_request_pause(int32_t* pauseTargets) {

  printf("netplay_request_pause; pauseTargets: [");
  int i;
  for (i = 0; i < 4; i++) {
    printf("%d,", pauseTargets[i]);

    if (netplayPaused && pauseTargets[i] != -1 && pauseTargets[i] > l_pauseTargets[i]) {
      netplayPaused = 0; // Resume until we get to the new target
    }

    l_pauseTargets[i] = pauseTargets[i];
  }
  printf("]\n");

  l_pauseRequested = 1;
}

int EMSCRIPTEN_KEEPALIVE netplay_request_resume() {
  l_pauseRequested = 0;
  netplayPaused = 0;

  netplay_read_registration(l_cin_compats);
}

int netplay_lag()
{
  return l_canFF;
}

int netplay_next_controller()
{
    return l_netplay_controller;
}

void netplay_set_controller(uint8_t player)
{
    l_netplay_control[player] = l_netplay_controller++;
    l_spectator = 0;

    if (player == 0) {
      l_player_is_host = 1;
    }
}

int netplay_get_controller(uint8_t player)
{
    return l_netplay_control[player];
}

int netplay_get_is_host()
{
    return l_player_is_host;
}

file_status_t netplay_read_storage(const char *filename, void *data, size_t size)
{
    //This function syncs save games.
    //If the client is controlling player 1, it sends its save game to the server
    //All other players receive save files from the server
    const char *short_filename = strrchr(filename, '/');
    if (short_filename == NULL)
        short_filename = strrchr(filename, '\\');
    short_filename += 1;

    uint32_t buffer_pos = 0;
    char *output_data = malloc(size + strlen(short_filename) + 6);

    file_status_t ret;
    uint8_t request;
    if (l_netplay_control[0] != -1)
    {
        request = TCP_SEND_SAVE;
        memcpy(&output_data[buffer_pos], &request, 1);
        ++buffer_pos;
        
         //send file name
        memcpy(&output_data[buffer_pos], short_filename, strlen(short_filename) + 1);
        buffer_pos += strlen(short_filename) + 1;

        ret = read_from_file(filename, data, size);
        if (ret == file_open_error)
            memset(data, 0, size); //all zeros means there is no save file

        SDLNet_Write32((int32_t)size, &output_data[buffer_pos]); //file data size
        buffer_pos += 4;
        memcpy(&output_data[buffer_pos], data, size); //file data
        buffer_pos += size;

#if (!EMSCRIPTEN) 
        SDLNet_TCP_Send(l_tcpSocket, &output_data[0], buffer_pos);
#else
        sendReliableMessage(output_data, buffer_pos);
#endif
    }
    else
    {
        request = TCP_RECEIVE_SAVE;
        memcpy(&output_data[buffer_pos], &request, 1);
        ++buffer_pos;

        printf("Requesting file: %s\n", short_filename);

        //name of the file we are requesting
        memcpy(&output_data[buffer_pos], short_filename, strlen(short_filename) + 1);
        buffer_pos += strlen(short_filename) + 1;

        char *data_array = data;
#if (!EMSCRIPTEN)
        SDLNet_TCP_Send(l_tcpSocket, &output_data[0], buffer_pos);
        size_t recv = 0;
        while (recv < size)
            recv += SDLNet_TCP_Recv(l_tcpSocket, data_array + recv, size - recv);
#else // EMSCRIPTEN
        sendReliableMessage(output_data, buffer_pos);
        waitForReliableMessage(data_array);
#endif
        int sum = 0;
        for (int i = 0; i < size; ++i)
            sum |= data_array[i];

        if (sum == 0) //all zeros means there is no save file
            ret = file_open_error;
        else
            ret = file_ok;
    }
    free(output_data);
    return ret;
}

void netplay_sync_settings(uint32_t *count_per_op, uint32_t *count_per_op_denom_pot, uint32_t *disable_extra_mem, int32_t *si_dma_duration, uint32_t *emumode, int32_t *no_compiled_jump)
{
    if (!netplay_is_init())
        return;

    char output_data[25];
    uint8_t request;
    if (l_netplay_control[0] != -1) //player 1 is the source of truth for settings
    {
        request = TCP_SEND_SETTINGS;
        memcpy(&output_data[0], &request, 1);
        SDLNet_Write32(*count_per_op, &output_data[1]);
        SDLNet_Write32(*count_per_op_denom_pot, &output_data[5]);
        SDLNet_Write32(*disable_extra_mem, &output_data[9]);
        SDLNet_Write32(*si_dma_duration, &output_data[13]);
        SDLNet_Write32(*emumode, &output_data[17]);
        SDLNet_Write32(*no_compiled_jump, &output_data[21]);

#if (!EMSCRIPTEN)
        SDLNet_TCP_Send(l_tcpSocket, &output_data[0], 25);
#else
        sendReliableMessage(output_data, 25);
#endif
    }
    else
    {
        request = TCP_RECEIVE_SETTINGS;
        memcpy(&output_data[0], &request, 1);

#if (!EMSCRIPTEN)
        SDLNet_TCP_Send(l_tcpSocket, &output_data[0], 1);
        int32_t recv = 0;
        while (recv < 24)
            recv += SDLNet_TCP_Recv(l_tcpSocket, &output_data[recv], 20 - recv);
#else
        sendReliableMessage(output_data, 1);
        waitForReliableMessage(output_data);
#endif

        *count_per_op = SDLNet_Read32(&output_data[0]);
        *count_per_op_denom_pot = SDLNet_Read32(&output_data[4]);
        *disable_extra_mem = SDLNet_Read32(&output_data[8]);
        *si_dma_duration = SDLNet_Read32(&output_data[12]);
        *emumode = SDLNet_Read32(&output_data[16]);
        *no_compiled_jump = SDLNet_Read32(&output_data[20]);
    }
}

void netplay_check_sync(struct cp0* cp0)
{
    //This function is used to check if games have desynced
    //Every 60 VIs, it sends the value of the CP0 registers to the server
    //The server will compare the values, and update the status byte if it detects a desync
    if (!netplay_is_init())
        return;

    const uint32_t* cp0_regs = r4300_cp0_regs(cp0);

    if (l_vi_counter % 60 == 0)
    {
        uint32_t packet_len = (CP0_REGS_COUNT * 4) + 5;

#if (!EMSCRIPTEN)
        UDPpacket *packet = SDLNet_AllocPacket(packet_len);
        packet->data[0] = UDP_SYNC_DATA;
        SDLNet_Write32(l_vi_counter, &packet->data[1]); //current VI count
        for (int i = 0; i < CP0_REGS_COUNT; ++i)
        {
            SDLNet_Write32(cp0_regs[i], &packet->data[(i * 4) + 5]);
        }
        packet->len = packet_len;
        SDLNet_UDP_Send(l_udpSocket, l_udpChannel, packet);
        SDLNet_FreePacket(packet);
#else
        char* packet = malloc(packet_len);
        packet[0] = UDP_SYNC_DATA;
        SDLNet_Write32(l_vi_counter, &packet[1]); //current VI count
        for (int i = 0; i < CP0_REGS_COUNT; ++i)
        {
            SDLNet_Write32(cp0_regs[i], &packet[(i * 4) + 5]);
        }

        sendUnreliableMessage(packet, packet_len);

        free(packet);
#endif
    }
    ++l_vi_counter;
}

void netplay_read_registration(struct controller_input_compat* cin_compats)
{
    //This function runs right before the game starts
    //The server shares the registration details about each player
    if (!netplay_is_init())
        return;

    l_cin_compats = cin_compats;

    uint32_t reg_id;
    char output_data = TCP_GET_REGISTRATION;
    char input_data[24];

#if (!EMSCRIPTEN)
    SDLNet_TCP_Send(l_tcpSocket, &output_data, 1);
    size_t recv = 0;
    while (recv < 24)
        recv += SDLNet_TCP_Recv(l_tcpSocket, &input_data[recv], 24 - recv);

#else

    sendReliableMessage(&output_data, 1);
    waitForReliableMessage(input_data);
#endif

    uint32_t curr = 0;
    for (int i = 0; i < 4; ++i)
    {

        reg_id = SDLNet_Read32(&input_data[curr]);

#if (EMSCRIPTEN)
        if (l_reg_id != 0 && l_reg_id == reg_id) {
          l_netplay_control[i] = 0;
        } else {
          l_netplay_control[i] = -1;
        }
#endif
        
        printf("RegistrationId for player %d: %d\n", i, reg_id);
        
        curr += 4;
        if (reg_id == 0) //No one registered to control this player
        {
            Controls[i].Present = 0;
            Controls[i].Plugin = 1;
            Controls[i].RawData = 0;
            curr += 2;
        }
        else
        {
            Controls[i].Present = 1;
            Controls[i].Plugin = input_data[curr];
            l_plugin[i] = Controls[i].Plugin;
            ++curr;
            Controls[i].RawData = input_data[curr];
            ++curr;
        }
    }

#if EMSCRIPTEN
    int isSpectator = 1;
    for (int i = 0; i < 4; ++i) {
      if (l_netplay_control[i] != -1) {
        isSpectator = 0;
      }
    }

    l_spectator = isSpectator;
#endif
}

static void netplay_send_raw_input(struct pif* pif)
{
    for (int i = 0; i < 4; ++i)
    {
        if (l_netplay_control[i] != -1)
        {
            if (pif->channels[i].tx && pif->channels[i].tx_buf[0] == JCMD_CONTROLLER_READ)
                netplay_send_input(i, *(uint32_t*)pif->channels[i].rx_buf);
        }
    }
}

static void netplay_get_raw_input(struct pif* pif)
{ 
    for (int i = 0; i < 4; ++i)
    {
        if (Controls[i].Present == 1)
        {
            if (pif->channels[i].tx)
            {
                *pif->channels[i].rx &= ~0xC0; //Always show the controller as connected

                if(pif->channels[i].tx_buf[0] == JCMD_CONTROLLER_READ)
                {
                    *(uint32_t*)pif->channels[i].rx_buf = netplay_get_input(i);
                }
                else if ((pif->channels[i].tx_buf[0] == JCMD_STATUS || pif->channels[i].tx_buf[0] == JCMD_RESET) && Controls[i].RawData)
                {
                    //a bit of a hack for raw input controllers, force the status
                    uint16_t type = JDT_JOY_ABS_COUNTERS | JDT_JOY_PORT;
                    pif->channels[i].rx_buf[0] = (uint8_t)(type >> 0);
                    pif->channels[i].rx_buf[1] = (uint8_t)(type >> 8);
                    pif->channels[i].rx_buf[2] = 0;
                }
                else if (pif->channels[i].tx_buf[0] == JCMD_PAK_READ && Controls[i].RawData)
                {
                    //also a hack for raw input, we return "mempak not present" if the game tries to read the mempak
                    pif->channels[i].rx_buf[32] = 255;
                }
                else if (pif->channels[i].tx_buf[0] == JCMD_PAK_WRITE && Controls[i].RawData)
                {
                    //also a hack for raw input, we return "mempak not present" if the game tries to write to mempak
                    pif->channels[i].rx_buf[0] = 255;
                }
            }
        }
    }

    if (l_pauseRequested) {

      int shouldPause = 1;
      for (int i = 0; i < 4; ++i) {
        if (l_pauseTargets[i] != -1 && (l_cin_compats[i].netplay_count < l_pauseTargets[i])) {
          shouldPause = 0;
        }
      }

      if (shouldPause) {
        netplayPaused = 1;

        uint32_t actualPauseCounts[4];
        for (int i = 0; i < 4; ++i) {
          actualPauseCounts[i] = l_cin_compats[i].netplay_count;
        }

        EM_ASM_INT({

            const pauseCountsPtr = $0;
            const pauseCounts = [];
            for (let i = 0; i < 4; i++) {
              pauseCounts[i] = Module.getValue(pauseCountsPtr + (i * 4), 'i32');
            }

            if (Module.netplay.pausePromiseResolve) {
              Module.netplay.pausePromiseResolve(pauseCounts);
            }
            return 0;
          }, actualPauseCounts);
      }
    }
}

void netplay_update_input(struct pif* pif)
{
    if (netplay_is_init())
    {
        netplay_send_raw_input(pif);
        netplay_get_raw_input(pif);
    }
}

void netplay_set_plugin(uint8_t control_id, uint8_t plugin)
{
    if (!(control_id > 0 && plugin == 2)) //Only P1 can use mempak
        l_plugin[control_id] = plugin;
}

m64p_error netplay_send_config(char* data, int size)
{
    if (!netplay_is_init())
        return M64ERR_NOT_INIT;

    if (l_netplay_control[0] != -1 || size == 1) //Only P1 sends settings, we allow all players to send if the size is 1, this may be a request packet
    {
#if (!EMSCRIPTEN)
        int result = SDLNet_TCP_Send(l_tcpSocket, data, size);
# else
        // TODO?
        int result = size;
        sendReliableMessage(data, size);
# endif
        
        if (result < size)
            return M64ERR_SYSTEM_FAIL;
        return M64ERR_SUCCESS;
    }
    else
        return M64ERR_INVALID_STATE;
}


m64p_error netplay_receive_config(char* data, int size)
{
    if (!netplay_is_init())
        return M64ERR_NOT_INIT;

    if (l_netplay_control[0] == -1) //Only P2-4 receive settings
    {

#if (!EMSCRIPTEN)
        int recv = 0;
        while (recv < size)
        {

          recv += SDLNet_TCP_Recv(l_tcpSocket, &data[recv], size - recv);


            if (recv < 1)
                return M64ERR_SYSTEM_FAIL;
        }
#else
        waitForReliableMessage(data);
#endif
        return M64ERR_SUCCESS;
    }
    else
        return M64ERR_INVALID_STATE;
}
