mergeInto(LibraryManager.library, {

  waitForReliableMessage: function(responseBufferPointer) {

    console.log("waitForReliableMessage");
    
    Asyncify.handleSleep((wakeUp) => {
      Module.netplayConfig.reliableChannel.onmessage = (event) => {
        
        console.log("received message: %o", event.data);

        const response = new Uint8Array(event.data);

        for (let i = 0; i < response.length; i++) {
          HEAPU8[(responseBufferPointer + i)] = response[i];
        }
        
        wakeUp();
      };
    });
    
  },

  checkForUnreliableMessages: function(responseBufferPointer,
                                       maxNumberOfMessages,
                                       numberOfMessagesPresentPointer) {
    
    Asyncify.handleSleep((wakeUp) => {

      setTimeout(() => {

        let numberOfMessages = 0
        for (let i = 0; i < maxNumberOfMessages; i++) {
          if (Module.netplay.pendingUnreliableMessages[0]) {

            const messageData = Module.netplay.pendingUnreliableMessages[0];
            Module.netplay.pendingUnreliableMessages.splice(0, 1);

            const data = new Uint8Array(messageData);

            const offset = i * 512;
            
            for (let j = 0; j < data.length; j++) {
              HEAPU8[(responseBufferPointer + j + offset)] = data[j];
            }
            
            numberOfMessages++;
            
          } else {
            break;
          }
        }
        
        Module.setValue(numberOfMessagesPresentPointer, numberOfMessages, 'i32');
        
        wakeUp();
      }, 0);
    });
  },
  
  netplayInit: function() {
    if (!Module.netplayConfig.reliableChannel || !Module.netplayConfig.unreliableChannel) {

      console.log('Invalid netplay config. We require both "reliableChannel" and "unreliableChannel"');
      throw `Invalid netplay config: ${Module.netplayConfig}. We require both "reliableChannel" and "unreliableChannel"`;
    }

    const reliableChannel = Module.netplayConfig.reliableChannel;
    const unreliableChannel = Module.netplayConfig.unreliableChannel;

    Module.netplay = {
      pendingReliableMessages: [],
      pendingUnreliableMessages: [],
      playerRegistered: false
    };

    reliableChannel.onmessage = (event) => {
      console.log("Received reliable message: %o", event.data);
      
      Module.netplay.pendingReliableMessages.push(event.data);
    }
    
    unreliableChannel.onmessage = (event) => {
      Module.netplay.pendingUnreliableMessages.push(event.data);
    };

    console.log("Netplay initialized!: %o", Module);
  },
  
  sendUnreliableMessage: function(messageDataPointer, messageLength) {
    const messageBuffer = new Uint8Array(messageLength);

    for (let i = 0; i < messageLength; i++) {
      messageBuffer[i] = HEAPU8[(messageDataPointer + i)];
    }

    Module.netplayConfig.unreliableChannel.send(messageBuffer.buffer);
  },

  sendReliableMessage: function(messageDataPointer, messageLength) {

    console.log("Sending reliable message: %o", HEAPU8[(messageDataPointer)]);
    
    const messageBuffer = new Uint8Array(messageLength);

    for (let i = 0; i < messageLength; i++) {
      messageBuffer[i] = HEAPU8[(messageDataPointer + i)];
    }

    console.log("Sending reliable message: %o", messageBuffer);

    Module.netplayConfig.reliableChannel.send(messageBuffer.buffer);
  },
  
  demandInput: function(controlId, inputIndex) {

    const checkValid = Module.cwrap('check_valid', 'number', ['number']);
    const netplayRequestInput = Module.cwrap('netplay_request_input', 'void', ['number']);
    
    const timeoutMillis = Date.now() + 10000;
    
    const spamInputRequests = () => {
      if (!checkValid(controlId, inputIndex)) {
        
        if (Date.now() > timeoutMillis) {
          // TODO - Something to signal that we've timed out
          // Similar to 'l_udpChannel = -1' from above
        }
        
        netplayRequestInput(controlId);
        
        setTimeout(spamInputRequests, 5);
      }
    }

    spamInputRequests();
  }
});
