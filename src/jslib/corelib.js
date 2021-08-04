mergeInto(LibraryManager.library, {

  beginStats: function() {
    if (Module.beginStats) {
      Module.beginStats();
    }
  },

  endStats: function() {
    if (Module.endStats) {
      Module.endStats();
    }
  },

  // See https://emscripten.org/docs/porting/connecting_cpp_and_javascript/Interacting-with-code.html#javascript-limits-in-library-files
  requestFileSync__postset: '_requestFileSync();',
  requestFileSync: function() {

    let fileSyncTimeout = null;

    _requestFileSync = function() {
      if (fileSyncTimeout) {
        return;
      }
      
      fileSyncTimeout = setTimeout(() => {       
        fileSyncTimeout = null;
        FS.syncfs(false, function(err) {
          if (err) {
            console.error("Error while syncing system data to IDBFS");
          }
        });
      }, 500);      
    }
  },
  
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

    let numberOfMessages = 0;
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
  },

  waitForUnreliableMessages: function(responseBufferPointer,
                                      maxNumberOfMessages,
                                      numberOfMessagesPresentPointer) {

    // If no messages are already present then we're forced to yield to the event loop
    // to get more. Ideally we avoid this as yielding inside the main loop tends to cause 
    // graphical glitches.
    //
    // Excuse the code duplication. I'm stupid and can't figure out how to create a helper function
    // that isn't local to this function.
    Asyncify.handleSleep((wakeUp) => {

      setTimeout(() => {

        let numberOfMessages = 0;
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

    try {
      Module.netplayConfig.unreliableChannel.send(messageBuffer.buffer);
    } catch (err) {
      console.error(err);
    }
  },

  sendReliableMessage: function(messageDataPointer, messageLength) {

    console.log("Sending reliable message: %o", HEAPU8[(messageDataPointer)]);
    
    const messageBuffer = new Uint8Array(messageLength);

    for (let i = 0; i < messageLength; i++) {
      messageBuffer[i] = HEAPU8[(messageDataPointer + i)];
    }

    console.log("Sending reliable message: %o", messageBuffer);

    try {
      Module.netplayConfig.reliableChannel.send(messageBuffer.buffer);
    } catch (err) {
      console.error(err);
    }
  }
});
