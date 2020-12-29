mergeInto(LibraryManager.library, {
  coreStartup: function() {
    const runBeforeNetplaySyncSettings = () => {
      return new Promise((resolve, reject) => {
        const doRunBeforeNetplaySyncSettings = Module.cwrap('_main_run_BeforeNetplaySyncSettings', 'number', ['number']);
        doRunBeforeNetplaySyncSettings();
        resolve();
      });
    };
    
    const runNetplaySyncSettings = () => {
      return new Promise((resolve, reject) => {
        const doRunNetplaySyncSettings = Module.cwrap('main_run_NetplaySyncSettings');
        doRunNetplaySyncSettings();
        
        resolve();
      });
    };

    const runAfterNetplaySyncSettings = () => {
      return new Promise((resolve, reject) => {
        const doRunAfterNetplaySyncSettings = Module.cwrap('main_run_AfterNetplaySyncSettings');
        doRunNetplaySyncSettings();
        resolve();
      });
    };

    const runBeforeOpenMPK = () => {
      return new Promise((resolve, reject) => {
        const doRunBeforeOpenMPK = Module.cwrap('main_run_BeforeOpenMPK');
        doRunBeforeOpenMPK();
        resolve();
      });
    };
    
    const runBeforeOpenEEP = () => {
      return new Promise((resolve, reject) => {
        const doRunBeforeOpenEEP = Module.cwrap('main_run_BeforeOpenEEP');
        doRunBeforeOpenEEP();
        resolve();
      });
    };

    const runBeforeOpenFLA = () => {
      return new Promise((resolve, reject) => {
        const doRunBeforeOpenFLA = Module.cwrap('main_run_BeforeOpenFLA');
        doRunBeforeOpenFLA();
        resolve();
      });
    };

    const runBeforeOpenSRA = () => {
      return new Promise((resolve, reject) => {
        const doRunBeforeOpenSRA = Module.cwrap('main_run_BeforeOpenSRA');
        doRunBeforeOpenSRA();
        resolve();
      });
    };

    const runAfterOpenSRA = () => {
      return new Promise((resolve, reject) => {
        const doRunAfterOpenSRA = Module.cwrap('main_run_AfterOpenSRA');
        doRunAfterOpenSRA();
        resolve();
      });
    };

    runBeforeNetplaySyncSettings()
      .then(runNetplaySyncSettings)
      .then(runAfterNetplaySyncSettings)
      .then(runBeforeOpenMPK)
      .then(runBeforeOpenEEP)
      .then(runBeforeOpenFLA)
      .then(runBeforeOpenSRA)
      .then(runAfterOpenSRA)
      .catch(function(error) {
        console.error("Error during core startup chain: %o", error);
      });

  },
  
  netplayInit: function() {
    if (!Module.netplayConfig.reliableChannel || !Module.netplayConfig.unreliableChannel) {

      console.log('Invalid netplay config. We require both "reliableChannel" and "unreliableChannel"');
      throw `Invalid netplay config: ${Module.netplayConfig}. We require both "reliableChannel" and "unreliableChannel"`;
    }

    const reliableChannel = Module.netplayConfig.reliableChannel;
    const unreliableChannel = Module.netplayConfig.unreliableChannel;

    console.log("foo");
    Module.netplay = {
      pendingReliableMessages: [],
      playerRegistered: false
    };

    console.log("bar");
    reliableChannel.onmessage = (event) => {
      console.log("Received reliable message: %o", event.data);
      
      Module.netplay.pendingReliableMessages.push(event.data);
    }
    console.log("baz");

    unreliableChannel.onmessage = (event) => {
      console.log("Received unreliable message: %o", event.data);
      // TODO
    }
    console.log("buz");

    console.log("Netplay initialized!: %o", Module);
  },

  sendUnreliableMessage: function(messageDataPointer, messageLength) {
    const messageBuffer = new Uint8Array(messageLength);

    for (let i = 0; i < messageLength; i++) {
      messageBuffer[i] = HEAP32[(messageDataPointer + i)>>2]; // TODO - Not sure why we bitshift
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

  waitForPlayerRegistrationResponse: function(bufferTargetPointer) {
    Module.netplayConfig.reliableChannel.onmessage = (event) => {
      console.log("Received reliable message: %o", event.data);

      const data = new Uint8Array(event.data);

      HEAPU8[bufferTargetPointer] = data[1];

      Module.netplay.playerRegistered = true;
    }
  },

  pollForReliableMessage: function(responseBufferPointer) {

    const maybeMessage = Module.netplay.pendingReliableMessages.pop();
    console.log("PollForreliablemessage: %o", maybeMessage); 
    if (!maybeMessage) {
      console.log("no message available: %o", Module.netplay.pendingReliableMessages.length);
      return 0;
    }

    const message = maybeMessage;

    console.log("Received message: %o", message);
    Module.netplay.pendingReliableMessages.splice(0, 1);

    const data = new Uint8Array(message);

    //writeArrayToMemory(data, responseBufferPointer);
    
    return 1;
  },

  demandInput: function(controlId, inputIndex) {

    const checkValid = Module.cwrap('check_valid', 'number', ['number']);
    const netplayRequestInput = Module.cwrap('netplay_request_input', 'void', ['number']);
    
    const timeoutMillis = Date.now() + 10000;
    
    const spamInputRequests = () => {
      if (!checkValid(controlId, netplayCount)) {
        
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
