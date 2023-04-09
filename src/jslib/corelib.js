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

  netplayInit: function() {
    if (!Module.netplayConfig.reliableChannel || !Module.netplayConfig.unreliableChannel) {

      console.log('Invalid netplay config. We require both "reliableChannel" and "unreliableChannel"');
      throw `Invalid netplay config: ${Module.netplayConfig}. We require both "reliableChannel" and "unreliableChannel"`;
    }

    const reliableChannel = Module.netplayConfig.reliableChannel;
    const unreliableChannel = Module.netplayConfig.unreliableChannel;

    Module.netplay = Object.assign({}, Module.netplay, {
      pendingReliableMessages: [],
      pendingUnreliableMessages: [],
      playerRegistered: false
    });

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
  },

  compileAndPatchModule: function(block,
                                  modulePointer,
                                  moduleLength,
                                  usedFunctionsPointerArray,
                                  numberOfFunctionsUsed) {

    const indirectFunctionTable = Module['asm']['__indirect_function_table'];
    const memory = Module['asm']['memory'];

    const table = new WebAssembly.Table({
      element: 'anyfunc',
      initial: numberOfFunctionsUsed
    });

    for (let i = 0; i < numberOfFunctionsUsed; i++) {
      const originalFunctionPointer = getValue(usedFunctionsPointerArray + i * 4, 'i32');
      //console.log('originalFunctionPointer: %o', originalFunctionPointer);
      table.set(i, indirectFunctionTable.get(originalFunctionPointer));
    }
    
    const env = {
      funcref: table,//indirectFunctionTable,
      mem: memory
    };
    const imports = {
      env
    };

    const moduleBytes = HEAPU8.slice(modulePointer, modulePointer + moduleLength);

    // TODO - Move to a proper "initialization" function
    if (!Module.availableFunctionTableSlots) {
      const tableLengthBefore = indirectFunctionTable.length;
      indirectFunctionTable.grow(500);

      Module.availableFunctionTableSlots = new Set();
      for (let i = 0; i < 500; i++) {
        Module.availableFunctionTableSlots.add(i + tableLengthBefore);
      }
    }
           //console.log(window);
    //console.log(window.binaryen);
    
    return Asyncify.handleAsync(function() {
      if (!Module.moduleCount) Module.moduleCount = 0;
      if (!Module.blockToCompiledFunctionIndexes) Module.blockToCompiledFunctionIndexes = {};
      
//      console.log("Generating module: %o", Module.moduleCount++);
//      console.log('moduleBytes: %o', moduleBytes);
      //console.log('indirectFunctionTable[346]: %o', indirectFunctionTable.get(346));
      
      return WebAssembly
        .instantiate(moduleBytes, imports)
        .then(function({ instance }) {
          
          const exportedFunction = instance.exports.func;

          const functionIndex = Module.availableFunctionTableSlots.values().next().value;
          Module.availableFunctionTableSlots.delete(functionIndex);
          
          //indirectFunctionTable.grow(1);
          //const functionIndex = indirectFunctionTable.length - 1;
//          console.log('setting functionIndex: %o', functionIndex);
          if (indirectFunctionTable.get(functionIndex) !== null) {
            throw "Entry in the function table is already set!";
          }
          indirectFunctionTable.set(functionIndex, exportedFunction);
//          console.log('set functionIndex: %o', functionIndex);

          if (!Module.blockToCompiledFunctionIndexes[block]) {
            Module.blockToCompiledFunctionIndexes[block] = [];
          }
          Module.blockToCompiledFunctionIndexes[block].push(functionIndex);
          return functionIndex;
        }).catch((err) => {
          console.error('failed to instantiate module!: ', err);
        });
    });
  },

  wasmFreeBlocks: function(startBlock, endBlock) {
    if (Module.blockToCompiledFunctionIndexes) {

  //    console.log("wasmFreeBLocks: %o, %o", startBlock, endBlock);
      const indirectFunctionTable = Module['asm']['__indirect_function_table'];
      
      for (let i = startBlock; i < endBlock; i++) {
        
        const functionIndexes = Module.blockToCompiledFunctionIndexes[i]
                              ? Module.blockToCompiledFunctionIndexes[i]
                              : [];
        functionIndexes.forEach(function(functionIndex) {
//          console.log("wasmFreeBlocks: %o; funcIndex=%o", i, functionIndex);
          indirectFunctionTable.set(functionIndex, null);
          Module.availableFunctionTableSlots.add(functionIndex);
        });

        Module.blockToCompiledFunctionIndexes[i] = [];
      }
    }
  },

  releaseWasmFunction: function(functionIndex) {
    const indirectFunctionTable = Module['asm']['__indirect_function_table'];
    //console.log("Releasing function: %o", functionIndex);
    indirectFunctionTable.set(functionIndex, null);
    Module.availableFunctionTableSlots.add(functionIndex);
  },

  notifyBlockAccess: function(address) {
    if (!Module.blockAccesses) {
      Module.blockAccesses = {};
    }

    if (!Module.blockAccesses[address]) {
      Module.blockAccesses[address] = 1;
      //console.log("number modules: %o", Object.keys(Module.blockAccesses).length);
    } else {
      Module.blockAccesses[address]++;
    }

    if (!Module.blockAccessPrintInterval) {
/*      Module.blockAccessPrintInterval = setInterval(() => {

        const accessCountCounts = {};
        let accessCountCountsSum = 0;
        Object.values(Module.blockAccesses).forEach((accessCount) => {
          if (!accessCountCounts[accessCount]) {
            accessCountCounts[accessCount] = 1;
          } else {
            accessCountCounts[accessCount]++;
          }
          accessCountCountsSum++;
        });

//        console.log('foo: %o', Object.keys(Module.blockAccesses).length);
//        console.log('accessCountCounts: %o; total=%d', accessCountCounts, accessCountCountsSum);
        Object.keys(accessCountCounts)
              .forEach((accessCount) => {
                console.log('accessCount %d: %d (%f)',
                            accessCount,
                            accessCountCounts[accessCount],
                            accessCountCounts[accessCount] / accessCountCountsSum);
              });
}, 500);
      */
    }
  }
});
