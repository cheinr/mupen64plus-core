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

  initWasmRecompiler: function() {

    console.log("initWasmRecompiler");

    // TODO - Rename to "recompilingInstructions"
    Module.recompilingFunctionsByBlock = {};
    Module.recompiledFunctionsByBlock = {};

    const initialNumberOfFunctionTableSlots = 15000;
    Module.numberOfFunctionTableSlotsToGrowBy = 15000;
    
    const indirectFunctionTable = Module['asm']['__indirect_function_table'];
    const tableLengthBefore = indirectFunctionTable.length;
    indirectFunctionTable.grow(initialNumberOfFunctionTableSlots);

    Module.availableFunctionTableSlots = new Set();
    for (let i = 0; i < initialNumberOfFunctionTableSlots; i++) {
      Module.availableFunctionTableSlots.add(i + tableLengthBefore);
    }

    console.log("maxTableLength: %o", indirectFunctionTable.length);

    Module.moduleCount = 0;
    Module.blockToCompiledFunctionIndexes = {};
  },

  compileAndPatchModule: function(block,
                                  modulePointer,
                                  moduleLength,
                                  usedFunctionsPointerArray,
                                  numberOfFunctionsUsed,
                                  recompTargetFunctionPointers,
                                  numRecompTargets,
                                  instructionSize) {

    return Asyncify.handleAsync(function() {

      //console.log("compileAndPatchModule! moduleLength=%d", moduleLength);
      
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

      //console.log(window);
      //console.log(window.binaryen);
      //console.log("table: %o", table);
      
      //      console.log("Generating module: %o", Module.moduleCount++);
      //console.log('moduleBytes: %o', moduleBytes);
      //console.log('indirectFunctionTable[346]: %o', indirectFunctionTable.get(346));

      //const before = performance.now();


      return WebAssembly
        .instantiate(moduleBytes, imports)
        .then(function({ instance }) {
          //console.log("Instantiation finished!");
          
          //const exportedFunction = instance.exports.func;

          //console.log(Module.availableFunctionTableSlots);

          if (!Module.blockToCompiledFunctionIndexes[block]) {
            Module.blockToCompiledFunctionIndexes[block] = [];
          }

          for (let i = 0; i < numRecompTargets; i++) {

            const exportedFunction = instance.exports[`f${i}`];

            if (Module.availableFunctionTableSlots.size < 1) {

              const tableLengthBefore = indirectFunctionTable.length;
              const numberOfSlotsToAdd = Module.numberOfFunctionTableSlotsToGrowBy;
              indirectFunctionTable.grow(numberOfSlotsToAdd);

              for (let i = 0; i < numberOfSlotsToAdd; i++) {
                Module.availableFunctionTableSlots.add(i + tableLengthBefore);
              }
            }

            const functionIndex = Module.availableFunctionTableSlots.values().next().value;

            if (!functionIndex) {
              throw "Unexpectedly ran out of function indexes!";
            }

            Module.availableFunctionTableSlots.delete(functionIndex);

            if (indirectFunctionTable.get(functionIndex) !== null) {
              throw "Entry in the function table is already set!";
            }

            indirectFunctionTable.set(functionIndex, exportedFunction);

            Module.blockToCompiledFunctionIndexes[block].push(functionIndex);

            
            const instructionOpsPointer = recompTargetFunctionPointers + (i * 4);
            //console.log("Writing %d to instructionOpsPointer: %d", functionIndex, instructionOpsPointer);
            setValue(instructionOpsPointer, functionIndex, 'i32');
            //setValue(instructionOpsPointer, functionIndex, 'i32');
          }

//          const after = performance.now();

/*          console.log("Finished compiling wasm module of size %d with %d functions after %f millis",
                      moduleBytes.length,
                      numRecompTargets,
                      after - before);*/

/*          const now = performance.now();
          console.log('Finished recompiling block with %d functions in %f millis! Time spent instantiating wasm module=%f millis; Time spent assembling wasm=%f millis',
                      numRecompTargets,
                      now - Module._lastRecompileBlockStartTime,
                      now - before,
                      Module._recompTargetWasmGenerationEnd - Module._recompTargetWasmGenerationStart);*/
                      
                      
        }).catch((err) => {
          console.error('failed to instantiate module!: ', err);
        });
    });
  },

  wasmReleaseBlock: function(block) {
    if (Module.blockToCompiledFunctionIndexes[block]) {

      const indirectFunctionTable = Module['asm']['__indirect_function_table'];
      const compiledFunctionIndexes = Module.blockToCompiledFunctionIndexes[block];
      
      while (compiledFunctionIndexes.length > 0) {
        const functionIndex = compiledFunctionIndexes.pop();
        indirectFunctionTable.set(functionIndex, null);
        Module.availableFunctionTableSlots.add(functionIndex);
      }
    }
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
