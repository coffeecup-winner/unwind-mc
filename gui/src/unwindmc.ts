import * as ffi from 'ffi'
import * as ref from 'ref'

const _unwindmc = ffi.Library('libunwindmc', {
    version: ['string', []],
    init: ['bool', ['pointer']],
    open_binary_file: ['bool', ['string']],
    open_db: ['bool', ['string']],
    save_db: ['void', ['string']],
    get_functions: ['void', ['string', 'size_t']],
    get_instructions: ['void', ['uint64', 'string', 'size_t']],
    get_il: ['bool', ['uint64', 'string', 'size_t']],
    get_flow_blocks: ['bool', ['uint64', 'string', 'size_t']],
    decompile_il: ['void', []],
    analyze_control_flow: ['void', []]
})

const _buffer = Buffer.alloc(16 * 1024 * 1024)
var _logCallback: Buffer;

export interface Function {
    readonly address: number,
    readonly status: string,
    readonly callingConvention: 'Unknown' | 'Stdcall',
    readonly argumentsSize: number,
    readonly name: string,
}

export interface Instruction {
    readonly address: number,
    readonly hex: string,
    readonly assembly: string,
}

export default {
    version(): string {
        return _unwindmc.version()
    },

    init(logCallback: (line: string) => void): boolean {
        _logCallback = ffi.Callback('void', ['string'], logCallback)
        return _unwindmc.init(_logCallback)
    },

    openBinaryFile(file: string): boolean {
        return _unwindmc.open_binary_file(file)
    },

    openDB(file: string): boolean {
        return _unwindmc.open_db(file)
    },

    saveDB(file: string) {
        _unwindmc.save_db(file)
    },

    getFunctions(): Function[] {
        _unwindmc.get_functions(_buffer, _buffer.byteLength)
        return JSON.parse(ref.readCString(_buffer))
    },

    getInstructions(func: number): Instruction[] {
        _unwindmc.get_instructions(func, _buffer, _buffer.byteLength)
        return JSON.parse(ref.readCString(_buffer))
    },

    getIL(func: number) {
        if (_unwindmc.get_il(func, _buffer, _buffer.byteLength)) {
            return JSON.parse(ref.readCString(_buffer))
        } else {
            return null
        }
    },

    getFlowBlocks(func: number) {
        if (_unwindmc.get_flow_blocks(func, _buffer, _buffer.byteLength)) {
            return JSON.parse(ref.readCString(_buffer))
        } else {
            return null
        }
    },

    decompileIL() {
        _unwindmc.decompile_il()
    },

    analyzeControlFlow() {
        _unwindmc.analyze_control_flow()
    },
}
