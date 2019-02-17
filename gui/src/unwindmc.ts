import * as ffi from 'ffi'
import * as ref from 'ref'

const _unwindmc = ffi.Library('libunwindmc', {
    version: ['string', []],
    init: ['bool', ['pointer']],
    open_binary_file: ['uint32', ['string']],
    open_db: ['uint32', ['string']],
    save_db: ['void', ['uint32', 'string']],
    get_functions: ['void', ['uint32', 'string', 'size_t']],
    get_instructions: ['void', ['uint32', 'uint64', 'string', 'size_t']],
    decompile_il: ['bool', ['uint32', 'uint64', 'string', 'size_t']],
})

const _buffer = Buffer.alloc(16 * 1024 * 1024)
var _logCallback: Buffer;

export interface Function {
    readonly address: number,
    readonly status: string,
    readonly callingConvention: 'Unknown' | 'Stdcall',
    readonly argumentsSize: number,
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

    openBinaryFile(file: string): number {
        return _unwindmc.open_binary_file(file)
    },

    openDB(file: string): number {
        return _unwindmc.open_db(file)
    },

    saveDB(handle: number, file: string) {
        _unwindmc.save_db(handle, file)
    },

    getFunctions(handle: number): Function[] {
        _unwindmc.get_functions(handle, _buffer, _buffer.byteLength)
        return JSON.parse(ref.readCString(_buffer))
    },

    getInstructions(handle: number, func: number): Instruction[] {
        _unwindmc.get_instructions(handle, func, _buffer, _buffer.byteLength)
        return JSON.parse(ref.readCString(_buffer))
    },

    decompileIL(handle: number, func: number) {
        if (_unwindmc.decompile_il(handle, func, _buffer, _buffer.byteLength)) {
            return JSON.parse(ref.readCString(_buffer))
        } else {
            return null
        }
    },
}
