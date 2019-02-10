import * as ffi from 'ffi'
import * as ref from 'ref'

const _unwindmc = ffi.Library('libunwindmc', {
    version: ['string', []],
    init: ['bool', ['pointer']],
    open_binary_file: ['int', ['string']],
    open_db: ['int', ['string']],
    save_db: ['void', ['int', 'string']],
    get_functions: ['void', ['int', 'string', 'int']],
    print_instructions: ['void', ['int', 'string', 'int']],
})

const _buffer = Buffer.alloc(16 * 1024 * 1024)
var _logCallback: Buffer;

export interface Function {
    readonly address: number,
    readonly status: string,
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

    getInstructions(handle: number): Instruction[] {
        _unwindmc.print_instructions(handle, _buffer, _buffer.byteLength)
        return JSON.parse(ref.readCString(_buffer))
    },
}
