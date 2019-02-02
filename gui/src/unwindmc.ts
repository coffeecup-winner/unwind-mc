import * as ffi from 'ffi'
import * as ref from 'ref'

const _unwindmc = ffi.Library('libunwindmc', {
    version: ['string', []],
    init: ['bool', []],
    open_binary_file: ['int', ['string']],
    open_db: ['int', ['string']],
    save_db: ['void', ['int', 'string']],
    print_instructions: ['void', ['int', 'string', 'int']],
})

const _buffer = Buffer.alloc(16 * 1024 * 1024)

export interface Instruction {
    readonly address: number,
    readonly hex: string,
    readonly assembly: string,
}

export default {
    version(): string {
        return _unwindmc.version()
    },

    init(): boolean {
        return _unwindmc.init()
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

    getInstructions(handle: number): Instruction[] {
        _unwindmc.print_instructions(handle, _buffer, _buffer.byteLength)
        return JSON.parse(ref.readCString(_buffer))
    }
}
