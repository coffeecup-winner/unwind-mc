import * as ffi from 'ffi'
import * as ref from 'ref'

const _unwindmc = ffi.Library('libunwindmc', {
    version: ['string', []],
    init: ['bool', []],
    open_binary_file: ['int', ['string']],
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

    getInstructions(handle: number): Instruction[] {
        _unwindmc.print_instructions(handle, _buffer, _buffer.byteLength)
        return JSON.parse(ref.readCString(_buffer))
    }
}
