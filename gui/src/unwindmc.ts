import * as ffi from 'ffi'
import * as ref from 'ref'

const _unwindmc = ffi.Library('libunwindmc', {
    version: ['string', []],
    init: ['bool', []],
    open_binary_file: ['int', ['string']],
    print_instructions: ['void', ['int', 'string', 'int']],
})

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
        let buffer = Buffer.alloc(4096)
        _unwindmc.print_instructions(handle, buffer, 4096)
        return JSON.parse(ref.readCString(buffer, 0))
    }
}
