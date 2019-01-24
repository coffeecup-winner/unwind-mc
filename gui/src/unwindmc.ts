import * as ffi from 'ffi'

export const unwindmc = ffi.Library('libunwindmc', {
    version: ['string', []],
    init: ['bool', []],
    open_binary_file: ['int', ['string']],
    print_instructions: ['void', ['int', 'string', 'int']],
})
