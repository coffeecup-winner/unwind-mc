<template lang="jade">
    div
        button(v-on:click='openClicked') Open File
        table.main
            tr(v-for='insn in instructions')
                td {{ Number(insn.address).toString(16).padStart(8, '0') }}
                td {{ insn.hex }}
                td {{ insn.assembly }}
</template>

<style lang="scss" scoped>
    .main {
        font-family: 'Courier New', Courier, monospace
    }
</style>

<script lang="ts">
import { unwindmc } from '../unwindmc'
import * as ref from 'ref'
import { remote as e } from 'electron'

export default {
    created: function () {
        unwindmc.init()
    },
    data: {
        handle: null,
    },
    computed: {
        instructions: function () {
            if (this.handle == null) {
                return []
            }
            let buffer = Buffer.alloc(4096)
            unwindmc.print_instructions(this.handle, buffer, 4096)
            return JSON.parse(ref.readCString(buffer, 0))
        },
    },
    methods: {
        openClicked: function () {
            e.dialog.showOpenDialog({
                title: 'Open a binary file',
            }, f => {
                if (f.length > 0) {
                    this.handle = unwindmc.open_binary_file(f[0])
                } else {
                    this.handle = null
                }
            })
        },
    },
}
</script>
