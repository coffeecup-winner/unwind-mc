<template lang="jade">
    div
        button(v-on:click='openClicked') Open File
        button(v-on:click='openDBClicked') Open DB
        button(v-on:click='saveDBClicked') Save DB
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
import unwindmc from '../unwindmc'
import { remote as e } from 'electron'

export default {
    created: function () {
        unwindmc.init(line => console.log(line))
    },
    data: {
        handle: null as (number | null),
    },
    computed: {
        instructions: function () {
            if (this.handle == null) {
                return []
            }
            return unwindmc.getInstructions(this.handle)
        },
    },
    methods: {
        openClicked: function () {
            e.dialog.showOpenDialog({
                title: 'Open a binary file',
            }, f => {
                if (f.length > 0) {
                    this.handle = unwindmc.openBinaryFile(f[0])
                } else {
                    this.handle = null
                }
            })
        },

        openDBClicked: function () {
            e.dialog.showOpenDialog({
                title: 'Open a DB',
                filters: [{
                    name: 'UnwindMC DB',
                    extensions: ['db']
                }],
            }, f => {
                if (f.length > 0) {
                    this.handle = unwindmc.openDB(f[0])
                } else {
                    this.handle = null
                }
            })
        },

        saveDBClicked: function () {
            e.dialog.showSaveDialog({
                title: 'Save the DB',
                filters: [{
                    name: 'UnwindMC DB',
                    extensions: ['db']
                }],
            }, f => {
                if (f) {
                    unwindmc.saveDB(this.handle, f)
                }
            })
        },
    },
}
</script>
