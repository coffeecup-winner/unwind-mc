<template lang="jade">
div
    button(v-on:click='openClicked') Open File
    button(v-on:click='openDBClicked') Open DB
    button(v-on:click='saveDBClicked') Save DB
    #layout_functions
        ul.functions
            Function.function(v-for='func in functions' v-bind:func='func')
    #layout_main
        Asm(v-bind:instructions='instructions')
</template>

<style lang="scss" scoped>
#layout_functions {
    position: fixed;
    overflow-y: scroll;
    top: 40px;
    left: 0;
    width: 200px;
    height: calc(100vh - 40px);
}

#layout_main {
    position: fixed;
    overflow-y: scroll;
    top: 40px;
    left: 200px;
    right: 0;
    height: calc(100vh - 40px);
}

.functions {
    margin: 0;
    padding: 0 0 0 10px;
}
</style>

<script lang="ts">
import * as Asm from './Asm.vue'
import * as Function from './Function.vue'

import unwindmc,{ Instruction } from '../unwindmc'
import { remote as e } from 'electron'

module.exports = {
    created: function () {
        unwindmc.init(line => console.log(line))
    },
    data() {
        return {
            handle: null as (number | null),
        }
    },
    components: { Asm, Function },
    computed: {
        functions(): Function[] {
            if (this.handle == null) {
                return []
            }
            return unwindmc.getFunctions(this.handle)
        },
        instructions(): Instruction[] {
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
