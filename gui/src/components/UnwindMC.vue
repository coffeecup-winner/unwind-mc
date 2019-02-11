<template lang="jade">
div
    button(v-on:click='openClicked') Open File
    button(v-on:click='openDBClicked') Open DB
    button(v-on:click='saveDBClicked') Save DB
    button(v-on:click='decompileILClicked') Decompile IL
    #layout_functions
        ul.functions
            Function.function(
                v-for='func in functions'
                v-bind:func='func'
                v-on:functionClick='functionClicked'
            )
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
            functions: [] as Function[],
            instructions: [] as Instruction[],
            selectedFunction: null as (Function | null),
        }
    },
    components: { Asm, Function },
    methods: {
        open(handle: number | null) {
            this.handle = handle
            if (handle == null) {
                this.functions = []
                this.instructions = []
                this.selectedFunction = null
                return
            }
            this.functions = unwindmc.getFunctions(this.handle)
            if (this.functions.length == 0) {
                this.selectedFunction = null
                this.instructions = unwindmc.getInstructions(this.handle, 0)
                return
            }
            this.selectedFunction = this.functions[0]
            this.instructions = unwindmc.getInstructions(this.handle, this.selectedFunction.address)
        },

        openClicked() {
            e.dialog.showOpenDialog({
                title: 'Open a binary file',
            }, f => {
                this.open(f.length > 0 ? unwindmc.openBinaryFile(f[0]) : null)
            })
        },

        openDBClicked() {
            e.dialog.showOpenDialog({
                title: 'Open a DB',
                filters: [{
                    name: 'UnwindMC DB',
                    extensions: ['db']
                }],
            }, f => {
                this.open(f.length > 0 ? unwindmc.openDB(f[0]) : null)
            })
        },

        saveDBClicked() {
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

        decompileILClicked() {
            if (this.selectedFunction == null) {
                return
            }
            let il = unwindmc.decompileIL(this.handle, this.selectedFunction.address)
            console.log(il)
        },

        functionClicked(func: Function) {
            this.selectedFunction = func
            this.instructions = unwindmc.getInstructions(this.handle, func.address)
        },
    },
}
</script>
