<template lang="jade">
div
    button(v-on:click='openClicked') Open File
    button(v-on:click='openDBClicked') Open DB
    button(v-on:click='saveDBClicked') Save DB
    button(v-on:click='decompileILClicked') Decompile IL
    button(v-on:click='analyzeControlFlowClicked') Analyze Control Flow
    #layout_functions
        ul.functions
            Function(
                v-for='func in functions'
                v-bind:func='func'
                v-on:functionClick='functionClicked'
            )
    #layout_main
        Asm(
            v-bind:func='selectedFunction'
            v-bind:functions='functions'
            v-bind:instructions='instructions'
            v-on:xrefClick='functionClicked'
        )
    #layout_extra
        button(v-on:click='ILViewClicked') IL
        button(v-on:click='flowViewClicked') Flow
        IL(
            v-bind:class='ILClassObj'
            v-bind:il='il'
        )
        div(v-bind:class='flowClassObj')
            {{ flow }}
</template>

<style lang="scss" scoped>
// TODO: use variables

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
    height: calc(100vh - 40px);
    width: calc((100vw - 200px) / 2);
}

#layout_extra {
    position: fixed;
    overflow-y: scroll;
    top: 40px;
    left: calc(200px + (100vw - 200px) / 2);
    height: calc(100vh - 40px);
    width: calc((100vw - 200px) / 2);
}

.functions {
    margin: 0;
    padding: 0 0 0 10px;
}

.hide {
    visibility: collapse;
}
</style>

<script lang="ts">
import * as Asm from './Asm.vue'
import * as Function from './Function.vue'
import * as IL from './IL.vue'

import unwindmc,{ Instruction } from '../unwindmc'
import { remote as e } from 'electron'

enum ExtraView {
    IL,
    Flow,
}

module.exports = {
    created: function () {
        unwindmc.init(line => console.log(line))
    },
    data() {
        return {
            functions: [] as Function[],
            instructions: [] as Instruction[],
            il: [],
            flow: [],
            selectedFunction: null as (Function | null),
            extraView: ExtraView.IL,
        }
    },
    computed: {
        ILClassObj() {
            return {
                'hide': this.extraView != ExtraView.IL
            }
        },

        flowClassObj() {
            return {
                'hide': this.extraView != ExtraView.Flow
            }
        },
    }
    components: { Asm, Function, IL },
    methods: {
        refresh(isValid: boolean) {
            if (!isValid) {
                this.functions = []
                this.instructions = []
                this.selectedFunction = null
                return
            }
            this.functions = unwindmc.getFunctions()
            if (this.functions.length == 0) {
                this.selectedFunction = null
                this.instructions = unwindmc.getInstructions(0)
                return
            }
            this.selectedFunction = this.functions[0]
            this.instructions = unwindmc.getInstructions(this.selectedFunction.address)
        },

        openClicked() {
            e.dialog.showOpenDialog({
                title: 'Open a binary file',
            }, f => {
                this.refresh(f.length > 0 && unwindmc.openBinaryFile(f[0]))
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
                this.refresh(f.length > 0 && unwindmc.openDB(f[0]))
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
                    unwindmc.saveDB(f)
                }
            })
        },

        decompileILClicked() {
            unwindmc.decompileIL()
            this.refresh(true)
            for (let f of this.functions) {
                console.log(f.address)
                console.log(f.status)
                if (f.status == 'BoundsResolved') {
                    let il = unwindmc.getIL(f.address)
                    console.log(il)
                }
            }
        },

        analyzeControlFlowClicked() {
            unwindmc.analyzeControlFlow()
            this.refresh(true)
            for (let f of this.functions) {
                console.log(f.address)
                console.log(f.status)
                if (f.status == 'ControlFlowAnalyzed') {
                    let il = unwindmc.getFlowBlocks(f.address)
                    console.log(il)
                }
            }
        },

        functionClicked(func: Function) {
            this.selectedFunction = func
            this.instructions = unwindmc.getInstructions(func.address)
            this.il = unwindmc.getIL(func.address)
            this.flow = unwindmc.getFlowBlocks(func.address)
        },

        ILViewClicked() {
            this.extraView = ExtraView.IL
        },

        flowViewClicked() {
            this.extraView = ExtraView.Flow
        }
    },
}
</script>
