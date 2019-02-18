<template lang="jade">
div
    table(v-bind:class='xrefClassObj')
        tr
            th Callers
            th Callees
        tr(v-for='row in xrefRows')
            td
                a.xref(v-on:click='onClick(row.caller)') {{ row.caller && row.caller.name }}
            td
                a.xref(v-on:click='onClick(row.callee)') {{ row.callee && row.callee.name }}
    table
        tr(v-for='insn in instructions')
            td {{ Number(insn.address).toString(16).padStart(8, '0') }}
            td {{ insn.hex }}
            td {{ insn.assembly }}
</template>

<style lang="scss" scoped>
table {
    font-family: 'Courier New', Courier, monospace;
}

.xref {
    color: dodgerblue;
    text-decoration: underline;
    cursor: pointer;
}

.hidden {
    visibility: hidden;
}
</style>

<script lang="ts">
module.exports = {
    props: [
        'func',
        'functions',
        'instructions',
    ],
    computed: {
        xrefClassObj() {
            return {
                'hidden': this.func == null,
            }
        },

        xrefRows() {
            if (this.func == null) {
                return []
            }
            let maxCount = Math.max(this.func.callers.length, this.func.callees.length)
            let result = []
            for (let i = 0; i < maxCount; i++) {
                result.push({
                    caller: i < this.func.callers.length
                        ? this.functions.find(f => f.address == this.func.callers[i])
                        : null,
                    callee: i < this.func.callees.length
                        ? this.functions.find(f => f.address == this.func.callees[i])
                        : null,
                })
            }
            return result
        },
    },
    methods: {
        onClick(xref) {
            this.$emit('xrefClick', xref)
        }
    },
}
</script>
