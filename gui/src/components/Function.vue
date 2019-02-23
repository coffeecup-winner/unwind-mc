<template lang="jade">
    li(v-bind:class='classObj')
        a(v-on:click='onClick()')
            {{ status }} {{ callconv }} {{ func.name }}
</template>

<style lang="scss" scoped>
li {
    display: block;
    font-family: 'Courier New', Courier, monospace;
    font-size: 9pt;
}

.decompile-fail {
    background-color: darkred;
}
.ildecompile-fail {
    background-color: orange;
}
</style>

<script lang="ts">
module.exports = {
    props: ['func'],
    computed: {
        classObj() {
            return {
                'decompile-fail':
                    this.func.status == 'BoundsNotResolvedInvalidAddress' ||
                    this.func.status == 'BoundsNotResolvedIncompleteGraph'
                'ildecompile-fail':
                    this.func.status == 'ILNotDecompiled'
            }
        },

        status() {
            switch this.func.status {
                case 'Created': return 'C'
                case 'BoundsResolved': return 'B'
                case 'BoundsNotResolvedInvalidAddress': return 'b'
                case 'BoundsNotResolvedIncompleteGraph': return 'b'
                case 'ILDecompiled': return 'I'
                case 'ILNotDecompiled': return 'i'
                case 'ControlFlowAnalyzed': return 'F'
                case 'ControlFlowNotAnalyzed': return 'f'
                case 'TypesResolved': return 'T'
                case 'TypesNotResolved': return 't'
                case 'AstBuilt': return 'A'
                case 'AstNotBuilt': return 'a'
                case 'SourceCodeEmitted': return 'S'
                case 'SourceCodeNotEmitted': return 's'
                default: return '!'
            }
        },

        callconv() {
            switch this.func.callingConvention {
                case 'Unknown': return '?'
                case 'Stdcall': return 'S'
                default: return '!'
            }
        },
    },
    methods: {
        onClick() {
            this.$emit('functionClick', this.func)
        }
    },
}
</script>
