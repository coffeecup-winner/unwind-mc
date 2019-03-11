<template lang="jade">
    #block
        div(v-if='isCondition')
            .indent {{ 'if' }}
                .indent(v-for='insn in block.condition') {{ insn }}
            .indent(v-if='block.trueBranch && block.trueBranch.length') {{ 'then' }}
                FlowBlock(
                    v-for='b in block.trueBranch'
                    v-bind:block='b'
                )
            .indent(v-if='block.falseBranch && block.falseBranch.length') {{ 'else' }}
                FlowBlock(
                    v-for='b in block.falseBranch'
                    v-bind:block='b'
                )
        div(v-if='isDoWhile')
            .indent {{ 'do' }}
            FlowBlock.indent(
                v-for='b in block.body'
                v-bind:block='b'
            )
            .indent {{ 'while' }}
                .indent(v-for='insn in block.condition') {{ insn }}
        div(v-if='isFor')
            .indent {{ 'for-condition' }}
                .indent(v-for='insn in block.condition') {{ insn }}
            .indent {{ 'for-modifier' }}
                .indent(v-for='insn in block.modifier') {{ insn }}
            .indent {{ 'for-body' }}
                FlowBlock(
                    v-for='b in block.body'
                    v-bind:block='b'
                )
        div(v-if='isSequence')
            .indent(v-for='insn in block.instructions') {{ insn }}
        div(v-if='isWhile')
            .indent {{ 'while' }}
                .indent(v-for='insn in block.condition') {{ insn }}
            FlowBlock.indent(
                v-for='b in block.body'
                v-bind:block='b'
            )
</template>

<style lang="scss" scoped>
#block {
    font-family: 'Courier New', Courier, monospace;
}

.indent {
    margin-left: 2em;
}
</style>

<script lang="ts">
module.exports = {
    name: 'FlowBlock',
    props: {
        block: Object,
    },
    computed: {
        isCondition(): boolean { return this.block.tag == 'CONDITION' },
        isDoWhile(): boolean { return this.block.tag == 'DO_WHILE' },
        isFor(): boolean { return this.block.tag == 'FOR' },
        isSequence(): boolean { return this.block.tag == 'SEQUENCE' },
        isWhile(): boolean { return this.block.tag == 'WHILE' },
    }
}
</script>
