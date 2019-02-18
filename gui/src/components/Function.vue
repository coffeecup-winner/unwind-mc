<template lang="jade">
    li(v-bind:class='classObj')
        a(v-on:click='onClick()') {{ func.callingConvention == 'Stdcall' ? 'S' : '?' }} {{ func.name }}
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
</style>

<script lang="ts">
module.exports = {
    props: ['func'],
    computed: {
        classObj() {
            return {
                'decompile-fail': this.func.status != 'BoundsResolved'
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
