<template lang="jade">
    li(v-bind:class='classObj')
        a(v-on:click='onClick()') {{ Number(func.address).toString(16).padStart(8, '0') }}
</template>

<style lang="scss" scoped>
li {
    display: block;
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
