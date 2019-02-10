<template lang="jade">
    li
        a(v-on:click='onClick()') {{ Number(func.address).toString(16).padStart(8, '0') }}
</template>

<style lang="scss" scoped>
li {
    display: block;
}
</style>

<script lang="ts">
module.exports = {
    props: ['func'],
    methods: {
        onClick() {
            this.$emit('functionClick', this.func)
        }
    }
}
</script>
