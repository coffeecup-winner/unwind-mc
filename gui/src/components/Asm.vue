<template lang="jade">
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
</style>

<script lang="ts">
module.exports = {
    props: ['instructions'],
}
</script>
