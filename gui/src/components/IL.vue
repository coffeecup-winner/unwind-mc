<template lang="jade">
table
    tr(v-for='insn in il')
        td {{ Number(insn.address).toString(16).padStart(4, '0') }}
        td {{ insn.text }}
</template>

<style lang="scss" scoped>
table {
    font-family: 'Courier New', Courier, monospace;
}
</style>

<script lang="ts">
module.exports = {
    props: [
        'il',
    ],
}
</script>
