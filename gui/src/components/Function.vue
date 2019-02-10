<template lang="jade">
    li
        span {{ Number(func.address).toString(16).padStart(8, '0') }}
</template>

<style lang="scss" scoped>
li {
    display: block;
}
</style>

<script lang="ts">
module.exports = {
    props: ['func'],
}
</script>
