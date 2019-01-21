<template lang="jade">
    div
        p.todo TODO: UnwindMC!
</template>

<script lang="ts">
import { unwindmc } from '../unwindmc'
export default {
    data: {
        
    },
    created: () => {
        unwindmc.init()
    }
}
</script>

<style lang="scss" scoped>
    .todo {
        padding-top: 75px;
    }
</style>
