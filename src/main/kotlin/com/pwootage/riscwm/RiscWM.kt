package com.pwootage.riscwm

import com.pwootage.riscwm.CPU.CPU
import com.pwootage.riscwm.memory.MMU

class RiscWM {
    val mmu = MMU()
    val cpu = CPU(mmu)

    fun interpret(cycles: Int) {
        repeat(cycles) {
            cpu.cycle()
        }
    }
}
