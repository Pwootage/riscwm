package com.pwootage.riscwm.CPU

import com.pwootage.riscwm.CPU.instr.exec
import com.pwootage.riscwm.memory.MMU

class CPU(val mmu: MMU) {
    val x: Array<Int> = Array(32) { 0 }
    val f: Array<Float> = Array(32) { 0f }
    var pc: UInt = 0x8000_0000u


    fun cycle() {
        val instr = RiscVInstruction(mmu.read32(pc))
        instr.exec(this)
    }

    inline fun setRd(rd: Int, res: Int) {
        if (rd == 0) return
        x[rd] = res
    }
}
