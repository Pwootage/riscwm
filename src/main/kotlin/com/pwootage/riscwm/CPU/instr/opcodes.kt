package com.pwootage.riscwm.CPU.instr

import com.pwootage.riscwm.CPU.CPU
import com.pwootage.riscwm.CPU.RiscVInstruction
import java.lang.IllegalStateException

object OPCODES {
    val OP_IMM = 0b0010011
    object OP_IMM_FUNCT3 {
        val ADDI = 0b000
        val SLTI = 0b010
        val SLTIU = 0b011
        val XORI = 0b100
        val ORI = 0b110
        val ANDI = 0b111
        val SLLI = 0b001
        val SRLI = 0b101
        val SRAI = 0b101
    }


}

inline fun RiscVInstruction.exec(cpu: CPU) {
    when (opcode) {
        OPCODES.OP_IMM -> op_imm(cpu)
        // TODO: throw interrupt instead
        else -> throw IllegalStateException("Invalid opcode")
    }
}

inline fun RiscVInstruction.op_imm(cpu: CPU) {
    require(funct3 in 0b000..0b111)
    val src = cpu.x[rs1]
    val imm = immed_i
    @Suppress("DUPLICATE_LABEL_IN_WHEN")
    val res = when (funct3) {
        OPCODES.OP_IMM_FUNCT3.ADDI -> src + imm
        OPCODES.OP_IMM_FUNCT3.SLTI -> if (src < imm) 1 else 0
        OPCODES.OP_IMM_FUNCT3.SLTIU -> if (src.toUInt() < imm.toUInt()) 1 else 0
        OPCODES.OP_IMM_FUNCT3.XORI -> src xor imm
        OPCODES.OP_IMM_FUNCT3.ORI -> src or imm
        OPCODES.OP_IMM_FUNCT3.ANDI -> src and imm
        OPCODES.OP_IMM_FUNCT3.SLLI -> src shl (imm and 0b11111)
        OPCODES.OP_IMM_FUNCT3.SRLI, OPCODES.OP_IMM_FUNCT3.SRAI -> if (bits(30) == 0) {
            src ushr (imm and 0b11111)
        } else {
            src shr  (imm and 0b11111)
        }
        else -> throw IllegalStateException("Impossible funct3")
    }
    cpu.setRd(rd, res)
}
