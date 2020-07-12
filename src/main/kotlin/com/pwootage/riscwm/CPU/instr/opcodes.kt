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

    val LUI = 0b0110111
    val AUIPC = 0b0010111
    val OP = 0b0110011

    object OP_FUNCT7 {
        val BASE = 0b0000000
        val MUL = 0b0000001
    }

    object OP_FUNCT3 {
        val ADD = 0b000
        val SUB = 0b000
        val SLL = 0b001
        val SLT = 0b010
        val SLTU = 0b011
        val XOR = 0b100
        val SRL = 0b101
        val SRA = 0b101
        val OR = 0b110
        val AND = 0b111
    }

    object OP_MUL_FUNCT3 {
        val MUL = 0b000
        val MULH = 0b001
        val MULHSU = 0b010
        val MULHU = 0b011
        val DIV = 0b100
        val DIVU = 0b101
        val REM = 0b110
        val REMU = 0b111
    }

    val JAL = 0b1101111
    val JALR = 0b1100111
    val B = 0b1100011

    object B_FUNCT3 {
        val BEQ = 0b000
        val BNE = 0b001
        val BLT = 0b100
        val BGE = 0b101
        val BLTU = 0b110
        val BGEU = 0b111
    }

    val LOAD = 0b0000011

    object LOAD_FUNCT3 {
        val LB = 0b000
        val LH = 0b001
        val LW = 0b010
        val LBU = 0b100
        val LHU = 0b101
    }

    val STORE = 0b0100011

    object STORE_FUNCT3 {
        val SB = 0b000
        val SH = 0b001
        val SW = 0b010
    }

    val MISC_MEM = 0b0001111

    object MISC_MEM_FUNCT3 {
        val FENCE = 0b000
        val FENCE_I = 0b001
    }

    val SYSTEM = 0b1110011

    object SYSTEM_FUNCT12 {
        val ECALL = 0b000000000000
        val EBREAK = 0b000000000001
    }
}

inline fun RiscVInstruction.exec(cpu: CPU) {
    when (opcode) {
        OPCODES.OP_IMM -> op_imm(cpu)
        OPCODES.LUI -> lui(cpu)
        OPCODES.AUIPC -> auipc(cpu)
        OPCODES.OP -> op(cpu)
        OPCODES.JAL -> jal(cpu)
        OPCODES.JALR -> jalr(cpu)
        OPCODES.B -> b(cpu)
        OPCODES.LOAD -> load(cpu)
        OPCODES.STORE -> store(cpu)
        OPCODES.MISC_MEM -> misc_mem(cpu)
        OPCODES.SYSTEM -> system(cpu)
        // TODO: throw interrupt instead
        else -> throw IllegalStateException("Invalid opcode")
    }
}

inline fun RiscVInstruction.op_imm(cpu: CPU) {
    if (rd == 0) return // nop
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
            src shr (imm and 0b11111)
        }
        else -> throw IllegalStateException("Impossible funct3")
    }
    cpu.setRd(rd, res)
}

inline fun RiscVInstruction.lui(cpu: CPU) {
    cpu.setRd(rd, immed_u)
}

inline fun RiscVInstruction.auipc(cpu: CPU) {
    // PC is still equal to this instruction
    cpu.setRd(rd, immed_u + cpu.pc.toInt())
}

inline fun RiscVInstruction.op(cpu: CPU) {
    if (rd == 0) return // nop
    val src1 = cpu.x[rs1]
    val src2 = cpu.x[rs2]
    val res: Int = if (funct7 == OPCODES.OP_FUNCT7.MUL) {
        when (funct3) {
            OPCODES.OP_MUL_FUNCT3.MUL -> src1 * src2
            OPCODES.OP_MUL_FUNCT3.MULH -> ((src1.toLong() * src2.toLong()) shr 32).toInt()
            OPCODES.OP_MUL_FUNCT3.MULHU -> ((src1.toUInt().toULong() * src2.toUInt().toULong()) shr 32).toInt()
            OPCODES.OP_MUL_FUNCT3.MULHSU -> ((src1.toULong() * src2.toUInt().toULong()) shr 32).toInt()
            OPCODES.OP_MUL_FUNCT3.DIV -> {
                if (src2 == 0) {
                    0.inv()
                } else if (src1 == Int.MIN_VALUE && src2 == -1) {
                    Int.MIN_VALUE
                } else {
                    src1 / src2
                }
            }
            OPCODES.OP_MUL_FUNCT3.DIVU -> {
                if (src2 == 0) {
                    0.inv()
                } else {
                    (src1.toUInt() / src2.toUInt()).toInt()
                }
            }
            OPCODES.OP_MUL_FUNCT3.REM -> {
                if (src2 == 0) {
                    src1
                } else if (src1 == Int.MIN_VALUE && src2 == -1) {
                    0
                } else {
                    src1 % src2
                }
            }
            OPCODES.OP_MUL_FUNCT3.REMU -> {
                if (src2 == 0) {
                    src1
                } else {
                    (src1.toUInt() % src2.toUInt()).toInt()
                }
            }
            else -> throw IllegalStateException("Impossible mul funct3")
        }
    } else {
        @Suppress("DUPLICATE_LABEL_IN_WHEN")
        when (funct3) {
            OPCODES.OP_FUNCT3.ADD, OPCODES.OP_FUNCT3.SUB -> if (bits(30) == 0) src1 + src2 else src1 - src2
            OPCODES.OP_FUNCT3.SLL -> src1 shl (src2 and 0b11111)
            OPCODES.OP_FUNCT3.SRL, OPCODES.OP_FUNCT3.SRA -> if (bits(30) == 0) src1 ushr (src2 and 0b11111) else src1 shr (src2 and 0b11111)
            OPCODES.OP_FUNCT3.SLT -> if (src1 < src2) 1 else 0
            OPCODES.OP_FUNCT3.SLTU -> if (src1.toUInt() < src2.toUInt()) 1 else 0
            OPCODES.OP_FUNCT3.XOR -> src1 xor src2
            OPCODES.OP_FUNCT3.OR -> src1 or src2
            OPCODES.OP_FUNCT3.AND -> src1 and src2
            else -> throw IllegalStateException("Impossible op funct3")
        }
    }
    cpu.setRd(rd, res)
}

inline fun RiscVInstruction.jal(cpu: CPU) {
    // PC is still equal to this instruction
    val oldPC = cpu.pc
    val newPC = oldPC.toInt() + immed_j
    cpu.pc = newPC.toUInt()
    cpu.setRd(rd, oldPC.toInt() + 4)
}

inline fun RiscVInstruction.jalr(cpu: CPU) {
    // PC is still equal to this instruction
    val oldPC = cpu.pc
    val newPC = (cpu.x[rs1] + immed_i) and 0b1.inv()
    cpu.pc = newPC.toUInt()
    cpu.setRd(rd, oldPC.toInt() + 4)
}

inline fun RiscVInstruction.b(cpu: CPU) {
    val src1 = cpu.x[rs1]
    val src2 = cpu.x[rs2]
    val takeBranch = when (funct3) {
        OPCODES.B_FUNCT3.BEQ -> src1 == src2
        OPCODES.B_FUNCT3.BNE -> src1 != src2
        OPCODES.B_FUNCT3.BLT -> src1 < src2
        OPCODES.B_FUNCT3.BLTU -> src1.toUInt() < src2.toUInt()
        OPCODES.B_FUNCT3.BGE -> src1 >= src2
        OPCODES.B_FUNCT3.BGEU -> src1.toUInt() >= src2.toUInt()
        else -> throw IllegalStateException("Invalid branch funct3") // TODO: CPU exception
    }
    if (takeBranch) {
        // PC is still equal to this instruction
        val newPC = cpu.pc.toInt() + immed_b
    }
}

inline fun RiscVInstruction.load(cpu: CPU) {
    val addr = (cpu.x[rs1] + immed_i).toUInt()
    val v: Int = when (funct3) {
        OPCODES.LOAD_FUNCT3.LW -> cpu.mmu.read32(addr)
        OPCODES.LOAD_FUNCT3.LB -> cpu.mmu.read8(addr).toInt()
        OPCODES.LOAD_FUNCT3.LBU -> cpu.mmu.read8(addr).toInt() and 0xFF
        OPCODES.LOAD_FUNCT3.LH -> cpu.mmu.read16(addr).toInt()
        OPCODES.LOAD_FUNCT3.LHU -> cpu.mmu.read16(addr).toInt() and 0xFFFF
        else -> throw IllegalStateException("Invalid load funct3") // TODO: CPU exception
    }
}

inline fun RiscVInstruction.store(cpu: CPU) {
    val value = cpu.x[rs2]
    val addr = (cpu.x[rs1] + immed_s).toUInt()
    when (funct3) {
        OPCODES.STORE_FUNCT3.SW -> cpu.mmu.write32(addr, value)
        OPCODES.STORE_FUNCT3.SB -> cpu.mmu.write8(addr, value.toByte())
        OPCODES.STORE_FUNCT3.SH -> cpu.mmu.write16(addr, value.toShort())
        else -> throw IllegalStateException("Invalid write funct3") // TODO: CPU exception
    }
}

inline fun RiscVInstruction.misc_mem(cpu: CPU) {
    when (funct3) {
        OPCODES.MISC_MEM_FUNCT3.FENCE -> {
            //noop
            // TODO: multi-hart might need to impl this
        }
        OPCODES.MISC_MEM_FUNCT3.FENCE_I -> {
            //noop, unless we add an instruction cache
        }
        else -> throw IllegalStateException("Invalid memory funct3") // TODO: CPU exception
    }
}

inline fun RiscVInstruction.system(cpu: CPU) {
    when (immed_i) {
        OPCODES.SYSTEM_FUNCT12.ECALL -> {
            //noop
            // TODO: implement this
        }
        OPCODES.SYSTEM_FUNCT12.EBREAK -> {
            //noop, for now. Might be needed for debugger.
        }
        else -> throw IllegalStateException("Invalid memory funct3") // TODO: CPU exception
    }
}
