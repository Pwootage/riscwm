package com.pwootage.riscwm.memory

import java.nio.ByteBuffer
import java.nio.ByteOrder

class RawMemory(
    override val start: UInt,
    override val length: UInt
): MemoryDevice {
    override val name = "raw_memory"
    override val description = "Raw memory buffer"

    private val memoryBuffer = ByteBuffer.allocate(length.toInt())
        .order(ByteOrder.LITTLE_ENDIAN)

    override fun read8(offset: UInt): Byte {
        return memoryBuffer.get((offset - start).toInt())
    }

    override fun read16(offset: UInt): Short {
        return memoryBuffer.getShort((offset - start).toInt())
    }

    override fun read32(offset: UInt): Int {
        return memoryBuffer.getInt((offset - start).toInt())
    }

    override fun read64(offset: UInt): Long {
        return memoryBuffer.getLong((offset - start).toInt())
    }

    override fun write8(offset: UInt, value: Byte) {
        memoryBuffer.put((offset - start).toInt(), value)
    }

    override fun write16(offset: UInt, value: Short) {
        memoryBuffer.putShort((offset - start).toInt(), value)
    }

    override fun write32(offset: UInt, value: Int) {
        memoryBuffer.putInt((offset - start).toInt(), value)
    }

    override fun write64(offset: UInt, value: Long) {
        memoryBuffer.putLong((offset - start).toInt(), value)
    }
}
