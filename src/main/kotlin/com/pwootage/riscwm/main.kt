import com.pwootage.riscwm.CPU.RiscVInstruction
import com.pwootage.riscwm.RiscWM

fun main() {
    println("hi")

    val instr = RiscVInstruction(0xFFF00793u.toInt())

    val vm = RiscWM()
    vm.mmu.write32(0x8000_0000u, instr.instr.toInt())
    vm.interpret(1)
    println(vm.cpu.x[5])
}
