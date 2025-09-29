import de.inetsoftware.jwebassembly.*;
import de.inetsoftware.jwebassembly.module.*;

public class TestAdderEmitter implements Emitter {
    interface TestAdder {
        int add();
    }

    public void emit() {
// Instruction Example: 5 + 8
// load int const 5 on stack
        JWebAssembly.il.add(new WasmConstInstruction(5, 0));
// load int const 8 on stack
        JWebAssembly.il.add(new WasmConstInstruction(8, 0));
// add it
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.i32, 0));
// result top of stack
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    public static void main(String[] args) throws Exception {
        JWebAssembly.emitCode(TestAdder.class, new TestAdderEmitter());
    }
}
