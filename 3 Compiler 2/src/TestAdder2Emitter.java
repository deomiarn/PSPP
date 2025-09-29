import de.inetsoftware.jwebassembly.*;
import de.inetsoftware.jwebassembly.module.*;

public class TestAdder2Emitter implements Emitter {
    interface TestAdder2 {
        double add(double a, double b);
    }

    public void emit() {
        // 1 + arg0 + arg1 as double
        // load int const 1 on stack
        JWebAssembly.il.add(new WasmConstInstruction(1.0, 0));
        JWebAssembly.il.add(new WasmLoadStoreInstruction(true,
                JWebAssembly.local(ValueType.f64, "$arg0"), 0));
        // add it
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
        JWebAssembly.il.add(new WasmLoadStoreInstruction(true,
                JWebAssembly.local(ValueType.f64, "$arg1"), 0));
        // add it
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
        // result top of stack
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    public static void main(String[] args) throws Exception {
        JWebAssembly.emitCode(TestAdder2.class, new TestAdder2Emitter());
    }
}
