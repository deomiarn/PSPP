import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

/**
 * User: Karl Rege
 */


public class Calculator2 implements Emitter {

    @Override
    public void emit() {
        try {
            program();
            emitLoad("value");
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            if (op == Token.PLUS) {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
            } else if (op == Token.MINUS) {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.sub, ValueType.f64, 0));
            }
        }
    }

    public static void term() throws Exception {
        factor();
        while (Scanner.la == Token.TIMES || Scanner.la == Token.SLASH) {
            Scanner.scan();
            int op = Scanner.token.kind;
            factor();
            if (op == Token.SLASH) {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.div, ValueType.f64, 0));
            } else if (op == Token.TIMES) {
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.mul, ValueType.f64, 0));
            }
        }
    }

    public static void factor() throws Exception {
        if (Scanner.la == Token.LBRACK) {
            Scanner.check(Token.LBRACK);
            expr();
            Scanner.check(Token.RBRACK);
        } else if (Scanner.la == Token.NUMBER) {
            Scanner.check(Token.NUMBER);
            JWebAssembly.il.add(new WasmConstInstruction(Scanner.token.val, 0));
        } else if (Scanner.la == Token.IDENT) {
            Scanner.check(Token.IDENT);
            String name = Scanner.token.str;
            if ("PI".equals(name)) {
                JWebAssembly.il.add(new WasmConstInstruction(Math.PI, 0));
            } else if ("E".equals(name)) {
                JWebAssembly.il.add(new WasmConstInstruction(Math.E, 0));
            } else {
                emitLoad(name);
            }
        } else {
            Scanner.error("factor expected");
        }
    }

    public static void assignment() throws Exception {
        Scanner.check(Token.IDENT);
        String left = Scanner.token.str;
        Scanner.check(Token.EQUAL);
        expr();
        emitStore(left);
        Scanner.check(Token.SCOLON);
    }

    public static void statement() throws Exception {
        assignment();
    }

    public static void statementSequence() throws Exception {
        do {
            statement();
        } while (isStatementStart());
    }

    public static void program() throws Exception {
        statementSequence();
        Scanner.check(Token.EOF);
    }

    private static int slot(String name) {
        return JWebAssembly.local(ValueType.f64, name);
    }

    private static void emitLoad(String name) {
        int s = slot(name);
        JWebAssembly.il.add(new WasmLoadStoreInstruction(true, s, 0));
    }

    private static void emitStore(String name) {
        int s = slot(name);
        JWebAssembly.il.add(new WasmLoadStoreInstruction(false, s, 0));
    }

    private static boolean isStatementStart() {
        return Scanner.la == Token.IDENT;
    }


    public static void main(String[] args) throws Exception {
        String src = "b = 4; c = 2; value = 2*PI*c*c + 2*PI*b*c;";
        Scanner.init(src);
        Scanner.scan();
        JWebAssembly.emitCode(ICalculator2.class, new Calculator2());
    }
}
