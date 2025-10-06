import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

/**
 * User: Karl Rege
 */


public class Program implements Emitter {

    @Override
    public void emit() {
        try {
            program();
            emitLoad("value");
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.UNREACHABLE, null, 0));
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
        if (isAssignmentStart()) {
            assignment();
        } else if (isReturnStart()) {
            returnStatement();
        } else if (isIfStart()) {
            ifStatement();
        } else if (isWhileStart()) {
            whileStatement();
        } else if (isBlockStart()) {
            block();
        } else {
            Scanner.error("statement expected");
        }
    }

    public static void block() throws Exception {
        Scanner.check(Token.LCBRACK);
        statementSequence();
        Scanner.check(Token.RCBRACK);
    }

    public static void returnStatement() throws Exception {
        Scanner.check(Token.RETURN);
        expr();
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, 0, 0));
        Scanner.check(Token.SCOLON);
    }

    public static void condition() throws Exception {
        Scanner.check(Token.LBRACK);
        boolean hasNot = false;
        if (Scanner.la == Token.NOT) {
            Scanner.check(Token.NOT);
            hasNot = true;
        }
        expr();
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.nearest, ValueType.f64, 0));
        JWebAssembly.il.add(new WasmConvertInstruction(ValueTypeConvertion.d2i, 0));
        if (hasNot) {
            JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.eqz, ValueType.i32, 0));
        }
        Scanner.check(Token.RBRACK);
    }

    public static void ifStatement() throws Exception {
        Scanner.check(Token.IF);
        condition();
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.IF, null, 0));
        statement();
        if (Scanner.la == Token.ELSE) {
            Scanner.check(Token.ELSE);
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.ELSE, null, 0));
            statement();
        }
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, null, 0));
    }

    public static void whileStatement() throws Exception {
        Scanner.check(Token.WHILE);
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BLOCK, null, 0));
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.LOOP, null, 0));
        condition();
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.eqz, ValueType.i32, 0));
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BR_IF, 1, 0));
        statement();
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BR, 0, 0));
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, null, 0));
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, null, 0));
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

    private static boolean isAssignmentStart() {
        return Scanner.la == Token.IDENT;
    }

    private static boolean isReturnStart() {
        return Scanner.la == Token.RETURN;
    }

    private static boolean isIfStart() {
        return Scanner.la == Token.IF;
    }

    private static boolean isWhileStart() {
        return Scanner.la == Token.WHILE;
    }

    private static boolean isBlockStart() {
        return Scanner.la == Token.LCBRACK;
    }


    private static boolean isStatementStart() {
        return isAssignmentStart() || isReturnStart() || isIfStart() || isWhileStart() || isBlockStart();
    }


    public static void main(String[] args) throws Exception {
        String src =
                "m = $arg0;\n" +
                        " s = 1;\n" +
                        " while (m) {\n" +
                        " s = s * m;\n" +
                        " m = m - 1;\n" +
                        " }\n" +
                        " return s;";
        Scanner.init(src);
        Scanner.scan();
        JWebAssembly.emitCode(IProgram.class, new Program());
    }
}
