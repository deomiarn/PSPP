import java.util.Stack;

/**
 * User: Karl Rege
 */


public class Calculator {
    private static Stack<Double> stack = new Stack<>();

    public static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS
                || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            if (op == Token.PLUS) {
                stack.push(stack.pop() + stack.pop());
            } else if (op == Token.MINUS) {
                stack.push(-stack.pop() + stack.pop());
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
                Double divisor = stack.pop();
                Double dividend = stack.pop();
                stack.push(dividend / divisor);
            } else if (op == Token.TIMES) {
                stack.push(stack.pop() * stack.pop());
            }
        }
    }

    public static void factor() throws Exception {
        if (Scanner.la == Token.LBRACK) {
            Scanner.scan();
            expr();
            Scanner.check(Token.RBRACK);
        } else if (Scanner.la == Token.NUMBER) {
            Scanner.scan();
            stack.push(Scanner.token.val);
        }
    }

    public static double start(String expr) throws Exception {
        Scanner.init(expr);
        Scanner.scan();
        expr();

        // return result
        return stack.pop();
    }


    public static void main(String[] args) throws Exception {
        System.out.println("result="+start("3+2-4"));
    }

}
