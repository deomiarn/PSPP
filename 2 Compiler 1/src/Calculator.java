/**
 * User: Karl Rege
 */


public class Calculator {

    public static void expr()	throws Exception {
        term();
        while (Scanner.la == Token.PLUS
                || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
        }
    }

    public static void term()	throws Exception {
        factor();
        while (Scanner.la == Token.TIMES || Scanner.la == Token.SLASH) {
            Scanner.scan();
            int op = Scanner.token.kind;
            factor();
        }
    }

    public static void factor() throws Exception {
        if (Scanner.la == Token.LBRACK) {
            Scanner.scan();
            expr();
            Scanner.check(Token.RBRACK);
        } else if (Scanner.la == Token.NUMBER) {
            Scanner.scan();
        }
    }

    public static double start(String expr) throws Exception {
        Scanner.init(expr);
        Scanner.scan();
        expr();
        // return result
        return 0;
    }


    public static void main(String[] args) throws Exception {
        System.out.println("result="+start("3+2-4"));
    }

}
