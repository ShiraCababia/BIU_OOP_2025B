import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * A test class for demonstrating various logical expression operations,
 * including construction, evaluation, assignment, simplification,
 * nandification, and norification.
 */
public class ExpressionsTestOne {
    /**
     * Main method that runs multiple tests on logical expressions.
     *
     * @param args command-line arguments (not used)
     * @throws Exception if evaluation fails due to unassigned variables
     */
    public static void main(String[] args) throws Exception {

        Expression expr1 = new Xor(new And(new Var("x"), new Var("y")), new Val(true));
        String s = expr1.toString();
        System.out.println(s);
        // Expected: ((x & y) ^ T)

        List<String> vars = expr1.getVariables();
        for (String v : vars) {
            System.out.println(v);
        }
        // Expected: x y

        Expression expr2 = expr1.assign("y", expr1);
        System.out.println(expr2);
        // Expected: ((x & ((x & y) ^ T)) ^ T)
        expr2 = expr2.assign("x", new Val(false));
        System.out.println(expr2);
        // Expected: ((F & ((F & y) ^ T)) ^ T)

        Map<String, Boolean> assignment = new TreeMap<>();
        assignment.put("x", true);
        assignment.put("y", false);
        Boolean value = expr1.evaluate(assignment);
        System.out.println("The result is: " + value);
        // Expected: The result is: true

        Expression expr3 = new Xor(new Var("x"), new Var("y"));
        System.out.println(expr3.nandify());
        // Expected: ((x A (x A y)) A (y A (x A y)))
        System.out.println(expr3.norify());
        // Expected: (((x V x) V (y V y)) V (x V y))

        Expression ex = new Xor(new And(new Var("x"), new Val(false)), new Or(new Var("y"),
                new Val(false)));
        System.out.println(ex);
        // Expected: ((x & F) ^ (y | F))

        Expression ex2 = new Xor(new And(new Var("x"), new Val(false)), new Or(new Var("y"),
                new Val(false)));
        System.out.println(ex2);
        // Expected: ((x & F) ^ (y | F))
        System.out.println(ex2.simplify());
        // Expected: y

        Expression expr4 = new And(new Xnor(new Var("x"), new Var("x")), new Var("y"));
        System.out.println(expr4);
        // Expected: ((x # x) & y)
        System.out.println(expr4.simplify());
        // Expected: y

        Expression expr5 = new Xor(new And(new Var("x"), new Var("y")), new Val(true));
        System.out.println("expr5: " + expr5);
        // ((x & y) ^ T)
        Expression one = new Val(true);
        Expression expr6 = new And(expr5, one);
        System.out.println("expr5 & T: " + expr6);
        // (((x & y) ^ T) & T)
        System.out.println("expr5.simplify(): " + expr5.simplify());
        System.out.println("expr6.simplify(): " + expr6.simplify() + " - Same as above.");
        // (~(x & y))
        System.out.println(" ");

        Expression expr7 = new Not(new Not(new Var("x")));
        System.out.println("expr7: " + expr7);
        System.out.println("expr7.simplify(): " + expr7.simplify());
        System.out.println("Not in expression: " + new Not(new And(new Var("x"), new Var("y"))));
        System.out.println("Not in single var: " + new Not(new Var("x")));
        System.out.println(" ");

        Expression extr8 = new And(new Xnor(new Var("x"), new Var("x")), new Var("y"));
        System.out.println("extr8: " + extr8);
        System.out.println("extr8.simplify(): " + extr8.simplify());
        System.out.println(" ");

        Expression extr9 = new Xor(new Or(new And(new Val(true), new Val(true)),
                new Val(false)), new Val(true));
        System.out.println("extr9: " + extr9);
        System.out.println("extr9.simplify(): " + extr9.simplify());
        System.out.println(" ");

        System.out.println(new And(new Var("x"), new Val(true)).simplify());
        System.out.println(new And(new Var("x"), new Val(false)).simplify());
        System.out.println(new And(new Var("x"), new Var("x")).simplify());
        System.out.println(new Or(new Var("x"), new Val(true)).simplify());
        System.out.println(new Or(new Var("x"), new Val(false)).simplify());
        System.out.println(new Or(new Var("x"), new Var("x")).simplify());
        System.out.println(new Xor(new Var("x"), new Val(true)).simplify());
        System.out.println(new Xor(new Var("x"), new Val(false)).simplify());
        System.out.println(new Xor(new Var("x"), new Var("x")).simplify());
        System.out.println(new Nand(new Var("x"), new Val(true)).simplify());
        System.out.println(new Nand(new Var("x"), new Val(false)).simplify());
        System.out.println(new Nand(new Var("x"), new Var("x")).simplify());
        System.out.println(new Nor(new Var("x"), new Val(true)).simplify());
        System.out.println(new Nor(new Var("x"), new Val(false)).simplify());
        System.out.println(new Nor(new Var("x"), new Var("x")).simplify());
        System.out.println(new Xnor(new Var("x"), new Var("x")).simplify());
    }
}
