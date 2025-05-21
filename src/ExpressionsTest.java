import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class ExpressionsTest {
    public static void main(String[] args) throws Exception {

        /*
         * {
         * Expression e2 = new Xor(new And(new Var("x"), new Var("y")), new Val(true));
         * String s = e2.toString();
         * System.out.println(s);
         * // ((x & y) ^ T)
         * 
         * List<String> vars = e2.getVariables();
         * for (String v : vars) {
         * System.out.println(v);
         * }
         * // x y
         * 
         * Expression e3 = e2.assign("y", e2);
         * System.out.println(e3);
         * // ((x & ((x & y) ^ T)) ^ T)
         * e3 = e3.assign("x", new Val(false));
         * System.out.println(e3);
         * // ((F & ((F & y) ^ T)) ^ T)
         * 
         * Map<String, Boolean> assignment = new TreeMap<>();
         * assignment.put("x", true);
         * assignment.put("y", false);
         * Boolean value = e2.evaluate(assignment);
         * System.out.println("The result is: " + value);
         * // The result is: true
         * 
         * Expression e = new Xor(new Var("x"), new Var("y"));
         * System.out.println(e.nandify());
         * // ((x A (x A y)) A (y A (x A y)))
         * System.out.println(e.norify());
         * // (((x V x) V (y V y)) V (x V y))
         * 
         * Expression ex = new Xor(new And(new Var("x"), new Val(false)), new Or(new
         * Var("y"),
         * new Val(false)));
         * System.out.println(ex);
         * // ((x & F) ^ (y | F))
         * 
         * Expression ex2 = new Xor(new And(new Var("x"), new Val(false)), new Or(new
         * Var("y"),
         * new Val(false)));
         * System.out.println(ex2);
         * // ((x & F) ^ (y | F))
         * System.out.println(ex2.simplify());
         * // y
         * 
         * Expression expr = new And(new Xnor(new Var("x"), new Var("x")), new
         * Var("y"));
         * System.out.println(expr);
         * // ((x # x) & y)
         * System.out.println(expr.simplify());
         * // y
         * }
         */

        // 
        
         Expression x = new Xor(new And(new Var("x"), new Var("y")), new Val(true));
        // ((x & y) ^ T)
        Expression one = new Val(true);
        Expression expr = new And(x, one);

        System.out.println(expr); // prints: (x & T)
        System.out.println(expr.simplify()); // prints: x
    }
}
