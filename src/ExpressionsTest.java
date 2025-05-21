import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class ExpressionsTest {
    public static void main(String[] args) throws Exception {

        Expression expr1 = new Xor(new And(new Var("x"), new Var("y")), new Val(true));
        String s = expr1.toString();
        System.out.println(s);
        // ((x & y) ^ T)

        List<String> vars = expr1.getVariables();
        for (String v : vars) {
            System.out.println(v);
        }
        // x y

        Expression expr2 = expr1.assign("y", expr1);
        System.out.println(expr2);
        // ((x & ((x & y) ^ T)) ^ T)
        expr2 = expr2.assign("x", new Val(false));
        System.out.println(expr2);
        // ((F & ((F & y) ^ T)) ^ T)

        Map<String, Boolean> assignment = new TreeMap<>();
        assignment.put("x", true);
        assignment.put("y", false);
        Boolean value = expr1.evaluate(assignment);
        System.out.println("The result is: " + value);
        // The result is: true

        Expression expr3 = new Xor(new Var("x"), new Var("y"));
        System.out.println(expr3.nandify());
        // ((x A (x A y)) A (y A (x A y)))
        System.out.println(expr3.norify());
        // (((x V x) V (y V y)) V (x V y))

        Expression ex = new Xor(new And(new Var("x"), new Val(false)), new Or(new Var("y"),
                new Val(false)));
        System.out.println(ex);
        // ((x & F) ^ (y | F))

        Expression ex2 = new Xor(new And(new Var("x"), new Val(false)), new Or(new Var("y"),
                new Val(false)));
        System.out.println(ex2);
        // ((x & F) ^ (y | F))
        System.out.println(ex2.simplify());
        // y

        Expression expr4 = new And(new Xnor(new Var("x"), new Var("x")), new Var("y"));
        System.out.println(expr4);
        // ((x # x) & y)
        System.out.println(expr4.simplify());
        // y

        System.out.println("///////////////////////////////////////////////////////////////////////");

        Expression expr5 = new Xor(new And(new Var("x"), new Var("y")), new Val(true));
        System.out.println("expr5: " + expr5);
        // ((x & y) ^ T)
        Expression one = new Val(true);
        Expression expr6 = new And(expr5, one);
        System.out.println("expr5 & T: " + expr6);
        // (((x & y) ^ T) & T)
        System.out.println("expr5.simplify(): " + expr5.simplify());
        System.out.println("expr6.simplify(): " + expr6.simplify() + " - Same as above.");
        // 2 similar prints! ?- ((x & y) ^ T)

        System.out.println(" ");
        System.out.println("Checks the (NOT~) Why is it outside of the parentheses ?");

    }
}
