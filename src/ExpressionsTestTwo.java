/**
 * A test class for demonstrating various logical expression operations,
 * including construction, evaluation, assignment, simplification,
 * nandification, and norification.
 */
public class ExpressionsTestTwo {
    /**
     * Main method that runs multiple tests on logical expressions.
     *
     * @param args command-line arguments (not used)
     * @throws Exception if evaluation fails due to unassigned variables
     */
    public static void main(String[] args) throws Exception {

        Expression x = new Var("x");

        // 1. NOT(TRUE)
        Expression notTrue = new Not(new Val(true));
        System.out.println("~T → " + notTrue.simplify()); // Expected: F

        // 2. NOT(FALSE)
        Expression notFalse = new Not(new Val(false));
        System.out.println("~F → " + notFalse.simplify()); // Expected: T

        // 3. Double NOT: ~~x
        Expression doubleNotX = new Not(new Not(x));
        System.out.println("~~x → " + doubleNotX.simplify()); // Expected: x

        // 4. NOT(x)
        Expression notX = new Not(x);
        System.out.println("~x → " + notX.simplify()); // Expected: ~(x)

        // 5. Nested simplification: ~~(x & T)
        Expression nested = new Not(new Not(new And(x, new Val(true))));
        System.out.println("(x & T) → " + nested.simplify()); // Expected: x

        // 6. ~~(x ^ F) → x
        Expression xorFalse = new Not(new Not(new Xor(x, new Val(false))));
        System.out.println("(x ^ F) → " + xorFalse.simplify()); // Expected: x

        // 7. ~(x ^ T) → ~x
        Expression xorTrue = new Not(new Xor(x, new Val(true)));
        System.out.println("~(x ^ T) → " + xorTrue.simplify()); // Expected: ~x

        // 8. ~(x ^ x) → ~F → T
        Expression xorSame = new Not(new Xor(x, x));
        System.out.println("~(x ^ x) → " + xorSame.simplify()); // Expected: T

        // 9. NOT(Val(false) | x) → ~(F | x) → ~x
        Expression orFalseX = new Not(new Or(new Val(false), x));
        System.out.println("~(F | x) → " + orFalseX.simplify()); // Expected: ~x

        // 10. NOT(Val(true) & x) → ~(T & x) → ~x
        Expression andTrueX = new Not(new And(new Val(true), x));
        System.out.println("~(T & x) → " + andTrueX.simplify()); // Expected: ~x
    }
}
