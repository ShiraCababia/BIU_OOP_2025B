/**
 * A test class for demonstrating various logical expression operations,
 * including construction, evaluation, assignment, simplification,
 * nandification, and norification.
 */
public class ExpressionsTestThree {
        /**
         * Main method that runs multiple tests on logical expressions.
         *
         * @param args command-line arguments (not used)
         * @throws Exception if evaluation fails due to unassigned variables
         */
        public static void main(String[] args) throws Exception {
                Expression x = new Var("x");
                Object[][] tests = new Object[][] {
                                // AND rules
                                {new And(x, new Val(true)), x, "(x & T) → x" },
                                {new And(x, new Val(false)), new Val(false), "(x & F) → F" },
                                {new And(x, x), x, "(x & x) → x" },
                                // OR rules
                                {new Or(x, new Val(true)), new Val(true), "(x | T) → T" },
                                {new Or(x, new Val(false)), x, "(x | F) → x" },
                                {new Or(x, x), x, "(x | x) → x" },
                                // XOR rules
                                {new Xor(x, new Val(true)), new Not(x), "(x ^ T) → ~(x)" },
                                {new Xor(x, new Val(false)), x, "(x ^ F) → x" },
                                {new Xor(x, x), new Val(false), "(x ^ x) → F" },
                                // NAND rules
                                {new Nand(x, new Val(true)), new Not(x), "(x A T) → ~(x)" },
                                {new Nand(x, new Val(false)), new Val(true), "(x A F) → T" },
                                {new Nand(x, x), new Not(x), "(x A x) → ~(x)" },
                                // NOR rules
                                {new Nor(x, new Val(true)), new Val(false), "(x V T) → F" },
                                {new Nor(x, new Val(false)), new Not(x), "(x V F) → ~(x)" },
                                {new Nor(x, x), new Not(x), "(x V x) → ~(x)" },
                                // XNOR rule
                                {new Xnor(x, x), new Val(true), "(x # x) → T" }, };

                for (Object[] row : tests) {
                        Expression expr = (Expression) row[0];
                        Expression expected = (Expression) row[1];
                        String label = (String) row[2];
                        Expression simplified = expr.simplify();
                        System.out.printf("%-15s -> %-15s   %s%n", label, simplified,
                                        simplified.equals(expected) ? "Y"
                                                        : "Got: " + simplified + " expected: " + expected);
                }

                // Test “no variables” evaluation and simplification:
                Expression noVars = new Xor(new Or(new And(new Val(true), new Val(true)),
                                new Val(false)), new Val(true));
                Expression noVarsSimpl = noVars.simplify();
                System.out.printf("%-15s -> %-15s   %s%n", noVars.toString() + " ?", noVarsSimpl,
                                noVarsSimpl.equals(new Val(false)) ? "Y"
                                                : "Got: " + noVarsSimpl + " expected: F");
        }
}
