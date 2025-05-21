import java.util.Map;
import java.util.TreeMap;

/**
 * A test class for demonstrating various logical expression operations,
 * including construction, evaluation, assignment, simplification,
 * nandification, and norification.
 */
public class ExpressionsTest {
    /**
     * Main method that runs multiple tests on logical expressions.
     *
     * @param args command-line arguments (not used)
     * @throws Exception if evaluation fails due to unassigned variables
     */
    public static void main(String[] args) throws Exception {

        // Create an expression with at least three variables.
        // Print the expression.
        // Print the value of the expression with an assignment to every variable.
        // Print the Nandified version of the expression.
        // Print the Norified version of the expression.
        // Print the simplified version of the expression.

        Expression expr1 = new Xor(new And(new Var("x"), new Var("y")), new Var("z"));
        System.out.println(expr1);

        Map<String, Boolean> assignment1 = new TreeMap<>();
        assignment1.put("x", true);
        assignment1.put("y", false);
        assignment1.put("z", true);
        System.out.println(expr1.evaluate(assignment1));

        System.out.println(expr1.nandify());
        System.out.println(expr1.norify());
        System.out.println(expr1.simplify());
    }
}
