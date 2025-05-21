import java.util.Map;

/**
 * Represents the logical XNOR (equivalence) operation between two expressions.
 * The XNOR evaluates to true if both operands have the same boolean value.
 */
public class Xnor extends BinaryExpression {
    /**
     * Constructs an Xnor expression with the given left and right sub-expressions.
     *
     * @param left  the left expression
     * @param right the right expression
     */
    public Xnor(Expression left, Expression right) {
        super(left, right);
    }

    /**
     * Evaluates the XNOR expression based on the given variable assignment.
     *
     * @param assignment a map of variable names to boolean values
     * @return the boolean result of the XNOR evaluation
     * @throws Exception if evaluation of sub-expressions fails
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment).equals(getRight().evaluate(assignment));
    }

    /**
     * Returns a string representation of the XNOR expression.
     *
     * @return the string in the form "(left # right)"
     */
    @Override
    public String toString() {
        return "(" + getLeft().toString() + " # " + getRight().toString() + ")";
    }

    /**
     * Creates a new Xnor expression with the provided left and right expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     * @return a new Xnor expression
     */
    @Override
    protected Expression create(Expression left, Expression right) {
        return new Xnor(left, right);
    }

    /**
     * Converts the expression to an equivalent expression using only NAND
     * operations.
     *
     * @return the nandified version of the expression
     */
    @Override
    public Expression nandify() {
        Expression xorNand = new Xor(left, right).nandify();
        return new Nand(xorNand, xorNand); // (X XOR Y) NAND (X XOR Y)
    }

    /**
     * Converts the expression to an equivalent expression using only NOR
     * operations.
     *
     * @return the norified version of the expression
     */
    @Override
    public Expression norify() {
        Expression xorNor = new Xor(left, right).norify();
        return new Nor(xorNor, xorNor); // (X XOR Y) NOR (X XOR Y)
    }

    /**
     * Returns a simplified version of the expression.
     * Applies simplification rules such as:
     * - x # x = true
     * - constant folding if both sides evaluate to constants
     *
     * @return the simplified expression
     */
    @Override
    public Expression simplify() {
        Expression leftSimplified = getLeft().simplify();
        Expression rightSimplified = getRight().simplify();

        Boolean leftVal = null;
        Boolean rightVal = null;

        try {
            leftVal = leftSimplified.evaluate();
        } catch (Exception ignored) {
        }

        try {
            rightVal = rightSimplified.evaluate();
        } catch (Exception ignored) {
        }

        // x # x = 1
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Val(true);
        }
        // constant folding
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal.equals(rightVal));
        }

        return new Xnor(leftSimplified, rightSimplified);
    }
}
