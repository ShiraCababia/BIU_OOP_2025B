import java.util.Map;

/**
 * Represents a logical OR operation between two expressions.
 * Extends the BinaryExpression abstract class.
 */
public class Or extends BinaryExpression {

    /**
     * Constructs an Or expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     */
    public Or(Expression left, Expression right) {
        super(left, right);
    }

    /**
     * Evaluates the OR expression given a variable assignment map.
     *
     * @param assignment a map from variable names to boolean values
     * @return the boolean result of left OR right
     * @throws Exception if either sub-expression contains a variable not in the
     *                   assignment
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment) || getRight().evaluate(assignment);
    }

    /**
     * Returns a string representation of the OR expression.
     *
     * @return string in the format "(left | right)"
     */
    @Override
    public String toString() {
        return "(" + getLeft().toString() + " | " + getRight().toString() + ")";
    }

    /**
     * Creates a new Or expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     * @return a new Or instance
     */
    @Override
    protected Expression create(Expression left, Expression right) {
        return new Or(left, right);
    }

    /**
     * Converts this OR expression into an equivalent expression using only NAND
     * operators.
     *
     * @return NAND equivalent expression of this OR expression
     */
    @Override
    public Expression nandify() {
        Expression nandLeft = left.nandify();
        Expression nandRight = right.nandify();
        return new Nand(new Nand(nandLeft, nandLeft), new Nand(nandRight, nandRight)); // (A NAND A) NAND (B NAND B)
    }

    /**
     * Converts this OR expression into an equivalent expression using only NOR
     * operators.
     *
     * @return NOR equivalent expression of this OR expression
     */
    @Override
    public Expression norify() {
        Expression norLeft = left.norify();
        Expression norRight = right.norify();
        Expression nor = new Nor(norLeft, norRight);
        return new Nor(nor, nor); // (A NOR B) NOR (A NOR B)
    }

    /**
     * Simplifies this OR expression by applying boolean algebra rules:
     * - x | 0 = x
     * - x | 1 = 1
     * - x | x = x
     * - If both sides evaluate to constants, compute the result
     *
     * @return simplified expression
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

        // x | 0 = x
        if (Boolean.FALSE.equals(leftVal)) {
            return rightSimplified;
        }
        if (Boolean.FALSE.equals(rightVal)) {
            return leftSimplified;
        }
        // x | 1 = 1
        if (Boolean.TRUE.equals(leftVal) || Boolean.TRUE.equals(rightVal)) {
            return new Val(true);
        }
        // x | x = x
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return leftSimplified;
        }
        // Evaluate if both sides are constants
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal || rightVal);
        }

        return new Or(leftSimplified, rightSimplified);
    }
}
