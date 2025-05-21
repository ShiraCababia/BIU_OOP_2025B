import java.util.Map;

/**
 * Represents a logical AND operation between two expressions.
 * Extends the BinaryExpression abstract class.
 */
public class And extends BinaryExpression {

    /**
     * Constructs an And expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression.
     * @param right the right sub-expression.
     */
    public And(Expression left, Expression right) {
        super(left, right);
    }

    /**
     * Evaluates the AND expression given a variable assignment map.
     *
     * @param assignment a map from variable names to boolean values.
     * @return the boolean result of left AND right.
     * @throws Exception if either sub-expression contains a variable not in the
     *                   assignment.
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment) && getRight().evaluate(assignment);
    }

    /**
     * Returns a string representation of the AND expression.
     *
     * @return string in the format "(left & right)".
     */
    @Override
    public String toString() {
        return "(" + getLeft().toString() + " & " + getRight().toString() + ")";
    }

    /**
     * Creates a new And expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression.
     * @param right the right sub-expression.
     * @return a new And instance.
     */
    @Override
    protected Expression create(Expression left, Expression right) {
        return new And(left, right);
    }

    /**
     * Converts this AND expression into an equivalent expression using only NAND
     * operators.
     *
     * @return NAND equivalent expression of this AND expression.
     */
    @Override
    public Expression nandify() {
        Expression nandLeft = getLeft().nandify();
        Expression nandRight = getRight().nandify();
        Expression nand = new Nand(nandLeft, nandRight);
        // (A NAND B) NAND (A NAND B) is equivalent to A AND B.
        return new Nand(nand, nand);
    }

    /**
     * Converts this AND expression into an equivalent expression using only NOR
     * operators.
     *
     * @return NOR equivalent expression of this AND expression.
     */
    @Override
    public Expression norify() {
        Expression norLeft = getLeft().norify();
        Expression norRight = getRight().norify();
        return new Nor(new Nor(norLeft, norLeft), new Nor(norRight, norRight)); // (A NOR A) NOR (B NOR B)
    }

    /**
     * Simplifies this AND expression by applying boolean algebra rules:
     * - x & 1 = x
     * - x & 0 = 0
     * - x & x = x
     * - If both sides evaluate to constants, compute the result.
     *
     * @return simplified expression.
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

        // x & 1 = x
        if (Boolean.TRUE.equals(leftVal)) {
            return rightSimplified;
        }
        if (Boolean.TRUE.equals(rightVal)) {
            return leftSimplified;
        }
        // x & 0 = 0
        if (Boolean.FALSE.equals(leftVal) || Boolean.FALSE.equals(rightVal)) {
            return new Val(false);
        }
        // x & x = x
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return leftSimplified;
        }
        // Evaluate if both sides are constants.
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal && rightVal);
        }

        return new And(leftSimplified, rightSimplified);
    }
}
