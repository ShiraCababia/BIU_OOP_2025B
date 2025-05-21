import java.util.Map;

/**
 * Represents a logical NOR operation between two expressions.
 * Extends the BinaryExpression abstract class.
 */
public class Nor extends BinaryExpression {

    /**
     * Constructs a Nor expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     */
    public Nor(Expression left, Expression right) {
        super(left, right);
    }

    /**
     * Evaluates the NOR expression given a variable assignment map.
     *
     * @param assignment a map from variable names to boolean values
     * @return the boolean result of NOR operation on left and right
     * @throws Exception if either sub-expression contains a variable not in the
     *                   assignment
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return !(getLeft().evaluate(assignment) || getRight().evaluate(assignment));
    }

    /**
     * Returns a string representation of the NOR expression.
     *
     * @return string in the format "(left V right)"
     */
    @Override
    public String toString() {
        return "(" + getLeft().toString() + " V " + getRight().toString() + ")";
    }

    /**
     * Creates a new Nor expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     * @return a new Nor instance
     */
    @Override
    protected Expression create(Expression left, Expression right) {
        return new Nor(left, right);
    }

    /**
     * Converts this NOR expression into an equivalent expression using only NAND
     * operators.
     *
     * @return NAND equivalent expression of this NOR expression
     */
    @Override
    public Expression nandify() {
        Expression firstExpr = getLeft().nandify();
        Expression secExpr = getRight().nandify();

        // ( (firstExpr NAND firstExpr) NAND (secExpr NAND secExpr) ) NAND ( (firstExpr
        // NAND firstExpr) NAND (secExpr NAND secExpr) )
        Expression part1 = new Nand(new Nand(firstExpr, firstExpr), new Nand(secExpr, secExpr));
        return new Nand(part1, part1);
    }

    /**
     * Converts this NOR expression into an equivalent expression using only NOR
     * operators.
     *
     * @return NOR equivalent expression of this NOR expression (itself)
     */
    @Override
    public Expression norify() {
        Expression firstExpr = getLeft().norify();
        Expression secExpr = getRight().norify();
        return new Nor(firstExpr, secExpr); // remains the same
    }

    /**
     * Simplifies this NOR expression by applying boolean algebra rules:
     * - x V 1 = 0
     * - x V 0 = ~x
     * - x V x = ~x
     * - If both sides evaluate to constants, compute the result.
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

        // x V 1 = 0
        if (Boolean.TRUE.equals(rightVal) || Boolean.TRUE.equals(leftVal)) {
            return new Val(false);
        }
        // x V 0 = ~x
        if (Boolean.FALSE.equals(rightVal)) {
            return new Not(leftSimplified).simplify();
        }
        if (Boolean.FALSE.equals(leftVal)) {
            return new Not(rightSimplified).simplify();
        }
        // x V x = ~x
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Not(leftSimplified).simplify();
        }
        // constant folding for NOR
        if (leftVal != null && rightVal != null) {
            return new Val(!(leftVal || rightVal));
        }

        return new Nor(leftSimplified, rightSimplified);
    }
}
