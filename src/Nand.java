import java.util.Map;

/**
 * Represents a logical NAND operation between two expressions.
 * Extends the BinaryExpression abstract class.
 */
public class Nand extends BinaryExpression {

    /**
     * Constructs a Nand expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     */
    public Nand(Expression left, Expression right) {
        super(left, right);
    }

    /**
     * Evaluates the NAND expression given a variable assignment map.
     *
     * @param assignment a map from variable names to boolean values
     * @return the boolean result of NAND operation on left and right
     * @throws Exception if either sub-expression contains a variable not in the
     *                   assignment
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return !(getLeft().evaluate(assignment) && getRight().evaluate(assignment));
    }

    /**
     * Returns a string representation of the NAND expression.
     *
     * @return string in the format "(left A right)"
     */
    @Override
    public String toString() {
        return "(" + getLeft().toString() + " A " + getRight().toString() + ")";
    }

    /**
     * Creates a new Nand expression with the given left and right sub-expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     * @return a new Nand instance
     */
    @Override
    protected Expression create(Expression left, Expression right) {
        return new Nand(left, right);
    }

    /**
     * Converts this NAND expression into an equivalent expression using only NAND
     * operators.
     *
     * @return NAND equivalent expression of this NAND expression (itself)
     */
    @Override
    public Expression nandify() {
        Expression firstExpr = getLeft().nandify();
        Expression secExpr = getRight().nandify();
        return new Nand(firstExpr, secExpr); // stays the same
    }

    /**
     * Converts this NAND expression into an equivalent expression using only NOR
     * operators.
     * Uses the formula:
     * ( (firstExpr NOR firstExpr) NOR (secExpr NOR secExpr) )
     * NOR ( ( (firstExpr NOR firstExpr) NOR (secExpr NOR secExpr) ) NOR
     * (firstExpr NOR secExpr) )
     * @return NOR equivalent expression of this NAND expression
     */
    @Override
    public Expression norify() {
        Expression firstExpr = getLeft().norify();
        Expression secExpr = getRight().norify();

        Expression part1 = new Nor(new Nor(firstExpr, firstExpr), new Nor(secExpr, secExpr));
        Expression part2 = new Nor(firstExpr, secExpr);
        Expression inner = new Nor(part1, part2);
        return new Nor(part1, inner);
    }

    /**
     * Simplifies this NAND expression by applying boolean algebra rules:
     * - x A 1 = ~x
     * - x A 0 = 1
     * - x A x = ~x
     * - Constant folding if both sides evaluate to constants.
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

        // x A 1 = ~x
        if (Boolean.TRUE.equals(rightVal)) {
            return new Not(leftSimplified).simplify();
        }
        if (Boolean.TRUE.equals(leftVal)) {
            return new Not(rightSimplified).simplify();
        }
        // x A 0 = 1
        if (Boolean.FALSE.equals(rightVal) || Boolean.FALSE.equals(leftVal)) {
            return new Val(true);
        }
        // x A x = ~x
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Not(leftSimplified).simplify();
        }
        // constant folding
        if (leftVal != null && rightVal != null) {
            return new Val(!(leftVal && rightVal));
        }

        return new Nand(leftSimplified, rightSimplified);
    }
}
