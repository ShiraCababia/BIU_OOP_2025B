import java.util.Map;

/**
 * Represents the logical XOR (exclusive or) operation between two expressions.
 * The XOR evaluates to true if exactly one of the operands is true.
 */
public class Xor extends BinaryExpression {
    /**
     * Constructs an Xor expression with the given left and right sub-expressions.
     *
     * @param left  the left expression
     * @param right the right expression
     */
    public Xor(Expression left, Expression right) {
        super(left, right);
    }

    /**
     * Evaluates the XOR expression based on the given variable assignment.
     *
     * @param assignment a map of variable names to boolean values
     * @return the boolean result of the XOR evaluation
     * @throws Exception if evaluation of sub-expressions fails
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment) ^ getRight().evaluate(assignment);
    }

    /**
     * Returns a string representation of the XOR expression.
     *
     * @return the string in the form "(left ^ right)"
     */
    @Override
    public String toString() {
        return "(" + getLeft().toString() + " ^ " + getRight().toString() + ")";
    }

    /**
     * Creates a new Xor expression with the provided left and right expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     * @return a new Xor expression
     */
    @Override
    protected Expression create(Expression left, Expression right) {
        return new Xor(left, right);
    }

    /**
     * Converts the expression to an equivalent expression using only NAND
     * operations.
     *
     * @return the nandified version of the expression
     */
    @Override
    public Expression nandify() {
        Expression firstExpr = getLeft().nandify();
        Expression secExpr = getRight().nandify();
        Expression nandAB = new Nand(firstExpr, secExpr);
        Expression part1 = new Nand(firstExpr, nandAB);
        Expression part2 = new Nand(secExpr, nandAB);
        // (firstExpr NAND (firstExpr NAND secExpr)) NAND (secExpr NAND (firstExpr NAND
        // secExpr))
        return new Nand(part1, part2);
    }

    /**
     * Converts the expression to an equivalent expression using only NOR
     * operations.
     *
     * @return the norified version of the expression
     */
    @Override
    public Expression norify() {
        Expression firstExpr = getLeft().norify();
        Expression secExpr = getRight().norify();
        Expression part1 = new Nor(firstExpr, firstExpr);
        Expression part2 = new Nor(secExpr, secExpr);
        Expression part3 = new Nor(firstExpr, secExpr);
        // ((firstExpr NOR firstExpr) NOR (secExpr NOR secExpr)) NOR (firstExpr NOR
        // secExpr)
        return new Nor(new Nor(part1, part2), part3);
    }

    /**
     * Returns a simplified version of the expression.
     * Applies simplification rules such as:
     * - x ^ 0 = x
     * - x ^ 1 = ~x
     * - x ^ x = 0
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

        // x ^ 0 = x
        if (Boolean.FALSE.equals(rightVal)) {
            return leftSimplified;
        }
        if (Boolean.FALSE.equals(leftVal)) {
            return rightSimplified;
        }
        // x ^ 1 = ~x
        if (Boolean.TRUE.equals(rightVal)) {
            return new Not(leftSimplified).simplify();
        }
        if (Boolean.TRUE.equals(leftVal)) {
            return new Not(rightSimplified).simplify();
        }
        // x ^ x = 0
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Val(false);
        }
        // constant folding
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal ^ rightVal);
        }

        return new Xor(leftSimplified, rightSimplified);
    }
}
