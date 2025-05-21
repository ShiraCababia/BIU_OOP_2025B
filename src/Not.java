import java.util.Map;

/**
 * Represents a logical NOT operation on an expression.
 * Extends the UnaryExpression abstract class.
 */
public class Not extends UnaryExpression {

    /**
     * Constructs a Not expression with the given sub-expression.
     *
     * @param expression the expression to negate
     */
    public Not(Expression expression) {
        super(expression);
    }

    /**
     * Evaluates the NOT expression given a variable assignment map.
     *
     * @param assignment a map from variable names to boolean values
     * @return the negated boolean result of the sub-expression
     * @throws Exception if the sub-expression contains a variable not in the
     *                   assignment
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return !getExpression().evaluate(assignment);
    }

    /**
     * Returns a string representation of the NOT expression.
     *
     * @return string in the format "~(expression)"
     */
    @Override
    public String toString() {
        String inner = getExpression().toString();

        // Check if the inner expression already has parentheses wrapping it fully
        if (inner.startsWith("(") && inner.endsWith(")")) {
            // Inner already parenthesized, just prepend '~' inside parentheses
            return "(~" + inner + ")";
        } else {
            // Otherwise, add parentheses around inner expression
            return "(~(" + inner + "))";
        }
    }

    /**
     * Creates a new Not expression with the given sub-expression.
     *
     * @param expression the sub-expression
     * @return a new Not instance wrapping the expression
     */
    @Override
    protected Expression create(Expression expression) {
        return new Not(expression);
    }

    /**
     * Converts this expression to an equivalent expression using only NAND
     * operators.
     *
     * @return NAND equivalent expression of this NOT expression
     */
    @Override
    public Expression nandify() {
        Expression nandExpr = getExpression().nandify();
        return new Nand(nandExpr, nandExpr); // A NAND A is equivalent to NOT A
    }

    /**
     * Converts this expression to an equivalent expression using only NOR
     * operators.
     *
     * @return NOR equivalent expression of this NOT expression
     */
    @Override
    public Expression norify() {
        Expression norExpr = getExpression().norify();
        return new Nor(norExpr, norExpr); // A NOR A is equivalent to NOT A
    }

    /**
     * Overrides the default implementation to return the expression
     * being negated (i.e., the inner expression).
     *
     * @return the inner expression of this Not
     */
    @Override
    public Expression getNegated() {
        return getExpression();
    }

    /**
     * Simplifies this NOT expression by:
     * - Returning the negated constant if inner expression evaluates to a boolean.
     * - Removing double negations by unwrapping nested NOT expressions.
     * - Otherwise, returning a NOT expression with the simplified inner expression.
     *
     * @return the simplified expression
     */
    @Override
    public Expression simplify() {
        Expression simplifiedExpr = getExpression().simplify();
        // Constant folding
        try {
            return new Val(!simplifiedExpr.evaluate());
        } catch (Exception ignored) {
        }
        // Double negation: if the simplified expression is a Not, unwrap it
        Expression inner = simplifiedExpr.getNegated();
        if (inner != null) {
            return inner.simplify();
        }
        return new Not(simplifiedExpr);
    }
}
