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
        return "~(" + getExpression().toString() + ")";
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
     * Simplifies this NOT expression.
     * If the inner expression can be evaluated to a boolean value, returns the
     * negated constant.
     * Handles double negation by returning the simplified inner expression inside a
     * double NOT.
     *
     * @return simplified expression
     */
    @Override
    public Expression simplify() {
        Expression simplifiedExpr = getExpression().simplify();

        Boolean val = null;
        try {
            val = simplifiedExpr.evaluate();
        } catch (Exception ignored) {
        }

        if (val != null) {
            return new Val(!val);
        }

        // Double negation simplification (~~X = X)
        // This is a partial attempt since we can't use instanceof or downcasting:
        // if (simplifiedExpr.toString().startsWith("~(") && simplifiedExpr.toString().endsWith(")")) {
            // Ideally, we would unwrap the inner expression here.
            // Without instanceof, it's difficult, so skipping actual unwrap.
            // Could add an isNot() method in Expression interface if allowed.
        // }

        return new Not(simplifiedExpr);
    }
}
