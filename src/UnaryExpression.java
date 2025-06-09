import java.util.List;

/**
 * Abstract class representing a unary logical expression with a single operand.
 * Extends BaseExpression and implements common functionality for unary
 * expressions.
 */
public abstract class UnaryExpression extends BaseExpression {
    private final Expression expression;

    /**
     * Constructs a UnaryExpression with the given sub-expression.
     *
     * @param expression the operand expression
     */
    public UnaryExpression(Expression expression) {
        this.expression = expression;
    }

    /**
     * Returns the operand expression.
     *
     * @return the expression
     */
    protected Expression getExpression() {
        return expression;
    }

    /**
     * Returns a list of variables in the operand expression.
     *
     * @return list of variable names used in the expression
     */
    @Override
    public List<String> getVariables() {
        return expression.getVariables();
    }

    /**
     * Returns a new expression where all occurrences of the variable var are
     * replaced
     * with the provided expression, applied recursively to the operand expression.
     *
     * @param var  the variable name to replace
     * @param expr the expression to replace the variable with
     * @return new expression with replacements applied
     */
    @Override
    public Expression assign(String var, Expression expr) {
        return create(expression.assign(var, expr));
    }

    /**
     * Abstract factory method for subclasses to create an instance of the correct
     * subclass type with the given expression.
     *
     * @param expression the sub-expression
     * @return new expression of the subclass type
     */
    protected abstract Expression create(Expression expression);
}
