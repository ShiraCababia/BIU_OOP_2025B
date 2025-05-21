import java.util.List;
import java.util.ArrayList;

/**
 * Abstract class representing a binary logical expression with two operands.
 * Extends BaseExpression and implements common functionality for binary
 * expressions.
 */
public abstract class BinaryExpression extends BaseExpression {
    private final Expression left;
    private final Expression right;

    /**
     * Constructs a BinaryExpression with the given left and right sub-expressions.
     *
     * @param left  the left operand expression
     * @param right the right operand expression
     */
    public BinaryExpression(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }

    /**
     * Returns the left operand expression.
     *
     * @return the left expression
     */
    public Expression getLeft() {
        return left;
    }

    /**
     * Returns the right operand expression.
     *
     * @return the right expression
     */
    public Expression getRight() {
        return right;
    }

    /**
     * Returns a list of unique variables from both left and right expressions.
     *
     * @return list of variable names used in the expression
     */
    @Override
    public List<String> getVariables() {
        List<String> vars = new ArrayList<>();
        vars.addAll(left.getVariables());
        for (String v : right.getVariables()) {
            if (!vars.contains(v)) {
                vars.add(v);
            }
        }
        return vars;
    }

    /**
     * Returns a new expression where all occurrences of the variable var are
     * replaced
     * with the provided expression, recursively applying to left and right
     * sub-expressions.
     *
     * @param var        the variable name to replace
     * @param expression the expression to replace the variable with
     * @return new expression with replacements applied
     */
    @Override
    public Expression assign(String var, Expression expression) {
        Expression newLeft = left.assign(var, expression);
        Expression newRight = right.assign(var, expression);
        return create(newLeft, newRight);
    }

    /**
     * Abstract factory method for subclasses to create an instance of the correct
     * subclass type with given left and right expressions.
     *
     * @param left  the left sub-expression
     * @param right the right sub-expression
     * @return new expression of the subclass type
     */
    protected abstract Expression create(Expression left, Expression right);
}
