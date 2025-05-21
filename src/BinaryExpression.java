import java.util.*;

public abstract class BinaryExpression extends BaseExpression {
    protected final Expression left;
    protected final Expression right;

    public BinaryExpression(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }

    protected Expression getLeft() {
        return left;
    }

    protected Expression getRight() {
        return right;
    }

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

    @Override
    public Expression assign(String var, Expression expression) {
        Expression newLeft = left.assign(var, expression);
        Expression newRight = right.assign(var, expression);
        return create(newLeft, newRight);
    }

    // Each subclass will implement this method to recreate the right type (e.g.,
    // And, Or...)
    protected abstract Expression create(Expression left, Expression right);
}
