import java.util.*;

public abstract class UnaryExpression extends BaseExpression {
    protected final Expression expression;

    public UnaryExpression(Expression expression) {
        this.expression = expression;
    }

    protected Expression getExpression() {
        return expression;
    }

    @Override
    public List<String> getVariables() {
        return expression.getVariables();
    }

    @Override
    public Expression assign(String var, Expression expr) {
        return create(expression.assign(var, expr));
    }

    // This method must be implemented in concrete classes
    protected abstract Expression create(Expression expression);
}
