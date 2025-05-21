import java.util.Map;

public class Not extends UnaryExpression {
    public Not(Expression expression) {
        super(expression);
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return !getExpression().evaluate(assignment);
    }

    @Override
    public String toString() {
        return "~(" + getExpression().toString() + ")";
    }

    @Override
    protected Expression create(Expression expression) {
        return new Not(expression);
    }

    @Override
    public Expression nandify() {
        Expression nandExpr = expression.nandify();
        return new Nand(nandExpr, nandExpr); // A NAND A
    }

    @Override
    public Expression norify() {
        Expression norExpr = expression.norify();
        return new Nor(norExpr, norExpr); // A NOR A
    }

    @Override
public Expression simplify() {
    Expression simplifiedExpr = getExpression().simplify();

    Boolean val = null;
    try {
        val = simplifiedExpr.evaluate();
    } catch (Exception ignored) {}

    if (val != null) {
        return new Val(!val);
    }

    // Double negation: ~~X = X
    if (simplifiedExpr.toString().startsWith("~(") && simplifiedExpr.toString().endsWith(")")) {
        // extract inner expression string and compare? Risky, but no other option without instanceof
        // safer to just return simplifiedExpr.simplify() if it is a Not inside Not
        // We can override Not.toString carefully to help here.

        // Or implement a method isNot() in Expression interface? (optional)
    }

    return new Not(simplifiedExpr);
}
}
