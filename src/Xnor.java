import java.util.Map;

public class Xnor extends BinaryExpression {
    public Xnor(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment).equals(getRight().evaluate(assignment));
    }

    @Override
    public String toString() {
        return "(" + getLeft().toString() + " # " + getRight().toString() + ")";
    }

    @Override
    protected Expression create(Expression left, Expression right) {
        return new Xnor(left, right);
    }

    @Override
    public Expression nandify() {
        Expression xorNand = new Xor(left, right).nandify();
        return new Nand(xorNand, xorNand); // (X XOR Y) NAND (X XOR Y)
    }

    @Override
    public Expression norify() {
        Expression xorNor = new Xor(left, right).norify();
        return new Nor(xorNor, xorNor); // (X XOR Y) NOR (X XOR Y)
    }

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

        // x # x = 1
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Val(true);
        }
        // constant folding
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal.equals(rightVal));
        }

        return new Xnor(leftSimplified, rightSimplified);
    }
}
