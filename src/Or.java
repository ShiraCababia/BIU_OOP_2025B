import java.util.Map;

public class Or extends BinaryExpression {
    public Or(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment) || getRight().evaluate(assignment);
    }

    @Override
    public String toString() {
        return "(" + getLeft().toString() + " | " + getRight().toString() + ")";
    }

    @Override
    protected Expression create(Expression left, Expression right) {
        return new Or(left, right);
    }

    @Override
    public Expression nandify() {
        Expression nandLeft = left.nandify();
        Expression nandRight = right.nandify();
        return new Nand(new Nand(nandLeft, nandLeft), new Nand(nandRight, nandRight)); // (A NAND A) NAND (B NAND B)
    }

    @Override
    public Expression norify() {
        Expression norLeft = left.norify();
        Expression norRight = right.norify();
        Expression nor = new Nor(norLeft, norRight);
        return new Nor(nor, nor); // (A NOR B) NOR (A NOR B)
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

        if (Boolean.FALSE.equals(leftVal)) {
            return rightSimplified;
        }
        if (Boolean.FALSE.equals(rightVal)) {
            return leftSimplified;
        }
        if (Boolean.TRUE.equals(leftVal) || Boolean.TRUE.equals(rightVal)) {
            return new Val(true);
        }
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return leftSimplified;
        }
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal || rightVal);
        }

        return new Or(leftSimplified, rightSimplified);
    }
}
