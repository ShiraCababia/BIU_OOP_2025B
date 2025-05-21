import java.util.Map;

public class And extends BinaryExpression {
    public And(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment) && getRight().evaluate(assignment);
    }

    @Override
    public String toString() {
        return "(" + getLeft().toString() + " & " + getRight().toString() + ")";
    }

    @Override
    protected Expression create(Expression left, Expression right) {
        return new And(left, right);
    }

    @Override
    public Expression nandify() {
        Expression nandLeft = left.nandify();
        Expression nandRight = right.nandify();
        Expression nand = new Nand(nandLeft, nandRight);
        return new Nand(nand, nand); // (A NAND B) NAND (A NAND B)
    }

    @Override
    public Expression norify() {
        Expression norLeft = left.norify();
        Expression norRight = right.norify();
        return new Nor(new Nor(norLeft, norLeft), new Nor(norRight, norRight)); // (A NOR A) NOR (B NOR B)
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

        // Apply rules without using instanceof
        if (Boolean.TRUE.equals(leftVal)) {
            return rightSimplified;
        }
        if (Boolean.TRUE.equals(rightVal)) {
            return leftSimplified;
        }
        if (Boolean.FALSE.equals(leftVal) || Boolean.FALSE.equals(rightVal)) {
            return new Val(false);
        }
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return leftSimplified;
        }
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal && rightVal);
        }

        return new And(leftSimplified, rightSimplified);
    }
}
