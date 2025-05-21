import java.util.Map;

public class Nor extends BinaryExpression {
    public Nor(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return !(getLeft().evaluate(assignment) || getRight().evaluate(assignment));
    }

    @Override
    public String toString() {
        return "(" + getLeft().toString() + " V " + getRight().toString() + ")";
    }

    @Override
    protected Expression create(Expression left, Expression right) {
        return new Nor(left, right);
    }

    @Override
    public Expression nandify() {
        Expression A = left.nandify();
        Expression B = right.nandify();

        // ( (A NAND A) NAND (B NAND B) ) NAND ( (A NAND A) NAND (B NAND B) )
        Expression part1 = new Nand(new Nand(A, A), new Nand(B, B));
        return new Nand(part1, part1);
    }

    @Override
    public Expression norify() {
        Expression A = left.norify();
        Expression B = right.norify();
        return new Nor(A, B); // stays the same
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

        // x V 1 = 0
        if (Boolean.TRUE.equals(rightVal) || Boolean.TRUE.equals(leftVal)) {
            return new Val(false);
        }
        // x V 0 = ~x
        if (Boolean.FALSE.equals(rightVal)) {
            return new Not(leftSimplified).simplify();
        }
        if (Boolean.FALSE.equals(leftVal)) {
            return new Not(rightSimplified).simplify();
        }
        // x V x = ~x
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Not(leftSimplified).simplify();
        }
        // constant folding
        if (leftVal != null && rightVal != null) {
            return new Val(!(leftVal || rightVal));
        }

        return new Nor(leftSimplified, rightSimplified);
    }
}
