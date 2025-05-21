import java.util.Map;

public class Nand extends BinaryExpression {
    public Nand(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return !(getLeft().evaluate(assignment) && getRight().evaluate(assignment));
    }

    @Override
    public String toString() {
        return "(" + getLeft().toString() + " A " + getRight().toString() + ")";
    }

    @Override
    protected Expression create(Expression left, Expression right) {
        return new Nand(left, right);
    }

    @Override
    public Expression nandify() {
        Expression A = left.nandify();
        Expression B = right.nandify();
        return new Nand(A, B); // stays the same
    }

    @Override
    public Expression norify() {
        Expression A = left.norify();
        Expression B = right.norify();

        // ( (A NOR A) NOR (B NOR B) ) NOR ( ( (A NOR A) NOR (B NOR B) ) NOR (A NOR B) )
        Expression part1 = new Nor(new Nor(A, A), new Nor(B, B));
        Expression part2 = new Nor(A, B);
        Expression inner = new Nor(part1, part2);
        return new Nor(part1, inner);
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

        // x A 1 = ~x
        if (Boolean.TRUE.equals(rightVal)) {
            return new Not(leftSimplified).simplify();
        }
        if (Boolean.TRUE.equals(leftVal)) {
            return new Not(rightSimplified).simplify();
        }
        // x A 0 = 1
        if (Boolean.FALSE.equals(rightVal) || Boolean.FALSE.equals(leftVal)) {
            return new Val(true);
        }
        // x A x = ~x
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Not(leftSimplified).simplify();
        }
        // constant folding
        if (leftVal != null && rightVal != null) {
            return new Val(!(leftVal && rightVal));
        }

        return new Nand(leftSimplified, rightSimplified);
    }
}
