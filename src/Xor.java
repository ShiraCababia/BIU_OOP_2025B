import java.util.Map;

public class Xor extends BinaryExpression {
    public Xor(Expression left, Expression right) {
        super(left, right);
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        return getLeft().evaluate(assignment) ^ getRight().evaluate(assignment);
    }

    @Override
    public String toString() {
        return "(" + getLeft().toString() + " ^ " + getRight().toString() + ")";
    }

    @Override
    protected Expression create(Expression left, Expression right) {
        return new Xor(left, right);
    }

    @Override
    public Expression nandify() {
        Expression A = left.nandify();
        Expression B = right.nandify();
        Expression nandAB = new Nand(A, B);
        Expression part1 = new Nand(A, nandAB);
        Expression part2 = new Nand(B, nandAB);
        return new Nand(part1, part2); // (A NAND (A NAND B)) NAND (B NAND (A NAND B))
    }

    @Override
    public Expression norify() {
        Expression A = left.norify();
        Expression B = right.norify();
        Expression part1 = new Nor(A, A);
        Expression part2 = new Nor(B, B);
        Expression part3 = new Nor(A, B);
        return new Nor(new Nor(part1, part2), part3); // ((A NOR A) NOR (B NOR B)) NOR (A NOR B)
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

        // x ^ 0 = x
        if (Boolean.FALSE.equals(rightVal)) {
            return leftSimplified;
        }
        if (Boolean.FALSE.equals(leftVal)) {
            return rightSimplified;
        }
        // x ^ 1 = ~x
        if (Boolean.TRUE.equals(rightVal)) {
            return new Not(leftSimplified).simplify();
        }
        if (Boolean.TRUE.equals(leftVal)) {
            return new Not(rightSimplified).simplify();
        }
        // x ^ x = 0
        if (leftSimplified.toString().equals(rightSimplified.toString())) {
            return new Val(false);
        }
        // constant folding
        if (leftVal != null && rightVal != null) {
            return new Val(leftVal ^ rightVal);
        }

        return new Xor(leftSimplified, rightSimplified);
    }
}
