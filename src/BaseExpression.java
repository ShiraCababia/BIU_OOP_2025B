import java.util.*;;

/**
 * Abstract base class for all logical expressions.
 * Provides common implementation for evaluating expressions without variable
 * assignments.
 */
public abstract class BaseExpression implements Expression {

    /**
     * Evaluates the expression with an empty variable assignment.
     * @return the boolean result of evaluating the expression
     * @throws Exception if the expression contains unassigned variables
     */
    @Override
    public Boolean evaluate() throws Exception {
        return evaluate(new TreeMap<>()); // empty assignment
    }
}
