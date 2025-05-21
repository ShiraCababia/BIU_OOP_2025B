import java.util.List;
import java.util.Map;

/**
 * Represents a logical expression that can be evaluated, assigned variables,
 * converted to NAND/NOR logic, simplified, and represented as a string.
 */
public interface Expression {
    /**
     * Evaluates the expression using the given variable assignment.
     * @param assignment a map of variable names to their boolean values
     * @return the boolean result of evaluating the expression
     * @throws Exception if the expression contains a variable not in the assignment
     */
    Boolean evaluate(Map<String, Boolean> assignment) throws Exception;

    /**
     * Evaluates the expression without any variable assignments.
     * @return the boolean result of evaluating the expression.
     * @throws Exception if the expression contains variables (not assigned)
     */
    Boolean evaluate() throws Exception;

    /**
     * Returns a list of all variables used in the expression.
     * @return list of variable names as strings.
     */
    List<String> getVariables();

    /**
     * Returns a string representation of the expression.
     * @return string form of the expression
     */
    String toString();

    /**
     * Returns a new expression where all occurrences of the specified variable
     * are replaced with the provided expression.
     * @param var        the variable name to replace
     * @param expression the expression to replace the variable with
     * @return a new expression with the variable replaced
     */
    Expression assign(String var, Expression expression);

    /**
     * Converts the expression to an equivalent expression using only NAND
     * operations.
     * @return the NAND-only expression tree
     */
    Expression nandify();

    /**
     * Converts the expression to an equivalent expression using only NOR
     * operations.
     * @return the NOR-only expression tree
     */
    Expression norify();

    /**
     * Returns a simplified version of this expression, applying logical
     * simplifications.
     * @return the simplified expression
     */
    Expression simplify();
}
