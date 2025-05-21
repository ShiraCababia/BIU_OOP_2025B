import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * Represents a variable in a logical expression.
 * Implements the Expression interface.
 */
public class Var implements Expression {
    private final String name;

    /**
     * Constructs a Var expression with the specified variable name.
     *
     * @param name the name of the variable
     */
    public Var(String name) {
        this.name = name;
    }

    /**
     * Evaluates the variable using the provided variable assignment map.
     *
     * @param assignment a map from variable names to boolean values
     * @return the boolean value assigned to this variable
     * @throws Exception if the variable name is not found in the assignment map
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        if (!assignment.containsKey(name)) {
            throw new Exception("Variable '" + name + "' not found in assignment.");
        }
        return assignment.get(name);
    }

    /**
     * Throws an exception since a variable cannot be evaluated without an
     * assignment.
     *
     * @return never returns normally
     * @throws Exception always throws because evaluation without assignment is
     *                   invalid
     */
    @Override
    public Boolean evaluate() throws Exception {
        throw new Exception("Cannot evaluate variable '" + name + "' without assignment.");
    }

    /**
     * Returns a list containing the name of this variable.
     *
     * @return list of variables containing only this variable's name
     */
    @Override
    public List<String> getVariables() {
        List<String> vars = new ArrayList<>();
        vars.add(name);
        return vars;
    }

    /**
     * Returns a new expression where occurrences of the variable named var
     * are replaced with the provided expression.
     *
     * @param var        the variable name to replace
     * @param expression the expression to substitute in place of the variable
     * @return the substituted expression or this if the variable names don't match
     */
    @Override
    public Expression assign(String var, Expression expression) {
        if (this.name.equals(var)) {
            return expression;
        }
        return this;
    }

    /**
     * Returns the string representation of the variable.
     *
     * @return the variable's name as string
     */
    @Override
    public String toString() {
        return name;
    }

    /**
     * Returns this variable unchanged in the nandify conversion.
     *
     * @return this expression unchanged
     */
    @Override
    public Expression nandify() {
        return this;
    }

    /**
     * Returns this variable unchanged in the norify conversion.
     *
     * @return this expression unchanged
     */
    @Override
    public Expression norify() {
        return this;
    }

    /**
     * Returns this variable unchanged since no simplification is possible.
     *
     * @return this expression unchanged
     */
    @Override
    public Expression simplify() {
        return this;
    }
}
