import java.util.*;

/**
 * Represents a constant logical value (true or false).
 * Implements the Expression interface.
 */
public class Val implements Expression {
    private final Boolean value;

    /**
     * Constructs a Val expression with the specified boolean value.
     *
     * @param value the boolean value of this expression
     */
    public Val(Boolean value) {
        this.value = value;
    }

    /**
     * Returns the boolean value of this constant expression.
     *
     * @param assignment variable assignments (ignored)
     * @return the boolean value of this expression
     */
    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) {
        return value;
    }

    /**
     * Returns the boolean value of this constant expression.
     *
     * @return the boolean value of this expression
     */
    @Override
    public Boolean evaluate() {
        return value;
    }

    /**
     * Returns an empty list as this expression contains no variables.
     *
     * @return empty list of variables
     */
    @Override
    public List<String> getVariables() {
        return new ArrayList<>();
    }

    /**
     * Returns this expression unchanged because it does not contain variables.
     *
     * @param var        variable name to assign (ignored)
     * @param expression expression to assign (ignored)
     * @return this expression unchanged
     */
    @Override
    public Expression assign(String var, Expression expression) {
        return this;
    }

    /**
     * Returns the string representation of the value: "T" for true, "F" for false.
     *
     * @return string representation of the constant value
     */
    @Override
    public String toString() {
        return value ? "T" : "F";
    }

    /**
     * Returns this constant expression unchanged for nandify conversion.
     *
     * @return this expression unchanged
     */
    @Override
    public Expression nandify() {
        return this;
    }

    /**
     * Returns this constant expression unchanged for norify conversion.
     *
     * @return this expression unchanged
     */
    @Override
    public Expression norify() {
        return this;
    }

    /**
     * Returns this expression as it is already simplified.
     *
     * @return this expression unchanged
     */
    @Override
    public Expression simplify() {
        return this;
    }
}
