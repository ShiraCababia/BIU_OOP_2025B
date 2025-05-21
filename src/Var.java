import java.util.*;

public class Var implements Expression {
    private final String name;

    public Var(String name) {
        this.name = name;
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) throws Exception {
        if (!assignment.containsKey(name)) {
            throw new Exception("Variable '" + name + "' not found in expression.");
        }
        return assignment.get(name);
    }

    @Override
    public Boolean evaluate() throws Exception {
        throw new Exception("Cannot evaluate variable '" + name + "' without expression.");
    }

    @Override
    public List<String> getVariables() {
        List<String> vars = new ArrayList<>();
        vars.add(name);
        return vars;
    }

    @Override
    public Expression assign(String var, Expression expression) {
        if (this.name.equals(var)) {
            return expression;
        }
        return this;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public Expression nandify() {
        return this; // variable remains the same
    }

    @Override
    public Expression norify() {
        return this; // variable remains the same
    }

    @Override
    public Expression simplify() {
        return this; // no simplification possible
    }
}
