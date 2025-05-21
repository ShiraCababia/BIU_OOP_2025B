import java.util.*;

public class Val implements Expression {
    private final Boolean value;

    public Val(Boolean value) {
        this.value = value;
    }

    @Override
    public Boolean evaluate(Map<String, Boolean> assignment) {
        return value;
    }

    @Override
    public Boolean evaluate() {
        return value;
    }

    @Override
    public List<String> getVariables() {
        return new ArrayList<>();
    }

    @Override
    public Expression assign(String var, Expression expression) {
        return this;
    }

    @Override
    public String toString() {
        return value ? "T" : "F";
    }

    @Override
    public Expression nandify() {
        return this; // constants remain the same
    }

    @Override
    public Expression norify() {
        return this; // constants remain the same
    }

    @Override
    public Expression simplify() {
        return this; // already simplest
    }
}
