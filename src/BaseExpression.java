import java.util.*;

public abstract class BaseExpression implements Expression {
    @Override
    public Boolean evaluate() throws Exception {
        return evaluate(new TreeMap<>()); // empty assignment
    }
}
