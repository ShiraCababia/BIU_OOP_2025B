/**
 * The Velocity class specifies the change in position on the x and y axes.
 * It can be applied to points and constructed from an angle and speed.
 */
public class Velocity {
    private double dx;
    private double dy;

    /**
     * Constructs a Velocity object with the specified dx and dy components.
     *
     * @param dx change in x-axis
     * @param dy change in y-axis
     */
    public Velocity(double dx, double dy) {
        this.dx = dx;
        this.dy = dy;
    }

    /**
     * @return the horizontal component (dx) of the velocity
     */
    public double getDx() {
        return this.dx;
    }

    /**
     * @return the vertical component (dy) of the velocity
     */
    public double getDy() {
        return this.dy;
    }

    /**
     * Creates a Velocity instance from an angle and a speed.
     * Angle is in degrees where 0 is "up".
     *
     * @param angle the direction in degrees
     * @param speed the speed (magnitude of the velocity)
     * @return a Velocity object representing the direction and speed
     */
    public static Velocity fromAngleAndSpeed(double angle, double speed) {
        double radians = Math.toRadians(angle);
        double dx = speed * Math.sin(radians);
        double dy = -speed * Math.cos(radians);
        return new Velocity(dx, dy);
    }

    /**
     * Applies the velocity to a given point to compute the next position.
     *
     * @param p the point to which the velocity is applied
     * @return a new Point after applying the velocity
     */
    public Point applyToPoint(Point p) {
        return new Point(p.getX() + this.dx, p.getY() + this.dy);
    }
}
