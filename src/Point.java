import biuoop.DrawSurface;
import java.awt.Color;

/**
 * The Point class represents a point in 2D space and provides methods for
 * calculating distances, equality, and drawing the point for visualization.
 */
public class Point {
    private double x;
    private double y;
    private static final int RADIUS_POINT = 3;
    private static final double ERROR_RANGE = 0.00001;

    /**
     * Constructs a Point with the specified coordinates.
     *
     * @param x the x-coordinate
     * @param y the y-coordinate
     */
    public Point(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Returns the x-coordinate of the point.
     *
     * @return x value
     */
    public double getX() {
        return x;
    }

    /**
     * Returns the y-coordinate of the point.
     *
     * @return y value
     */
    public double getY() {
        return y;
    }

    /**
     * Calculates the distance between this point and another.
     *
     * @param other the other point
     * @return the Euclidean distance between the two points
     */
    public double distance(Point other) {
        return Math.sqrt((this.x - other.x) * (this.x - other.x) + (this.y - other.y) * (this.y - other.y));
    }

    /**
     * Checks whether this point is approximately equal to another,
     * within a small margin of error.
     *
     * @param other the other point
     * @return true if the points are equal within error range, false otherwise
     */
    public boolean equals(Point other) {
        double deltaX = this.x - other.getX();
        double deltaY = this.y - other.getY();
        return Math.abs(deltaX) <= ERROR_RANGE && Math.abs(deltaY) <= ERROR_RANGE;
    }

    /**
     * Draws a small blue circle representing the middle point on the surface.
     *
     * @param d the DrawSurface to draw on
     */
    public void drawMiddlePoint(DrawSurface d) {
        d.setColor(Color.BLUE);
        d.fillCircle((int) x, (int) y, RADIUS_POINT); // Draw a small circle to represent the point
    }

    /**
     * Draws a small red circle representing an intersection point on the surface.
     *
     * @param d the DrawSurface to draw on
     */
    public void drawIntersectionPoint(DrawSurface d) {
        d.setColor(Color.RED);
        d.fillCircle((int) x, (int) y, RADIUS_POINT); // Draw a small circle to represent the point
    }

    /**
     * Draws a small black circle representing the point on the surface.
     *
     * @param d the DrawSurface to draw on
     */
    public void drawOn(DrawSurface d) {
        d.setColor(Color.BLACK);
        d.fillCircle((int) x, (int) y, RADIUS_POINT);
    }
}
