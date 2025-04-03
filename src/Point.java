import biuoop.DrawSurface;
import java.awt.Color;

public class Point {
    private double x;
    private double y;
    private static final int RADIUS_POINT = 3;
    private static final double ERROR_RANGE = 0.00001;

    // Constructs a Point with the specified coordinates.
    public Point(double x, double y) {
        this.x = x;
        this.y = y;
    }

    // Returns the x-coordinate of the point.
    public double getX() {
        return x;
    }

    // Returns the y-coordinate of the point.
    public double getY() {
        return y;
    }

    // Distance - Return the distance of this point to the other point
    public double distance(Point other) {
        return Math.sqrt((this.x - other.x) * (this.x - other.x) + (this.y - other.y) * (this.y - other.y));
    }

    // Equals - Returns true if the points are equal (considering minor percentage
    // deviation), false otherwise
    public boolean equals(Point other) {
        double deltaX = this.x - other.getX();
        double deltaY = this.y - other.getY();
        return Math.abs(deltaX) <= ERROR_RANGE && Math.abs(deltaY) <= ERROR_RANGE;
    }

    // Method to draw a small blue circle representing the middle point
    public void drawMiddlePoint(DrawSurface d) {
        d.setColor(Color.BLUE);
        d.fillCircle((int) x, (int) y, RADIUS_POINT); // Draw a small circle to represent the point
    }

    // Method to draw a small red circle representing the intersection point
    public void drawIntersectionPoint(DrawSurface d) {
        d.setColor(Color.RED);
        d.fillCircle((int) x, (int) y, RADIUS_POINT); // Draw a small circle to represent the point
    }

    // Method to draw a small black circle representing a point on the surface
    public void drawOn(DrawSurface d) {
        d.setColor(Color.BLACK);
        d.fillCircle((int) x, (int) y, RADIUS_POINT);
    }
}