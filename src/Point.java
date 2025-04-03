import biuoop.GUI;
import biuoop.DrawSurface;
import java.awt.Color;
import java.util.Random;

public class Point {
    private double x;
    private double y;

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

    // Equals - Returns true if the points are equal, false otherwise
    public boolean equals(Point other) {
        return (this.x == other.x) && (this.y == other.y);
    }

    // Method to draw a small blue circle representing the middle point
    public void drawMiddlePoint(DrawSurface d) {
        d.setColor(Color.BLUE);
        d.fillCircle((int) x, (int) y, 3); // Draw a small circle to represent the point
    }

    // Method to draw a small red circle representing the intersection point
    public void drawIntersectionPoint(DrawSurface d) {
        d.setColor(Color.RED);
        d.fillCircle((int) x, (int) y, 3); // Draw a small circle to represent the point
    }

    // Method to draw a small black circle representing a point on the surface
    public void drawOn(DrawSurface d) {
        d.setColor(Color.BLACK);
        d.fillCircle((int) x, (int) y, 3);
    }
}