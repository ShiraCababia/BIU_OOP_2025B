import biuoop.GUI;
import biuoop.DrawSurface;
import java.awt.Color;
import java.util.Random;

// The Point class represents a point in a 2D space.
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

    // distance -- return the distance of this point to the other point
    public double distance(Point other) {
        return Math.sqrt((this.x - other.x) * (this.x - other.x) + (this.y - other.y) * (this.y - other.y));
    }

    // equals -- return true is the points are equal, false otherwise
    public boolean equals(Point other) {
        return true;
    }

    //
    public void drawOn(DrawSurface d) {
        d.setColor(Color.MAGENTA);
        d.fillCircle((int) x, (int) y, 3); // Draw a small circle to represent the point
    }

}