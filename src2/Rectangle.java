package shapes;
import biuoop.DrawSurface;
import java.awt.*;
import java.util.List;
import java.util.ArrayList;

/**
 * The Rectangle class represents a rectangle defined by its upper-left point,
 * width, and height. It can detect intersections with lines and determine
 * whether a point lies inside it.
 */
public class Rectangle {
    private Point upperLeft;
    private double width, height;

    /**
     * Creates a new rectangle with a specified location and size.
     *
     * @param upperLeft the upper-left corner of the rectangle
     * @param width     the width of the rectangle
     * @param height    the height of the rectangle
     */
    public Rectangle(Point upperLeft, double width, double height) {
        this.upperLeft = upperLeft;
        this.width = width;
        this.height = height;
    }

    /**
     * Returns the width of the rectangle.
     *
     * @return the width
     */
    public double getWidth() {
        return this.width;
    }

    /**
     * Returns the height of the rectangle.
     *
     * @return the height
     */
    public double getHeight() {
        return this.height;
    }

    /**
     * Returns the upper-left point of the rectangle.
     *
     * @return the upper-left point
     */
    public Point getUpperLeft() {
        return this.upperLeft;
    }

    /**
     * Returns a list of all intersection points between the rectangle and the given line.
     * The list can be empty if there are no intersections.
     *
     * @param line the line to check for intersections
     * @return list of intersection points
     */
    public java.util.List<Point> intersectionPoints(Line line) {
        List<Point> intersectionPoints = new ArrayList<>();
        Point upperLeft = this.upperLeft;
        double x = upperLeft.getX();
        double y = upperLeft.getY();
        double width = this.width;
        double height = this.height;
        Line topLine = new Line(x, y, x + width, y);
        Line bottomLine = new Line(x, y + height, x + width, y + height);
        Line leftLine = new Line(x, y, x, y + height);
        Line rightLine = new Line(x + width, y, x + width, y + height);

        Line[] edges = { topLine, bottomLine, leftLine, rightLine };
        for (Line edge : edges) {
            Point intersection = line.intersectionWith(edge);
            if (intersection != null) {
                intersectionPoints.add(intersection);
            }
        }
        return intersectionPoints;
    }

    /**
     * Draws the rectangle on the provided DrawSurface in orange.
     *
     * @param d the drawing surface
     */
    public void drawOn(DrawSurface d) {
        d.setColor(Color.ORANGE);
        d.fillRectangle((int) upperLeft.getX(), (int) upperLeft.getY(), (int) width, (int) height);
    }

    /**
     * Checks whether a given point is inside the rectangle.
     *
     * @param p the point to test
     * @return true if the point is inside, false otherwise
     */
    public boolean isInside(Point p) {
        double x = p.getX();
        double y = p.getY();
        double rx = this.upperLeft.getX();
        double ry = this.upperLeft.getY();
        return (x >= rx && x <= rx + width && y >= ry && y <= ry + height);
    }
}
