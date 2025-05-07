import biuoop.DrawSurface;
import java.awt.*;
import java.util.List;
import java.util.ArrayList;

// Rectangle Class
class Rectangle {
    private Point upperLeft;
    private double width, height;

    // Create a new rectangle with location and width/height.
    public Rectangle(Point upperLeft, double width, double height) {
        this.upperLeft = upperLeft;
        this.width = width;
        this.height = height;
    }

    // Return the width of the rectangle.
    public double getWidth() {
        return this.width;
    }

    // Return the height of the rectangle.
    public double getHeight() {
        return this.height;
    }

    // Returns the upper-left point of the rectangle.
    public Point getUpperLeft() {
        return this.upperLeft;
    }

    // Return a (possibly empty) List of intersection points
    // with the specified line.
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

    public void drawOn(DrawSurface d) {
        d.setColor(Color.ORANGE);
        d.fillRectangle((int) upperLeft.getX(), (int) upperLeft.getY(), (int) width, (int) height);
    }

    public boolean isInside(Point p) {
        double x = p.getX();
        double y = p.getY();
        double rx = this.upperLeft.getX();
        double ry = this.upperLeft.getY();
        return (x >= rx && x <= rx + width && y >= ry && y <= ry + height);
    }

}