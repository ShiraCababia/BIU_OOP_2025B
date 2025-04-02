
import biuoop.DrawSurface;
import java.awt.*;

public class Line {

    private Point start;
    private Point end;
    private boolean isHorizontal;
    private boolean isVertical;
    private Double slope;
    private Double yIntercept;

    public Line(Point start, Point end) {
        this.start = start;
        this.end = end;
        this.isHorizontal = start.getY() == end.getY();
        this.isVertical = start.getX() == end.getX();
        this.slope = isVertical ? null : (end.getY() - start.getY()) / (end.getX() - start.getX());
        this.yIntercept = isVertical ? null : start.getY() - slope * start.getX();
    }

    public Line(double x1, double y1, double x2, double y2) {
        ;
    }

    // Return the length of the line
    public double length() {
        return 0;
    }

    // Returns the middle point of the line
    public Point middle() {
        return null;
    }

    // Returns the start point of the line
    public Point start() {
        return null;
    }

    // Returns the end point of the line
    public Point end() {
        return null;
    }

    public boolean isIntersecting(Line other) {
        Point intersection = intersectionWith(other);
        return intersection != null && isPointOnLine(intersection) && other.isPointOnLine(intersection);
    }

    // Returns true if this 2 lines intersect with this line, false otherwise
    public boolean isIntersecting(Line other1, Line other2) {
        return true;
    }

    public Point intersectionWith(Line other) {
        if (this.equals(other)) {
            return null;
        }
        if (this.isVertical && other.isVertical) {
            return null;
        }
        if (this.isHorizontal && other.isHorizontal) {
            return null;
        }
        if (this.slope != null && this.slope.equals(other.slope)) {
            return null;
        }
        double x, y;
        if (this.isVertical) {
            x = this.start.getX();
            y = other.slope * x + other.yIntercept;
        } else if (other.isVertical) {
            x = other.start.getX();
            y = this.slope * x + this.yIntercept;
        } else {
            double m1 = this.slope;
            double b1 = this.yIntercept;
            double m2 = other.slope;
            double b2 = other.yIntercept;
            x = (b2 - b1) / (m1 - m2);
            y = m1 * x + b1;
        }
        Point intersection = new Point(x, y);
        if (isPointOnLine(intersection) && other.isPointOnLine(intersection)) {
            return intersection;
        }
        return null;
    }

    // equals -- return true if the lines are equal, false otherwise
    // public boolean equals(Line other) { }

    private boolean isPointOnLine(Point point) {
        double minX = Math.min(start.getX(), end.getX());
        double maxX = Math.max(start.getX(), end.getX());
        double minY = Math.min(start.getY(), end.getY());
        double maxY = Math.max(start.getY(), end.getY());
        return point.getX() >= minX && point.getX() <= maxX && point.getY() >= minY && point.getY() <= maxY;
    }

    public void drawOn(DrawSurface d) {
        d.setColor(Color.BLUE);
        d.drawLine((int) start.getX(), (int) start.getY(),
                (int) end.getX(), (int) end.getY());
    }
}
