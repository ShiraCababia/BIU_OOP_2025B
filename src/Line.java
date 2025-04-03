
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
        this.start = new Point(x1, y1);
        this.end = new Point(x2, y2);
        this.isHorizontal = start.getY() == end.getY();
        this.isVertical = start.getX() == end.getX();
        this.slope = isVertical ? null : (end.getY() - start.getY()) / (end.getX() - start.getX());
        this.yIntercept = isVertical ? null : start.getY() - slope * start.getX();
    }

    // Return the length of the line
    public double length() {
        return Math.sqrt((this.start.getX() - this.end.getX()) * (this.start.getX() - this.end.getX()) +
                (this.start.getY() - this.end.getY()) * (this.start.getY() - this.end.getY()));
    }

    // Returns the middle point of the line
    public Point middle() {
        Point middle = new Point((this.start.getX() + this.end.getX()) / 2, (this.start.getY() + this.end.getY()) / 2);
        return middle;
    }

    // Returns the start point of the line
    public Point start() {
        return this.start;
    }

    // Returns the end point of the line
    public Point end() {
        return this.end;
    }

    public boolean isIntersecting(Line other) {
        Point intersection = intersectionWith(other);
        return intersection != null && isPointOnLine(intersection) && other.isPointOnLine(intersection);
    }

    // Returns true if this 2 lines intersect with this line, false otherwise
    public boolean isIntersecting(Line other1, Line other2) {
        return this.isIntersecting(other1) && this.isIntersecting(other2);
    }

    public Point intersectionWith(Line other) {
        double p1StartX = this.start.getX(), p1StartY = this.start.getY();
        double p1EndX = this.end.getX(), p1EndY = this.end.getY();
        double p2StartX = other.start.getX(), p2StartY = other.start.getY();
        double p2EndX = other.end.getX(), p2EndY = other.end.getY();
        // Compute the determinant (denominator of the intersection formulas)
        double denominator = (p1StartX - p1EndX) * (p2StartY - p2EndY) - (p1StartY - p1EndY) * (p2StartX - p2EndX);
        // If the determinant is 0 the lines are either parallel or coincident, and if
        // there's no shared edge point - there is no intersection. */
        Point sharedEdgePoint = this.sharedEdgePoint(other);
        if (denominator == 0 && sharedEdgePoint == null) {
            return null;
        }
        if (sharedEdgePoint != null) {
            return sharedEdgePoint;
        }
        // Compute the intersection point using Cramer's rule
        double intersectX = ((p1StartX * p1EndY - p1StartY * p1EndX) * (p2StartX - p2EndX) -
                (p1StartX - p1EndX) * (p2StartX * p2EndY - p2StartY * p2EndX)) / denominator;
        double intersectY = ((p1StartX * p1EndY - p1StartY * p1EndX) * (p2StartY - p2EndY) -
                (p1StartY - p1EndY) * (p2StartX * p2EndY - p2StartY * p2EndX)) / denominator;
        Point intersection = new Point(intersectX, intersectY);
        // Check if the intersection point lies within both line segments
        if (intersectX >= Math.min(p1StartX, p1EndX) && intersectX <= Math.max(p1StartX, p1EndX) &&
                intersectY >= Math.min(p1StartY, p1EndY) && intersectY <= Math.max(p1StartY, p1EndY) &&
                intersectX >= Math.min(p2StartX, p2EndX) && intersectX <= Math.max(p2StartX, p2EndX) &&
                intersectY >= Math.min(p2StartY, p2EndY) && intersectY <= Math.max(p2StartY, p2EndY)) {
            return intersection;
        }
        // The intersection point is outside the segments' range
        return null;
    }

    public Point sharedEdgePoint(Line other) {
        if (start.equals(other.start) || start.equals(other.end)) {
            return start;
        }
        if (end.equals(other.start) || end.equals(other.end)) {
            return end;
        }
        return null;
    }

    // Equals -- Returns true if the lines are equal, false otherwise
    public boolean equals(Line other) {
        return (this.start.equals(other.start) && this.end.equals(other.end)) ||
                (this.start.equals(other.end) && this.end.equals(other.start));
    }

    private boolean isPointOnLine(Point point) {
        // Calculate the boundaries of the line (minimum and maximum X and Y values)
        double minX = Math.min(start.getX(), end.getX());
        double maxX = Math.max(start.getX(), end.getX());
        double minY = Math.min(start.getY(), end.getY());
        double maxY = Math.max(start.getY(), end.getY());
        // Check if the point is within the bounds of the line segment
        if (point.getX() < minX || point.getX() > maxX || point.getY() < minY || point.getY() > maxY) {
            return false;
        }
        // Calculate the differences in X and Y between the start and end points
        double dx = end.getX() - start.getX();
        double dy = end.getY() - start.getY();
        // Handle vertical and horizontal lines separately
        if (dx == 0) {
            return point.getX() == start.getX();
        }
        if (dy == 0) {
            return point.getY() == start.getY();
        }
        // Calculate the slopes of the line and the point, and check if they are
        // approximately equal.
        double slope = dy / dx;
        double pointSlope = (point.getY() - start.getY()) / (point.getX() - start.getX());
        // If the slopes match (with a small margin for floating point precision), the
        // point is on the line.
        return Math.abs(slope - pointSlope) < 0.000001;
    }

    public void drawOn(DrawSurface d) {
        d.setColor(Color.BLACK);
        d.drawLine((int) start.getX(), (int) start.getY(),
                (int) end.getX(), (int) end.getY());
    }

    public void drawLineForTriangle(DrawSurface d) {
        d.setColor(Color.GREEN);
        d.drawLine((int) start.getX(), (int) start.getY(),
                (int) end.getX(), (int) end.getY());
    }
}
