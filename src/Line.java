import biuoop.DrawSurface;
import java.awt.*;
import java.util.List;

/**
 * The Line class represents a line segment between two points and provides
 * methods for geometric operations such as finding intersections and checking equality.
 */
public class Line {

    private Point start;
    private Point end;
    private boolean isVertical;
    private Double slope;

    // Constructor that takes two Points (start and end) to define the line

    /**
     * Constructs a line segment from two given points.
     *
     * @param start the start point of the line
     * @param end   the end point of the line
     */
    public Line(Point start, Point end) {
        this.start = start;
        this.end = end;
        this.isVertical = start.getX() == end.getX();
        this.slope = isVertical ? null : (end.getY() - start.getY()) / (end.getX() - start.getX());
    }

    // Constructor that takes four double values (x1, y1, x2, y2) to define the line

    /**
     * Constructs a line segment from x and y coordinates.
     *
     * @param x1 start x-coordinate
     * @param y1 start y-coordinate
     * @param x2 end x-coordinate
     * @param y2 end y-coordinate
     */
    public Line(double x1, double y1, double x2, double y2) {
        this.start = new Point(x1, y1);
        this.end = new Point(x2, y2);
        this.isVertical = start.getX() == end.getX();
        this.slope = isVertical ? null : (end.getY() - start.getY()) / (end.getX() - start.getX());
    }

    /**
     * @return the length of the line segment
     */
    public double length() {
        return start.distance(end);
    }

    /**
     * @return the midpoint of the line segment
     */
    public Point middle() {
        Point middle = new Point((this.start.getX() + this.end.getX()) / 2, (this.start.getY() + this.end.getY()) / 2);
        return middle;
    }

    /**
     * @return the start point of the line
     */
    public Point start() {
        return this.start;
    }

    /**
     * @return the end point of the line
     */
    public Point end() {
        return this.end;
    }

    /**
     * Determines whether this line intersects with another line.
     *
     * @param other the other line
     * @return true if they intersect, false otherwise
     */
    public boolean isIntersecting(Line other) {
        Point intersection = intersectionWith(other);
        if (intersection != null) {
            return true;
        }
        return this.isCollinearAndOverlapping(other);
    }

    // Helper method to check if two lines are collinear and overlapping

    /**
     * Checks whether this line and another are collinear and overlapping.
     *
     * @param other the other line
     * @return true if they are collinear and overlapping, false otherwise
     */
    private boolean isCollinearAndOverlapping(Line other) {
        if (this.slope != null && this.slope.equals(other.slope)) {
            return Math.max(this.start.getX(), this.end.getX()) >= Math.min(other.start.getX(), other.end.getX()) &&
                    Math.min(this.start.getX(), this.end.getX()) <= Math.max(other.start.getX(), other.end.getX()) &&
                    Math.max(this.start.getY(), this.end.getY()) >= Math.min(other.start.getY(), other.end.getY()) &&
                    Math.min(this.start.getY(), this.end.getY()) <= Math.max(other.start.getY(), other.end.getY());
        }
        return false;
    }

    /**
     * Checks whether this line intersects with two other lines.
     *
     * @param other1 the first line
     * @param other2 the second line
     * @return true if this line intersects with both, false otherwise
     */
    public boolean isIntersecting(Line other1, Line other2) {
        return this.isIntersecting(other1) && this.isIntersecting(other2);
    }

    /**
     * Finds the intersection point between this line and another line using Cramer's rule.
     *
     * @param other the other line
     * @return the intersection point, or null if they do not intersect
     */
    public Point intersectionWith(Line other) {
        double p1StartX = this.start.getX(), p1StartY = this.start.getY();
        double p1EndX = this.end.getX(), p1EndY = this.end.getY();
        double p2StartX = other.start.getX(), p2StartY = other.start.getY();
        double p2EndX = other.end.getX(), p2EndY = other.end.getY();
        double denominator = (p1StartX - p1EndX) * (p2StartY - p2EndY) - (p1StartY - p1EndY) * (p2StartX - p2EndX);
        if (denominator == 0) {
            Point sharedEdgePoint = this.intersectWhenDet0(other);
            if (sharedEdgePoint != null) {
                return sharedEdgePoint;
            }
            return null;
        }
        double intersectX = ((p1StartX * p1EndY - p1StartY * p1EndX) * (p2StartX - p2EndX) -
                (p1StartX - p1EndX) * (p2StartX * p2EndY - p2StartY * p2EndX)) / denominator;
        double intersectY = ((p1StartX * p1EndY - p1StartY * p1EndX) * (p2StartY - p2EndY) -
                (p1StartY - p1EndY) * (p2StartX * p2EndY - p2StartY * p2EndX)) / denominator;
        Point intersection = new Point(intersectX, intersectY);
        if (intersectX >= Math.min(p1StartX, p1EndX) && intersectX <= Math.max(p1StartX, p1EndX) &&
                intersectY >= Math.min(p1StartY, p1EndY) && intersectY <= Math.max(p1StartY, p1EndY) &&
                intersectX >= Math.min(p2StartX, p2EndX) && intersectX <= Math.max(p2StartX, p2EndX) &&
                intersectY >= Math.min(p2StartY, p2EndY) && intersectY <= Math.max(p2StartY, p2EndY)) {
            return intersection;
        }
        return null;
    }

    /**
     * Handles edge intersections when the determinant is zero (lines are parallel or coincident).
     *
     * @param other the other line
     * @return the shared edge point if it exists, null otherwise
     */
    public Point intersectWhenDet0(Line other) {
        if ((start.equals(other.start) && (!this.isPointOnLine(other.end))) ||
                (start.equals(other.end) && (!this.isPointOnLine(other.start)))) {
            return start;
        }
        if ((end.equals(other.start) && (!this.isPointOnLine(other.end))) ||
                (end.equals(other.end) && (!this.isPointOnLine(other.start)))) {
            return end;
        }
        return null;
    }

    /**
     * Checks whether this line is equal to another line segment.
     *
     * @param other the other line
     * @return true if the lines are equal (regardless of direction), false otherwise
     */
    public boolean equals(Line other) {
        return (this.start.equals(other.start) && this.end.equals(other.end)) ||
                (this.start.equals(other.end) && this.end.equals(other.start));
    }

    // Helper method to check if a point lies on the line segment

    /**
     * Determines whether a given point lies on this line segment.
     *
     * @param point the point to check
     * @return true if the point is on the line, false otherwise
     */
    private boolean isPointOnLine(Point point) {
        double minX = Math.min(start.getX(), end.getX());
        double maxX = Math.max(start.getX(), end.getX());
        double minY = Math.min(start.getY(), end.getY());
        double maxY = Math.max(start.getY(), end.getY());
        if (point.getX() < minX || point.getX() > maxX || point.getY() < minY || point.getY() > maxY) {
            return false;
        }
        double dx = end.getX() - start.getX();
        double dy = end.getY() - start.getY();
        if (dx == 0) {
            return point.getX() == start.getX();
        }
        if (dy == 0) {
            return point.getY() == start.getY();
        }
        double slope = dy / dx;
        double pointSlope = (point.getY() - start.getY()) / (point.getX() - start.getX());
        return Math.abs(slope - pointSlope) < 0.00001;
    }

    /**
     * Draws the line on the provided DrawSurface.
     *
     * @param d the DrawSurface to draw on
     */
    public void drawOn(DrawSurface d) {
        d.setColor(Color.BLACK);
        d.drawLine((int) start.getX(), (int) start.getY(),
                (int) end.getX(), (int) end.getY());
    }

    /**
     * Finds the closest intersection point between the line and the rectangle.
     *
     * @param rectangle the rectangle to test against
     * @return the closest intersection point to the start of the line, or null if none exist
     */
    public Point closestIntersectionToStartOfLine(Rectangle rectangle) {
        List<Point> intersectionPoints = rectangle.intersectionPoints(this);
        if (intersectionPoints.isEmpty()) {
            return null;
        }

        Point closestIntersection = intersectionPoints.get(0);
        double minDistance = this.start.distance(closestIntersection);

        for (Point p : intersectionPoints) {
            double distance = this.start.distance(p);
            if (distance < minDistance) {
                closestIntersection = p;
                minDistance = distance;
            }
        }
        return closestIntersection;
    }
}
