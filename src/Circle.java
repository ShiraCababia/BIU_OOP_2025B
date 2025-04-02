import biuoop.DrawSurface;

import java.awt.*;

// Circle Class
class Circle {
    private Point center;
    private int radius;

    public Circle(Point center, int radius) {
        this.center = center;
        this.radius = radius;
    }

    public void drawOn(DrawSurface d) {
        d.setColor(Color.RED);
        d.fillCircle((int) center.getX(), (int) center.getY(), radius);
    }
}
