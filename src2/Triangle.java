import biuoop.DrawSurface;

import java.awt.*;

// Triangle Class
class Triangle {
    private Point p1, p2, p3;

    public Triangle(Point p1, Point p2, Point p3) {
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
    }

    public void drawOn(DrawSurface d) {
        d.setColor(Color.GREEN);
        d.drawLine((int) p1.getX(), (int) p1.getY(), (int) p2.getX(), (int) p2.getY());
        d.drawLine((int) p2.getX(), (int) p2.getY(), (int) p3.getX(), (int) p3.getY());
        d.drawLine((int) p3.getX(), (int) p3.getY(), (int) p1.getX(), (int) p1.getY());
    }
}
