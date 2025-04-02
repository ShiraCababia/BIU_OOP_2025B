import biuoop.DrawSurface;

import java.awt.*;

// Rectangle Class
class Rectangle {
    private Point topLeft;
    private int width, height;

    public Rectangle(Point topLeft, int width, int height) {
        this.topLeft = topLeft;
        this.width = width;
        this.height = height;
    }

    public void drawOn(DrawSurface d) {
        d.setColor(Color.ORANGE);
        d.fillRectangle((int) topLeft.getX(), (int) topLeft.getY(), width, height);
    }
}