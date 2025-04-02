import biuoop.DrawSurface;

import java.awt.*;

// Square Class
class Square {
    private Point topLeft;
    private int size;

    public Square(Point topLeft, int size) {
        this.topLeft = topLeft;
        this.size = size;
    }

    public void drawOn(DrawSurface d) {
        d.setColor(Color.GREEN);
        d.drawRectangle((int) topLeft.getX(), (int) topLeft.getY(), size, size);
    }
}