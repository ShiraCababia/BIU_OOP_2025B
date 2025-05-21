import biuoop.DrawSurface;
import java.awt.*;

/**
 * The Square class represents a green square that can be drawn on the screen.
 */
class Square {
    private Point topLeft;
    private int size;

    /**
     * Constructs a square given its top-left corner and size.
     *
     * @param topLeft the top-left point of the square
     * @param size    the length of the sides of the square
     */
    public Square(Point topLeft, int size) {
        this.topLeft = topLeft;
        this.size = size;
    }

    /**
     * Draws the square on the provided DrawSurface in green.
     *
     * @param d the drawing surface
     */
    public void drawOn(DrawSurface d) {
        d.setColor(Color.GREEN);
        d.drawRectangle((int) topLeft.getX(), (int) topLeft.getY(), size, size);
    }
}
