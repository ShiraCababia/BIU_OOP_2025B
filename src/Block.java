import biuoop.DrawSurface;
import java.awt.Color;

public class Block implements Collidable, Sprite {

    private Rectangle rectangle;
    private Color color = java.awt.Color.GRAY;

    public Block(Rectangle rectangle) {
        this.rectangle = rectangle;
    }

    public Block(Rectangle rectangle, Color color) {
        this.rectangle = rectangle;
        this.color = color;
    }

    public Rectangle getCollisionRectangle() {
        return this.rectangle;
    }

    public Velocity hit(Point collisionPoint, Velocity currentVelocity) {
        double dx = currentVelocity.getDx();
        double dy = currentVelocity.getDy();
        boolean hitTopOrBottom = false;
        boolean hitLeftOrRight = false;
        double left = rectangle.getUpperLeft().getX();
        double right = left + rectangle.getWidth();
        double top = rectangle.getUpperLeft().getY();
        double bottom = top + rectangle.getHeight();
        double x = collisionPoint.getX();
        double y = collisionPoint.getY();

        if (Math.abs(y - top) < 0.0001 || Math.abs(y - bottom) < 0.0001) {
            hitTopOrBottom = true;
        }
        if (Math.abs(x - left) < 0.0001 || Math.abs(x - right) < 0.0001) {
            hitLeftOrRight = true;
        }
        if (hitTopOrBottom) {
            dy = -dy;
        }
        if (hitLeftOrRight) {
            dx = -dx;
        }
        return new Velocity(dx, dy);
    }

    // Method to draw the block on the given DrawSurface
    public void drawOn(DrawSurface surface) {
        surface.setColor(this.color);
        int x = (int) this.rectangle.getUpperLeft().getX();
        int y = (int) this.rectangle.getUpperLeft().getY();
        int width = (int) this.rectangle.getWidth();
        int height = (int) this.rectangle.getHeight();
        surface.fillRectangle(x, y, width, height);
        surface.setColor(Color.BLACK);
        surface.drawRectangle(x, y, width, height);
    }

    public void timePassed() {
    }

    public void addToGame(Game g) {
        g.addSprite(this);
        g.addCollidable(this);
    }

}
