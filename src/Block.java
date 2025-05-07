import biuoop.DrawSurface;

public class Block implements Collidable, Sprite {

    private Rectangle rectangle;

    public Block(Rectangle rectangle) {
        this.rectangle = rectangle;
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
        // Set color (you can customize the color)
        surface.setColor(java.awt.Color.GRAY); // You can choose a different color if needed
        // Fill the rectangle (draw it on the surface)
        surface.fillRectangle((int) this.rectangle.getUpperLeft().getX(),
                (int) this.rectangle.getUpperLeft().getY(),
                (int) this.rectangle.getWidth(),
                (int) this.rectangle.getHeight());
    }

    public void timePassed() {

    }

    public void addToGame(Game g) {
        g.addSprite(this);
        g.addCollidable(this);
    }

}
