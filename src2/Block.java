package shapes;

import biuoop.DrawSurface;
import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import core.*;

/**
 * The Block class represents a rectangular block in the game.
 * A block can be collided with and can be drawn on the screen.
 */
public class Block implements Collidable, Sprite, HitNotifier {

    private Rectangle rectangle;
    private Color color = java.awt.Color.GRAY;
    private List<HitListener> hitListeners = new ArrayList<>();

    /**
     * Constructor.
     * Creates a block with the given rectangle and default gray color.
     *
     * @param rectangle the rectangle defining the block's position and size
     */
    public Block(Rectangle rectangle) {
        this.rectangle = rectangle;
    }

    /**
     * Constructor.
     * Creates a block with the given rectangle and color.
     *
     * @param rectangle the rectangle defining the block's position and size
     * @param color     the fill color of the block
     */
    public Block(Rectangle rectangle, Color color) {
        this.rectangle = rectangle;
        this.color = color;
    }

    /**
     * Returns the color of the block.
     *
     * @return the block's color
     */
    public Color getColor() {
        return this.color;
    }

    /**
     * Returns the shape (rectangle) of the block used for collision detection.
     *
     * @return the block's collision rectangle
     */
    public Rectangle getCollisionRectangle() {
        return this.rectangle;
    }

    /**
     * Handles the logic for what happens when the ball hits the block.
     * Reverses the velocity direction based on the edge of the block hit.
     *
     * @param collisionPoint  the point where the collision occurred
     * @param currentVelocity the current velocity of the ball
     * @return the new velocity after the collision
     */
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

    /**
     * Draws the block on the given DrawSurface.
     *
     * @param surface the drawing surface
     */
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

    /**
     * This method is called once per frame.
     * Currently, the block does not change over time, so this is empty.
     */
    public void timePassed() {
    }

    /**
     * Adds the block to the game, registering it as both a sprite and a collidable.
     *
     * @param g the Game instance
     */
    public void addToGame(Game g) {
        g.addSprite(this);
        g.addCollidable(this);
    }

    public boolean ballColorMatch(Ball ball) {
        return this.color.equals(ball.getColor());
    }

    public void removeFromGame(Game game) {
        game.removeCollidable(this);
        game.removeSprite(this);
    }

    public void addHitListener(HitListener hl) {
        this.hitListeners.add(hl);
    }

    // Remove hl from the list of listeners to hit events.
    public void removeHitListener(HitListener hl) {
        this.hitListeners.remove(hl);
    }

    // This method is called whenever the beingHit object is hit.
    // The hitter parameter is the Ball that's doing the hitting.
    void hitEvent(Block beingHit, Ball hitter) {

    }

    private void notifyHit(Ball hitter) {
        // Make a copy of the hitListeners before iterating over them.
        List<HitListener> listeners = new ArrayList<HitListener>(this.hitListeners);
        // Notify all listeners about a hit event:
        for (HitListener hl : listeners) {
            hl.hitEvent(this, hitter);
        }
    }

    // Notice that we changed the hit method to include a "Ball hitter" parameter --
    // update the
    // Collidable interface accordingly.
    public Velocity hit(Ball hitter, Point collisionPoint, Velocity currentVelocity) {
        Velocity newVel = this.hit(collisionPoint, currentVelocity); // Call original logic
        if (!ballColorMatch(hitter)) {
            hitter.setColor(this.color); // Ball takes the block's color
            this.notifyHit(hitter); // Notify listeners, so they can handle removal
        }
        return newVel;
    }

}
