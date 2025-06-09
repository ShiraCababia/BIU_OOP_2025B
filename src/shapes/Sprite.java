package shapes;
import biuoop.DrawSurface;

/**
 * The Sprite interface should be implemented by all game objects
 * that can be drawn to the screen and updated as time progresses.
 */
public interface Sprite {

    /**
     * Draws the sprite on the given drawing surface.
     *
     * @param d the surface to draw the sprite on
     */
    void drawOn(DrawSurface d);

    /**
     * Notifies the sprite that a unit of time has passed.
     * This method should update the sprite's state accordingly.
     */
    void timePassed();
}
