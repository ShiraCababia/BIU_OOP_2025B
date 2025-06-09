package shapes;

import biuoop.DrawSurface;
import java.util.List;
import java.util.ArrayList;

/**
 * The SpriteCollection class manages a list of Sprite objects.
 * It allows updating and drawing all sprites in the collection.
 */
public class SpriteCollection {

    private List<Sprite> sprites;

    /**
     * Constructs a new, empty SpriteCollection.
     */
    public SpriteCollection() {
        this.sprites = new ArrayList<>();
    }

    /**
     * Adds a sprite to the collection.
     *
     * @param s the sprite to add
     */
    public void addSprite(Sprite s) {
        this.sprites.add(s);
    }

    /**
     * Calls timePassed() on all sprites in the collection,
     * allowing them to update their state.
     */
    public void notifyAllTimePassed() {
        for (Sprite s : sprites) {
            s.timePassed();
        }
    }

    /**
     * Calls drawOn(d) on all sprites in the collection,
     * rendering them to the provided DrawSurface.
     *
     * @param d the drawing surface
     */
    public void drawAllOn(DrawSurface d) {
        for (Sprite s : sprites) {
            s.drawOn(d);
        }
    }

    public void removeSprite(Sprite s) {
        this.sprites.remove(s);
    }

}
