package shapes;
/**
 * The Collidable interface should be implemented by any object
 * that can be collided with, such as blocks or the paddle.
 */
public interface Collidable {

    /**
     * Returns the "collision shape" of the object.
     * This is typically a rectangle used for detecting collisions.
     *
     * @return the Rectangle representing the shape of the collidable object
     */
    Rectangle getCollisionRectangle();

    /**
     * Notifies the object that a collision occurred at a specific point
     * with a given velocity. The object calculates the resulting velocity
     * after the collision, based on its own physical properties.
     *
     * @param collisionPoint  the point where the collision occurred
     * @param currentVelocity the current velocity of the object hitting it
     * @return the new velocity after the collision
     */
    Velocity hit(Point collisionPoint, Velocity currentVelocity);
}
