package shapes;

/**
 * The CollisionInfo class stores information about a collision event,
 * including the point of collision and the object involved.
 */
public class CollisionInfo {

    private Point collisionPoint;
    private Collidable collisionObject;

    /**
     * Constructor.
     * Initializes the collision information with a point and the collidable object.
     *
     * @param point  the point where the collision occurs
     * @param object the object that is collided with
     */
    public CollisionInfo(Point point, Collidable object) {
        this.collisionPoint = point;
        this.collisionObject = object;
    }

    /**
     * Returns the point at which the collision occurs.
     *
     * @return the collision point
     */
    public Point collisionPoint() {
        return this.collisionPoint;
    }

    /**
     * Returns the collidable object involved in the collision.
     *
     * @return the object collided with
     */
    public Collidable collisionObject() {
        return this.collisionObject;
    }
}
