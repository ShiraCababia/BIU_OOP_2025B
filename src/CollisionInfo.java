public class CollisionInfo {

    private Point collisionPoint;
    private Collidable collisionObject;

    public CollisionInfo(Point point, Collidable object) {
        this.collisionPoint = point;
        this.collisionObject = object;
    }

    // The point at which the collision occurs.
    public Point collisionPoint() {
        return this.collisionPoint;
    }

    // The collidable object involved in the collision.
    public Collidable collisionObject() {
        return this.collisionObject;
    }
}