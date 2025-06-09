package core;

import java.util.List;
import java.util.ArrayList;
import shapes.*;

/**
 * The GameEnvironment class holds all the collidable objects in the game.
 * It provides methods to add collidables and determine potential collisions
 * based on a given trajectory.
 */
public class GameEnvironment {

    private List<Collidable> collidables;

    /**
     * Constructs a new GameEnvironment with an empty list of collidables.
     */
    public GameEnvironment() {
        this.collidables = new ArrayList<>();
    }

    /**
     * Adds the given collidable to the environment.
     *
     * @param c the collidable object to add
     */
    public void addCollidable(Collidable c) {
        this.collidables.add(c);
    }

    /**
     * Determines the closest collision that will occur if an object moves along the
     * given trajectory.
     * If no collision occurs, returns null.
     *
     * @param trajectory the path the object is expected to move along
     * @return a CollisionInfo object describing the closest collision, or null if
     *         none
     */
    public CollisionInfo getClosestCollision(Line trajectory) {
        Point closestPoint = null;
        Collidable closestObject = null;
        double minDistance = Double.MAX_VALUE;
        for (Collidable c : collidables) {
            Rectangle rectangle = c.getCollisionRectangle();
            Point intersection = trajectory.closestIntersectionToStartOfLine(rectangle);
            if (intersection != null) {
                double distance = trajectory.start().distance(intersection);
                if (distance < minDistance) {
                    minDistance = distance;
                    closestPoint = intersection;
                    closestObject = c;
                }
            }
        }
        if (closestPoint == null) {
            return null;
        }
        return new CollisionInfo(closestPoint, closestObject);
    }

    /**
     * Returns the list of all collidable objects in the environment.
     *
     * @return list of collidables
     */
    public List<Collidable> getCollidables() {
        return this.collidables;
    }

    public void removeCollidable(Collidable c) {
        this.collidables.remove(c);
    }

}
