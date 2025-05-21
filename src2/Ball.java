import biuoop.DrawSurface;

/**
 * The Ball class represents a ball in the game, which can move,
 * detect collisions, and draw itself on the screen.
 */
public class Ball implements Sprite {

    private Point centerP;
    private int size;
    private java.awt.Color color;
    private Velocity velocity;
    private double minX, minY, maxX, maxY;
    private GameEnvironment environment;

    final int WIDTH = 800; // X
    final int HEIGHT = 600; // Y

    /**
     * Constructor.
     * Initializes a ball at the given center point, with the specified radius and color.
     *
     * @param center the center point of the ball
     * @param r      the radius of the ball
     * @param color  the color of the ball
     */
    public Ball(Point center, int r, java.awt.Color color) {
        this.centerP = center;
        this.size = r;
        this.color = color;
        this.minX = 0;
        this.minY = 0;
        this.maxX = WIDTH;
        this.maxY = HEIGHT;
    }

    /**
     * Constructor for given x, y instead of Point-object.
     *
     * @param x     x-coordinate of center
     * @param y     y-coordinate of center
     * @param r     radius of the ball
     * @param color color of the ball
     */
    public Ball(double x, double y, int r, java.awt.Color color) {
        this.centerP = new Point(x, y);
        this.size = r;
        this.color = color;
        this.minX = 0;
        this.minY = 0;
        this.maxX = WIDTH;
        this.maxY = HEIGHT;
    }

    /**
     * @return the x position of the ball's center as an integer
     */
    public int getX() {
        return (int) this.centerP.getX();
    }

    /**
     * @return the y position of the ball's center as an integer
     */
    public int getY() {
        return (int) this.centerP.getY();
    }

    /**
     * @return the radius of the ball
     */
    public int getSize() {
        return this.size;
    }

    /**
     * @return the ball's color
     */
    public java.awt.Color getColor() {
        return this.color;
    }

    /**
     * @return the velocity of the ball
     */
    public Velocity getVelocity() {
        return this.velocity;
    }

    /**
     * Sets the game environment for the ball to interact with.
     *
     * @param environment the GameEnvironment instance
     */
    public void setGameEnvironment(GameEnvironment environment) {
        this.environment = environment;
    }

    /**
     * Sets the velocity of the ball.
     *
     * @param v the new velocity
     */
    public void setVelocity(Velocity v) {
        this.velocity = v;
    }

    /**
     * Sets the velocity of the ball using dx and dy values.
     *
     * @param dx the horizontal speed
     * @param dy the vertical speed
     */
    public void setVelocity(double dx, double dy) {
        this.velocity = new Velocity(dx, dy);
    }

    /**
     * Method to draw the ball on the given DrawSurface.
     *
     * @param surface the drawing surface
     */
    public void drawOn(DrawSurface surface) {
        surface.setColor(this.color);
        surface.fillCircle((int) this.centerP.getX(), (int) this.centerP.getY(), this.size);
    }

    /**
     * Sets the boundaries (frame) for the ball's movement.
     *
     * @param minX minimum x bound
     * @param minY minimum y bound
     * @param maxX maximum x bound
     * @param maxY maximum y bound
     */
    public void setFrame(double minX, double minY, double maxX, double maxY) {
        this.minX = minX;
        this.minY = minY;
        this.maxX = maxX;
        this.maxY = maxY;
    }

    /**
     * Advances the ball based on its velocity and checks for collisions
     * with game elements and screen boundaries.
     */
    public void moveOneStep() {
        if (velocity == null || environment == null) {
            return;
        }
        double dx = velocity.getDx();
        double dy = velocity.getDy();
        Point projected = new Point(centerP.getX() + dx, centerP.getY() + dy);
        Line path = new Line(centerP, projected);
        CollisionInfo impact = environment.getClosestCollision(path);
        if (impact == null) {
            // Check if the new position is overlapping any object (especially paddle)
            for (Collidable item : environment.getCollidables()) {
                Rectangle bounds = item.getCollisionRectangle();
                if (bounds.isInside(projected)) {
                    if (item instanceof Paddle) {
                        // Ball hit the paddle, force it upward and place above
                        velocity = new Velocity(dx, -Math.abs(dy));
                        centerP = new Point(centerP.getX(), bounds.getUpperLeft().getY() - 1);
                    } else {
                        // Estimate direction of contact to reflect manually
                        double leftGap = Math.abs(projected.getX() - bounds.getUpperLeft().getX());
                        double rightGap = Math
                                .abs(projected.getX() - (bounds.getUpperLeft().getX() + bounds.getWidth()));
                        double topGap = Math.abs(projected.getY() - bounds.getUpperLeft().getY());
                        double bottomGap = Math
                                .abs(projected.getY() - (bounds.getUpperLeft().getY() + bounds.getHeight()));

                        if (Math.min(leftGap, rightGap) < Math.min(topGap, bottomGap)) {
                            dx = -dx;
                        } else {
                            dy = -dy;
                        }
                        velocity = new Velocity(dx, dy);
                    }
                    return;
                }
            }
            // No collision found; proceed to move
            centerP = projected;
        } else {
            // Collision occurred; adjust position slightly before impact
            Point hitPoint = impact.collisionPoint();
            Velocity updatedVelocity = impact.collisionObject().hit(hitPoint, velocity);
            centerP = new Point(hitPoint.getX() - dx * 0.1, hitPoint.getY() - dy * 0.1);
            velocity = updatedVelocity;
        }
        // Safety net: if ball ends up inside a shape, reposition it outward
        for (Collidable object : environment.getCollidables()) {
            if (object.getCollisionRectangle().isInside(centerP)) {
                double mag = Math.sqrt(dx * dx + dy * dy);
                if (mag != 0) {
                    double ux = dx / mag;
                    double uy = dy / mag;
                    centerP = new Point(centerP.getX() - ux * 2, centerP.getY() - uy * 2);
                    velocity = new Velocity(-dx, -dy);
                }
                break;
            }
        }
        // Ensure ball stays within screen bounds
        double x = centerP.getX();
        double y = centerP.getY();
        if (x - size < 0) {
            x = size;
            velocity = new Velocity(Math.abs(velocity.getDx()), velocity.getDy());
        } else if (x + size > WIDTH) {
            x = WIDTH - size;
            velocity = new Velocity(-Math.abs(velocity.getDx()), velocity.getDy());
        }
        if (y - size < 0) {
            y = size;
            velocity = new Velocity(velocity.getDx(), Math.abs(velocity.getDy()));
        } else if (y + size > HEIGHT) {
            y = HEIGHT - size;
            velocity = new Velocity(velocity.getDx(), -Math.abs(velocity.getDy()));
        }
        centerP = new Point(x, y);
    }

    /**
     * Method for moving the ball with additional restrictions
     * for specific layout zones in the game.
     */
    public void moveOneStepWhenLimited() {
        if (this.velocity != null) {
            double nextX = this.centerP.getX() + this.velocity.getDx();
            double nextY = this.centerP.getY() + this.velocity.getDy();

            // Top wall of the left gray square
            if (nextX - size < 500 && nextX + size > 50 &&
                    centerP.getY() < 50 && nextY + size >= 50) {
                this.velocity = new Velocity(this.velocity.getDx(), -this.velocity.getDy());
                nextY = 50 - size;
            }

            // Top wall of the right gray square
            if (nextX + size > 500 && nextX - size < 600 &&
                    centerP.getY() < 450 && nextY + size >= 450) {
                this.velocity = new Velocity(this.velocity.getDx(), -this.velocity.getDy());
                nextY = 450 - size;
            }

            // Bottom wall of the left gray square
            if (nextX - size < 450 && nextX + size > 50 &&
                    centerP.getY() > 500 && nextY - size <= 500) {
                this.velocity = new Velocity(this.velocity.getDx(), -this.velocity.getDy());
                nextY = 500 + size;
            }

            // Left wall of the left gray square
            if (nextY - size < 450 && nextY + size > 50 &&
                    centerP.getX() > 500 && nextX - size <= 500) {
                this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
                nextX = 500 + size;
            }

            // Left wall of the right gray square
            if (nextY - size < 600 && nextY + size > 450 &&
                    centerP.getX() > 600 && nextX - size <= 600) {
                this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
                nextX = 600 + size;
            }

            // Right wall of the left gray square
            if (nextY + size > 50 && nextY - size < 500 &&
                    centerP.getX() < 50 && nextX + size >= 50) {
                this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
                nextX = 50 - size;
            }

            // Right wall of the right gray square
            if (nextY + size > 500 && nextY - size < 600 &&
                    centerP.getX() < 450 && nextX + size >= 450) {
                this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
                nextX = 450 - size;
            }

            // Check if the ball is out of bounds on the X-axis (left or right)
            if (nextX - size < minX || nextX + size > maxX) {
                this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
                if (nextX - size < minX) {
                    nextX = size + minX;
                }
                if (nextX + size > maxX) {
                    nextX = maxX - size;
                }
            }

            // Check if the ball is out of bounds on the Y-axis (top or bottom)
            if (nextY - size < minY || nextY + size > maxY) {
                this.velocity = new Velocity(this.velocity.getDx(), -this.velocity.getDy());
                if (nextY - size < minY) {
                    nextY = size + minY;
                }
                if (nextY + size > maxY) {
                    nextY = maxY - size;
                }
            }

            this.centerP = new Point(nextX, nextY);
        }
    }

    /**
     * Called to notify that time has passed; moves the ball.
     */
    public void timePassed() {
        moveOneStep();
    }

    /**
     * Adds the ball to the game as a sprite.
     *
     * @param g the Game instance
     */
    public void addToGame(Game g) {
        g.addSprite(this);
    }
}
