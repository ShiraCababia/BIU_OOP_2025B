import biuoop.DrawSurface;

public class Ball implements Sprite {

    private Point centerP;
    private int size;
    private java.awt.Color color;
    private Velocity velocity;
    private double minX, minY, maxX, maxY;
    private GameEnvironment environment;

    final int WIDTH = 800; // X
    final int HEIGHT = 600; // Y

    // Constructor
    public Ball(Point center, int r, java.awt.Color color) {
        this.centerP = center;
        this.size = r;
        this.color = color;
        this.minX = 0;
        this.minY = 0;
        this.maxX = WIDTH;
        this.maxY = HEIGHT;
    }

    // Constructor for given X,Y instead of Point-Object
    public Ball(double x, double y, int r, java.awt.Color color) {
        this.centerP = new Point(x, y);
        this.size = r;
        this.color = color;
        this.minX = 0;
        this.minY = 0;
        this.maxX = WIDTH;
        this.maxY = HEIGHT;
    }

    // Accessors
    public int getX() {
        return (int) this.centerP.getX();
    }

    public int getY() {
        return (int) this.centerP.getY();
    }

    public int getSize() {
        return this.size;
    }

    public java.awt.Color getColor() {
        return this.color;
    }

    public Velocity getVelocity() {
        return this.velocity;
    }

    public void setGameEnvironment(GameEnvironment environment) {
        this.environment = environment;
    }

    public void setVelocity(Velocity v) {
        this.velocity = v;
    }

    public void setVelocity(double dx, double dy) {
        this.velocity = new Velocity(dx, dy);
    }

    // Method to draw the ball on the given DrawSurface
    public void drawOn(DrawSurface surface) {
        surface.setColor(this.color);
        surface.fillCircle((int) this.centerP.getX(), (int) this.centerP.getY(), this.size);
    }

    public void setFrame(double minX, double minY, double maxX, double maxY) {
        this.minX = minX;
        this.minY = minY;
        this.maxX = maxX;
        this.maxY = maxY;
    }

    // Method to move the ball one step within the defined boundaries
    /*
     * public void moveOneStep() {
     * if (this.velocity != null) {
     * // Calculate the next position based on velocity
     * double nextX = this.centerP.getX() + this.velocity.getDx();
     * double nextY = this.centerP.getY() + this.velocity.getDy();
     * // Check if the ball is out of bounds on the X-axis (left or right)
     * if (nextX - size < minX || nextX + size > maxX) {
     * this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
     * // Adjust the X-coordinate to keep the ball inside the bounds
     * if (nextX - size < minX) {
     * nextX = size + minX;
     * }
     * if (nextX + size > maxX) {
     * nextX = maxX - size;
     * }
     * }
     * // Check if the ball is out of bounds on the Y-axis (top or bottom)
     * if (nextY - size < minY || nextY + size > maxY) {
     * this.velocity = new Velocity(this.velocity.getDx(), -this.velocity.getDy());
     * // Adjust the Y-coordinate to keep the ball inside the bounds
     * if (nextY - size < minY) {
     * nextY = size + minY;
     * }
     * if (nextY + size > maxY) {
     * nextY = maxY - size;
     * }
     * }
     * // Update the ball's position to the new calculated position
     * this.centerP = new Point(nextX, nextY);
     * }
     * }
     */

    public void moveOneStep() {
        if (this.velocity == null) {
            return;
        }
        double nextX = this.centerP.getX() + this.velocity.getDx();
        double nextY = this.centerP.getY() + this.velocity.getDy();
        int minX = size;
        int maxX = WIDTH - size;
        int minY = size;
        int maxY = HEIGHT - size;
        if (nextX < minX || nextX > maxX) {
            this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
        }
        if (nextY < minY || nextY > maxY) {
            this.velocity = new Velocity(this.velocity.getDx(), -this.velocity.getDy());
        }

        nextX = this.centerP.getX() + this.velocity.getDx();
        nextY = this.centerP.getY() + this.velocity.getDy();
        Point end = new Point(nextX, nextY);
        Line trajectory = new Line(this.centerP, end);

        CollisionInfo collisionInfo = environment.getClosestCollision(trajectory);

        if (collisionInfo != null) {
            Point collisionPoint = collisionInfo.collisionPoint();
            Collidable collidedObject = collisionInfo.collisionObject();
            double dx = this.velocity.getDx();
            double dy = this.velocity.getDy();
            double smallOffset = 0.1;
            double newX = collisionPoint.getX() - dx * smallOffset;
            double newY = collisionPoint.getY() - dy * smallOffset;

            this.centerP = new Point(newX, newY);
            this.velocity = collidedObject.hit(collisionPoint, this.velocity);
        } else {
            this.centerP = end;
        }
    }

    // Method for moving the ball with additional restrictions for the gray and
    // yellow rectangles.
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

    public void timePassed() {
        moveOneStep();
    }

    public void addToGame(Game g) {
        g.addSprite(this);
    }

}