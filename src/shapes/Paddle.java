package shapes;
import biuoop.DrawSurface;
import biuoop.KeyboardSensor;
import java.awt.Color;
import core.*;

/**
 * The Paddle class represents the player-controlled paddle in the game.
 * It can move left or right in response to keyboard input, and it deflects balls upon collision.
 */
public class Paddle implements Sprite, Collidable {
    private Rectangle rectangle;
    private Color color;
    private int speed;
    private int screenWidth;
    private biuoop.KeyboardSensor keyboard;

    /**
     * Constructs a Paddle with a given shape, color, keyboard input handler, speed, and screen width.
     *
     * @param rectangle    the rectangle defining the paddle's shape
     * @param color        the paddle's color
     * @param keyboard     the KeyboardSensor for detecting key presses
     * @param speed        the movement speed of the paddle
     * @param screenWidth  the width of the screen, used for circular wrapping
     */
    public Paddle(Rectangle rectangle, Color color, biuoop.KeyboardSensor keyboard, int speed, int screenWidth) {
        this.rectangle = rectangle;
        this.color = color;
        this.keyboard = keyboard;
        this.speed = speed;
        this.screenWidth = screenWidth;
    }

    /**
     * Moves the paddle left by its speed. If it moves past the left edge, wraps to the right side.
     */
    public void moveLeft() {
        double newX = rectangle.getUpperLeft().getX() - speed;
        if (newX < 0) {
            newX = screenWidth - rectangle.getWidth();
        }
        this.rectangle = new Rectangle(new Point(newX, rectangle.getUpperLeft().getY()), rectangle.getWidth(),
                rectangle.getHeight());
    }

    /**
     * Moves the paddle right by its speed. If it moves past the right edge, wraps to the left side.
     */
    public void moveRight() {
        double newX = rectangle.getUpperLeft().getX() + speed;
        if (newX + rectangle.getWidth() > screenWidth) {
            newX = 0;
        }
        this.rectangle = new Rectangle(new Point(newX, rectangle.getUpperLeft().getY()), rectangle.getWidth(),
                rectangle.getHeight());
    }

    // Sprite

    /**
     * Called once per frame. Moves the paddle based on keyboard input.
     */
    public void timePassed() {
        if (keyboard.isPressed(KeyboardSensor.LEFT_KEY)) {
            moveLeft();
        }
        if (keyboard.isPressed(KeyboardSensor.RIGHT_KEY)) {
            moveRight();
        }
    }

    /**
     * Draws the paddle on the given drawing surface.
     *
     * @param d the DrawSurface to draw on
     */
    public void drawOn(DrawSurface d) {
        d.setColor(this.color);
        d.fillRectangle((int) rectangle.getUpperLeft().getX(),
                (int) rectangle.getUpperLeft().getY(),
                (int) rectangle.getWidth(),
                (int) rectangle.getHeight());
        d.setColor(Color.BLACK);
        d.drawRectangle((int) rectangle.getUpperLeft().getX(),
                (int) rectangle.getUpperLeft().getY(),
                (int) rectangle.getWidth(),
                (int) rectangle.getHeight());
    }

    // Collidable

    /**
     * Returns the rectangle representing the paddle's collision area.
     *
     * @return the collision rectangle
     */
    public Rectangle getCollisionRectangle() {
        return rectangle;
    }

    /**
     * Handles the collision with a ball by changing its velocity based on the hit region.
     *
     * @param collisionPoint  the point at which the ball hits the paddle
     * @param currentVelocity the velocity of the ball before the collision
     * @return the new velocity of the ball after the hit
     */
    public Velocity hit(Point collisionPoint, Velocity currentVelocity) {
        double x = collisionPoint.getX();
        double regionWidth = rectangle.getWidth() / 5;
        double startX = rectangle.getUpperLeft().getX();
        int region = (int) ((x - startX) / regionWidth) + 1;
        double speed = Math.sqrt(currentVelocity.getDx() * currentVelocity.getDx()
                + currentVelocity.getDy() * currentVelocity.getDy());

        switch (region) {
            case 1:
                return Velocity.fromAngleAndSpeed(300, speed);
            case 2:
                return Velocity.fromAngleAndSpeed(330, speed);
            case 3:
                return new Velocity(currentVelocity.getDx(), -currentVelocity.getDy());
            case 4:
                return Velocity.fromAngleAndSpeed(30, speed);
            case 5:
                return Velocity.fromAngleAndSpeed(60, speed);
            default:
                return new Velocity(currentVelocity.getDx(), -currentVelocity.getDy());
        }
    }

    /**
     * Adds this paddle to the game as both a sprite and a collidable object.
     *
     * @param g the game instance
     */
    public void addToGame(Game g) {
        g.addSprite(this);
        g.addCollidable(this);
    }
}
