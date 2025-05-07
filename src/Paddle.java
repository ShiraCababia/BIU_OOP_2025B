import biuoop.DrawSurface;
import biuoop.KeyboardSensor;
import java.awt.Color;

public class Paddle implements Sprite, Collidable {
    private Rectangle rectangle;
    private Color color;
    private int speed;
    private int screenWidth;
    private biuoop.KeyboardSensor keyboard;

    public Paddle(Rectangle rectangle, Color color, biuoop.KeyboardSensor keyboard, int speed, int screenWidth) {
        this.rectangle = rectangle;
        this.color = color;
        this.keyboard = keyboard;
        this.speed = speed;
        this.screenWidth = screenWidth;
    }

    public void moveLeft() {
        double newX = rectangle.getUpperLeft().getX() - speed;
        if (newX < 0) {
            newX = screenWidth - rectangle.getWidth();
        }
        this.rectangle = new Rectangle(new Point(newX, rectangle.getUpperLeft().getY()), rectangle.getWidth(),
                rectangle.getHeight());
    }

    public void moveRight() {
        double newX = rectangle.getUpperLeft().getX() + speed;
        if (newX + rectangle.getWidth() > screenWidth) {
            newX = 0;
        }
        this.rectangle = new Rectangle(new Point(newX, rectangle.getUpperLeft().getY()), rectangle.getWidth(),
                rectangle.getHeight());
    }

    // Sprite
    public void timePassed() {
        if (keyboard.isPressed(KeyboardSensor.LEFT_KEY)) {
            moveLeft();
        }
        if (keyboard.isPressed(KeyboardSensor.RIGHT_KEY)) {
            moveRight();
        }
    }

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
    public Rectangle getCollisionRectangle() {
        return rectangle;
    }

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

    // Add this paddle to the game.
    public void addToGame(Game g) {
        g.addSprite(this);
        g.addCollidable(this);
    }
}