import biuoop.GUI;
import biuoop.DrawSurface;
import biuoop.Sleeper;
import java.awt.Color;

public class Main {

    public static void main(String[] args) {
        GUI gui = new GUI("Ball and Blocks Game", 800, 600);
        Sleeper sleeper = new Sleeper();
        GameEnvironment environment = new GameEnvironment();
        Ball ball = new Ball(new Point(400, 300), 5, Color.RED);
        ball.setVelocity(new Velocity(5, 5)); // Initial velocity
        ball.setGameEnvironment(environment);
        Block block1 = new Block(new Rectangle(new Point(100, 100), 50, 10));
        Block block2 = new Block(new Rectangle(new Point(200, 200), 50, 10));
        Block block3 = new Block(new Rectangle(new Point(300, 300), 50, 10));
        environment.addCollidable(block1);
        environment.addCollidable(block2);
        environment.addCollidable(block3);

        // Main animation loop
        while (true) {
            DrawSurface surface = gui.getDrawSurface();
            ball.drawOn(surface);
            block1.drawOn(surface);
            block2.drawOn(surface);
            block3.drawOn(surface);
            ball.moveOneStep();
            gui.show(surface);
            sleeper.sleepFor(20); // 20 milliseconds
        }
    }
}
