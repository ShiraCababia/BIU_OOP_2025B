import biuoop.DrawSurface;
import biuoop.GUI;
import biuoop.Sleeper;
import java.util.Random;
import java.awt.Color;

public class MultipleFramesBouncingBallsAnimation {
    public static void main(String[] args) {

        final int WIDTH = 800;
        final int HEIGHT = 600;
        int numBalls = args.length;

        GUI gui = new GUI("Multiple Frames Bouncing Balls Animation", WIDTH, HEIGHT);
        Sleeper sleeper = new Sleeper();
        Random rand = new Random();

        // Create one Ball per argument (considering case when no arguments given -
        // create one ball only as default)
        // + defaultExtra
        Ball[] balls = new Ball[numBalls];

        for (int i = 0; i < numBalls; i++) {
            int size = Integer.parseInt(args[i]);
            double speed = 100.0 / size;
            if (speed < 1) {
                speed = 1;
            }
            double angle = rand.nextDouble() * 360;
            Color color = new Color(rand.nextInt(256), rand.nextInt(256), rand.nextInt(256));
            Ball b;

            if (i < numBalls / 2) {
                // מסגרת אפורה מ-(50,50) עד (500,500)
                double x1 = 50, y1 = 50, x2 = 500, y2 = 500;
                double x = x1 + size + rand.nextDouble() * (x2 - x1 - 2 * size);
                double y = y1 + size + rand.nextDouble() * (y2 - y1 - 2 * size);
                b = new Ball(x, y, size, color);
                b.setVelocity(Velocity.fromAngleAndSpeed(angle, speed));
                b.setFrame(x1, y1, x2, y2);
            } else {
                // מסגרת כללית: כל המסך (למעט שתיהן)
                double x1 = 0, y1 = 0, x2 = WIDTH, y2 = HEIGHT;
                double x = size + rand.nextDouble() * (x2 - 2 * size);
                double y = size + rand.nextDouble() * (y2 - 2 * size);
                b = new Ball(x, y, size, color);
                b.setVelocity(Velocity.fromAngleAndSpeed(angle, speed));
                b.setFrame(x1, y1, x2, y2);
            }

            balls[i] = b;

        }

        // Animation loop
        while (true) {
            DrawSurface d = gui.getDrawSurface();
            d.setColor(Color.WHITE);
            d.fillRectangle(0, 0, WIDTH, HEIGHT);
            d.setColor(Color.GRAY);
            d.fillRectangle(50, 50, 450, 450);
            d.setColor(Color.YELLOW);
            d.fillRectangle(450, 450, 150, 150);

            // move & draw all balls
            for (Ball b : balls) {
                b.moveOneStep();
                b.drawOn(d);
            }
            gui.show(d);
            sleeper.sleepFor(50);
        }
    }

}
