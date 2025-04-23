import biuoop.DrawSurface;
import biuoop.GUI;
import biuoop.Sleeper;
import java.util.Random;
import java.awt.Color;

public class MultipleFramesBouncingBallsAnimation {
    public static void main(String[] args) {

        final int WIDTH = 800;
        final int HEIGHT = 600;
        final int MIN_SIZE = 5;
        final int MAX_SIZE = 140;
        int numBalls = args.length;
        boolean isDefault = false;

        GUI gui = new GUI("Multiple Frames Bouncing Balls Animation", WIDTH, HEIGHT);
        Sleeper sleeper = new Sleeper();
        Random rand = new Random();

        // Handling no arguments given - Default - 1 Ball is created
        if (args.length == 0 || !args[0].matches("-?\\d+") || args[0].equals("${args}")) {
            System.out.println("No arguments given. Default implemented!");
            numBalls = 1;
            isDefault = true;
        }

        // Create an array to hold the balls
        Ball[] balls = new Ball[numBalls];

        // Initialize each ball based on input size
        for (int i = 0; i < numBalls; i++) {
            int size;
            if (!isDefault) {
                size = Integer.parseInt(args[i]);
            } else {
                size = Integer.parseInt("20");
            }
            // Clamp size to allowed range
            if (size < MIN_SIZE) {
                size = MIN_SIZE;
            }
            if (size > MAX_SIZE) {
                size = MAX_SIZE;
            }
            // Calculate speed inversely proportional to size
            double speed = 100.0 / size;
            if (speed < 1) {
                speed = 1;
            }
            // Random angle and color
            double angle = rand.nextDouble() * 360;
            Color color = new Color(rand.nextInt(256), rand.nextInt(256), rand.nextInt(256));
            Ball b;

            if (i < numBalls / 2) {
                // First half of the balls move inside a gray frame (50,50) to (500,500)
                double x1 = 50, y1 = 50, x2 = 500, y2 = 500;
                double x = x1 + size + rand.nextDouble() * (x2 - x1 - 2 * size);
                double y = y1 + size + rand.nextDouble() * (y2 - y1 - 2 * size);
                b = new Ball(x, y, size, color);
                b.setVelocity(Velocity.fromAngleAndSpeed(angle, speed));
                b.setFrame(x1, y1, x2, y2);
            } else {
                // Second half of the balls move in the whole screen, avoiding the two
                // rectangles
                double x1 = 0, y1 = 0, x2 = WIDTH, y2 = HEIGHT;
                double x = size + rand.nextDouble() * (x2 - 2 * size);
                double y = size + rand.nextDouble() * (y2 - 2 * size);
                // To prevent outside-of-rectangles-balls from being created in the gray\yellow
                // rectangles.
                while ((x + size >= 50 && x - size <= 500 && y + size >= 50 && y - size <= 500) ||
                        (x + size >= 450 && x - size <= 600 && y + size >= 450 && y - size <= 600) ||
                        // To prevent balls from being created when hidden by the frame
                        (x + size > WIDTH) || (x - size < 0) || (y + size > HEIGHT) || (y - size < 0)) {
                    x = size + rand.nextDouble() * (x2 - 2 * size);
                    y = size + rand.nextDouble() * (y2 - 2 * size);
                }
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
            // Move and draw all balls
            for (int i = 0; i < balls.length; i++) {
                if (i < numBalls / 2) {
                    balls[i].moveOneStep();
                } else {
                    balls[i].moveOneStepWhenLimited();
                }
                balls[i].drawOn(d);
            }
            d.setColor(Color.YELLOW);
            d.fillRectangle(450, 450, 150, 150);
            gui.show(d);
            sleeper.sleepFor(50);
        }
    }

}
