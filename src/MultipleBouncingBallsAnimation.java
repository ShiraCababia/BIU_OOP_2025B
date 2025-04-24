import biuoop.DrawSurface;
import biuoop.GUI;
import biuoop.Sleeper;
import java.util.Random;
import java.awt.Color;

public class MultipleBouncingBallsAnimation {
    public static void main(String[] args) {

        final int WIDTH = 800;
        final int HEIGHT = 600;
        final int MIN_SIZE = 5;
        final int MAX_SIZE = 140;
        final int MAX_BALLS = 50;
        int numBalls = args.length;
        boolean isDefault = false;
        boolean isMessageShown = false;

        // Handling no arguments given - Default - 1 Ball is created
        if (args.length == 0 || !args[0].matches("-?\\d+") || args[0].equals("${args}")) {
            System.out.println("No arguments given. Default implemented!");
            numBalls = 1;
            isDefault = true;
        }

        GUI gui = new GUI("Multiple Bouncing Balls Animation", WIDTH, HEIGHT);
        Sleeper sleeper = new Sleeper();
        Random rand = new Random();

        // Create an array to hold the balls, handling too many balls
        if (numBalls > MAX_BALLS) {
            System.out.println("Too many arguments given (Max is " + MAX_BALLS + "). Default implemented!");
            numBalls = MAX_BALLS;
        }
        Ball[] balls = new Ball[numBalls];

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
                if (!isMessageShown) {
                    System.out.println(
                            "Unacceptable size given (range is " + MIN_SIZE + "-" + MAX_SIZE
                                    + "). Default implemented!");
                    isMessageShown = true;
                }
            }
            if (size > MAX_SIZE) {
                size = MAX_SIZE;
                if (!isMessageShown) {
                    System.out.println(
                            "Unacceptable size given (range is " + MIN_SIZE + "-" + MAX_SIZE
                                    + "). Default implemented!");
                    isMessageShown = true;
                }
            }
            // Pick a random center so that the entire ball lies inside the screen
            double x = size + rand.nextDouble() * (WIDTH - 2 * size);
            double y = size + rand.nextDouble() * (HEIGHT - 2 * size);
            Color color = new Color(rand.nextInt(256), rand.nextInt(256), rand.nextInt(256));
            Ball b = new Ball(x, y, size, color);
            // Set speed inversely proportional to ball size (smaller balls move faster),
            // but enforce a minimum speed of 1 so no ball ever remains stationary.
            double speed = 100.0 / size;
            if (speed < 1) {
                speed = 1;
            }
            double angle = rand.nextDouble() * 360;
            b.setVelocity(Velocity.fromAngleAndSpeed(angle, speed));
            balls[i] = b;
        }

        // Animation loop
        while (true) {
            DrawSurface d = gui.getDrawSurface();
            d.setColor(Color.WHITE);
            d.fillRectangle(0, 0, WIDTH, HEIGHT);
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
