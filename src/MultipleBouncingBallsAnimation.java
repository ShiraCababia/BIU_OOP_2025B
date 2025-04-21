import biuoop.DrawSurface;
import biuoop.GUI;
import biuoop.Sleeper;
import java.util.Random;
import java.awt.Color;

public class MultipleBouncingBallsAnimation {
    public static void main(String[] args) {

        // System.out.println(" arguments given:" + args.length + " value: " + args[0]);

        final int WIDTH = 800;
        final int HEIGHT = 600;
        // int defaultExtra = 0;
        // if (args.length == 0) {
        // System.out.println("No arguments given. Default implemented");
        // defaultExtra = 1;
        // }
        GUI gui = new GUI("Multiple Bouncing Balls Animation", WIDTH, HEIGHT);
        Sleeper sleeper = new Sleeper();
        Random rand = new Random();

        // Create one Ball per argument (considering case when no arguments given -
        // create one ball only as default)
        // + defaultExtra
        Ball[] balls = new Ball[args.length];
        for (int i = 0; i < args.length; i++) {
            int size = Integer.parseInt(args[i]);
            // Pick a random center so that the entire ball lies inside the screen
            double x = size + rand.nextDouble() * (WIDTH - 2 * size);
            double y = size + rand.nextDouble() * (HEIGHT - 2 * size);
            Color color = new Color(rand.nextInt(256), rand.nextInt(256), rand.nextInt(256));
            Ball b = new Ball(x, y, size, color);
            // Set speed inversely proportional to ball size (smaller balls move faster),
            // but enforce a minimum speed of 1 so no ball ever remains stationary.
            double speed = 50.0 / size;
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
