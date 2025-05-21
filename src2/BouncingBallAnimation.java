import biuoop.DrawSurface;
import biuoop.GUI;
import biuoop.Sleeper;

public class BouncingBallAnimation {

    // Method to run the animation loop: moves the ball, draws it, and waits between
    // frames.
    static private void drawAnimation(Point start, double dx, double dy) {
        GUI gui = new GUI("title", 800, 600);
        Sleeper sleeper = new Sleeper();
        Ball ball = new Ball(start.getX(), start.getY(), 45, java.awt.Color.BLACK);
        ball.setVelocity(dx, dy);
        while (true) {
            ball.moveOneStep();
            DrawSurface d = gui.getDrawSurface();
            ball.drawOn(d);
            gui.show(d);
            sleeper.sleepFor(50); // wait for 50 milliseconds.
        }
    }

    // Method that Receives arguments from the command line and starts the
    // animation.
    public static void main(String[] args) {
        // Handling not enough arguments given - Default - 1 Ball is created
        if (args.length != 4) {
            System.out.print("4 arguments needed. Default implemented!");
            drawAnimation(new Point(20, 20), 5, 5);
        } else {
            Point p = new Point(Double.parseDouble(args[0]), Double.parseDouble(args[1]));
            drawAnimation(p, Double.parseDouble(args[2]), Double.parseDouble(args[3]));
        }
    }
}
