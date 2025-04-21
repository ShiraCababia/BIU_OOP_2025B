import biuoop.DrawSurface;
import biuoop.GUI;
import biuoop.Sleeper;

public class BouncingBallAnimation {

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

    public static void main(String[] args) {
        if (args.length != 4) {
            System.out.print("Not enough arguments. Default implemented");
            drawAnimation(new Point(20, 20), 5, 5);
        } else {
            Point p = new Point(Double.parseDouble(args[0]), Double.parseDouble(args[1]));
            drawAnimation(p, Double.parseDouble(args[2]), Double.parseDouble(args[3]));
        }
    }
}
