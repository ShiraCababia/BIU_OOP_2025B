import biuoop.GUI;
import biuoop.Sleeper;
import biuoop.DrawSurface;

public class BallsTest1 {

    static private void drawStaticBalls() {
        GUI gui = new GUI("Balls Test 1", 800, 600);
        DrawSurface d = gui.getDrawSurface();

        Ball b1 = new Ball(100, 100, 30, java.awt.Color.RED);
        Ball b2 = new Ball(100, 150, 10, java.awt.Color.BLUE);
        Ball b3 = new Ball(80, 249, 50, java.awt.Color.GREEN);

        b1.drawOn(d);
        b2.drawOn(d);
        b3.drawOn(d);
        gui.show(d);
    }

    static private void drawAnimation(Point start, double dx, double dy) {
        GUI gui = new GUI("title", 800, 600);
        Sleeper sleeper = new Sleeper();
        Ball ball = new Ball(start.getX(), start.getY(), 20, java.awt.Color.BLACK);
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
        Point p = new Point(1, 1);
        drawAnimation(p, 5.0, 8.0);
    }
}