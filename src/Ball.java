
// import java.awt.Color;
// import biuoop.GUI;
import biuoop.DrawSurface;

public class Ball {

    private Point centerP;
    private int size;
    private java.awt.Color color;
    private Velocity velocity;

    final int WIDTH = 800;
    final int HEIGHT = 600;

    // constructor
    public Ball(Point center, int r, java.awt.Color color) {
        this.centerP = center;
        this.size = r;
        this.color = color;
    }

    // constructor for X,Y instead of Point-Object
    public Ball(double x, double y, int r, java.awt.Color color) {
        this.centerP = new Point(x, y);
        this.size = r;
        this.color = color;
    }

    // accessors
    public int getX() {
        return (int) this.centerP.getX();
    }

    public int getY() {
        return (int) this.centerP.getY();
    }

    public int getSize() {
        return this.size;
    }

    public java.awt.Color getColor() {
        return this.color;
    }

    // draw the ball on the given DrawSurface
    public void drawOn(DrawSurface surface) {
        surface.setColor(this.color);
        surface.fillCircle((int) this.centerP.getX(), (int) this.centerP.getY(), this.size);
    }

    public Velocity getVelocity() {
        return this.velocity;
    }

    public void setVelocity(Velocity v) {
        this.velocity = v;
    }

    public void setVelocity(double dx, double dy) {
        this.velocity = new Velocity(dx, dy);
    }

    public void moveOneStep() {
        if (this.velocity != null) {
            double nextX = this.centerP.getX() + this.velocity.getDx();
            double nextY = this.centerP.getY() + this.velocity.getDy();
            //
            if (nextX - size < 0 || nextX + size > WIDTH) {
                this.velocity = new Velocity(-this.velocity.getDx(), this.velocity.getDy());
                if (nextX - size < 0) {
                    nextX = size;
                }
                if (nextX + size > WIDTH) {
                    nextX = WIDTH - size;
                }
            }
            //
            if (nextY - size < 0 || nextY + size > HEIGHT) {
                this.velocity = new Velocity(this.velocity.getDx(), -this.velocity.getDy());
                if (nextY - size < 0) {
                    nextY = size;
                }
                if (nextY + size > HEIGHT) {
                    nextY = HEIGHT - size;
                }
            }
            //
            // this.centerP = this.velocity.applyToPoint(this.centerP);
            this.centerP = new Point(nextX, nextY);
        }
    }

}