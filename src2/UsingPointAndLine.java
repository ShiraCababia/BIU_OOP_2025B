import biuoop.DrawSurface;
import biuoop.GUI;

// import java.awt.Color;

public class UsingPointAndLine {
    public void drawShapes() {
        GUI gui = new GUI("Line and Point Visualization", 500, 500);
        DrawSurface d = gui.getDrawSurface();

        // Create points
        Point p1 = new Point(100, 100);
        Point p2 = new Point(400, 400);
        Point p3 = new Point(100, 400);
        Point p4 = new Point(400, 100);

        // Create lines
        Line line1 = new Line(p1, p2);
        Line line2 = new Line(p3, p4);

        // Draw points
        p1.drawOn(d);
        p2.drawOn(d);
        p3.drawOn(d);
        p4.drawOn(d);

        // Draw lines
        line1.drawOn(d);
        line2.drawOn(d);

        // Check for intersection and draw it
        if (line1.isIntersecting(line2)) {
            Point intersection = line1.intersectionWith(line2);
            if (intersection != null) {
                intersection.drawOn(d);
            }
        }

        gui.show(d);
    }

    public static void main(String[] args) {
        UsingPointAndLine example = new UsingPointAndLine();
        example.drawShapes();
    }
}
