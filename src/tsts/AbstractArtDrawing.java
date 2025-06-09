import biuoop.GUI;
import biuoop.DrawSurface;
import java.util.Random;
import shapes.*;

public class AbstractArtDrawing {

    private static final int MAX_LINES = 10;

    public void drawRandomShapes() {
        Random rand = new Random(); // create a random-number generator
        GUI gui = new GUI("Random Circles Example", 800, 600);
        DrawSurface d = gui.getDrawSurface();
        Line[] drawnLines = new Line[MAX_LINES];
        for (int i = 0; i < MAX_LINES; ++i) {
            // Creating the relevant shapes
            Point p1 = new Point((rand.nextInt(800) + 1), (rand.nextInt(600) + 1));
            Point p2 = new Point((rand.nextInt(800) + 1), (rand.nextInt(600) + 1));
            Line newLine = new Line(p1, p2);
            drawnLines[i] = newLine;
            // Drawing
            newLine.drawOn(d);
            Point middle = newLine.middle();
            middle.drawMiddlePoint(d);
        }
        // Checking intersections between lines.
        for (int i = 0; i < MAX_LINES; i++) {
            for (int j = i + 1; j < MAX_LINES; j++) {
                Point intersection = drawnLines[i].intersectionWith(drawnLines[j]);
                if (intersection != null) {
                    intersection.drawIntersectionPoint(d);
                }
            }
        }
        // Checking triangles between lines.
        for (int i = 0; i < MAX_LINES; i++) {
            for (int j = i + 1; j < MAX_LINES; j++) {
                for (int k = j + 1; k < MAX_LINES; k++) {
                    Point intersectA = drawnLines[i].intersectionWith(drawnLines[j]);
                    Point intersectB = drawnLines[j].intersectionWith(drawnLines[k]);
                    Point intersectC = drawnLines[k].intersectionWith(drawnLines[i]);
                    if (intersectA != null && intersectB != null && intersectC != null) {
                        Triangle trngle = new Triangle(intersectA, intersectB, intersectC);
                        trngle.drawOn(d);
                    }
                }
            }
        }

        gui.show(d);
    }

    public static void main(String[] args) {
        AbstractArtDrawing example = new AbstractArtDrawing();
        example.drawRandomShapes();
    }
}