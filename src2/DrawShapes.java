import biuoop.DrawSurface;
import biuoop.GUI;

// Main Class
public class DrawShapes {
    public void drawShapes() {
        GUI gui = new GUI("Shapes Example", 500, 500);
        DrawSurface d = gui.getDrawSurface();

        // Create and draw random shapes
        new Point(250, 250).drawOn(d); // Draw center point
        new Line(new Point(100, 100), new Point(400, 400)).drawOn(d); // Diagonal line
        new Square(new Point(50, 50), 100).drawOn(d);
        new Rectangle(new Point(200, 50), 150, 100).drawOn(d);
        new Triangle(new Point(300, 300), new Point(350, 400), new Point(250, 400)).drawOn(d);
        new Circle(new Point(250, 250), 50).drawOn(d);

        gui.show(d);
    }

    public static void main(String[] args) {
        DrawShapes example = new DrawShapes();
        example.drawShapes();
    }
}
