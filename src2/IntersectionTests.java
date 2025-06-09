import biuoop.GUI;
import biuoop.DrawSurface;
import java.awt.Color;
import shapes.*;

public class IntersectionTests {

    public static void test() {
        GUI gui = new GUI("Intersection Tests", 1200, 400);
        DrawSurface surface = gui.getDrawSurface();
        surface.setColor(Color.BLACK);
        // Test Case 1: Perpendicular intersection at (10,10)
        Line line1 = new Line(5, 10, 10, 10);
        Line line2 = new Line(10, 0, 10, 20);
        System.out.println(line2.middle().getX());
        drawTestCase(surface, line1, line2, "Perpendicular", 10);
        // Test Case 2: Diagonal intersection
        Line line3 = new Line(0, 0, 20, 20);
        Line line4 = new Line(0, 20, 20, 0);
        drawTestCase(surface, line3, line4, "Diagonal", 40);
        // Test Case 3: Endpoint intersection at (10,20)
        Line line5 = new Line(5, 20, 10, 20);
        Line line6 = new Line(10, 20, 15, 20);
        drawTestCase(surface, line5, line6, "Endpoint", 70);
        // Test Case 4: Collinear overlap (should return null intersection)
        Line line7 = new Line(5, 30, 10, 30);
        Line line8 = new Line(7, 30, 10, 30);
        drawTestCase(surface, line7, line8, "Collinear", 100);
        // Test Case 5: Angled intersection with one line ending at intersection
        Line line9 = new Line(0, 0, 20, 20);
        Line line10 = new Line(10, 20, 15, 15);  // This line ends at the intersection point
        drawTestCase(surface, line9, line10, "Angled", 130);
        gui.show(surface);
    }

    private static void drawTestCase(DrawSurface surface, Line line1, Line line2,
                                     String testName, int yOffset) {
        // Draw the lines
        surface.setColor(Color.BLACK);
        surface.drawLine((int) line1.start().getX(), (int) line1.start().getY() + yOffset,
                (int) line1.end().getX(), (int) line1.end().getY() + yOffset);
        surface.drawLine((int) line2.start().getX(), (int) line2.start().getY() + yOffset,
                (int) line2.end().getX(), (int) line2.end().getY() + yOffset);
        // Test intersection
        boolean isIntersecting = line1.isIntersecting(line2);
        Point intersectionPoint = line1.intersectionWith(line2);
        // Display results
        String result = testName + " Test - ";
        boolean testPassed = false;
        String expected = "";
        String got = "";
        if (testName.equals("Perpendicular")) {
            expected = "Intersection at (10.000000, 10.000000)";
            got = String.format("isIntersecting=%b, point=%s", isIntersecting,
                    intersectionPoint != null ? String.format("(%f, %f)",
                            intersectionPoint.getX(), intersectionPoint.getY()) : "null");
            testPassed = isIntersecting && intersectionPoint != null
                    && Math.abs(intersectionPoint.getX() - 10) < 0.001
                    && Math.abs(intersectionPoint.getY() - 10) < 0.001;
        } else if (testName.equals("Diagonal")) {
            expected = "Intersection at (10.000000, 10.000000)";
            got = String.format("isIntersecting=%b, point=%s", isIntersecting,
                    intersectionPoint != null ? String.format("(%f, %f)",
                            intersectionPoint.getX(), intersectionPoint.getY()) : "null");
            testPassed = isIntersecting && intersectionPoint != null
                    && Math.abs(intersectionPoint.getX() - 10) < 0.001
                    && Math.abs(intersectionPoint.getY() - 10) < 0.001;
        } else if (testName.equals("Endpoint")) {
            expected = "Intersection at (10.000000, 20.000000)";
            got = String.format("isIntersecting=%b, point=%s", isIntersecting,
                    intersectionPoint != null ? String.format("(%f, %f)",
                            intersectionPoint.getX(), intersectionPoint.getY()) : "null");
            testPassed = isIntersecting && intersectionPoint != null
                    && Math.abs(intersectionPoint.getX() - 10) < 0.001
                    && Math.abs(intersectionPoint.getY() - 20) < 0.001;
        } else if (testName.equals("Collinear")) {
            expected = "Intersecting but no point (overlap)";
            got = String.format("isIntersecting=%b, point=%s", isIntersecting,
                    intersectionPoint != null ? "not null" : "null");
            testPassed = isIntersecting && intersectionPoint == null;
        } else if (testName.equals("Angled")) {
            expected = "Intersection at (15.000000, 15.000000)";
            got = String.format("isIntersecting=%b, point=%s", isIntersecting,
                    intersectionPoint != null ? String.format("(%f, %f)",
                            intersectionPoint.getX(), intersectionPoint.getY()) : "null");
            testPassed = isIntersecting && intersectionPoint != null
                    && Math.abs(intersectionPoint.getX() - 15) < 0.001
                    && Math.abs(intersectionPoint.getY() - 15) < 0.001;
        }
        // Set color based on test result and display results
        surface.setColor(testPassed ? Color.GREEN : Color.RED);
        result += String.format("Expected: %s | Got: %s", expected, got);
        surface.drawText(150, yOffset + 10, result, 12);
        // Draw intersection point if it exists
        if (!testName.equals("Collinear") && intersectionPoint != null) {
            surface.setColor(Color.RED);
            surface.fillCircle((int) intersectionPoint.getX(),
                    (int) intersectionPoint.getY() + yOffset, 3);
        }
    }

    public static void main(String[] args) {
        test();
    }
}