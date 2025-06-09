package core;

import biuoop.DrawSurface;
import java.awt.Color;
import biuoop.Sleeper;
import shapes.*;
import tsts.PrintingHitListener;

/**
 * The Game class is responsible for setting up and running
 * the Arkanoid-style game. It manages game elements such as
 * sprites, collidables, paddle, and the main game loop.
 */
public class Game {

    final int WIDTH = 800;
    final int HEIGHT = 600;
    private SpriteCollection sprites;
    private GameEnvironment environment;
    private biuoop.GUI gui;
    private biuoop.KeyboardSensor keyboard;
    private Paddle paddle; // Declare the paddle
    private Sleeper sleeper; // Declare the sleeper object

    /**
     * Constructs a new Game object, initializes GUI components,
     * input handling, and game object containers.
     */
    public Game() {
        this.sprites = new SpriteCollection();
        this.environment = new GameEnvironment();
        this.gui = new biuoop.GUI("Paddle Game", WIDTH, HEIGHT);
        this.keyboard = gui.getKeyboardSensor();
        this.sleeper = new Sleeper(); // Instantiate the sleeper
    }

    /**
     * Adds a collidable object to the game environment.
     *
     * @param c the Collidable to add
     */
    public void addCollidable(Collidable c) {
        environment.addCollidable(c);
    }

    /**
     * Adds a sprite object to the game.
     *
     * @param s the Sprite to add
     */
    public void addSprite(Sprite s) {
        sprites.addSprite(s);
    }

    /**
     * Initialize the game by creating and adding the ball, blocks, and paddle.
     */
    public void initialize() {

        Rectangle paddleRect = new Rectangle(new Point(350, 560), 150, 20);
        paddle = new Paddle(paddleRect, Color.ORANGE, keyboard, 5, WIDTH);
        paddle.addToGame(this);

        Ball ball1 = new Ball(new Point(400, 500), 7, Color.BLACK);
        ball1.setVelocity(Velocity.fromAngleAndSpeed(300, 5));
        ball1.setGameEnvironment(this.environment);
        ball1.addToGame(this);
        Ball ball2 = new Ball(new Point(300, 450), 7, Color.BLACK);
        ball2.setVelocity(Velocity.fromAngleAndSpeed(60, 5));
        ball2.setGameEnvironment(this.environment);
        ball2.addToGame(this);

        // Block layout configuration
        int blockWidth = 50;
        int blockHeight = 20;
        int startY = 100;
        Color[] colors = { Color.GRAY, Color.RED, Color.ORANGE, Color.BLUE, Color.PINK, Color.GREEN };
        
        PrintingHitListener printer = new PrintingHitListener(); // One instance for all
        
        for (int row = 0; row < colors.length; row++) {
            Color color = colors[row];
            int blocksInRow = 12 - row;
            int y = startY + row * blockHeight;
            int startX = WIDTH - 10 - blocksInRow * blockWidth;
            for (int col = 0; col < blocksInRow; col++) {
                int x = startX + col * blockWidth;
                Rectangle rect = new Rectangle(new Point(x, y), blockWidth, blockHeight);
                Block block = new Block(rect, color);
                block.addHitListener(printer);
                block.addToGame(this);
            }
        }

        // Boarders of the screen
        int thickness = 20;
        Block top = new Block(new Rectangle(new Point(0, 0), WIDTH, thickness), Color.GRAY);
        Block left = new Block(new Rectangle(new Point(0, 0), thickness, HEIGHT), Color.GRAY);
        Block right = new Block(new Rectangle(new Point(WIDTH - thickness, 0), thickness, HEIGHT), Color.GRAY);
        Block bottom = new Block(new Rectangle(new Point(0, HEIGHT - thickness), WIDTH, thickness), Color.GRAY);
        top.addToGame(this);
        left.addToGame(this);
        right.addToGame(this);
        bottom.addToGame(this);
    }

    /**
     * Run the game -- main game loop.
     * Handles frame rendering and updates sprites every frame.
     */
    public void run() {
        int framesPerSecond = 60;
        int millisecondsPerFrame = 1000 / framesPerSecond;

        while (true) {
            long startTime = System.currentTimeMillis(); // timing
            DrawSurface d = gui.getDrawSurface();
            this.sprites.drawAllOn(d); // Draw all sprites on the screen
            gui.show(d); // Display the surface
            this.sprites.notifyAllTimePassed(); // Notify all sprites that time has passed
            // timing
            long usedTime = System.currentTimeMillis() - startTime;
            long milliSecondLeftToSleep = millisecondsPerFrame - usedTime;
            if (milliSecondLeftToSleep > 0) {
                sleeper.sleepFor(milliSecondLeftToSleep); // Add the sleep to ensure correct frame rate
            }
        }
    }

    public void removeCollidable(Collidable c) {
        environment.removeCollidable(c);
    }

    public void removeSprite(Sprite s) {
        sprites.removeSprite(s);
    }

    /**
     * The main method to launch and run the game.
     *
     * @param args command-line arguments (not used)
     */
    public static void main(String[] args) {
        Game game = new Game();
        game.initialize(); // Initialize the game
        game.run(); // Start the game loop
    }
}
