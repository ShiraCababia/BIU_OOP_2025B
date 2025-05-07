/**
 * The Ass3Game class contains the entry point for running the Arkanoid game.
 */
public class Ass3Game {

    /**
     * The main method launches the game.
     * It creates a Game instance, initializes it with game objects,
     * and starts the main game loop.
     
     * @param args command-line arguments (not used).
     */
    public static void main(String[] args) {
        Game game = new Game();
        game.initialize();
        game.run();
    }
}
