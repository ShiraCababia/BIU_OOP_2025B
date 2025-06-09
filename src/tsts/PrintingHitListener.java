package tsts;

import core.*;
import shapes.*;

public class PrintingHitListener implements HitListener {

    public void hitEvent(Block beingHit, Ball hitter) {
        System.out.println("A Block was hit. ");
    }

    // public static void main(String[] args) {
    //     Game game = new Game();
    //     game.initialize();
    //     game.run();
    // }
}
