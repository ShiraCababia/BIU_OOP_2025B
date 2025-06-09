package core;

import shapes.*;

public class BlockRemover implements HitListener {
    private Game game;

    public BlockRemover(Game game) {
        this.game = game;
    }

    @Override
    public void hitEvent(Block beingHit, Ball hitter) {
        beingHit.removeFromGame(this.game);
    }
}
