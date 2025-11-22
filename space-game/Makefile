# Makefile for Space Game

.PHONY: all clean run

SRC = src/Main.hs \
      src/GameState.hs \
      src/Input.hs \
      src/Render.hs \
      src/Assets.hs \
      src/Enemy.hs \
      src/Update.hs \
      src/Wave.hs

all: game

game: $(SRC)
	mkdir -p bin
	ghc -Wall -O2 -threaded -outputdir bin -o game \
	$(SRC)

run: all
	./game

clean:
	rm -rf bin game
	rm -f src/*.hi src/*.o
