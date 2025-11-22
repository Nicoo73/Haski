# Makefile for Space Game

.PHONY: all clean run

all: game

game: src/Main.hs src/GameState.hs src/Input.hs src/Render.hs src/Assets.hs
	mkdir -p bin
	ghc -Wall -O2 -threaded -outputdir bin -o game \
		src/Main.hs \
		src/GameState.hs \
		src/Input.hs \
		src/Render.hs \
		src/Assets.hs

run: all
	./game

clean:
	rm -rf bin game
	rm -f src/*.hi src/*.o
