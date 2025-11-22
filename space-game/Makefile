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

all: build

build:
	cabal build

game: build

run: build
	cabal run

clean:
	rm -rf bin game
	rm -f src/*.hi src/*.o
	rm -rf dist-newstyle