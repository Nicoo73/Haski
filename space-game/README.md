# Space Game - Haskell

A simple space shooter game written in Haskell using Gloss and JuicyPixels for rendering.

## Requirements

- GHC (Glasgow Haskell Compiler)
- Gloss library
- Gloss-juicy library (for PNG loading)

## Installation

Install dependencies:

```bash
cabal update
cabal install gloss gloss-juicy JuicyPixels
```

## Build and Run

```bash
make run
```

Or manually:

```bash
ghc -Wall -O2 -threaded -outputdir bin -o game src/Main.hs src/GameState.hs src/Input.hs src/Render.hs src/Assets.hs
./game
```

## Controls

- **W**: Move up
- **A**: Move left
- **S**: Move down
- **D**: Move right

## Project Structure

```
space-game/
├── ../assets/                   (shared assets folder)
│   ├── player/
│   │   └── spaceship_up.png    (32x32, contains 4 frames of 16x16)
│   └── background/
│       └── background_1.jpg
├── src/
│   ├── Main.hs
│   ├── GameState.hs
│   ├── Input.hs
│   ├── Render.hs
│   └── Assets.hs
├── bin/                         (compiled objects)
├── Makefile
└── README.md
```

## Features

- Player movement with WASD keys
- Animated spaceship sprite (4 frames from 32x32 sprite sheet)
- Background rendering
