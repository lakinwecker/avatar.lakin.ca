# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Animated avatar for lakin.ca built in Elm 0.19.1. Implements a dual-color variant of Conway's Game of Life that spawns the initials "LW" and then evolves them. Pure frontend SPA with no backend.

## Build and Development

Uses devenv + direnv for the dev environment (provides Elm, bun, Node.js). Vite with `vite-plugin-elm` handles bundling.

```bash
dev        # Vite dev server with HMR
build      # Production build to build/
build-opt  # Production build with --optimize Elm flag
deploy     # Optimized build + rsync to lakin.ca
```

No test files exist yet, but `elm-explorations/test` is configured as a dependency.

## Architecture

The app follows The Elm Architecture (TEA) in a single file: `src/Main.elm`.

**Animation state machine**: `LoadingAnimation` (cells appear one-by-one at 30ms) -> `LoadingPause` (1500ms pause) -> `Evolving` (Game of Life runs at 1000ms ticks).

**Cell lifecycle**: `Dead` -> `Spawning` -> `Alive` -> `Dying` -> `Dead`. CSS keyframe animations in `index.html` handle the visual spawn/die transitions (1.1s).

**Dual-color Game of Life**: Cells are White or Black. Neighbor counting tracks colors separately, and spawning rules differ by color. The board is toroidal (wraps at edges) with a 32x32 grid.

**Key types**: `Cells` is a `Dict (Int, Int) Cell`. The `Model` holds the board, a loading queue for the initial animation, viewport size, and animation state.

`src/index.js` bootstraps the Elm app, passing window dimensions as flags. `index.html` is the entry point at the project root (Vite convention), with CSS animations. Static assets live in `public/`.
