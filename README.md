# Liminality

Liminality: the gateway between two stages.

## Goals
This dungeon crawler is about transition, and what is gained and lost in the process.
It aims to make battles as engaging the 1000th time as the 100th.

But short term the goals are:
- To actually exist
- Functional game loop that runs at a reasonable pace
- Full level editor that can be understood by anyone.
- The modest game assets achievable by a non-artist.

## Current State

3D world is walkable, and movement is animated over time. ECS system and in-game debugger made.

## Requirements

Currently only written on Lispworks 8, this uses Lispworks libraries to run, so at least Lispworks Personal Edition is required.

Usage instructions also assumes quicklisp.

## Usage

Pull this repository to your quicklisp/local-projects directory. Then run the following to get started:

``` common-lisp
(ql:quickload :liminality)
(in-package :liminality)
(main)
(load-level "level-one")
```

## Todo

- Write raylib bindings for importing and manipulating 3d models
- Adjust the ECS system to use events as a queue rather than the byzantine timer system I thought was a good idea.
- Transition system for allowing switching to battles/menus(?)
- start to actually design the game, not the engine.
