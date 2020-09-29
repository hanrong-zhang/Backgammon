# Backgammon
 a Backgammon game written in Typed Racket

## Background
This Backgammon game was originally written for UChicago CS151, an introductory computer science course taught with the functional programming language Typed Racket. The program can be run in IDE DrRacket 7.0.

## Instructions
The interface takes mouse and keyboard input from two players.\
"u" undoes a move, can be done as many times unless reaches the starting setup of the game.\
"s" saves the current game file, will prompt the user for an output file location.\
"l" loads an existing game file, will prompt the user with a pop up window to choose a file.\
Move a checker by clicking on the one that needs to be moved, then click on the position where it desires to be.

## Updates
12/10/2019 3rd phase: implemented undo operations, detection of a winning state, ability to save and load game files.\
11/26/2019 2nd phase: implemented move operations, legal move checking, and user input.\
11/10/2019 1st phase: implemented GUI and game data structures such as checker location types and a checker position list.
