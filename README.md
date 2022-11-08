# evil-wordle
UCSD CSE 230 Final Project -- Creating the evil-wordle game in Haskell using Brick library.
**Evil wordle game is a word guessing game with a twist. Everytime you try guessing a word, we change the right answer**

## Members
- Harshith Nagubandi
- Aditya Addepalli
- Sri Harsha Pamidi
- Sanju Prabhath Reddy

## Proposal

The purpose of the project is to create a version of wordle called evil-wordle. In evil-wordle, unlike the original, there's no word set by default. Every time the user takes a guess, we look at all possible 5-letter words that would fit the previous guesses, and choose a match that results in the most possible words. The goal is to maximize the amount of guesses it takes to find the word.
