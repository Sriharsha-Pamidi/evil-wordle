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

## Details

The user can guess any five letter word in the target dictionary.

The guess will be marked by two possible colors:

‣ Black: This indicates that the letter is not in the right position

‣ Green: The letter is in the right position and appears in that position in the final word

The game ends when the user guesses the word. This happens when there are no alternatives to choose from given the previous guesses.

Every time the user starts a new game, a new word is chosen at random. 

## How do we maximize the number of guesses?

At every game state, we maintain the possible list of words that can be the final answer based on the previous user guesses

Based on the current guess word, the algorithm matches nth letters from the list of possible words. The position ‘n’ that gives list with maximum cardinality will be the next possible word list.

The match pattern is updated based on the above process 

The final word to be guessed will be that matches the pattern

At every point we try to maximize the list of possible words that user needs to guess from






