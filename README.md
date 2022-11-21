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

## App State 
- Assuming the word length to be 5 letters 

| State         | Data          | Description                 |
| ------------- | ------------- | --------------------------- |
| word_list     | [String]      | list of possible words      |
| guess_words   | [String]      | list of words guessed       |
| max_chances   | Int           | maximum number of chances   |
| word_length   | Int           | word length ('5' as default)|
| chances       | Int           | number of chances left      |
| alpha_dict    | String: lState| Alphabet to lState map      |
| game_result   | gState        | game state                  |

### lState 
At each state of the game, we maintain a mapping for all the alphabets based on the previous guesses and maximizing the chances that player needs
| Value     | Description                         |
| --------- | ----------------------------------- | 
| 0         | Not available - guessed incorrectly |
| 1         | Available - not yet guessed         | 
| 2         | Available - guessed correctly       | 

### gState 
State of the current game 

| value     | Description                                   |
| --------- | --------------------------------------------- |
| "Begin"   | Represents the first chance                   |
| "Playing" | Started playing the game, result is not known |
| "Won"     | Won the game - guessed the word correctly     |
| "Lost"    | Exhausted all the chances                     | 



