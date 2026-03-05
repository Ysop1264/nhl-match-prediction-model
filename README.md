# NHL Regulation-Time Prediction Modelling (R)

## Overview
This project predicts NHL match outcomes **at the end of regulation time** (Home Win / Draw / Away Win).
It builds and evaluates multiple probabilistic models using match-level and player-game data (2011–2023),
and tests whether model probabilities can generate positive expected value bets against bookmaker odds.

## What’s included
- Feature engineering from player-level data to team/game-level inputs
- Rolling-window tuning for short-term form variables
- ELO rating system with hyperparameter tuning
- Ordered logit models (ELO / rolling / rest / combined)
- Dixon–Coles inspired Poisson goal model (time-decay + low-score correction)
- Out-of-sample evaluation using Log Loss and multiclass Brier Score
- A simple positive-EV betting strategy evaluation

## Data
Two CSV files are included within a zip file (regular season games, regulation outcomes only):
- `data/playergamedata.csv` (player–team–game level)
- `data/gamedata.csv` (game level + bookmaker odds)

A full column dictionary is provided in `data/README_nhl.pdf`.

## Repository structure
