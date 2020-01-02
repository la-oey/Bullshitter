# Bullshitter
Study examining how we calibrate our lies to be believable and how we determine mis-calibrated lies, i.e. how we become better liars and detectors.

The experiment consists of a game that is a hybrid of Xu & Garcia (2008) balls in a box design, the ultimatum game, and sender-receiver games.


Exp1 (Exp1 in CogSci2019 paper)
How do people lie and detect lies?
- 40 rounds
- Lying and false accusation penalties scale with reported number
- Explicit feedback about participant's behavior every trial (feedback)
- No manipulation to base rate
- Marble drawing sampled from binomial distribution

Exp2
How does the base rate probability change lying and lie detecting?
- 40 rounds
- Lying and false accusation penalties scale with reported number
- Explicit feedback about participant's behavior every trial (feedback)
- Within subject manipulation to base rate (p_red = {0.2, 0.5, 0.8})
- Marble drawing sampled from binomial distribution

Exp3 (Exp2 in CogSci2019 paper)
How does the base rate probability change lying and lie detecting?
- 80 rounds
- Getting caught lying is always -5 for liar and +5 reward for detector; false accusation penalty is always reported - 5 (liePenalty = 5)
- Feedback about players' scores every fifth trial (no feedback)
- Between subject manipulation to base rate (p_red = {0.2, 0.5, 0.8})
- Marble drawing sampled from binomial distribution

Exp4
How does the base rate probability change lying and lie detecting?
- 100 rounds
- Getting caught lying is always -5 for liar and +5 reward for detector; false accusation penalty is always reported - 5
- Feedback about players' scores every fifth trial (no feedback)
- Between subject manipulation to base rate (p_red = {0.2, 0.5, 0.8})
- Artificially inflated rate of marbles drawn and reported: Marble drawing sampled from uniform-binomial mixture distribution

Exp5
How does inverting player utility change lying and lie detecting?
- 100 rounds
- Getting caught lying is always -5 for liar and +5 reward for detector; false accusation penalty is always reported - 5
- Feedback about players' scores every fifth trial (no feedback)
- Between subject manipulation to base rate (p_red = {0.2, 0.5, 0.8})
- Artificially inflated rate of marbles drawn and reported: Marble drawing sampled from uniform-binomial mixture distribution
- Invert utility structure (i.e. marble drawer gets points for blue marbles, responder gets points for red marbles)

Exp6
How does manipulating the magnitude of the liar's penalty to utility change lying and lie detecting?
- 100 rounds
- Getting caught lying is always -X for liar and +X reward for detector; false accusation penalty is always reported - 5
- Feedback about players' scores every fifth trial (no feedback)
- No manipulation to base rate
- Artificially inflated rate of marbles drawn and reported: Marble drawing sampled from uniform-binomial mixture distribution
- Between subject manipulation to lying penalty (liePenalty = {2, 5, 10})

Exp7
How does playing against a random computer change lying and lie detecting?
- N rounds until player scores 500+ points more than opponent
- Getting caught lying is always -5 for liar and +5 reward for detector; false accusation penalty is always reported - 5
- Feedback about players' scores every fifth trial (no feedback)
- No manipulation to base rate
- Artificially inflated rate of marbles drawn and reported: Marble drawing sampled from uniform-binomial mixture distribution
- Explicitly told playing against a computer taking random actions: liar randomly picks a number between 0 and 10; detector randomly rejects 10\% of the time

Exp8
How does playing as only one role change lying and lie detecting?
- 50 rounds
- Getting caught lying is always -5 for liar and +5 reward for detector; false accusation penalty is always reported - 5
- Feedback about players' scores every fifth trial (no feedback)
- Between subject manipulation to base rate (p_red = {0.2, 0.5, 0.8})
- Artificially inflated rate of marbles drawn and reported: Marble drawing sampled from uniform-binomial mixture distribution
- Participants only played as one role
- Between subject manipulation to participant playing as marble drawer or responder

Exp9
How does live 2-player interaction change lying and lie detecting?
- 50 rounds
- Getting caught lying is always -5 for liar and +5 reward for detector; false accusation penalty is always reported - 5
- Feedback about players' scores every fifth trial (no feedback)
- Between dyad manipulation to base rate (p_red = {0.2, 0.5, 0.8})
- Artificially inflated rate of marbles drawn and reported: Marble drawing sampled from uniform-binomial mixture distribution
- Player Red (gains points for red marbles) vs Player Blue (gains points for blue marbles)
- Trials consist of switching between players as each role



Bugs fixed along the way
- BS detector AI doesn't adjust offset on detecting rate to base rate condition (not a big deal in Exp4 because participant doesn't get feedback about opponent's decision) (Exp2-Exp4)
- In detector role, conflicting instructions on trial screens incorrectly state that the player gets points for red marbles (prompt on trials saying points players would get if detector accepts is correct) (Exp1-Exp4)

*Exp4_2 fixes bugs from Exp4, but we haven't run this version experimentally. It works as a template for future experiments with similar structure.