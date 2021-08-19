# ASOIAF Survival
Survival Analysis for George R.R. Martin's ASOIAF Universe

I figured the best way to keep my coding skills sharp this summer would be to put them to the ultimate test: making sense of all that bloodletting in George R.R. Martin's "A Song of Ice and Fire" universe. 

Over the course of multiple weeks over at my blog (https://peterbarston.blogspot.com/2021/08/collating-game-of-thrones-survival.html), I dug into ASOIAF to figure out how to best visualize and then predict survival rates among characters.

There are a few files in this repository:

#Data contains the primary dataset as well as two additional datasets.
Special thanks to the work done by Myles O'Neill to compile these here (https://www.kaggle.com/mylesoneill/game-of-thrones).

The dataset we will primarily use is "character-deaths.csv", a list of the 917 characters identified in the first five books along with their house allegiances (if noted), information on their death (year, book, chapter), the chapter of their first appearance, and some biographical info (gender, nobility) as well as a boolean appear/not appear in each of the five books.

This is a direct grab of the "character-deaths.csv" from Erin Pierce and Ben Kahle. This dataset was created as a part of their Bayesian Survival Analysis which can be found here: http://allendowney.blogspot.com/2015/03/bayesian-survival-analysis-for-game-of.html. Thanks, Erin and Ben!


#Thrones Pre-Processing is the code for to compute just the chapter span data. If you want to branch out and conduct your own analysis, then this is the place for you.


#Thrones Final Code is my full code to follow along with my blog posts (https://peterbarston.blogspot.com/2021/08/collating-game-of-thrones-survival.html). These highlight specific findings and generally are great practice in GGPLOT and Tidyverse.


Where would you take the analysis next? Will GRRM ever publish again? Reach out to me and let me know!



