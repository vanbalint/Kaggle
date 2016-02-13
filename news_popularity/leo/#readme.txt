2/13/2016

#Literature
Haven't read thoroughly, but the message seems to be:

a) Better data, beats better algorithm
b) Focus on feature engineering, and later ensemble and tweak

#Prediction Attempts

1. Balint's RF + unscaled, randomized, balanced classes
2. GBM+unscaled, unbalanced classes with decomposed time-related features

Insight
a) Class balancing did not perform better than unbalanced classes

# Images
1. Correlation Plot
- low redundancy

2. Variable Importance Comparision (RF vs. GBM)
- interesting how different algorithms rank differently variable importance
- gbm seems to force many coefficients to zero

3. PCA observations raw (bipolar) using scaled data
- no obvious patterns

4. PCA combined
- PC1:related with low vs. quantity of words, entertainment vs world, LDA 3&1 vs LDA 2 (respectively correlated)
- PC2:related with LDA 4 vs. LDA2, tech vs. entertainment
- PC3:related with socmed vs entertainment, LDA 00, LDA 01

- popularity 4,5,3 have positive coordinates on PC1, while 1,2 have negative coordinates. Out of this, the contribution of 4,5,3 is highest
- popularity 2,3 have positive coordinates on PC2, while 1,4,5 have negative coordinates. Out of this, the contribution of 2,1 is highest
- popularity 2,3,4,5 have positive coordinates on PC3, while 1 has negative coordinates. Out of this, the contribution of 3,5,1 is highest

Insight
a) biggest variations explained by word quantity, type of news, and LDA (what is LDA?)

5. Strip_is_weekend (strip plots popularity ~ x | is_weekend)
- After performing PCA, would be insightful to condition on news type and LDA to see if there are any patterns
- I have yet to go over each plot

# Functions created
1. Simple class balancer function
2. Normalizing (from 0~1) function
