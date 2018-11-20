Replication materials for \`\`Winners, Losers, and Referendums: The Impact of Brexit on Political Attitudes''
================

Data
====

The primary source of data for this paper is the [British Election Study Combined Wave 1-14 Internet Panel](https://www.britishelectionstudy.com/data-object/british-election-study-combined-wave-internet-panel/)

Data for local authority-level election results comes from [The Elections Centre](http://www.electionscentre.co.uk/)

Replication instructions
========================

1.  `data_tidying.R` cleans up the variables used in my models.
2.  `data_local_election_dummies.R` cleans up data to create a dataframe with local authority in the rows and years (dummy indicator whether the local authority had an election in the given year or not) in the columns.
3.  `data_tidying_panel.R` converts the data into from wide into long format, which is required for the analysis. I also scale the most variables to range from 0-1 for ease of interpretation.
4.  `interaction_plot_clustered.R` creates the `interaction_plot_binary()` function, used to calculate marginal effects with clustered standard errors.
5.  `models.R` runs all of the models included in the paper. This file also produces (1) the LaTeX regression output used in the paper (using `texreg`) and (2) marginal effects plots.
