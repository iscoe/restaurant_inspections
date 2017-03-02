# Improving the Restaurant Inspection Process through Data Science
In 2015 the City of Chicago along with some partner organizations built a model
to predict the risk that restaurants would fail health inspections. The data, model,
and documentation can be found in their [Github repository](https://github.com/Chicago/food-inspections-evaluation).
Montgomery County in Maryland with help from Open Data Nation [replicated](http://www.mayorsinnovation.org/images/uploads/pdf/1_-_Montgomery_MD.pdf)
the Chicago work with similar results.

In 2016, JHU/APL collected data from three cities to perform an additional
replication study. These cities are Denver, Raleigh, and Syracuse. We also
revisited the Chicago model before and after building the models for the
three new cities. The models achieved comparable performance to the 
Chicago model, but varied widely in the choice of variables.

Data
---------------
The types of variables in the models can be broken into 3 categories:
  * information about the restaurant (how long it has been open, does it serve alcohol, cuisine served)
  * inspection information (previous inspection violations, time since last inspection, who is the inspector)
  * location information (crime statistics, complaint data, census data)

For the three cities analyzed in this study, we found that location specific 
variables either did not provide any predictive power or were 
a proxy for another variable. For example, with the Raleigh data the mean
score of nearby restaurants was an important variable in the model until
we controlled for inspector id. In the Syracuse model, a particular 
zip code had a large weight which we determined was due to the 
type of food establishments in that zip code. With the Chicago model, we 
found that we achieved equivalent performance by dropping the location
specific variables (crime and complaint data).

Inspector Influence
--------------------
When analyzing the original Chicago model, we found that we could attain
almost the same results by dropping all variables except for the 
inspector id. The Raleigh data was similarly influenced by the inspector id
variable. We did not have inspector level information for Syracuse. Denver
did not appear to have any inspector influence in the data.
