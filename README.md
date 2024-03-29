# Statistical-Analysis-of-Factors-Affecting-U.S.-and-Boston-Housing-Rental-Prices
## Objective
This study aims to employ statistical methods to rank factors affecting rental prices across U.S. cities with an additional analysis case dedicated to the greater Boston area, and to establish a reliable regression model to predict fair rent prices given a property’s features.
Price of rent in dollars/month serves as the main dependent variable, and will be analyzed for correlation with various factors including property type, number of beds, number of baths, square feet footprint, dog permittance, cat permittance, wheelchair access, electric vehicle plug availability, and furnishings.
The resulting degrees of correlation will be ranked to draw a statistical inference on features most likely to yield high housing rent prices across the U.S., and finally a regression model to predict fair rental prices to assist students in their apartment searches next semester.

## Study Design
- The study aims to ultimately estimate and rank correlation coefficients between rent prices vs. various factors.
- Two study cases were conducted; one for the entire continental U.S., and a subset dedicated to the greater Boston area.
- Descriptive Statistics was initially employed to perform preliminary analysis. Unfiltered data was sorted into categories by property types at first, with means, standard deviations, and 95% confidence intervals.
- Based on the results of the descriptive statistics, property types deemed similar were tested for similarity via t-tests, followed by ANOVA, and any post-hoc tests if the means were deemed sufficiently different via ANOVA.
- We then plotted the results to establish an overall pattern, and to identify any obviously anomalous data, which were then removed.
- To determine which variables were most significantly correlated to the price of rent, Spearman’s correlation coefficient method was chosen, primarily due to its resistance to outliers when compared to Pearson’s method. The resultant correlation coefficients were then ranked in the order of significance on its impact on rent price.
- Finally, using the resulting data, we constructed a linear regression model to predict fair rental prices based on a hypothetical property’s specifications. Our explanatory variables consisted of both continuous (i.e. number of bedrooms) and categorical (pets allowed/disallowed) types - therefore, a multiple regression model was employed.
