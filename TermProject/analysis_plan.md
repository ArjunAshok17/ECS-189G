# Analysis Plan #
The below document describes the steps we will take/have taken in order to analyze the data included in Scutari's FairML package and the referenced analysis paper "The Five Factor Model of personality and evaluation of drug consumption risk" by Fehrman et al.

## Exploration ##
First step is to explore the dataset, fleshing out how the dataset is laid out in terms of features, size, types of features, what the features describe, etc. We can then move onto picking apart the sensitive features we ourselves deem risky to use (e.g. gender, race, age, etc.) in an effort to avoid discrimination, and then diagnose any proxies in the other features by comparing correlations.

Because our current exploration has revealed the output of the model, risk of abusing a given drug (for the set of all drugs tracked in the dataset), is a numeric variable masked as a factor, we've decided to go ahead with converting them to their proportional numeric value. This time value is derived with respect to a year, so essentially using a drug within the last decade means a 1 / 10 year span, last year means a 1 / 1 span, last month means a 1 * 12 span, etc. This accomplishes two things: (1) the output of the model can essentially be seen as a risk factor, where risk is anywhere in the range $[0, 365.25]$ for never used up till used in the last day. (2) the output is scaled according to a numeric output instead of using dummy variables, allowing us to (a) convert to a probability easier, and (b) give a more nuanced prediction on how long it's been since the last use. This method, though it could prove very functional, may be replaced by the standard approach of one-hot-encoding and a categorical prediction model.

## Scenario Setup ##
Next, we'll come up with a scenario that models (1) the importance of solving this problem and (2) the context in which we try to gauge a fair prediction of what will happen.

## Analysis ##
For our own analysis, we employ the lessons learned from our exploration to best balance fairness and utility with this dataset, taking care to try out different approaches we theorized in the exploration stage.


# Paper Layout #
Ø. Abstract \
    (a) Layout a little context behind the problem, why we're trying to solve it, and how we are attempting to solve it \
    (b) Mention how our findings differ/affirm our comparison research paper

I. Introduction \
    (a) Term defining & background information \
    (b) Relevance & Importance \
    (c) Introduce solution \
    (d) Lay out paper's outline

II. Dataset Exploration

III. Methodology \
    (a) Prior Work \
    (b) Data Engineering \
    (b) Numerical Approach for Modeling

IV. Evaluation \
    (a) Evaluation Criteria \
    (b) Performance in Fairness & Utility

V. Acknowledgements \
    (a) General \
    (b) Author Contributions

VI. Supplementary Materials

VII. References