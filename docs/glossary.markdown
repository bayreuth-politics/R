---
layout: page
title: Glossary
permalink: /glossary/
has_children: false
nav_order: 2
---

# Glossary

Here you find a brief explanation of relevant terms and concepts we will be using in class and those which are relevant when using R.

----

## Statistics 

- **Balance (on Pre-Treatment Characteristics):** Refers to the idea that treatment and control groups are similar (ideally identical)  along a range of pre-treatment, observable characteristics (e.g. as many democratic as autocratic countries, as many rich as poor individuals, as many men as women).

- **Bias:** Refers to the tendency of a measurement process to overestimate or underestimate the real value of a population parameter. In statistics, the bias of an estimator is the difference between the estimator's expected value and the true value of the parameter estimated.

- **Bin (of a Histogram):** When creating a histogram, you start by dividing the range of values in a series of intervals called *bins*, before counting how many values fall into each of them. Bins are usually consecutive, non-overlapping intervals of the same size (though that is not strictly required).

- **Bivariate Analysis:** Refers to the analysis of the relationship between two variables - as opposed to univariate analysis (analysis of one variable) and multivariate analysis (analysis of the relationship between two or more variables simultaneously).

- **Central Tendency:** The tendency of a variable to cluster around its *mean*, *median* or *mode* (which are therefore referred to as measures of central tendency).

- **Counfounding Factor or Confounder and Confounding Bias:** Any pretreatment variable that influences both the dependent and independent variables. They result in confounding bias, making it impossible to draw causal inferences from the data.

- **Cross-Sectional Data:** Data containing observations of different units at one point in time. We also talk about a cross-section of a study population.

- **Descriptive Methods:** Range of statistical methods used to summarize data - such as the mean, average, standard deviation etc. - and are usually reported to make it easier to understand and report information.

- **Experimental Study:** An empirical investigation in which researchers introduce an intervention (or *treatment*) and study its effects. Experimental studies are usually randomized, meaning the subjects are randomly allocated to either the treatment or the control group (we then talk about *Randomized Controlled Trials*). The researcher is *in control* of study conditions. This is important because it ensures that the average difference in the outcome of interest between the treatment and control groups can be attributed only to the treatment. Whether one is treated or not is independent of other contextual factors.

- **External Validity:** The extent to which conclusions can be generalized beyond  a particular study.

- **Inferential Methods:** Range of statistical methods which use data on a sample of the population to make predictions about the population as a whole.

- **Internal Validity:** The extent to which the assumptions needed to draw causal claims from a study are satisfied.

- **Kernel Density Estimation:** Non-parametric density estimation method. The intuition is the following: one of the issues with histograms when used to visualize the distribution of a continuous variable is that they are not smooth as they depend on the width and end point of the bins. Kernel estimators centre a function called kernel function at each data point. It amounts to a local average, as Kernel estimators smooth the contribution of each data point over a local neighborhood of that observation.

- **Natural Experiment:** A type of observational study in which an event allowing for a (quasi)-random assignment of study subjects to different groups is exploited to investigate a particular research question. They are often used in situations in which controlled experiments are not feasible (e.g. for ethical reasons). Examples: weather events, natural disasters, large and unexpected migration flows.

- **Observational Study:** An empirical investigation that draws inference about the population based on a sample, or estimates the effects of one (set of) variables on another, when the main independent variables are not deliberately administered. In other words, the researcher is not *in control* of study conditions but works with what is out there.

- **Observations:** The units under study in an empirical inquiry. Typically, the rows in a data frame or data set.

- **Outlier:** An outlier is an observation that lies at an *extreme* distance from the rest of the values for a variable in a random sample from a population. There is no ex ante, unique definition of what an abnormal distance is, it is often left to the researcher's subjective judgement. When visualizing a boxplot in R, outliers are observations which fall outside of the interval *[Q1 - 1.5xIQR;Q3 + 1.5xIQR]* where Q1 and Q3 are the first and third quartiles respectively; and IQR is the inter-quartile range.

- **Population and Sample:** The *population* is the universe of potential and possible cases of interest (individuals, events, objects etc.) defined with respect to a study. The population of a national election study, for instance, is all the possible voters in that country. A *sample* is the subset of the population for which the study collects and includes data on.

- **Pre-Treatment Variable:** Variables realized before the  administration of the treatment, which are therefore not affected by the latter (e.g. gender, age).

- **Random Sample:** Sample of *i* subjects from a population in which each sample of that size has the same probability i.e. chance of being selected. Random means there is an equal chance of selection.

- **Regression Analysis:** A statistical method allowing us to examine the relationship between two or more variables of interest. At the core, most types of regression analyses study the influence of one or more independent variables (right-hand side variables) on a dependent variable (left-hand side variable, also known as the outcome of interest).

- **Study Arms:** The arm of a randomized experimental study refers to a group of subjects who receive a specific treatment (or no treatment) among several possible treatments. One can think of them as different treatment conditions.

- **Subgroups:** Sub-division of the sample population into smaller groups, typically according to a categorical variable - or meaningful cut-offs of continuous variables.

- **Treatment and Control Groups:** The treatment group is the group that has received the treatment (hence we only know what the post-treatment outcome is under treatment conditions for all subjects in this group). The control group is the group which does not receive the treatment (hence we only observe the post-treatment outcome under no treatment for all subjects in this group).

- **Treatment Variable:** A key causal variable of interest, the effect of which we want to estimate  - not related to medical treatment!

- **Variable:** Generally speaking, a variable is any characteristic that can vary in value (e.g. 1, 2, 3... or blue, green) among observations in a sample or in the population.

  
-----

## R + RStudio

- **Append:** To add elements to the end of a vector or data frame.
  
- **Binary Variable or 'Dummy' Variable:** Also known as an indicator variable, it is a dichotomous, numeric variable that can take on any two specific quantitative values. Usually, for interpretation purposes in regression analysis, a dummy takes the value 0 to indicate the absence of some qualitative attribute and 1 to indicate its presence. Example of dummy: party = 1 if Labour, 0 otherwise; gender = 1 if female, 0 otherwise.

- **Class:** A class is a type of object. R recognises different types of objects by assigning each object to a class; which allows it to perform appropriate operations depending on the object's class. For instance, a number is stored as a numeric object, whereas a character string is stored as a character object. The function *class()* tells us which class an object is.
  
- **Command:** You type R code into the bottom line of the *RStudio Console* panel and then click *Enter* to run it. The code you type is called a command, because it will command your computer to do something for you. The line you type it into is called the command line.

- **Data Frame:** A data frame is a table or a two-dimensional structure in which each column contains values of one variable and each row contains one value from each column. The columns may have different types, but all the elements in each column must have the same type. Data frame rows and columns may have names (*rownames, colnames*). Typically, columns are named but rows are not. Usually, data sets are stored as data frames.

- **Discrete/Categorical Variables v. Continuous Variables:** A variable is discrete or categorical if the values it takes form a set of separate numbers e.g. (0, 1, 2, 3...); it is continuous if it can take an innite continuum of possible real number values e.g. any value in [0;1]. Examples of categorical variable: countries, year, gender, occupation; examples of continuous variables: revenue, vote share, year.

- **Factors:** A factor in R is a categorical vector, where the elements can be one of several different levels. It can be either a numeric or string variable.
  
- **Function:** A function is a command that takes a series of inputs, and returns an object (or objects) as output. R has a large number of in-built functions (e.g. sum(); mean ()) but you can also create your own functions which can be saved to the environment.

- **Indexing:** Vector elements are accessed using indexing. You can access an individual element of a vector by its position (or "index"), indicated using square brackets [ ]. In R, the first element has an index of 1. Example: to get the 7th element of the colors vector: *colors[7]*.

- **List:** A list, like a vector, is a one-dimensional object. Unlike vectors, lists can store objects of dierent types. The elements of a list are accessed using double square brackets i.e. *my_list[[3]]* returns the third element of *my_list*.

- **Logical or Boolean Expressions:** Boolean expressions are logical statements that are either TRUE or FALSE; e.g. we can use Boolean expressions to compare quantities. For instance, the Boolean expression 1 < 2 is *true*, whereas the Boolean expression 1 > 2 is *false*. When you type a Boolean expression in R, R will output *TRUE* if the expression is true and *FALSE* if the expression is false.

- **Matrix:** A matrix is a two dimensional tabular data structure in which all the elements are of the same type. We will typically be dealing with numeric matrices, but it is also possible to have character or logical matrices, etc.  Matrix rows and columns may have names (*rownames, colnames*).

- **Missing Value in R:** A missing value is a value which is unknown - e.g. corresponding to a blank cell in an excel spreadsheet or a dot in *Stata*. They are represented in R by the symbol *NA* (Not Available). Impossible values (e.g. divided by zero) are represented by the symbol *NaN* (Not a Number). Most modeling functions in R offer options for dealing with missing values. You should always be careful when dealing with data that has missing values - which is the case most of the time.

- **R Environment:** An environment can be thought of as a collection of objects (functions, variables etc.). An environment is created when we first fire up the R interpreter. Any variable we define is now stored in this environment. The global environment is your workspace: that's where the computation takes place.

- **R Object:** An object is a shortcut to some information stored in memory. Pretty much everything in R is an object. Objects exist in the *Global Environment* and are called by name. Importantly, this means you can save and call various objects such as data sets indepedently of each other.

- **R Operators:** R has many operators to carry out different mathematical and logical operations. Main types of operators are arithmetic operators (used to carry out mathematical operations like addition and multiplication, e.g. +, -, *, /); relational operators (used to compare between values); logical operators (used to carry out Boolean operations e.g. *and* (&), *or* (-), *not* (!); and assignment operators, used to assign values to variables - e.g. <- and =. The $ operator is used to access a column by name (i.e. data.frame$variable.to.call).

- **R Package, Directory and Library:** Packages are collections of R functions, data, and compiled code in a well-defined format. The directory where packages are stored is called the library. R comes with a standard set of packages. Others are available for download and installation. Once installed, they have to be loaded into the session to be used.

- **R Working Directory:** The starting folder from which R tries to read and write to files or folders. The working directory tells R where to start looking for files, and is hierarchical.

- **Subsetting:** Selecting and excluding variables (columns) and/or observations (rows), keeping only a part of the original dataframe.

- **Variable in R:** In R, a variable can store a vector, group of vectors or a combination of many R objects. A valid variable name consists of letters, numbers and dot or underscore characters.

- **Vector:** A vector is one-dimensional data structure in R. Simply put, a vector is a sequence of data elements of the same basic type, stored in a particular order. Members of a vector are called components or elements. Vectors can be numeric, characters or logical. Vectors are notated in R using the letter *c* followed by parentheses (where c means combine). A vector containing three numeric values 2, 3 and 5 for instance is: *c(2, 3, 5)*. A vector containing five logical values: *c(TRUE, FALSE, TRUE, FALSE, FALSE)*. A vector containing character strings: c(*blue*, *green*, *red*). Numbers in R are just vectors of length 1. The elements of a vector are accessed using single square brackets i.e. *my_vec[1]* returns the first value in the vector *my_vec*.
