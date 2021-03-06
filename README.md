# Workbook 5

The purpose of this workbook is to gain experience working with survey data. More specifically, you'll practice:

- Reading survey **documentation**
- Downloading **survey data** and loading it into r
- Calculating population level values using **survey weights**

## Instructions
Perform the following steps to practice using NHANES data.

### Set up


1. Begin by reviewing the [documentation](https://www.cdc.gov/nchs/nhanes/about_nhanes.htm) on NHANES survey weighting. You will want to follow multiple links on that page to learn more about the process they used (to inform the process you'll use in R).

2. Download the 2015 - 2016 NHANES data that can be found on [this page](https://wwwn.cdc.gov/nchs/nhanes/Default.aspx). More specifically, you'll need to download the **Demographics Data** and **Alcohol Use Data** (which can be found on the _Questionnaire Data_ page). You'll download the data in XPT format. Make sure to **find the codebooks** when you download these files, as you'll need them to understand the data.

3. Using the `sasxport.get()` function (part of the `Hmisc` package), load both the demographics dataset and alcohol use dataset.

4. Join these two datasets together by the identifying characteristic (you may have to consult the codebook to find it). Think about which type of join you want to do to analyze the alcohol questions.

5. Using the documentation, find the variable that you believe represents the **sample weight** of each individual _interviewed_ in the dataset. Take the sum of the that column and **interpret the result** (in a comment in R is fine).

### Analysis

The following steps will allow you to calculate the **survey weighted** percentage of people in the United States who have ever have more than 4 (women) or 5 (men) drinks per day (not a great survey question...). As you do so, remember that an easy way to calculate a percentage is to take the mean of a binary (`0`, `1`) variable where `1` is a positive response. **Make sure** to properly deal with people who _chose not to repond_!

1. Using the codebook, find the question that asks about ever drinking have 4/5 or more drinks every day. Then, change the values in the response to make it easier to calculate a percentage (see above).

2. Create a **survey design** that indicates the `id`, `strata`, and `weights` of your dataset. Again, you'll need to see the codebook. This will also prompt you to indicate that the strata are Nested (an R error will guide you through this).

3. Using the `svymean` function, calculate the percentage of people that have ever had more than 4/5 drinks per day.

4. Using the `svyby` function, get the percentage of respondents **by gender** who have ever had more than 4/5 drinks per day

# Explanation

3. Using the svymean function, calculate the percentage of people that have ever had more than 4/5 drinks per day.

This is done using the svymean, and the explanation for svymean can be found under the surveysummary section in the documentation of the  (Links to an external site.)survey library (Links to an external site.):

svymean(x, design, na.rm=FALSE)

The x here is the column we've been working in, which is alq151. Don't forget to put a wave (~) in front of column names when we're working with the survey library!

The design here is the survey design we created for question 2, which we called nhanes_survey during section. We also want NA's to be ignored, so let's make sure we do na.rm=TRUE.

4. Using the svyby function, get the percentage of respondents by gender who have ever had more than 4/5 drinks per day

The svyby function calculates statistics "by" a certain standard; in this case, it's gender.We once again look for the function in the documentation (Links to an external site.), which gives us the code:
svyby(formula, by ,design, FUN,...)

formula here refers to the "A formula specifying the variables to pass to FUN (or a matrix, data frame, or vector)", which is just another way of saying the column/data frame we're currently working in. Once again, that is alq151 for us.

The by parameter expects a column that another "formula" that serves as the dividing factor that we are dividing "by". In this case, that is gender, so we look for the gender column. Thankfully, we have the DEMO_I codebook (Links to an external site.), which tells us that ~riagendr is the column we're looking for.

The design parameter once again refers to the survey design object, which is nhanes_survey.

Lastly, FUN is the function we're applying to the formula, which is mean. In the survey library, that is svymean.