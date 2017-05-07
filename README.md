# Analysis of German Credit Dataset using Logistic Regression

Now suppose you are working in the risk management team at a german bank and you have to assist the credit manager to decide whether to approve the loan for a prospective customer or not. So you have to make a logistic regression model to predict the chances of the customer defaulting on the loan or not. The amount the customer will default is not to be predicted. You have to just predict whether the customer will default or not.

**Step 1: Data Understanding and Data Exploration**
Since you already know the business context of the problem, this is the first  stage of solving the problem. In this step, we  get well versed with the dataset. 
 
Once the data is loaded into the R working environment, we do basic exploration of data set which includes:
* Summary and structure of the dataset
* Univariate plots of any 5 variables. 

**step 2: Data Cleaning and Transformation**
In this step, we will identify the missing values and outliers for each variable and impute them accordingly.
Generate dummy variables for factor variables. (Certain variables which should be ideally of factor type might be of character or integer type. you need to first convert such variables into factor variables using “as.factor()”)

**step 3: Splitting the Dataset into train and test**
As you know to learn your model you should use only a portion of the data and keep the rest aside for testing the model. so we Split the given data set such into the train (70%) and test data set (30%). 
 
**step 4:Modeling**
In this step, we will be actually building the logistic regression model on the data set.
* We Make our initial model including all the variables and then select variables using step-wise function in R
* The model from the step-wise procedure should be checked using for multicollinearity using VIF in R (We use a VIF threshold of 3).
* Report the AIC value and Null deviance and Residual Deviance of the final model

**Step 5: Model Evaluation**
Once the model is built evaluate the model using C-statistic and KS-statistic for both train and test data. Based on the values of C-statistic and KS-statistic, determine whether your model has good accuracy or not.
 
**Step 6: Threshold value**
After model evaluation, determine the threshold value of probability using ROC curve. Once the optimal value of threshold value is determined, generate misclassification table for both train and test data and calculate the following:
* Sensitivity
* Specificity
* Overall Accuracy

**In addition to R code, a PDF document is also included in the repository which will further explain the procedure followed and analyze the results obtained**





