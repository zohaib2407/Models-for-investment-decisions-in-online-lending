# Models-for-investment-decisions-in-online-lending
This analysis will consider in more detail the question on effective investment decisions based on our predictive models.

This is a continuation of the previous analysis where we developed tree based models to predict “Fully Paid” vs “Charged Off” loans in the Lending Club platform. Here we will consider in more detail the question on effective investment decisions based on our predictive models. We will also look into balancing the training data, parameter tuning, and reliable performance estimates through resampling and cross-validation.

At first, we developed boosted tree models to predict loan status experimenting with different parameters using a grid of parameter values. 

For predicting ‘loan status’ using tree-based methods, we used gradient boosted model using GBM package in R. The default settings in gbm includes a learning rate (shrinkage) of 0.001. This is a very small learning rate and typically requires a large number of trees to find the minimum MSE. However, gbm uses a default number of trees of 100, which is rarely sufficient. Consequently, we crank it up to 2,000 trees. The default depth of each tree (interaction.depth) is 4, which means we are ensembling a bunch of stumps. Lastly, we also include cv.folds to perform a 5 fold cross validation. 

After our initial gradient boosted model was built, we plot the loss function as a result of n trees added to the ensemble.

![image](https://user-images.githubusercontent.com/35283246/143790852-c80ecd63-3ae4-4f9d-8b1c-04047f6aa236.png)

We then looked at the RMSE of the model and the number of trees at which the model converge. For our model, the number of trees where our model converged was at 1991 trees. This proved the fact that we don’t need to build more than 2000 trees so as for our model to converge. We also checked the variable importance to check if that is aligned with our initial variable selection. And found out it is very much aligned with the results of variable selection initially.
For ex. Sub-grade, DTI, interest rate are some of the variable with highest influence on the target class.

![image](https://user-images.githubusercontent.com/35283246/143790867-d62d152f-da35-4f66-9adc-13affff89275.png)

However, rarely do the default settings suffice. We could tune parameters one at a time to see how the results change. For example, we increased the learning rate to take larger steps down the gradient descent, reduced the number of trees (since we are reducing the learning rate), and increase the depth of each tree from using a single split to 3 splits. This model achieves a significantly lower RMSE than our initial model with only 1,260 trees. 
Then we evaluated our model on the unseen data. We used AUC, ROC and the confusion matrix for our evaluation. 

![image](https://user-images.githubusercontent.com/35283246/143790876-5ce64c21-32ae-4ad2-ab6f-c1c805939a6d.png)

However, a better option than manually tweaking hyperparameters one at a time is to perform a grid search which iterates over every combination of hyperparameter values and allows us to assess which combination tends to perform well. To perform a manual grid search, first we want to construct our grid of hyperparameter combinations. We’re going to search across 6 models with varying learning rates and tree depth. 

After about 30 minutes of training time our grid search ends. We see a few important results pop out. First, our top model has better performance than our previously fitted model above, with lower RMSE.

One possible option is to build a GBM model with 3000 trees, a shrinkage of 0.001 & interaction depth of 5. When we examine the loss function, the loss is smooth, but the number of trees can be further reduced.
![image](https://user-images.githubusercontent.com/35283246/143790895-b0dff3f1-a82b-42c6-8b43-8cd9a321674d.png)

If we look at the relative importance, we see that the variables Sub Grade, Account openings in past 24 months, Debt to income ratio, annual income and home ownership are at the top.

![image](https://user-images.githubusercontent.com/35283246/143790916-f9212eea-af17-4628-9d95-9be35f3583e6.png)

![image](https://user-images.githubusercontent.com/35283246/143790921-83cee242-7b0a-4e8b-b6de-23736a6c6817.png)

Final Model –

From the above grid, we can see that the best model performance is for tree depth of 2 and shrinkage parameter of 0.01. 

We then built our final gradient boosted model using the above parameters which gave us lowest mean squared error.
After building our model, we evaluated our model on the unseen data and looked at various evaluation metrics. 

![image](https://user-images.githubusercontent.com/35283246/143790933-affef98e-3755-4e82-97a0-31603438c0f7.png)

Combined ROC plot – As evident below, if we plot all the curves, the models perform quite similarly on unseen data but, the final model is marginally better when compared to model 1 & model 2. Model 1 performs better at the start but as evident from AUC analysis, the final model has 4% more area under the curve.

![image](https://user-images.githubusercontent.com/35283246/143790948-c0a6cf7a-b474-4dbd-861f-1a6adb1592bd.png)

Profit Curve - The profit curves at different cut-off values are shown in the graph below. All models perform best at a cut-off of 0.5. Among the three models, the final model gives a higher profit. At lower cut-off values, the models classify all loans as Fully paid. Therefore, the analysis starts at a threshold of “0.4”. The Red line indicates Model 1, blue line is Model 2 & the green line is the Final Model. 

![image](https://user-images.githubusercontent.com/35283246/143790956-3b196c6e-698d-4cc4-a265-2740469d9b3c.png)

**Models to Identify best returns**

In the next part we will develop models to provide best returns. 
We developed three different models : GLM, Random Forest and GBM. 

Linear Model using GLM

Before building linear regression model, we’ve first looked at the correlation between the different numeric variables in the dataset to check for multicollinearity. We have first built the correlation plot between all the pairs of numerical variables.

![image](https://user-images.githubusercontent.com/35283246/143791105-e8943198-a854-4b30-a8fa-5e4e96d5eeb1.png)

 We’ve further checked for the correlation between variables below a certain threshold. For implementing the threshold we’ve taken the cut-off value of 0.6. 
 
 ![image](https://user-images.githubusercontent.com/35283246/143791119-a031b7fe-fb1f-44a5-8a1f-2273a76f3489.png)
 
 From the above plot, we looked at the top ten variables having high correlation with the predictor variable i.e. ‘annual return’.

Next, we built two different linear models. One with lasso regularization and the other with Ridge regularization.
We experimented with different value of parameters, for both the models. After evaluating both the models, we picked the best one. 

**Lasso Regularization**

For lasso regularization, we used the value of alpha parameter as 1. We trained our model on training set and then checked the mean-squared error on cross validation samples for varying values of the regularization parameter (lambda).

![image](https://user-images.githubusercontent.com/35283246/143791144-283bde54-905d-4d9e-9618-e825ed81b763.png)


We then built the model with the values of lambda i.e. ‘lambda min’ and ‘lambda 1 STD’. After building the two models, we evaluated the two lasso models on our test data. 

We found the best model to be one with lambda value of ‘lambda 1 STD’ having a R-Squared value of 73.89%.  This means that around 74% of our variations is explained by our linear model. 

**Ridge Regression**

We also built a linear model with ridge regularization. For ridge regression, the value of our parameter, alpha would be 0. After building the model, we then looked at cross validation error for varying values of the regularization parameter i.e. lambda. 

![image](https://user-images.githubusercontent.com/35283246/143791154-0b8c1dfb-d383-4213-8396-4ba03b8bfc32.png)

We then evaluated our ridge regression model on our test data for lambda value that gives the minimum cross validation error, i.e., lambda.min.

The ridge regression model gave us a R-Squared value of 73.26% which is not substantially different from our lasso model. However, the lasso model seems to perform slightly better. Also, the lasso model has the inherent ability of variable selection. Hence, we chose final model to be lasso. 

For our final model, we built the lasso model. We then checked the QQ plot of some variables to see if the variables selected by default by the lasso model is in fact significant. Below is the QQ plot of interest rate which is highly correlated with our response variable and it is visible from the plot that interest rate is normally distributed.

![image](https://user-images.githubusercontent.com/35283246/143791171-2c7b114f-4ee1-4e9c-a54c-ec62acb7fbc0.png)

Also, we checked the L1 normalization plot of the lasso model to see how different variables have been handles by our lasso model.

![image](https://user-images.githubusercontent.com/35283246/143791177-1dbee382-fa49-449d-99bb-85cd3cc4c0ad.png)

Next we built Random forest and GBM and did compafrative evaluation. 









