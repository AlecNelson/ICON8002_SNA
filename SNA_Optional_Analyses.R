Attribute_test<-c(8,9,10)

Closeness_logistic_df <- subset(Metrics_logistic_df,select=c(1,Attribute_test))
Betweenness_logistic_df <- subset(Metrics_logistic_df,select=c(2,Attribute_test))
Degree_logistic_df <- subset(Metrics_logistic_df,select=c(3,Attribute_test))
Eigenvector_logistic_df <- subset(Metrics_logistic_df,select=c(4,Attribute_test))
Overlap_logistic_df <- subset(Metrics_logistic_df,select=c(5,Attribute_test))

data_logistic_total_df <- subset(data_logistic_df,
                                 select=c(1,3,11,14))

model <- glm(Keyplay.bool ~.,family=binomial(link='logit'),data=data_logistic_total_df)
summary(model)

#Model subset and selection

Closeness_model <- glm(Closeness.bool ~.,family=binomial(link='logit'),data=Closeness_logistic_df)
summary(Closeness_model)
Betweenness_model <- glm(Betweenness.bool ~.,family=binomial(link='logit'),data=Betweenness_logistic_df)
summary(Betweenness_model)
Degree_model <- glm(Degree.bool ~.,family=binomial(link='logit'),data=Degree_logistic_df)
summary(Degree_model)
Eigenvector_model <- glm(Eigenvector.bool ~.,family=binomial(link='logit'),data=Eigenvector_logistic_df)
summary(Eigenvector_model)
Overlap_model <- glm(Overlap.bool ~.,family=binomial(link='logit'),data=Overlap_logistic_df)
summary(Overlap_model)

#Use 95% confidence interval for Estimated effect size (95% confidence intervals) of attributes
str(Closeness_model)
Closeness_model$coefficients
Closeness_model$residuals
Closeness_model$fitted.values

closeness_CI<-CI(Closeness_model$effects,ci=0.95)
