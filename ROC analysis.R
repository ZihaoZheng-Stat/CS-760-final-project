## let's look at the AUC

AUC = matrix(NA, nrow = 4, ncol = 3)

rownames(AUC) = c("logistic", "SVM", "Random Forest", "Naive Bayes")

colnames(AUC) = c("lower bound", "predicted value", "upper bound")

AUC[1,] = as.numeric(ci(roc.logit))

AUC[2,] = as.numeric(ci(roc.svm))

AUC[3,] = as.numeric(ci(roc.forest))

AUC[4,] = as.numeric(ci(roc.NB))

## DeLong test for pairwise comparison

DeLong = matrix(NA, nrow = 4, ncol = 4)

rownames(DeLong) <- colnames(DeLong) <- rownames(AUC)

DeLong[1,2] = roc.test(roc.logit, roc.svm)$p.value
DeLong[1,3] = roc.test(roc.logit, roc.forest)$p.value
DeLong[1,4] = roc.test(roc.logit, roc.NB)$p.value
DeLong[2,3] = roc.test(roc.svm, roc.forest)$p.value
DeLong[2,4] = roc.test(roc.svm, roc.NB)$p.value
DeLong[3,4] = roc.test(roc.forest, roc.NB)$p.value

## for ROC figure

plot(roc.logit, col = "red", main = "ROC figure for learning models")
lines(roc.svm, col = "blue")
lines(roc.forest, col = "orange")
lines(roc.NB, col = "green")

legend("bottomright", legend = c("Logistic Regression", "Support Vector Machine", "Random Forest", "Naive Bayes"),
       col = c("red", "blue", "orange", "green"), lty = 1)