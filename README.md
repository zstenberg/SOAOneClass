# SOAOneClass
Example one class classification model using SOA mortality tables

This repo loops through a series of CSO/CET mortality tables, building a principal components analysis model to describe the general shape of the mortality curve in each.  

Then, using that PCA model, it fits a series of one-class support vector machines to demonstrate how an SVM can be used to test future mortality assumptions for validity.

Finally, it tests each of those SVMs against a pair of unit tests to catch an intentionally erroneous mortality table.
