import numpy as np

from time import time
from scipy.stats import randint as sp_randint
from sklearn import preprocessing
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import RandomizedSearchCV
from sklearn.datasets import load_digits
from sklearn.ensemble import RandomForestClassifier
import csv
import scipy.stats
import graphviz

# get some data
PathRead="./classification/dataset_referrals_I10-I16_discrete.csv"
filereader = csv.reader(open(PathRead), delimiter=",")
Xnames=next(filereader, None)
Xnames=Xnames[:-1]
#Xnames=np.fromstring(Xnames[0], dtype=int, sep=',')

X=[]
y=[]
ages=("Infant/Toddler","Preschool child","School-aged child","Adolescent","Young adult","Middle-aged adult","Elder")
for row in filereader:
    #row=np.fromstring(row[0], dtype=int, sep=',')
    row[242]=ages.index(row[242])
    X.append(row[:-1])
    y.append(row[-1])
    #print len(row)

#X=np.array(X)
#y=np.array(y)

#min_max_scaler = preprocessing.MinMaxScaler()
#X = min_max_scaler.fit_transform(X)

# build a classifier
clf = RandomForestClassifier(n_estimators=20)


# Utility function to report best scores
def report(results, n_top=3):
    for i in range(1, n_top + 1):
        candidates = np.flatnonzero(results['rank_test_score'] == i)
        for candidate in candidates:
            print("Model with rank: {0}".format(i))
            print("Mean validation score: {0:.3f} (std: {1:.3f})".format(
                  results['mean_test_score'][candidate],
                  results['std_test_score'][candidate]))
            print("Parameters: {0}".format(results['params'][candidate]))
            print("")
    best=np.flatnonzero(results['rank_test_score'] == 1)
    d=results['params'][best[0]]
    return d

# specify parameters and distributions to sample from
param_dist = {"max_depth": [30,None],
              "max_features": sp_randint(50, 100),
              "min_samples_split": sp_randint(2, 15),
              "min_samples_leaf": sp_randint(1, 15),
              "bootstrap": [True, False],
	      "min_impurity_decrease": scipy.stats.expon(scale=.1),
              "criterion": ["gini", "entropy"],
              "class_weight" : ["balanced"]
}

# run randomized search
n_iter_search = 20
random_search = RandomizedSearchCV(clf, param_distributions=param_dist,
                                   n_iter=n_iter_search,scoring="f1_weighted")

start = time()
random_search.fit(X, y)
print("RandomizedSearchCV took %.2f seconds for %d candidates"
      " parameter settings." % ((time() - start), n_iter_search))
d=report(random_search.cv_results_)

clf = RandomForestClassifier(n_estimators=20,class_weight="balanced",bootstrap=d["bootstrap"],criterion=d["criterion"],max_depth=d["max_depth"],max_features=d["max_features"],min_samples_leaf=d["min_samples_leaf"],min_samples_split=d["min_samples_split"])
#clf = RandomForestClassifier(n_estimators=20,bootstrap=True,criterion='gini',max_depth=None,max_features=10,min_samples_leaf=4,min_samples_split=3)
#print d["min_impurity_decrease"]
clf.fit(X,y)
from sklearn.tree import export_graphviz
dot_data=export_graphviz(clf.estimators_[0], out_file=None,feature_names=Xnames, class_names=["No","Yes"], filled=True, rounded=True)
graph = graphviz.Source(dot_data)
graph.render("RandomForestBolesti")