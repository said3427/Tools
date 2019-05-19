##Chapter 4 - DeepChem
# docker run -i -t deepchemio/deepchem:2.1.0-cpu ipython
import deepchem as dc
import numpy as np
x = np.random.random((4, 5))
y = np.random.random((4, 1))
dataset = dc.data.NumpyDataset(x, y)

##Datasets
tox21_tasks, tox21_datasets, transformers = dc.molnet.load_tox21()
train_dataset, valid_dataset, test_dataset = tox21_datasets

#Create model
model = dc.models.MultitaskClassifier(n_tasks=12,
	n_features=1024,
	layer_sizes=[1000])

#Fitting the model
model.fit(train_dataset, nb_epoch=10)

#Save metrics
metric = dc.metrics.Metric(dc.metrics.roc_auc_score, np.mean)

#Print metrics
train_scores = model.evaluate(train_dataset, [metric], transformers)
test_scores = model.evaluate(test_dataset, [metric], transformers)