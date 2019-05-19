##Chapter 4 - Solubility
# docker run -i -t deepchemio/deepchem:2.1.0-cpu ipython
import deepchem as dc
import numpy as np

#Loading datasets
tasks, datasets, transformers = dc.molnet.load_delaney(featurizer='GraphConv')
train_dataset, valid_dataset, test_dataset = datasets

#Model construction
model = GraphConvModel(n_tasks=1, mode='regression', dropout=0.2)
model.fit(train_dataset, nb_epoch=100)

#Evaluation of the model
metric = dc.metrics.Metric(dc.metrics.pearson_r2_score)
print(model.evaluate(train_dataset, [metric], transformers))
print(model.evaluate(test_dataset, [metric], transformers))