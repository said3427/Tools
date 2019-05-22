##Chapter 8 - GANs for LS
# docker run -i -t deepchemio/deepchem:2.1.0-cpu ipython
import deepchem as dc
tasks, datasets, transformers = dc.molnet.load_muv()
train_dataset, valid_dataset, test_dataset = datasets
train_smiles = train_dataset.ids

tokens = set()
for s in train_smiles:
  tokens = tokens.union(set(s))
tokens = sorted(list(tokens))
max_length = max(len(s) for s in train_smiles)


from deepchem.models.tensorgraph.optimizers import Adam, ExponentialDecay
from deepchem.models.tensorgraph.models.seqtoseq import AspuruGuzikAutoEncoder
model = AspuruGuzikAutoEncoder(tokens, max_length, model_dir='vae')
batches_per_epoch = len(train_smiles)/model.batch_size
learning_rate = ExponentialDecay(0.001, 0.95, batches_per_epoch)
model.set_optimizer(Adam(learning_rate=learning_rate))


def generate_sequences(epochs):
  for i in range(epochs):
    for s in train_smiles:
      yield (s, s)

model.fit_sequences(generate_sequences(50))

import numpy as np
from rdkit import Chem
predictions = model.predict_from_embeddings(np.random.normal(size=(1000,196)))
molecules = []
for p in predictions:
  smiles = ''.join(p)
  if Chem.MolFromSmiles(smiles) is not None:
    molecules.append(smiles)
for m in molecules:
  print(m)



smiles_list = ['CCCCCCNNNCCOCC',
'O=C(O)C(=O)ON/C=N/CO',
'C/C=N/COCCNSCNCCNN',
'CCCNC(C(=O)O)c1cc(OC(OC)[SH](=O)=O)ccc1N',
'CC1=C2C=CCC(=CC(Br)=CC=C1)C2',
'CCN=NNNC(C)OOCOOOOOCOOO',
'N#CNCCCCCOCCOC1COCNN1CCCCCCCCCCCCCCCCCCCOOOOOSNNCCCCCSCSCCCCCCCCCOCOOOSS',
'CCCC(=O)NC1=C(N)C=COO1',
'CCCSc1cc2nc(C)cnn2c1NC',
'CONCN1N=NN=CC=C1CC1SSS1',
'CCCOc1ccccc1OSNNOCCNCSNCCN',
'C[SH]1CCCN2CCN2C=C1N',
'CC1=C(C#N)N1NCCC1=COOO1',
'CN(NCNNNN)C(=O)CCSCc1ccco1',
'CCCN1CCC1CC=CC1=CC=S1CC=O',
'C/N=C/c1ccccc1',
'Nc1cccooo1',
'CCOc1ccccc1CCCNC(C)c1nccs1',
'CNNNNNNc1nocc1CCNNC(C)C',
'COC1=C(CON)C=C2C1=C(C)c1ccccc12',
'CCOCCCCNN(C)C',
'CCCN1C(=O)CNC1C',
'CCN',
'NCCNCc1cccc2c1C=CC=CC=C2',
'CCCCCN(NNNCNCCCCCCCCCCSCCCCCCCCCCCCCCNCCNCCCCSSCSSSSSSCCCCCCCCCCCCCSCCCCCSC)\
C(O)OCCN',
'CCCS1=CC=C(C)N(CN)C2NCC2=C1',
'CCNCCCCCCOc1cccc(F)c1',
'NN1O[SH](CCCCO)C12C=C2',
'Cc1cc2cccc3c(CO)cc-3ccc-2c1']



molecules = [Chem.MolFromSmiles(x) for x in smiles_list]​ 


print(sorted([x.GetNumAtoms() for x in molecules]))

good_mol_list = [x for x in molecules if x.GetNumAtoms() > 10
        and x.GetNumAtoms() < 50]
print(len(good_mol_list))



qed_list = [QED.qed(x) for x in good_mol_list]
final_mol_list = [(a,b) for a,b in
         zip(good_mol_list,qed_list) if b > 0.5]


MolsToGridImage([x[0] for x in final_mol_list],
molsPerRow=3,useSVG=True,
subImgSize=(250, 250),
legends=[f"{x[1]:.2f}" for x in final_mol_list])

