#!/usr/bin/python
import vcf
import numpy as np
import matplotlib.pyplot as plt

vcf_reader = vcf.Reader(open('leishmaniam.vcf', 'r'))

AC=[]
AF=[]

for record in vcf_reader:
	if record.CHROM=="chr02_bb":
		AC.extend(record.INFO['AC'])
		AF.extend(record.INFO['AF'])
	
AC=np.asarray(AC)
AF=np.asarray(AF)

y=np.histogram(AC,bins=len(AF))

AC2=np.asarray([0]*len(AC))

for i in range(0,len(AC)):
	AC2[((AC>y[1][i]) & (AC<y[1][i+1]))]=y[0][i]

freq=AC2/AC

fig, axes = plt.subplots(nrows=1, ncols=1)

axes.scatter(AF,freq)
axes.grid(True)
axes.axhline(0, color='black', lw=2)
plt.show()