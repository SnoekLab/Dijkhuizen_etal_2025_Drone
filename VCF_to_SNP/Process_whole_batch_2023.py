# -*- coding: utf-8 -*-
"""
Created on Thu Apr 27 11:19:14 2023

@author: Basten
"""


#%%
"""
# Here we have a script to write the file after a key is done, 
# as reading in the whole file does not fit in the memory
"""
import time
print(time.localtime())
from itertools import islice
j = 0
snpd = {"dummy":"dummy"}
mykey = "dummy"
with open("E:/Work/New_SNPs_2023_whole_batch/LK001_LK399_TEST.vcf","r") as f: ## reading 50 min or so during the batch version
    while True:
        next_n_lines = list(islice(f, 1000000))
        j += 1
        print(j)
        print(time.localtime())
        if not next_n_lines:
            break
        for myline in next_n_lines:
            if myline[0] != "#":    
                dl = myline.split("\t")
                useind = [0,1,3,4,5]
                lta = ""
                for i in useind:
                    lta += dl[i] 
                    lta += "\t"
                for i in range(9,len(dl)):
                    lta += dl[i].split(":")[0]
                    lta += "\t"
                    lta += dl[i].split(":")[1]
                    lta += "\t"
                lta += "\n"
                if dl[0] in snpd:
                    mykey = dl[0]
                    snpd[dl[0]] += [lta]
                else:
                    usename = "E:/Work/New_SNPs_2023_whole_batch/Processed/"+ mykey+"_LK001_399.txt"
                    print(usename)
                    usefile = open(usename,"w")
                    for l in snpd[mykey]:
                        usefile.write(l)
                    usefile.close()
                    snpd[mykey] = []
                    snpd[dl[0]] = [lta]
                    print("new key:",dl[0])
        else: # to write last new key/sequence id
            usename = "E:/Work/New_SNPs_2023_whole_batch/Processed/"+ mykey+"_LK001_399.txt"
            print(usename)
            usefile = open(usename,"w")
            for l in snpd[mykey]:
                usefile.write(l)
            usefile.close()
            snpd[mykey] = []
            snpd[dl[0]] = [lta]
            print("End of file; last key:",dl[0])
               

print(time.localtime())
