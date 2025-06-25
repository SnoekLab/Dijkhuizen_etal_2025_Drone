# -*- coding: utf-8 -*-
"""
Created on Wed Jun  7 12:27:36 2023

@author: Basten
"""


#%%
# Chromosome 1

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056623.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056623.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)

usefile_maf.close()



#%%
# Chromosome 2

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056624.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056624.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)

usefile_maf.close()

    
    

#%%
# Chromosome 3

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056625.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056625.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)

    
    

usefile_maf.close()


#%%
# Chromosome 4

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056626.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056626.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)

    
    

usefile_maf.close()


#%%
# Chromosome 5

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056627.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056627.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)

    
    

usefile_maf.close()


#%%
# Chromosome 6

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056628.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056628.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)




usefile_maf.close()


#%%
# Chromosome 7

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056629.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056629.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)




usefile_maf.close()


#%%
# Chromosome 8

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056630.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056630.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)




usefile_maf.close()


#%%
# Chromosome 9

import time
print(time.localtime())
from itertools import islice

maf_thr = 10 ## 198 genontypes 5% is roughly 10
snp_batch_size = 100000
selsnp = 0
j = 0
with open("E:/Work/New_SNPs_2023_whole_batch/Filter_Status_NC_056631.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
#    usefile = open("E:/Work/New_SNPs_2023/Processed/Status_NC_056623.1_LK001_025.txt","w") # to collect all
     usefile_maf = open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056631.1_LKSat.txt","w") # to collect filtered on quality
#    depthfile = open("E:/Work/New_SNPs_2023/Processed/Depth_NC_056623.1_LK001_025.txt","w") # to collect read depth per SNP
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile_maf.close()
            break
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ld = {'0':0,'1':0,'2':0,'9':0}
            for itm in myline[4:202]:
                if itm in ld:
                    ld[itm] += 1
                else:
                    ld[itm] = 1
            if (ld['0'] > maf_thr and ld['0'] < 198 - maf_thr ) or (ld['2'] > maf_thr and ld['2'] < 198 - maf_thr ) or (ld['9'] > maf_thr and ld['9'] < 198 - maf_thr ): 
                # print(ld)
                selsnp +=1
                usefile_maf.write(next_n_lines[i])
        print(selsnp,j)




usefile_maf.close()




#%%