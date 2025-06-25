# -*- coding: utf-8 -*-
"""
Created on Wed Jun  7 11:16:20 2023

@author: Basten
"""



#%%

## chromosome 1 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056623.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056623.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # 
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                 if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                     cscore[mykey] +=[mykey2]
                     all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()

#%%
## chromosome 2 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056624.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056624.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()


#%%
## chromosome 3 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056625.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056625.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()


#%%
## chromosome 4 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056626.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056626.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)

        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 


usefile.close()


#%%
## chromosome 5 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056627.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056627.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()



#%%
## chromosome 6 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056628.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056628.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()

#%%
## chromosome 7 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056629.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056629.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()


#%%
## chromosome 8 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056630.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056630.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()


#%%
## chromosome 9 | numpy version

import time
print(time.localtime())
from itertools import islice
import numpy as np

sim_thr = 5
cscore = {}   
j = 0
snp_batch_size = 2000

with open("E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056631.1_LKSat.txt","r") as f: ## take 3 min or so for chr 1
     usefile = open("E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056631.1_LKSat.txt","w") # to collect all
     while True:
        next_n_lines = list(islice(f, snp_batch_size))  ## <----- adjust batch size noof lines read at once.
        j += 1
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            break
        # split all lines and add to dict
        ldict = {}
        for i in range(len(next_n_lines)):
            myline = next_n_lines[i].rsplit()
            ldict[myline[0]] = np.array(myline)
        
        ## compare each snp and group # ca 3 sec per 100000 compas, so per SNP...... less the more snps are removed now
        all_keys = list(ldict.keys()) ## make list with keys so they can be remove if linked
        while len(all_keys)>0:
            mykey = all_keys[0]
            compa_snp = ldict[mykey][4:202]
            cscore[mykey] = []
            for mykey2 in all_keys:
                if (np.count_nonzero(compa_snp != ldict[mykey2][4:202])) < sim_thr :
                    cscore[mykey] +=[mykey2]
                    all_keys.remove(mykey2)
            grp_id = [";".join(cscore[mykey])]
            to_write = grp_id + list(ldict[mykey]) +["\n"]
            usefile.write("\t".join(to_write))
        print(j,"JJJJJJJJJJJJJJJJJJJJ")
        print(len(cscore.values()))
        print(time.localtime()) 

usefile.close()

#%%

usefile.close()

print(compa_snp)
print(ldict[mykey2][4:202])
print(np.array(compa_snp) != np.array(ldict[mykey2][4:202] ))

print("-------------------------")
print(np.count_nonzero(compa_snp != np.array(ldict[mykey2][4:202])))
print(np.count_nonzero(compa_snp != ldict[mykey2][4:202]))
print(np.array(compa_snp != np.array(ldict[mykey2][4:202] )).sum())

print(np.array(["A","B","D"])==np.array(["A","B","C"]))
np.count_nonzero(np.array(["A","E","D"])!=np.array(["A","B","C"]))


#%%

