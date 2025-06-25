# -*- coding: utf-8 -*-
"""
Created on Wed Jun  7 11:16:20 2023

@author: Basten
"""


#%%

## needed functions!


#%%

# sim_thr = 5 ## so 4 non-overlap (out of 198) is still a match
def make_GRPD(inputfile,outputfile,sim_thr = 5,snp_batch_size = 2000):

    import time
    print(time.localtime())
    from itertools import islice
    import numpy as np
    cscore = {}   
    j = 0
    lw = 0
    with open(inputfile,"r") as f: ## take 3 min or so for chr 1
         usefile = open(outputfile,"w") # to collect all
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
                snp_stat_to_add = ldict[mykey][4:202]
                key_to_add = mykey
                maf = 198 - max([np.count_nonzero(compa_snp=='0'),\
                                 np.count_nonzero(compa_snp=='1'),\
                                 np.count_nonzero(compa_snp=='2'),\
                                 np.count_nonzero(compa_snp=='9')])
                # compare snp under investigation to all snps still left in batch            
                for mykey2 in all_keys:
                    compa_snp2 = ldict[mykey2][4:202]
                    if (np.count_nonzero(compa_snp != compa_snp2)) < sim_thr :
                        cscore[mykey] +=[mykey2]
                        all_keys.remove(mykey2)
                        maf2 = 198 - max([np.count_nonzero(compa_snp2=='0'),\
                                          np.count_nonzero(compa_snp2=='1'),\
                                          np.count_nonzero(compa_snp2=='2'),\
                                          np.count_nonzero(compa_snp2=='9')])
                        if maf2 > maf: # take the status of the snp with the highest MAF
                            key_to_add = mykey2
                # count the snp status to select those snp with a certain featureset. 
                # so select snps with low amount of heterozygots.               
                tocount = ldict[key_to_add][4:202]
                count0 = np.count_nonzero(tocount=='0')
                count1 = np.count_nonzero(tocount=='1')
                count2 = np.count_nonzero(tocount=='2')
                count9 = np.count_nonzero(tocount=='9')
                if (count0 > 9 and count2 > 9) or\
                   (count0 > 9 and count9 > 9) or\
                   (count2 > 9 and count9 > 9):    
                    grp_id = [";".join(cscore[mykey])] + [str(count0),str(count1),str(count2),str(count9)]
                    to_write = grp_id + list(ldict[key_to_add]) +["\n"]
                    # print(to_write)
                    usefile.write("\t".join(to_write))
                    lw += 1
            # break
            print(j*2000,"JJJJJJJJJJJJJJJJJJJJ",lw,"PERC:",lw/(j*2000))
            # print(len(cscore.values()))
            print(time.localtime()) 
    
    usefile.close()


#%%
#chr 2 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056624.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056624.1_LKSat.txt")

#%%
#chr 3 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056625.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056625.1_LKSat.txt")

#%%
#chr 4 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056626.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056626.1_LKSat.txt")

#%%
#chr 5 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056627.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056627.1_LKSat.txt")

#%%
#chr 6 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056628.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056628.1_LKSat.txt")

#%%
#chr 7 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056629.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056629.1_LKSat.txt")

#%%
#chr 8 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056630.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056630.1_LKSat.txt")

#%%
#chr 9 x
make_GRPD(inputfile="E:/Work/New_SNPs_2023_whole_batch/MAF_Filter_Status_NC_056631.1_LKSat.txt",
outputfile = "E:/Work/New_SNPs_2023_whole_batch/GRPD_MAF_Filter_Status_NC_056631.1_LKSat.txt")





#%%
