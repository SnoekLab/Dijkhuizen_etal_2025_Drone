# -*- coding: utf-8 -*-
"""
Created on Sat Jun  3 08:49:00 2023

@author: Basten
"""


#%%
"""
# 
# 
"""
import time
print(time.localtime())
from itertools import islice
j = 0
with open("./NC_056628.1_LKSer.txt","r") as f: ## take 3 min or so for chr 1
    usefile = open("./Status_NC_056628.1_LKSer.txt","w") # to collect all
    usefile_filter = open("./Filter_Status_NC_056628.1_LKSer.txt","w") # to collect filtered on quality
    depthfile = open("./Depth_NC_056628.1_LKSer.txt","w") # to collect read depth per SNP
    while True:
        next_n_lines = list(islice(f, 1000000))  ## <----- adjust batch size noof lines read at once.
        j += 1
        print(j)
        print(time.localtime())
        if not next_n_lines: #or j > 1: # Toggle to only run one round, for testing.
            usefile.close()
            usefile_filter.close()
            depthfile.close()
            break
        for myline in next_n_lines:
            if len(myline)>10 : # deal with empty lines, not sure why they occur, but just in case.
                myline = myline.split("\t")
                # print(myline)
                status = myline[1:5] # index 0 is the contig id which can be stored in the file name, then it does not have to be repeated
                status += myline[5:len(myline):2]
                # reduce size by coding the status with integers, we ignore the difference between phased 0|1 and unphased 0/1
                for s in range(0,len(status)):
                    if status[s] == "0/0" or status[s] == "0|0":
                        status[s] = "0"
                    if status[s] == "0/1" or status[s] == "0|1" or status[s] == "1|0":
                        status[s] = "1"
                    if status[s] == "1/1" or status[s] == "1|1":
                        status[s] = "2"
                    if status[s] == "./." or status[s] == ".|.":
                        status[s] = "9"
                # print(status)
                if float(status[3]) > 1000:  ## <----------------------- set qual filter here
                    status2 = "\t".join(status)
                    usefile_filter.write(status2)
                status = "\t".join(status)
                # print(status)
                usefile.write(status)
                ## deal with the read depth scores
                depth = myline[1:5]
                depth += myline[6:len(myline):2]
                depth = "\t".join(depth)
                depthfile.write(depth)
                

print(time.localtime())
