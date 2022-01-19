
import os
import pandas as pd
import glob
import matplotlib.pyplot as plt


""" Module does some basic analytics on the learning games
    It is meant as a replacement for the R code and meant to
    be executed on the server

"""
# Paths 
## Define paths and folders - input
FILEPATH = '/Users/philippzahn/Documents/projects/learning/Software/results/cournot-ff5baf6649d29931bec9256f70ea1600b3f9ef56'
foldernames_phase1 = glob.glob(os.path.join(FILEPATH,"e*_*"))
foldernames_phase2 = glob.glob(os.path.join(FILEPATH,"p*_phase2_*"))
foldernames_phase3 = glob.glob(os.path.join(FILEPATH,"p*_phase3_*"))

## Define paths output 
OUTPUTPATH = '/Users/philippzahn/Documents/projects/learning/Software/results/output/cournot-ff5baf6649d29931bec9256f70ea1600b3f9ef56/'
# create directory if it does not exist
try:
    os.makedirs(OUTPUTPATH, exist_ok = True)
    print("Directory '%s' created successfully" % OUTPUTPATH)
except OSError as error:
    print("Directory '%s' can not be created" % OUTPUTPATH)

# Prepare outcome files
avgs_reward_phase1 = []
avgs_reward_phase2 = []
avgs_reward_phase3 = []

# For all relevant runs in phase 1 retrieve payoffs and provide average
for folder in foldernames_phase1:

    # Read reward file as df
    df = pd.read_csv(os.path.join(folder,"rewardsExtendedEndNLines.csv"), names=('Iteration','Player','ActionIndex','Randomization','Profit'))

    # average profit for that run
    avg =df['Profit'].mean()

    # append to average_reward_phase1
    avgs_reward_phase1.extend([avg])

# For all relevant runs in phase 2 retrieve payoffs and provide average
for folder in foldernames_phase2:

    # Read reward file as df
    df = pd.read_csv(os.path.join(folder,"rewardsExtendedEndNLines.csv"), names=('Iteration','Player','ActionIndex','Randomization','Profit'))

    # average profit for that run
    avg =df['Profit'].mean()

    # append to average_reward_phase1
    avgs_reward_phase2.extend([avg])

# For all relevant runs in phase 3 retrieve payoffs and provide average
for folder in foldernames_phase3:

    # Read reward file as df
    df = pd.read_csv(os.path.join(folder,"rewardsExtendedEndNLines.csv"), names=('Iteration','Player','ActionIndex','Randomization','Profit'))

    # average profit for that run
    avg =df['Profit'].mean()

    # append to average_reward_phase1
    avgs_reward_phase3.extend([avg])



# Basic statistics

# Create series average phase 1
avg_phase1 = (pd.Series(avgs_reward_phase1, name='Profit')).mean()
print(avg_phase1)

# Create series average phase 2
avg_phase2 = (pd.Series(avgs_reward_phase2, name='Profit')).mean()
print(avg_phase2)

# Create series average phase 3
avg_phase3 = (pd.Series(avgs_reward_phase3, name='Profit')).mean()
print(avg_phase3)


# Create new series

resultSeries = pd.Series([avg_phase1,avg_phase2,avg_phase3],
                         index=['Phase1','Phase2','Phase3'],
                         name='Profit')

# Plotting output

fig = plt.figure()
resultSeries.plot(kind='bar')
fig.savefig(os.path.join(OUTPUTPATH,'testBar.png'))
