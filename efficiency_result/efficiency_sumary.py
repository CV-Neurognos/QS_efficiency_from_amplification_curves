import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import glob


#CONSTANTS
ONLYFILES = glob.glob('*.csv')

#combine all files in the list
combined_csv = pd.concat([pd.read_csv(f) for f in ONLYFILES ])

# get unique probes for each
probes = combined_csv['Target'].unique()

# get mean and standard deviation
probes_dic = {}
for i in probes:
    subset_df = combined_csv[combined_csv['Target'] == i]
    ## remove Outliers
    mean = subset_df['efficiency'].mean()
    std = subset_df['efficiency'].std()
    probes_dic[i] = [mean , std]

print(probes_dic)

# g = sns.swarmplot(y = "Target",
#               x = 'efficiency',
#               data = combined_csv,
#               # Decrease the size of the points to avoid crowding
#               size = 7)
# # remove the top and right line in graph
# sns.despine()
# g.figure.set_size_inches(14,10)
# plt.show()

g = sns.boxplot(y = "Target",
              x = 'efficiency',
              data = combined_csv)
g = sns.swarmplot(y = "Target",
              x = 'efficiency',
              data = combined_csv,
              # Decrease the size of the points to avoid crowding
              size = 5,color = 'black')
# remove the top and right line in graph
sns.despine()
g.figure.set_size_inches(12,8)
plt.show()
