import pandas as pd
import matplotlib.patches as mpatches
import matplotlib.pyplot as plt
import numpy as np

from matplotlib import rc

"""
echo     & 16  & 8  & 2  &   13   &  3    &   17   \\
nginx    & 1141 & 274  & 10 &  2692 & 858   &  1324 \\
redis    & 3403 & 611 & 44 &   11502   &  2764   &   4691\\
postgres & 10868 & 2510 & 46  & 49458 & 12728 &   16751 \\
"""

N=4

TITLESIZE=10
LABELSIZE=8
TICKSIZE=6

data = dict(
           NBOTH=[16, 1141, 3403, 10868],
           NBAP=[8, 274, 611, 2510],
           NLLVM=[2, 10, 44, 46],
           EBOTH=[13, 2692, 11502, 49458],
           EBAP=[3, 858, 2764, 12728],
           ELLVM=[17, 1324, 4691, 16751]
)

for i in range(4):
    data["NBAP"][i] = data["NBAP"][i] + data["NBOTH"][i]
    data["NLLVM"][i] = data["NLLVM"][i] + data["NBAP"][i]

    data["EBAP"][i] = data["EBAP"][i] + data["EBOTH"][i]
    data["ELLVM"][i] = data["ELLVM"][i] + data["EBAP"][i]

df = pd.DataFrame(data)

import matplotlib.pyplot as plt
# %matplotlib inline
fig = plt.figure()

fig.set_figheight(3)
fig.set_figwidth(5)

colors = dict(
  both=(0.113, 0.515, 0.4414),
  bap=(0.9023, 0.90234, 0.8085),
  llvm=(0.7304, 0.859, 0.71875)
)

ecolors = dict(
  both=(0.99609375, 0.49609375, 0.31640625),
  bap=(203.0/256.0, 91.0/256.0, 68.0/256.0),
  llvm=(0.95703125, 0.79296875, 0.41015625)
)

e = [plt.bar([0, 1, 2, 3], df.NLLVM, align='edge', width= -0.2, color=colors["llvm"]),
     plt.bar([0, 1, 2, 3], df.NBAP, align='edge', width= -0.2, color=colors["bap"]),
     plt.bar([0, 1, 2, 3], df.NBOTH, align='edge', width= -0.2, color=colors["both"])]


v = [plt.bar([0, 1, 2, 3], df.ELLVM, align='edge', width=0.2, color=colors["llvm"]),
     plt.bar([0, 1, 2, 3], df.EBAP, align='edge', width=0.2, color=colors["bap"]),
     plt.bar([0, 1, 2, 3], df.EBOTH, align='edge', width=0.2, color=colors["both"])]

#font = {'family' : 'normal',
#        'weight' : 'bold',
#        'size'   : 22}

#rc('font', **font)

ind = np.arange(N)
plt.xticks(ind, ("echo", "nginx", "redis", "postgres"))
plt.yscale("log")
#plt.ylabel('Count')
#plt.xlabel('Nodes (Functions) and Edges (Function Calls) Detected by LLVM and BAP')
# plt.title('Evaluating Call Graphs Generated by LLVM and BAP')
plt.xticks(ind, ('echo', 'nginx', 'redis', 'postgres'))

ax, = fig.axes

ax.legend((v[2], v[1], v[0]), ('Both', 'BAP', 'LLVM'), numpoints=1, fontsize=TICKSIZE)

ax.set_title("Comparing Call Graphs Generated by BAP and LLVM", fontsize=TITLESIZE, fontweight="bold")
ax.set_ylabel("Count", fontsize=LABELSIZE, fontweight="bold")
ax.set_xlabel("Nodes (Functions) and Edges (Function Calls) Detected by BAP and LLVM", fontsize=LABELSIZE, fontweight="bold")
ax.set_yscale("log")

for tick in ax.get_xticklabels():
    tick.set_size(TICKSIZE)
for tick in ax.get_yticklabels():
    tick.set_size(TICKSIZE)

fig.savefig("callgraphs.pdf", bbox_inches='tight')

plt.show()
