import pandas as pd
import matplotlib.pyplot as plt

df = pd.DataFrame(dict(
    A=[1, 2, 3, 4],
    B=[2, 3, 4, 5],
    C=[3, 4, 5, 6]
))

df.diff(axis=1).fillna(df).astype(df.dtypes).plot.bar(stacked=True)

plt.show()
