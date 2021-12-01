from operator import concat
import numpy as np
import pandas as pd

# part 1
d = np.loadtxt("2021/day_1/input.txt")
d1 = d[1:len(d)]
d1 = np.append(d1, 0)

ans1 = sum(d < d1)

# part 2
dd = pd.read_table("2021/day_1/input.txt", names=["depths"])

dd["sum_roll3"] = dd["depths"].rolling(3).sum()

dd["lag"] = dd["sum_roll3"].shift(1)
dd["increase"] = dd["sum_roll3"] > dd["lag"]

ans2 = dd["increase"].sum()


print(f'The answer to Part 1 is {ans1}.\nThe answer to Part 2 is {ans2}.')