import pandas as pd

dd = pd.read_table("2021/day_2/input.txt", sep = " ", names=["dir", "len"])
# Part 1

dd["len"] = pd.to_numeric(dd["len"])

final_depth = sum(dd["len"][dd["dir"] == "down"]) - sum(dd["len"][dd["dir"] == "up"])

final_horiz = sum(dd["len"][dd["dir"] == "forward"]) 

ans1 = final_depth * final_horiz

# Part 2
aim = 0
horiz = 0
depth = 0

dd["dir"][1] == "down"

for i in range(len(dd["dir"])):
    if dd["dir"][i] == "down":
        aim += dd["len"][i]
    else:
        if dd["dir"][i] == "up":
            aim += -dd["len"][i]
        else:
            if dd["dir"][i] == "forward":
                horiz += dd["len"][i] 
                depth += dd["len"][i] * aim


ans2 = horiz*depth

print(f'The answer to Part 1 is {ans1}.\nThe answer to Part 2 is {ans2}.')