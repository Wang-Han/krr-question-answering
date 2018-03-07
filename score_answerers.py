content = ""
with open("qa1_single-supporting-fact_test.txt", mode="r", encoding="utf-8") as f:
	content = f.readlines()
    
for line in content:
    tokens = line.split()
    if "Where" in tokens:
        print("Task" + tokens[-2])