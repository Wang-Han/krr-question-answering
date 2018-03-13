test_content = ""
test = []
with open("qa1_single-supporting-fact_test.txt", mode="r", encoding="utf-8") as f:
	test_content = f.readlines()

for line in test_content:
    tokens = line.split()
    if "Where" in tokens:
        test.append("Task" + tokens[-2])
    
output = ""
with open("qa1_single-supporting-fact_test.out", mode="r", encoding="utf-8") as f:
	output = f.readlines()

error_counter = 0
    
for idx in range(min(len(test), len(output))):
    if not test[idx].strip().lower() == output[idx].strip().lower():
        print("error at idx = ", idx)
        print("gt = ", test[idx], " output = ", output[idx])
        print()
        error_counter += 1
        
print("number of errors: ", error_counter)
print("total output points: ", str(len(output)))
print("accuracy: ", str( (len(output)- error_counter) / len(output) * 100), "%")
    

        