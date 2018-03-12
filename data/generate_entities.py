import glob
from collections import Counter
import re

file_contents = ""
file_names = glob.glob("*.txt")

print(file_names)

for file_name in file_names:
    with open(file_name, mode="r", encoding="utf-8") as f:
        file_contents += "\n".join(f.readlines())
        
# file_contents = re.sub('[^a-zA-Z ]+', ' ', file_contents)

# words_counter = Counter(file_contents.split())
# for word, count in words_counter.most_common():
    # print(word)

verbs = []
for sentence in file_contents.split("\n"):
    words = sentence.split()
    if len(words) > 3 and words[1] == "Daniel":    
        verbs.append(words[2])
        
verbs = list(set(verbs))
print(verbs)