import glob
from collections import Counter
import re

file_contents = ""
file_names = glob.glob("*.txt")

for file_name in file_names:
    with open(file_name, mode="r", encoding="utf-8") as f:
        file_contents += "\n".join(f.readlines())

file_contents = re.sub('[^a-zA-Z ]+', ' ', file_contents)

words_counter = Counter(file_contents.split())
for word, count in words_counter.most_common():
    print(word)