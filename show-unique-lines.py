def compare_files(file1, file2):
    with open(file1, 'r') as f1:
        file1_contents = set(line.strip() for line in f1)

    with open(file2, 'r') as f2:
        file2_contents = set(line.strip() for line in f2)

    unique_lines = file2_contents - file1_contents

    return unique_lines

# Usage example
file1 = "/Users/jay/starship/init.el"
file2 = "/Users/jay/starship/init.txt"

unique_lines = compare_files(file1, file2)

print("Lines in", file2, "that do not appear in", file1, "are:")
for line in unique_lines:
    print(line)
