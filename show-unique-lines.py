def compare_files(file1, file2, output_file):
    with open(file1, 'r') as f1:
        file1_contents = set(line.strip() for line in f1)

    with open(file2, 'r') as f2:
        file2_contents = set(line.strip() for line in f2)

    unique_lines = file2_contents - file1_contents

    with open(output_file, 'w') as output:
        for line in unique_lines:
            output.write(line + '\n')

    print(f"Unique lines from {file2} not in {file1} have been saved to {output_file}.")


# Usage example
file1 = "spacemacs-new-config.el"
file2 = "init.txt"
output_file = "unique_lines.txt"

compare_files(file1, file2, output_file)
