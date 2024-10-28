#!/usr/bin/env python3
import nagisa
import sys
def insert_spaces(title_text):
    tokenized = nagisa.tagging(title_text)
    with_space = ""
    for word in tokenized.words:
        with_space += word + " "
    return with_space.strip() # Removes the trailing space
def parse_subtitle_file(input_filename, output_filename):
    with open(input_filename, 'r', encoding='utf-8') as infile, open(output_filename, 'w', encoding='utf-8') as outfile:
        lines_buffer = []
        for line in infile:
            if line.strip() == "":
                outfile.write('、 '.join(lines_buffer) + "\n\n") # flush buffer and write to outfile
                lines_buffer = []
            elif "-->" in line:
                outfile.write(line)
            elif line.strip().isdigit():
                outfile.write(line)
            else:
                lines_buffer.append(insert_spaces(line.strip()))
        # Handle the situation where the file doesn't end with an empty line
        if lines_buffer:
            outfile.write('、 '.join(lines_buffer) + "\n")
def main():
    if len(sys.argv) != 3:
        print("Usage: python script.py input_file.vtt/.srt output_file.vtt/.srt")
        return
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    parse_subtitle_file(input_file, output_file)
if __name__ == "__main__":
    main()
