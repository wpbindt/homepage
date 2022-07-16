#!/bin/env python3
import os
import subprocess


def convert_markup_to_html(
    input_file: str,
    output_file: str,
) -> None:
    subprocess.call([
        'generator/html_generator/html-generator', 
        input_file, 
        output_file,
    ])


for subdirectory in os.listdir('content'):
    print('HERE')
    subprocess.call(['mkdir', '-p', f'content/{subdirectory}'])
    for markup_file in os.listdir(f'content/{subdirectory}'):
        print(markup_file)
        output_filename = markup_file[:-len('.mu')] + '.html'
        convert_markup_to_html(
            input_file=f'content/{subdirectory}/{markup_file}',
            output_file=f'static/{subdirectory}/{output_filename}',
        )
