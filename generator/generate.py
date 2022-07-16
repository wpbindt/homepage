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
    subprocess.call(['mkdir', '-p', f'static/{subdirectory}'])
    all_files = os.listdir(f'content/{subdirectory}')
    markup_files = [
        filename
        for filename in all_files
        if filename.endswith('mu')
    ]
    for markup_file in markup_files:
        output_filename = markup_file[:-len('.mu')] + '.html'
        convert_markup_to_html(
            input_file=f'content/{subdirectory}/{markup_file}',
            output_file=f'static/{subdirectory}/{output_filename}',
        )

    html_files = [
        filename
        for filename in all_files
        if filename.endswith('html')
    ]
    for html_file in html_files:
        subprocess.call([
            'cp',
            f'content/{subdirectory}/{html_file}',
            f'static/{subdirectory}/{html_file}',
        ])
