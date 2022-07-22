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


def generate_static_content() -> None:
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


def filename_to_anchor(subdirectory: str, filename: str) -> str:
    link_text = (' '.join(filename[:-len('.html')].split('-'))).capitalize()
    return f'<li><a href="/{subdirectory}/{filename}">{link_text}</a></li>\n'


def generate_index() -> None:
    html = '<body>\n'
    html += '<h1>My homepage</h1>\n'

    for subdirectory in os.listdir('static'):
        html += f'<h2>{subdirectory.title()}</h2>\n<ul>\n'
        for filename in os.listdir(f'static/{subdirectory}'):
            html += filename_to_anchor(subdirectory, filename)
        html += '</ul>'

    html += '</body>'

    with open('static/index.html', 'w') as f:
        f.write(html)


if __name__ == '__main__':
    generate_static_content()
    generate_index()
