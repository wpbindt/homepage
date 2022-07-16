import os


def filename_to_anchor(subdirectory: str, filename: str) -> str:
    link_text = (' '.join(filename[:-len('.html')].split('-'))).capitalize()
    return f'<li><a href="/{subdirectory}/{filename}">{link_text}</a></li>\n'


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
