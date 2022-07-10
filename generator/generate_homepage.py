import os

BASE_URL = '109.237.219.178'


def filename_to_anchor(subdirectory: str, filename: str) -> str:
    link_text = ' '.join(filename[:-len('.html')].split('-'))
    return f'<li><a href="{BASE_URL}/{subdirectory}/{filename}">{link_text}</a></li>\n'


html = '<body>\n'
html += '<h1>My homepage</h1>\n'

for subdirectory in os.listdir('content'):
    html += '<h2>subdirectory</h2>\n<ul>\n'
    for filename in os.listdir(f'content/{subdirectory}'):
        html += filename_to_anchor(subdirectory, filename)
    html += '</ul>'

html += '</body>'

with open('content/index.html', 'w') as f:
    f.write(html)
