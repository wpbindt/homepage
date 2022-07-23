from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
import shutil
import subprocess
from typing import Callable, Iterable, List


@dataclass(frozen=True)
class StaticFile:
    converter: Callable[[Path, Path], None]
    file_path: Path

    def write_static(self, target_dir: Path) -> None:
        target_path = target_dir / self.target_filename
        target_path.parent.mkdir(exist_ok=True)
        self.converter(
            self.file_path, 
            target_path,
        )

    @property
    def target_filename(self) -> Path:
        return Path(*self.file_path.parts[-2:]).with_suffix('.html')
    
    @property
    def subdir(self) -> str:
        return self.file_path.parent.name
    
    @property
    def title_text(self) -> str:
        return ' '.join(self.file_path.stem.split('-')).capitalize()


def write_markup_file(input_file: Path, output_file: Path) -> None:
    subprocess.call([
        'generator/html_generator/html-generator', 
        input_file,
        output_file
    ])


def write_html_file(input_file: Path, output_file: Path) -> None:
    shutil.copy(
        input_file,
        output_file,
    )


def to_file(path: Path) -> StaticFile:
    if path.is_dir() or path.suffix not in {'.html', '.mu'}:
        raise ValueError
    if path.suffix == '.html':
        converter = write_html_file
    else:
        converter = write_markup_file
    return StaticFile(converter, path)


def get_parseables() -> Iterable[StaticFile]:
    content_dir = Path('content')
    return {
        to_file(file_path)
        for subdirectory in content_dir.iterdir()
        for file_path in subdirectory.iterdir()
        if subdirectory.is_dir()
    }


def get_index_html(files: Iterable[StaticFile]) -> List[str]:
    html = [
        '<html>', 
        '<head>', 
        '<title>My homepage</title>', 
        '</head>',
        '<body>',
        '<h1>My homepage</h1>',
    ]
    subdirs = defaultdict(set)
    for file in files:
        subdirs[file.subdir].add(file)
    for subdir, subfiles in subdirs.items():
        subdir_html = [
            f'<h2>{subdir.title()}</h2>',
            '<ul>',
        ]
        for file in subfiles:
            subdir_html.append(f'<li><a href="/{file.target_filename}">{file.title_text}</a></li>')
        subdir_html.append('</ul>')
        html.extend(subdir_html)
    html.extend(['</body>', '</html>'])
    return html


def main() -> None:
    files = get_parseables()
    target_dir = Path('static')
    target_dir.mkdir(exist_ok=True)
    for file in files:
        file.write_static(target_dir)
    index_html = get_index_html(files)
    (target_dir / 'index.html').write_text('\n'.join(index_html))


if __name__ == '__main__':
    main()
