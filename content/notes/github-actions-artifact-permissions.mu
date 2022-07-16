* 16-7-2022

When uploading an artifact in Github actions to use in another job, permissions are not preserved. So if one job uploads an executable, and another downloads it to use it, that should be dealt with:
> jobs:
>   upload-some-executable:
>     runs-on: ubuntu-latest
>     steps:
>       - uses: actions/upload-artifact@v2
>         with:
>           name: some-executable
>           path: path/to/executable.sh
>
>   use-some-executable:
>     runs-on: ubuntu-latest
>     needs: upload-some-executable
>     steps:
>       - uses: actions/download-artifact@v2
>         with:
>           name: some-executable
>           path: path/to
>       - run: |
>           chmod +x path/to/executable.sh
