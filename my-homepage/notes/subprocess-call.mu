*** 1-8-2022

It is possible to execute a shell command in Python by running
> os.system('echo "hi mom"')
The call spawns a shell which interprets and executes the command, and blocks until the the command returns. 
This function is a bit problematic, for example because it is vulnerable to shell injection. Given a function
> def shell_greet(name: str) -> None:
>     os.system(f'echo "hi {name}"')
Client code can then inject shell code by doing something like
> shell_greet('Mork"; rm -rf /tmp; echo "')

A generally better way to execute shell commands is by running
> subprocess.call(['echo', 'hi mom'])
This has the following benefits:
- it properly escapes the input, so is not vulnerable to shell injection as much as os.system is
- instead of spawning a subshell to interpret the command, subprocess.call instead directly spawns the requested process as a subprocess, resulting in a benefit to performance
- instead of returning the status code as os.system does, errors are raised as exceptions, which allows for more Pythonic exception handling

There are variants on subprocess.call, for example subprocess.check_call. subprocess.check_call differs from subprocess.call in that it is a bit more liberal with raising Exceptions. It will raise a CalledProcessError on any non-zero exit code, whereas call is a bit less liberal in raising exceptions. For example
> grep -r "non-existent needle"
will return 1 with call, and raise an exception with check_call.
