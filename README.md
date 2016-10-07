# Build Helper
[![Build Status](https://travis-ci.org/afonso360/build-helper.svg?branch=master)](https://travis-ci.org/afonso360/build-helper)

Build helper helps you set your build configurations in order.

It enables you to have different commands to build, run, test, etc.. per project.

Commands persist across sessions and in the same project different major modes can have different run commands


## Usage:

For a quick setup add these lines to your init.el

``` lisp
(require 'build-helper)
(build-helper-setup)
```
and M-x build-helper-run to build
you can then use M-x build-helper-re-run to run the last command

You can bind these to your preferred keys, however there are convinience
functions that run and re-run common targets (run, build, test)
these have the name build-helper-run-TARGET and build-helper-re-run-TARGET
replacing target with one of the 3 preset targets

## Advanced functionality:

build-helper can also run functions from other packages, for example
if you use cmake-ide-mode, you will most certainly want to execute
`cmake-ide-compile' when executing the run target on c++-mode
to achieve this add the following code to the init.el file

``` lisp
(build-helper-add-function 'c++-mode 'run #'cmake-ide-compile)
```

This adds the function `cmake-ide-compile' to be executed when the user
executes the "run" target in c++-mode on any project

### Function return values:

The return value of the executed functions is checked, when t no other function
will be executed and the user will not be prompted for a command.
If all the functions return nil the user will be prompted as usual

To ensure a function returns t you can add the following instead

``` lisp
(build-helper-add-function 'c++-mode 'run #'(lambda () (cmake-ide-compile) t))
```
