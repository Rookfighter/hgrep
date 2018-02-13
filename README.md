# hgrep

```hgrep``` is a utility to search for strings in files using regex patterns. It is basically a simplified clone of the famous ```grep``` tool written in Haskell.

## Install

This project uses the ```stack``` package management tool for Haskell.
Simply type the following commands to compile the code:

```
cd path/to/repo
stack build
```

The resulting executeable can be found in the standard ```bin``` directory of stack on your platform. For convenience you can also copy the binaries to your current working direcory using the ```install``` command of ```stack```.

```
stack install --local-bin-path "$(pwd)"
```

There are numerous unit tests to verify the regex engine powering ```hgrep```. These can be run by entering

```
stack test
```

## Usage

In the most basic mode ```hgrep``` expects first a regex pattern followed by a list of files. A basic example could look like this:

```
hgrep "Age:\w*[0-9]+" old.txt young.txt inbetween.txt
```

```hgrep``` also supports various extra features such as inverting the match or ignoring the case of alphabetical characters. For more information on these features see the next section or simply enter:

```
hgrep --help
```

## Features

The ```hgrep``` regex engine currently supports the following features:

* matching simple literals, e.g. ```"abc"```
* matching alternatives using ```|```, e.g. ```"a|b"```
* grouping using parenthesis, e.g. ```"a|(bc)"```
* matching zero or more elements using ```*```, e.g. ```ab*```
* matching one or more elements using ```+```, e.g. ```ab+```
* matching optional elements using ```?```, e.g. ```ab?```
* matching any character using ```.```, e.g. ```ab.```
* generalised repetition ```x{min,max}```, e.g. ```ab{1,3}```
* character sets using brackets, e.g. ```a[bc]```
* inverting character sets, e.g. ```a[^bc]```
* predefined ranges for character sets such as ```a-z```, ```a-Z```, ```A-Z```, ```0-9```
* escape sequences for predefined chars such as ```{}?[]()+^$*.|\``` using a ```\``` prefix
* escape sequences for digits ```\d```, letters ```\l```, whitespaces ```\w```

The ```hgrep``` command line interface currently supports the following features:

* match highlighting (for bash)
* disable highlighting
* ignoring cases of letters
* inverting matches
* print only matches
* quiet mode
