# Policeman

![logo](https://user-images.githubusercontent.com/4276606/73312656-262c1a80-4221-11ea-83c3-87b3973fa27f.png)

[![GitHub CI](https://github.com/kowainik/policeman/workflows/CI/badge.svg)](https://github.com/kowainik/policeman/actions)
[![Hackage](https://img.shields.io/hackage/v/policeman.svg?logo=haskell)](https://hackage.haskell.org/package/policeman)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)


👮 Policeman is the Kowainik's
[Bristol Haskell Hackathon](https://mpickering.github.io/bristol2020.html)
project.

🏗 The current status is MVP, though it could already be used on ordinary cases.

`Policeman` assists to properly choose the next version number according to
[PVP]() (Packaging Version Policy) for the Haskell packages based on the
semantical changes to the interface.

For a better understanding of the project basis and implementation details
please refer to the following blog post:

* [Kowainik: Policeman in da city](https://kowainik.github.io/posts/policeman-bristol)

## Prerequisites

Policeman heavily uses some modern features of the Haskell compiler
[GHC](https://www.haskell.org/ghc/).
Therefore, in order to start using `Policeman` on the projects, you are required
to have the following set:

 * Global GHC version `8.8` and higher
 * Support of GHC `8.8` and higher in your current project and the version you
   wish to compare to
 * [cabal](https://www.haskell.org/cabal/) build tool version `3.0` or higher
   installed on your machine

## Installation

The initial beta version of `Policeman` is already available on Hackage.
To install it locally you need to execute the following commands:

```shell
cabal update
cabal install policeman
```

Alternatively, you can install the `Policeman` tool from the sources. For
achieving that, you have a couple of options.

First one is to fetch the repo using `cabal`:

```shell
cabal get -s policeman
```

Or, you can clone the directory directly from GitHub:

```shell
git clone https://github.com/kowainik/policeman.git
```

The next steps are identical in both option. You need to step into the
directory:

```shell
cd policeman
```

And finally, build and install the tool using `cabal`:

```shell
cabal install policeman:exe:policeman
```

## How to use

By default, Policeman aims to compare your local version with the latest
available Hackage version of the package. It grabs the information about your
package automatically, so no need to provide that. That means that the simplest
command looks like this:

```shell
policeman
```

It will output the suggested new version (if applicable) based on your local
changes compared to the Hackage version.

Alternatively, you can provide the Hackage version you are willing to compare
against. For that purpose, you can use `-p | --previous` option. For example:

```shell
policeman --previous "0.1.0.0"
```

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/) is licensed by [CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
