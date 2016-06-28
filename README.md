# Scenario Reasoner

The UURAGE Scenario Reasoner is a CGI application that can be used to navigate through and get information about scenarios created by the UURAGE Scenario Editor.

## Prerequisites

* For client-side use: any modern operating system.

* For server-side use: a server stack that is capable of executing CGI executables.

* For development: the Glasgow Haskell Compiler, version 7.10. We recommend the use of the [Haskell Platform 7.10.3, available for Windows, Linux, and Mac OS X](https://www.haskell.org/platform/prior.html).

	* Ubuntu 16.04 has a `haskell-platform` package. Although its version is 2014.2.0.0.debian2 (or similar), installing this package does install the right version of GHC and the related libraries.

## Installation

### Setting up the build environment

After installing the prerequisites and cloning this repository, execute the following commands:

    cabal sandbox init
    cabal install --dependencies-only
    cabal configure

After these commands, the build environment is set up. You can build the application using `cabal build`. The build products will be placed in `dist/build`.

### Setting up the directory structure

It is recommended to set up the following directory structure (all of the items mentioned should become directories). Create this structure in a location outside the repository. If you want to use the Scenario Reasoner in a server-side setup, make sure this location is under your web root and your web server knows how to execute the CGI executables.

    <root of the deployment structure>
        cgi/
            bins/
            test_bins/

After creating this structure, symlink or copy the CGI executables to the `cgi` directory:

    <deployment root>/cgi/ScenarioParser.cgi ->
        <repository working directory>/dist/build/ScenarioParser.cgi/ScenarioParser.cgi
    <deployment root>/cgi/ScenarioReasoner.cgi ->
        <repository working directory>/dist/build/ScenarioReasoner.cgi/ScenarioReasoner.cgi

Note that under Windows, the executables will be named `ScenarioParser.cgi.exe` and `ScenarioReasoner.cgi.exe`. This is by design; if you copy them, rename them to remove the trailing `.exe`. Although symlinking is possible using recent versions of Windows, we have noticed certain Apache binaries cannot execute the symlinked files as CGI applications.

If you decide to copy the executables, make sure you don't forget to repeat that process after rebuilding.
