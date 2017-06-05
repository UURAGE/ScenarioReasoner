# Scenario Reasoner

[![](https://travis-ci.org/UURAGE/ScenarioReasoner.svg?branch=master)](https://travis-ci.org/UURAGE/ScenarioReasoner)
[![](https://ci.appveyor.com/api/projects/status/github/UURAGE/ScenarioReasoner?branch=master&svg=true)](https://github.com/UURAGE/ScenarioReasoner/releases/latest)
[![](https://badge.fury.io/gh/UURAGE%2FScenarioReasoner.svg)](https://github.com/UURAGE/ScenarioReasoner/releases/latest)

The UURAGE Scenario Reasoner is a CGI application that can be used to navigate through and get information about scenarios based on the [UUDSL](http://uudsl.github.io/scenario), such as those created by the [UURAGE Scenario Editor](https://github.com/UURAGE/ScenarioEditor).

## Overview

This software consists of two executables: the Scenario Parser and the Scenario Reasoner.

The Reasoner is a fully stateless application: it does not save any state between requests. To avoid the (significant) overhead of parsing a UUDSL scenario upon each incoming request, the Reasoner operates on a binary representation of a scenario instead of its XML representation. This binary representation is application-specific and may change between releases. The Parser can be used to generate a binary representation of a scenario, after which the Reasoner can be used to navigate it.

## Prerequisites

* For client-side use: any modern operating system.

* For server-side use: a server stack that is capable of executing CGI executables.

* For development: the Glasgow Haskell Compiler, version 7.10. We recommend the use of the [Haskell Platform 7.10.3, available for Windows, Linux, and Mac OS X](https://www.haskell.org/platform/prior.html).

	* Ubuntu 16.04 has a `haskell-platform` package. Although its version is 2014.2.0.0.debian2 (or similar), installing this package does install the right version of GHC and the related libraries.

## Building

After installing the prerequisites and cloning this repository, execute the following commands:

    cabal sandbox init
    cabal install --dependencies-only
    cabal configure

After these commands, the build environment is set up. You can build the application using `cabal build`. The build products will be placed in `dist/build`.

## Installation

### Setting up the directory structure

It is recommended to set up the following directory structure (all of the items mentioned should become directories). If you have cloned this repository, create the structure in a location outside the repository. If you want to use the Scenario Reasoner in a server-side setup, make sure this location is under your web root and your web server knows how to execute CGI executables.

    <root of the deployment structure>
        cgi/
            bins/

### Installing the executables

If you are using prebuilt executables, place them in the `cgi` directory.

If you are building the executables yourself, symlink or copy them to the `cgi` directory as shown in the following subsections. If you decide to copy the executables, make sure you don't forget to repeat that process after rebuilding.

#### Windows

    <deployment root>/cgi/ScenarioParser.exe ->
        <repository working directory>/dist/build/ScenarioParser/ScenarioParser.exe
    <deployment root>/cgi/ScenarioReasoner.cgi ->
        <repository working directory>/dist/build/ScenarioReasoner.cgi/ScenarioReasoner.cgi.exe

Note that under Windows, the executables will be named `ScenarioParser.exe` and `ScenarioReasoner.cgi.exe`. This is by design; if you copy them, rename *only `ScenarioReasoner.cgi.exe`* to remove the trailing `.exe`. Although symlinking is possible using recent versions of Windows, we have noticed certain Apache binaries cannot execute the symlinked files as CGI applications.

#### Other operating systems

    <deployment root>/cgi/ScenarioParser ->
        <repository working directory>/dist/build/ScenarioParser/ScenarioParser
    <deployment root>/cgi/ScenarioReasoner.cgi ->
        <repository working directory>/dist/build/ScenarioReasoner.cgi/ScenarioReasoner.cgi

## Usage

* For client-side use: please see the [Client-side Reasoner Demo](https://github.com/UURAGE/ClientSideReasonerDemo).
* For server-side use: send HTTP requests targeting the CGI executable on the server.
* For development: we recommend the [Client-side Reasoner Demo](https://github.com/UURAGE/ClientSideReasonerDemo).

### Reasoner

The Reasoner accepts HTTP requests (both POST and GET) with a JSON-encoded parameter `input` based on one of the [input JSON schemas](doc/schemas) and sends responses based on the [output JSON schemas](doc/schemas).
