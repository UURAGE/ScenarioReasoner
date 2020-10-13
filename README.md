<img src="./logo.svg" width="50%"/>

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

* For development: the Glasgow Haskell Compiler, version 8.6. We recommend the use of the [Haskell Platform 8.6.5, available for Windows, Linux, and Mac OS X](https://www.haskell.org/platform/prior.html). For Ubuntu 20.04 and similar environments, the following packages should suffice:

    * `ghc`
    * `cabal-install`
    * `zlib1g-dev`

## Building

After installing the prerequisites and cloning this repository, configure the application using `cabal v2-configure`. You can build the application using `cabal v2-build`. The build products will be placed in `dist-newstyle/build`. To locate the two executables, look for files with the `.exe` extension (Windows) or the execute permission (other operating systems).

## Installation

### Setting up the directory structure

It is recommended to set up the following directory structure (all of the items mentioned should become directories). If you have cloned this repository, create the structure in a location outside the repository. If you want to use the Scenario Reasoner in a server-side setup, make sure this location is under your web root and your web server knows how to execute CGI executables.

    <root of the deployment structure>
        cgi/
            bins/

### Installing the executables

If you are using prebuilt executables, place them in the `cgi` directory. If you are building the executables yourself, copy them to the `cgi` directory. Make sure you don't forget to repeat this process after every build.

Note that under Windows, the executables will be named `ScenarioParser.exe` and `ScenarioReasoner.cgi.exe`. This is by design; you will need to rename *only `ScenarioReasoner.cgi.exe`* to remove the trailing `.exe`.

## Usage

* For client-side use: please see the [Client-side Reasoner Demo](https://github.com/UURAGE/ClientSideReasonerDemo).
* For server-side use: send HTTP requests targeting the CGI executable on the server.
* For development: we recommend the [Client-side Reasoner Demo](https://github.com/UURAGE/ClientSideReasonerDemo).

### Reasoner

The Reasoner accepts HTTP requests (both POST and GET) with a JSON-encoded parameter `input` based on one of the [input JSON schemas](doc/schemas) and sends responses based on the [output JSON schemas](doc/schemas).
