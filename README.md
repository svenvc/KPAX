# KPAX

## A Common Lisp Application Framework

`KPAX` is a Common Lisp Web Application Framework. 
Altough KPAX is quite mature and has been in production use for years, the documentation is currently not good enough to support use by the general public.

### Contents

-   [Features](#features)
-   [Status](#status)
-   [News](#news)
-   [Platforms](#platforms)
-   [Downloading](#downloading)
-   [Installation](#installation)
-   [Usage](#usage)
-   [API Reference](#api)
-   [Mailinglist](#mailinglist)
-   [Changelog](#changelog)
-   [TODO](#tod)
-   [FAQ](#faq)
-   [Bugs](#bugs)
-   [Authors](#authors)
-   [Maintainers](#maintainers)
-   [License](#license)
-   [History](#history)
-   [References](#references)

### Features

`KPAX` allows you:

-   to build web applications
-   to run standalone using s-http-server and behind apache+mod\_lisp or
    portable allegro serve

### Status

`KPAX` is considered stable code.

### News

-   *Januari 2006* - There is now a mailinglist for KPAX,
    [KPAX-DEVEL](http://common-lisp.net/cgi-bin/mailman/listinfo/kpax-devel)
    hosted at [common-lisp.net](http://www.common-lisp.net).
-   *December 2005* - `KPAX` was featured in the controversial Lisp Movie,
    [Episode 2: (Re)writing Reddit in Lisp is 20 minutes and 100
    lines](http://homepage.mac.com/svc/LispMovies/index.html) [dead link].
-   *November 2005* - First publiciation under new struture.

### Platforms

`KPAX` is written in ANSI standard Common Lisp and should be portable
across any CL implementation, provided [S-SYSDEPS](https://github.com/svenvc/s-sysdeps) is available.

### Installation

The KPAX package is loaded using [ASDF](http://www.cliki.net/asdf).
There is an excellent [tutorial on
ASDF](http://constantly.at/lisp/asdf/) to get you started.

       CL-USER 1 > (asdf:oos 'asdf:load-op :kpax)

### Usage

Please consult the included examples and source code. There is a Lisp
Movie, [Episode 2: (Re)writing Reddit in Lisp is 20 minutes and 100
lines](http://homepage.mac.com/svc/LispMovies/index.html)[dead link], that is
actually a very good tutorial on using KPAX.

    ;; to be completed later ;-)

There are some recent posting to the KPAX-DEVEL mailing list that are a
beginning of documentation.

### API Reference

There is automatically generated [API Reference](API.html) documentation
available for the KPAX package.

### Mailinglist

There is a mailinglist for `KPAX` called
[KPAX-DEVEL](http://common-lisp.net/cgi-bin/mailman/listinfo/kpax-devel)
hosted at [common-lisp.net](http://www.common-lisp.net).

### Changelog

Release Notes:

-   release 1: moved `KPAX` under a new, public structure using various
    newly extracted sub packages

### TODO

Extend the documentation. You can help!

### FAQ

Nothing appropriate.

### Bugs

`KPAX` currently buffers request bodies as they are received and request
responses as they are built until they are committed. On some platforms
with limited strings sizes this might be a problem. For very large
transfers of data, a better solution is necessary.

### Authors

`KPAX` was written by Sven Van Caekenberghe

### Maintainers

`KPAX` is being maintained by Sven Van Caekenberghe

### License

You are granted the rights to distribute and use this software as
governed by the terms of the Lisp Lesser General Public License
([http://opensource.franz.com/preamble.html](http://opensource.franz.com/preamble.html)),
also known as the LLGPL.

### History

An older version of `KPAX` was first described in the article: [Rebel With
A Cause](http://homepage.mac.com/svc/RebelWithACause/index.html) -
Building Web Applications with Common Lisp, Deploying on an Apple Xserve
running Mac OS X Server (november 2003, dead link).

### References

Thera are no references.

Copyright © 2002-2006 Sven Van Caekenberghe, Beta Nine BVBA. All Right
Reserved.