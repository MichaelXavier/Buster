Buster
====
[![Build Status](https://travis-ci.org/MichaelXavier/Buster.png?branch=master)](https://travis-ci.org/MichaelXavier/Buster)

Utility for periodically hitting URLs. The intended use case is forcing caches
to be busted.

Building
====
Run `make`. Use a recent version of GHC. Requires cabal-dev. 

Usage
=====

`buster config.yml`

The program will log to stdout. If you want to reload the config without
stopping the program, send it a HUP signal:

`kill -HUP pid-of-buster`

Configuration
=============

Take a look in the example directory. Buster is configured with a yaml file:

```yaml
verbose: true
monitor: false
log_file: /var/log/buster.log
urls:
- url: http://www.example.com
  interval: 5
  method: GET
- url: http://www.example.net
  interval: 2
  method: GET
```

Interval is measured in seconds. Monitor will begin auto-loading your config.
If you have a process running and you decide you want the config monitored,
send a HUP to the process and from then on it will monitor the file. If no
`log_file` config is specified, it will log to stdout.

Status
======
The project currently works. Here are some improvements I want to make

* Release to hackage

Development
===========

1. Fork the project.
2. Make your changes in a feature branch.
3. Run tests with `make test`.
4. Send a pull request.
