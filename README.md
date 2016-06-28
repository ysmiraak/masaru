# masaru

A Clojure library for the Tomita GLR parsing algorithm.

## Usage

### Main functions ###

In `masaru.core`:

* `consume`
* `parse`
* `parse-for-result`
* `parsable?`
* `parse-forest-as-sexp`
* `number-of-parses`

See doc strings for usage details.

### Usage examples ###

In `masaru.trial`:

* `nvnpn`
* `pars`
* `111`

### TODO ###

* Automate CF grammar to SLR table conversion.
* Test efficiency and change to parallel process.
* Function for drawing parse forest in the dot language.

## License

Copyright © 2016 Ysmiraak

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
