# 2012-11-17

* Generate a Haskell module.

# 2012-11-16

* JSON: Move signal info into its own top-level item.

# 2012-11-13

* Add `USR0`…`USR3` signals.
* Replace `-` with `_` in names, that makes them valid identifiers in a larger
  subset of programming languages.
* Fix an error in the data sheet: `uart5_rxd` is an input.
* Add `/sys/class/pwm` names to PWM signals.
* There is actually a many-to-many mapping between MPU pins and signals. Switch
  to a relational model to avoid duplication of data. (Incidentally, doing this
  revealed the data sheet error mentioned above.)
* Generate the JSON file from the relational data.

# 2012-11-12

* Initial release.
