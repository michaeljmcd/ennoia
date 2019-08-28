# TODO

## General

### Features

* Handle the addition of large number of modes more gracefully. As it is, the
  view becomes cluttered if you add enough nodes. Ideas:
    * We could do this by reducing the node size so that the map fits in the
      viewport.
    * We could also handle this by keeping zoom the same and filling more space.
    * We could do both - fill more space and zoom out to fit.
* Saving maps as local files.
    * I'm lazy so EDN would probably be the format of choice for the file.

### Bug Fixes

## Release 0.1.0

* The ability to save maps is a must.
* Minimal functioning UI.
    * Save, quit.
    * Something better than neon pink.
    * Scrollbars as needed.
* Ability to rename constants.
* Some layout cleanup.
