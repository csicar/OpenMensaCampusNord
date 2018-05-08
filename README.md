# OpenMensaCampusNord
openMensa crawler for KIT Campus Nord

Building
--------

install `poppler-utils` for the `pdftotext` command

`cabal install`

for static compile:
`cabal build --ghc-options="-fPIC -optl-static -optl-pthread"`
dynamic linked:
`cabal build`
