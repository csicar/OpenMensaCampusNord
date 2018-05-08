# OpenMensaCampusNord
openMensa crawler for KIT Campus Nord

Building
--------

install `poppler-utils` for the `pdftotext` command
install `cabal-install` for `cabal`

```bash
$ cabal update
$ cabal sandbox init
$ cabal install
```

for static compile:
`cabal build --ghc-options="-fPIC -optl-static -optl-pthread"`
dynamic linked:
`cabal build`

Run
---

```cabal run serveruser@server.com:/path/to/file/to/write.xml```