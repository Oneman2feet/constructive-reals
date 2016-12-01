# constructive-reals

Start with arbitrary-precision integers: http://forge.ocamlcore.org/projects/zarith

How To Run
==========

    utop reals.ml

OR

    utop
    #use "reals.ml"
    <experiment with the R module>

Setup for MacOSX
================

    opam init -a -y --comp 4.03.0
    eval `opam config env`
    opam install -y utop ounit qcheck yojson lwt menhir

```brew install gmp```

```brew install opam```

```opam init```

```opam install conf-gmp``` (you may be able to skip this)

```opam install zarith```

```opam install oasis```