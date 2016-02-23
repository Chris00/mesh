Mesh
====

This library defines a data structure for triangular meshes and
provides several functions to manipulate them.  In particular, a
[binding](src/mesh\_triangle.mli) to
[Triangle](https://www.cs.cmu.edu/~quake/triangle.html) is provided.
It also allows to export meshes of functions defined on their nodes to
[LaTeX](src/mesh.mli#L216), [SciLab](src/mesh.mli#L280),
[Matlab](src/mesh.mli#L289), [Mathematica](src/mesh.mli#L2313), and
[Graphics](src/mesh_display.mli).


Install
-------

The easier way to install this library is using
[opam](http://opam.ocaml.org/):

    opam install mesh

If you clone this repository, you can compile the code with `make`.
