# Graph analysis in LISP

LISP code for graph analysis, including many implementations of Graph Theory algorithms

## TODO List:

- Better documentation.
- Add an ASDF system definition after it is stable enough to be used by others
    - Then make it available in Quicklisp or some other CLisp package manager maybe?
- Optimize the code.
- Support for various different types of TGF formats.
- Add support for non-sequential node-ids, labels (weights, descriptions etc.) to nodes and edges.
- Support for graph editing operations.
- Use structures instead of CLOS maybe?
- Add multithreading for better performance

## Issues

- Does not support unconnected graphs.
- Does not process user input on main.lisp, which leads to crashes.
