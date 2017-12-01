# Graph analysis in LISP

LISP code for graph analysis, including many implementations of Graph Theory algorithms

## TODO List:

- Better documentation.
- Add an ASDF system definition after it is stable enough to be used by others.
    - Then make it available in Quicklisp or some other CLisp package manager maybe?
- Optimize the code.
    - The small world algorithm is not optimized.
    - The average diameter and connectedness algorithms are too slow to my liking...
- Support for various different types of TGF formats (loading and saving).
- Add support for non-sequential node-ids, labels (weights, descriptions etc.) to nodes and edges.
- Support for graph editing operations.
- Use structures instead of CLOS maybe?
- Add multithreading for better performance.
- Algorithms:
    - Vulnerability.
    - Clustering coeficient (local and global).
    - Strong and weak connected component.
- Save the analysis data to disk.

## Issues

- Does not support unconnected graphs.
- Does not process user input on main.lisp, which leads to crashes.
- Regular graph generator crashes with uneven degree values.
- Time in progress printing is incorrect sometimes (probably due to accuracy of time counting...)
- Printing > 100% progress in small world algorithm.
