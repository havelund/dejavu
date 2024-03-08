# Garbage Collection

This example illustrates how the automated garbage collection can allow us to use
very few bits to represent data.

Perfirm run verifications with 3 bits, as in:

    dejavu prop1.qtl log1.csv 3
    dejavu prop2.qtl log1.csv 3
    dejavu prop3.qtl log1.csv 3
    dejavu prop4.qtl log1.csv 3

Properties 1 and 3 will run out of bit memory (not computer memory) since 
the properties are formulated in such a way that the garbage collector cannot 
collect garbage. Properties 2 and 4 on the other hand are formulated such that 
garbage can be collected, and therefore 3 bits are enough. The difference is the
use of the since operator in properties 2 and 4 of the form:  ``!p(x) S q(x)``. Using this form, a datum ``x`` will be released (and become garbage) when a ``p(x)`` is encountered in the trace after a ``q(x)``. This is in contrast to the formula ``P q(x)``, which is a weaker property, but which causes us to always track ``x`` once a ``q(x)`` is observed.
