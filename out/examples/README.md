
# Examples

Examples are divided into folders. Each leaf folder contains one example represented by one or more
property files with suffix ``.qtl``:

    prop1.qtl
    prop2.qtl
    ...
    
and one or more log files in CSV format with suffix ``.csv``:

    log1.csv
    log2.csv
    ...
    
Any of the properties can be checked against any of the log files in a leaf folder, for example:

    dejavu prop2.qtl log3.csv
    
Most logs violate properties, in which case error messages are printed.

