
# How to run tests

## Folder contents

The folder contains six property folders, one for each of the properties 
for the FMSD paper.

* property1 : access
* property2 : file
* property3 : fifo
* property4 : locks basic
* property5 : locks cycles
* property6 : locks data races

Each property folder property<i> contains:

* the dejavu spec.txt file
* the monpoly spec files: prop<i>.sig and prop<i>.mfotl (monpoly requires a signature of events as well as a formula)	
* a log folder named 10,000 : containing log-dejavu.txt and log-monpoly.txt (logs of size approxomately 10,000 events, in respective formats)
* a log folder named 100,000 : containing log-dejavu.txt and log-monpoly.txt (logs of size approximately 100,000 events, in respective formats)
* a log folder named 1,000,000 : containing log-dejavu.txt and log-monpoly.txt (logs of size approximately 1,000,000 events, in respective formats)

A folder my contain logs of other sizes as well.

## Step 1

* Install monpoly : http://www.infsec.ethz.ch/research/projects/mon_enf.html
* Install dejavu : https://github.com/havelund/tracecontract/tree/master/dejavu (jar files)


## Step 2

Say you want to analyze property1 traces. 

You may do a:

    cd properties/property1

To run dejavu on the log with around 10,000 events do:

    dejavu prop1.dejavu 10,000/log-dejavu.txt 20

The number 20 indicates how many bits are used to represent values of variables in the BDDs. If left out the default value is 20.

To run monpoly on the log with around 10,000 events do:

    monpoly -negate -sig prop1.sig -formula prop1.mfotl -log 10,000/log-monpoly.txt
      
## Timing

dejavu issues timing information, whereas monpoly does not. Use the UNIX time command to time a UNIX command, for example:

    time monpoly -negate -sig prop1.sig -formula prop1.mfotl -log 10,000/log-monpoly.txt
    
    