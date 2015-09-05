## chickollect

chickollect is a simple [CHICKEN](http://www.call-cc.org) library
which implements a set of system monitors that collect system data
(e.g., CPU, memory, network usage).  It has only been tested on the
few GNU/Linux systems I use as desktop systems.

It exports a single procedure, `collect-loop`, to which a handler
procedure must be given as argument.  The handler procedure will be
given collected data as argument.  Optionally, `collect-loop` also
accepts a configuration alist via the `conf` keyword parameter.

Usage example:

    (use chickollect extras)

    (collect-loop pp)

On my system, it prints something like the following output per
second:

    ((memory 7.5681555469636 . 0)
     (cpu 1.37117055646594
          0.277864043621644
          0.944676841489388
          0.290644100912223
          1.03938872272996
          0.300917171452537
          1.02536154084379
          0.349054889515253)
     (date . "2015-09-05")
     (time . "18:19:33")
     (battery (Full . 100))
     (network (eth0 0 0) (wlan0 0 0) (lo 0 0)))


### Format of the collected data given to the handler

    ((memory <RAM used> . <swap used>)
     (cpu <CPU0 usage> ...)
     (date . <yyyy-mm-dd>)
     (time . <hh:mm:ss>)
     (battery (<state> . <capacity>) ...)
     (network (<dev> <download> <upload>) ...))


#### memory

A pair `(<RAM used> . <swap used>)`.

* `<RAM used>`: a number that indicates the percentage of RAM used
* `<swap used>`: a number that indicates the percentage of swap used


#### cpu

A list of CPUs usage (percent).


#### date

A string `"yyyy-mm-dd"`.


#### time

A string `"hh:mm:ss"`.


#### battery

A list of pairs `(<state> . <capacity>)`

* `<state>`: a symbol or `#f` (no battery connected).  If it is a
  symbol, it is one of `Charging`, `Discharging` or `Full`.

* `<capacity>`: a number (percentage) or `#f` (no battery connected).


#### network

A list of `(<dev> <bytes sent> <bytes received>)`, where:

* `<dev>`: a symbol naming the network device (e.g., `eth0`).

* `<bytes received>`: a number representing the number of bytes
  received by `<dev>`

* `<bytes sent>`: a number representing the number of bytes sent by
  `<dev>`


### Configuration

`collect-loop` accepts an alist with configuration data.

The following options are handled:

* `time-format`: a string to be given to `time->string`.  Default: `"%T"`.

* `date-format`: a string to be given to `time->string`.  Default: `"%F"`.

* `collect-interval`: an integer that specifies the time (in seconds)
  between two collections.  Default: `1`.

* `monitors`: a list of symbols that specify monitors to collect data
  for.  The default value is a list with all available monitors:
  `(memory cpu date time battery network)`.
