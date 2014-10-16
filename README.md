# sr33

This is a library/program for reconstructing surfaces from organized collections of point samples, such as those produced by laser range-scanners.
The ideas behind this program are presented in the
[article](http://daystrom-data-concepts.com/SR/)
that accompanies this project.

While the ideas are well-developed, this implementation is somewhat of a prototype:
See the "Caveats and Bugs" section for more on this.

## Goals

This program is intended to demonstrate successful reconstruction of surfaces that contain sharp features like sharp edges, corners, and cones.
(The four reconstructions below are all without flaws, the funny looking corner on conebox is due to the fact that there is no sample there.)

![Alt L.obj](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/L.png?token=90f2ff964089324138508b87bcc0fe82e3e32f69)
![Alt cutcube.obj](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/cutcube.png?token=5ba564480ab9d98e2c9ab956f3b8f0e533c67bf0)
![Alt conebox.obj](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/conebox.png?token=f6e3dfb403185891ce8cfc5d3ef4f9401c77d12a)
![Alt fandisk.obj](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/fandisk.png?token=4477d052af10d07e06eba7cf7ada49be51c2e154)

Of course, the program also needs to also be able to reconstruct the smooth surfaces that all surface reconstruction algorithms have traditionally been able to.

![Alt Niccolo](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/nicolo.png?token=a9e87b9b9dca2abd08cad41c8a5536607e6f51b9)
![Alt Caesar](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/caesar.png?token=fd499985091af2e8156f01ebf6882596cc337c58)
![Alt Eros](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/eros.png?token=e2baaab4e06dc14f22f8fa8526f6a6527d04a235)
![Alt Foot](https://bytebucket.org/jwmfsu/sr33/raw/d8eb5038bb8abd5ccfba5a790ad14011f8be7492/renderings/foot.png?token=3bb4ed4f12fecade8cde972acffcfe3b86662e83)

## Dependencies

The project is written in Clojure, a lisp variant that runs on the Java virtual machine.
Before proceeding,
you will need to have the JDK installed and you will need to install [Leiningen](http://leiningen.org/),
if you have not done so already.
Once those tools are installed, you are ready to run the program.

## Using the code

Begin a REPL session by typing `lein repl` at the shell prompt while in the project directory or by bulding and running the jar file and connecting to it on port 4005 via nREPL.

From within a REPL session, type:

     $ (ns sr33.core)
     $ (reconstruct "/tmp/L.obj" 33)

to navigate into the main project namespace and reconstruct the Wavefront `.OBJ` formatted file `/tmp/L.obj`.
(The reconstruction process will ignore any surface data present in the file and just attempt to reconstruct the points.)
The reconstruction is done with a *neighborhood size* of 33.
The result of this process is two files: `/tmp/L.obj.recon.33.obj` and `/tmp/L.obj.recon.33.inc`.
The first file contains the reconstruction in Wavefront `.OBJ` format and the second contains a
[POV-Ray](http://www.povray.org/)
`mesh2` object suitable for use with that program.

The meaning of the neighborhood size parameter is explained in the
[article](http://daystrom-data-concepts.com/SR/).
In short, it is the number of nearest neighbors around each point to consider when building the graph whose edges bound the faces of the reconstruction.
The sample conditions for the reconstruction algorithm stipulate that local neighborhoods contain sufficient information to make local operation possible.

Type:

     $ (reconstruct "/tmp/Caesar.off" 107)

from within the `sr33.core` namespace to reconstruct the `.OFF` formatted file `/tmp/Caesar.off`.

If you want an audio alert when the reconstruction process is finished, try:

     $ (reconstruct "/tmp/Caesar.off" 107 :bell)

which does the same thing as the previous command, except with the sound.

The `core/reconstruct` function can be used to do the entire reconstruction process with one command, it is also possible to do the individual steps by hand.

     $ (def points (file/load-obj "/tmp/conebox.obj"))
     $ (def surface (recon/compute-surface points 33))
     $ (file/save-obj points surface "/tmp/conebox-recon.obj")
     $ (file/save-povray point surface "foot" "/tmp/conebox-recon.inc")

## Caveats and Bugs

### Occasional Holes

No effort is made to post-process the surfaces after they have been computed by the main algorithm.

The algorithm works by looking for small isometric cycles within the
[relative neighborhood graph](https://en.wikipedia.org/wiki/Relative_neighborhood_graph)
of the surface.
From within that collection of small cycles, a
[2-basis](https://en.wikipedia.org/wiki/Mac_Lane's_planarity_criterion)
representing the surface is extracted.
In the prototype, this is done by heuristically looking for isometric cycles within the k-neighborhood of each sample and
trying to determine which cycles are on the surface (versus inside or through it) by looking the number of times each edge is used in the total collection.
This typically leaves cycles of open edges (holes) which are searched for and added to the collection of faces.
This hole-filling process is not really post-processing as-such, it is actually just the second half of the heuristic search for the 2-basis.

Anyway, the main point of that was this:
Most if not all of the flaws that can be seen in the output could be managed heuristically and/or by known methods or programs.
That is not done here in the interest of keeping the code as simple as possible and also giving a clearer picture about the actual behavior of the algorithm.

### Speed

The code probably 5x slower than an equivalent C/C++ implementation would be.
I do not know how much of this is due to lack of expert performance tuning on my part versus the environment and language.
I hope that it is mostly the former because that would allow this implementation to be taken forward, but even if not, life is still okay.

## Data Sets

During the development of this code, I made use of datasets from
[The Stanford 3D Scanning Repository](http://graphics.stanford.edu/data/3Dscanrep/),
[The Digital Shape Workbench Shape Repository](http://shapes.aim-at-shape.net/),
as well as some datasets custom generated for this project (which are in the distribution).

## License

Copyright © 2014 James McClain

Distributed under the BSD-3 license; see the file COPYING.md in this distribution.
