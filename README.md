# mst

This is a library and program for reconstructing surfaces from organized collections of point samples (such as those produced by a laser range-scanner).
The ideas represented by this program and presented in the accompanying
[article](http://daystrom-data-concepts.com/SR/)
are well-developed (in my view), but the implementation itself is still somewhat of a prototype.
See the "Caveats and Bugs" section for more on this.

## Goals

This program is intended to demonstrate successful reconstruction of surfaces that contain sharp features like hard edges and cones, while still being able to reconstruct smooth surfaces.
Examples can be seen in the three pictures directly below.
The first is a reconstruction of
[conebox.obj](http://www.daystrom-data-concepts.com/SR/data/conebox.obj)
and is a manifold (all edges meet two faces, the funny looking top-left and bottom-right corners are due to the fact that there are no samples at those corners in the sample set).
The second is a reconstruction of
[fandisk.off](http://shapes.aim-at-shape.net/ontologies/shapes/viewmodels.jsp)
and is also a manifold.
The third image is a reconstruction of
[Foot.obj](http://shapes.aim-at-shape.net/ontologies/shapes/viewmodels.jsp)
and, out of 20592 total edges, it has 61 half-edges and 29 edges that meet three faces.

![Alt conebox.obj](http://daystrom-data-concepts.com/SR/images/conebox.png)
![Alt fandisk.obj](http://daystrom-data-concepts.com/SR/images/fandisk.png)
![Alt Foot.obj](http://daystrom-data-concepts.com/SR/images/Foot.png)

This goal can even be achieved in the case of undersampled surfaces where the lack of samples might be too severe for other methods to be usable.
Consider the two images below.
The first image is of a surface reconstructed from 456 vertices
([cutcube-lo.obj](http://www.daystrom-data-concepts.com/SR/data/cutcube-lo.obj)).
For comparison, a reconstruction of the same surface from 7088 samples
([cutcube.obj](http://www.daystrom-data-concepts.com/SR/data/cutcube.obj))
is shown.
The second reconstruction is a manifold whereas the first one would be except for three stray triangles (which results in 6 half-edges and 3 edges that meet three faces).
Being able to function in the presence of undersampling is important because all surfaces can be considered to be understampled near sharp features (from the point of view of the sample conditions of most reconstruction algorithms).

![Alt cutcube.obj (undersampled)](http://daystrom-data-concepts.com/SR/images/cutcube-lo.png)
![Alt cutcube.obj](http://daystrom-data-concepts.com/SR/images/cutcube-hi.png)

## Usage

From within a REPL session, type:

     $ (ns mst.core)
     $ (reconstruct "/tmp/Foot.obj" 33)

to navigate into the main project namespace and reconstruct the Wavefront `.OBJ` formatted file `/tmp/Foot.obj` (taking the vertices and ignoring any faces that may be in the file).
The reconstruction is done with a *neighborhood size* of 33.
The result of this function call is two files: `/tmp/Foot.obj.recon.33.obj` and `/tmp/Foot.obj.recon.33.inc`.
(The first file contains the reconstruction in Wavefront `.OBJ` format,
the second contains a
[POV-Ray](http://www.povray.org/)
`mesh2` suitable for use with that program.)

The meaning of the neighborhood size parameter is explained in the
[article](http://daystrom-data-concepts.com/SR/)
that discusses this project.
In short, it is the number of nearest neighbors around each point to consider when building the graph whose edges bound the faces of the reconstruction and when looking for cycles in that graph.
The sample conditions for the reconstruction algorithm stipulate that local neighborhoods contain sufficient information to make local operation possible.

The parameter also controls the maximum hole size that will be filled in during the final hole-filling stage of the reconstruction process.
Holes are defined as cycles of half-edges.
If the surface is closed (boundaryless) then this parameter can be as large as you like (at least if the sample set obeys the algorithm's sample conditions).
However, if the surface does have a boundary, then this parameter must be kept low enough to prevent the program from mistakenly thinking that a boundary is a large hole.

Type:

     $ (reconstruct "/tmp/Caesar.off" 33)

from within the `mst.core` namespace to reconstruct the `OFF` formatted file `/tmp/Caesar.off`.

The `core/reconstruct` function can be used to do the entire reconstruction process with one command, but the individual steps can also be done by hand:

     $ (def points (file/load-obj "/tmp/Foot.obj"))
     $ (def surface (recon/compute-surface points 33))
     $ (file/save-obj points surface "/tmp/Foot-recon.obj")
     $ (file/save-povray point surface "foot" "/tmp/Foot-recon.inc")

## Caveats and Bugs

### Occasional Holes

Even though hole-filling is done as a final step before completing the reconstruction process, the occasional hole is still present in the output.
Hole-filling is done by finding half-edges in the reconstruction (after all of the retries have been done), looking for isometric cycles of half-edges (the program's definition of "holes"), then triangulating those cycles.
This method fails in the presence of a collection of edges which bound a "hole" (now using the word in the normal sense) but which are not all half-edges, as for example when "overlapping triangles" are produced.
That problem can be managed heuristically, but I have hesitated to address it because I would like to keep this prototype code simple and would also like to think of a nicer way to handle this issue.

### Speed

The code probably 5x slower than an equivalent C/C++ implementation would be.
I do not know how much of this is due to lack of expert performance tuning on my part versus how much is due to the environment and language.
I hope that it is mostly the former because that would allow this implementation to be taken forward, but even if not, the many nice features of
[Clojure](http://www.clojure.org)
made it a good choice for this prototype.

## Data Sets

During the development of this code, I made use of data sets from
[The Stanford 3D Scanning Repository](http://graphics.stanford.edu/data/3Dscanrep/),
[The Digital Shape Workbench Shape Repository](http://shapes.aim-at-shape.net/ontologies/shapes/viewmodels.jsp),
as well as some data sets
[custom generated for this project](http://daystrom-data-concepts.com/SR/src/objgen-src.tar.gz)
of uniformly-sampled surfaces with sharp features.

## License

Copyright © 2014 James McClain

Distributed under the BSD-3 license; see the file COPYING.md in this distribution.

![Alt hole](http://daystrom-data-concepts.com/SR/images/ra-moses.png)
