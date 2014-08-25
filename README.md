# mst

This is a library and program for reconstructing surfaces from organized collections of point samples (such as those produced by a laser range-scanner).
The ideas represented by this program and presented in the accompanying
[article](http://daystrom-data-concepts.com/SR/)
are well-developed (in my view), but the implementation itself is still in the prototype stage.
See the "Caveats and Bugs" section for more on this.

## Goals

This program is intended to demonstrate successful reconstruction of surfaces that contain sharp features like hard edges and cones, while still being able to reconstruct smooth surfaces.
Examples of the former and the latter can be seen in the two pictures directly below.

![Alt conebox.obj](http://daystrom-data-concepts.com/SR/images/480/conebox-large.png)
![Alt Foot.obj](http://daystrom-data-concepts.com/SR/images/480/foot.png)

This goal can even be achieved in the case of undersampled surfaces where the lack of samples might be too severe for other methods to be usable.
Consider the two images below.
The first image is of a surface reconstructed from 456 vertices.
For comparison, a reconstruction of the same surface from 7088 samples is shown.
Being able to function in the presence of undersampling is important because all surfaces can be considered to be understampled near sharp features (from the point of view of the sample conditions of most reconstruction algorithms).

![Alt cutcube.obj (small)](http://daystrom-data-concepts.com/SR/images/480/cutcube-small.png)
![Alt cutcube.obj (large)](http://daystrom-data-concepts.com/SR/images/480/cutcube-large.png)

## Usage

From within a REPL session, type:

     $ (ns mst.core)
     $ (reconstruct "/tmp/Foot.obj" 9 3 33)

to navigate into the main project namespace and reconstruct the Wavefront `.OBJ` formatted file `/tmp/Foot.obj` (taking the vertices and ignoring any faces that may be in the file).
The reconstruction is done with a *neighborhood size* of 9, with a maximum of 3 *attempts* to reconstruct the surface, and with a *maximum fillable-hole size* of 33.

The meaning of the neighborhood size parameter is explained in the
[article](http://daystrom-data-concepts.com/SR/)
that discusses this project.
In short, it is the number of nearest neighbors around each point to consider when building the graph whose edges bound faces of the reconstruction and looking for cycles in that graph.
The sample conditions for the reconstruction algorithm stipulate that local neighborhoods contain sufficient information to make local operation possible.

When points are left isolated or as part of half-edges (edges that bound only one triangle of the reconstruction), the program will eliminate the reconstruction in the vicinity of such points and reconstruct that area again.
The second parameter controls the number of times that this is done.
Playing with this parameter can allow reconstruction even when the sample set does not strictly meet the sample conditions.

The third parameter controls the maximum hole size that will be filled in during the final hole-filling stage of the reconstruction process.
Holes are defined as cycles of half-edges.
If the surface is closed (boundaryless) then this parameter can be as large as you like.
However, if the surface does have a boundary, then this parameter must be kept low enough to prevent the program from mistakenly thinking that the boundary is a large hole.

Type:

     $ (reconstruct "/tmp/Caesar.off" 9 3 33)

from within the `mst.core` namespace to reconstruct the `OFF` formatted file `/tmp/Caesar.off`.

The result of this function call is two files: `/tmp/Caesar.off.recon.9.3.obj` and `/tmp/Caesar.off.recon.9.3.inc`.
The first file contains the reconstruction in Wavefront `.OBJ` format.
The second file contains a
[POV-Ray](http://www.povray.org/)
`mesh2` suitable for use with that program.
POV-ray was used to produce the pictures seen above.

The `core/reconstruct` function does the entire process, but the individual steps can also be done by hand:

     $ (def points (file/load-obj "/tmp/Foot.obj"))
     $ (def surface (recon/compute-surface points 9 3 33))
     $ (file/save-obj points surface "/tmp/Foot-reconstruction.obj")
     $ (file/save-povray point surface "foot" "/tmp/Foot.inc")

## Caveats and Bugs

### Holes

Even though hole-filling is done as a final step before completing the reconstruction process, the occasional hole is still present in the output.
Hole-filling is done by finding half-edges in the reconstruction (after all of the retries have been done), looking for isometric cycles of half-edges (the program's definition of "holes"), then triangulating those cycles.
This method fails in the presence of a collection of edges which bound a "hole" (now using the word in the normal sense) but which are not all half-edges, as for example when "overlapping triangles" are produced.
That problem can be managed heuristically, but I have hesitated to address it because I would like to keep this prototype code simple and would also like to think of a nicer way to handle this issue.

![Alt hole](http://daystrom-data-concepts.com/SR/images/hole.png)

### Speed

The code probably 10x slower than an equivalent C/C++ implementation would be.
I do not know how much of this is due to lack of expert performance tuning on my part versus how much is due to the environment and language.
I hope that it is mostly the former because that would allow this implementation to be taken forward, but even if the latter is true, the many nice features of
[Clojure](http://www.clojure.org)
made it a good choice for this prototype.

## Data Sets

During the development of this code, I made use of data sets from
[The Stanford 3D Scanning Repository](http://graphics.stanford.edu/data/3Dscanrep/),
[The Digital Shape Workbench Shape Repository](http://shapes.aim-at-shape.net/ontologies/shapes/viewmodels.jsp),
as well as some data sets
[custom generated for this project](http://daystrom-data-concepts.com/SR/)
of uniformly-sampled surfaces with sharp features.

## License

Copyright © 2014 James McClain

Distributed under the BSD-3 license; see the file COPYING.TXT in this distribution.