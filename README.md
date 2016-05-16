&&&&&&&&&&&&& THE PHF SEA ICE MIXED LAYER MODEL &&&&&&&&&&&

This is the code for the sea ice-mixed layer model used and described in:

Petty, A.A, D.L. Feltham, P.A. Holland (2013), Impact of atmospheric forcing on Antarctic continental shelf water masses, Journal of Physical Oceanography, 2013, doi:10.1175/JPO-D-12-0172.1


INSTALLATION NOTES

To get things going, you need to install a Fortran compiler. I used gfortran.

The easiest way to install this is probably using Homebrew if you have a Mac (Fink doesn’t work so well for some reason).

try 

$ Brew install gfortran

You obviously might need to install Homebrew, but I do recommend that or MacPorts for easy installation of things. You may need to play around with your PATH to make sure terminal is looking for this installation n the right place. I can help with this.

Then it should be pretty simple to compile the program using:


$ gfortran global_data.f90 ml_procs.f90 ml_run.f90 -o run (to compile the fortran)

Then to run the model:
 
$ ./run


DIRECTORY STRUCTURE

Model
- This folder contains the Fortran scripts I believe to be the most up-to-date ones needed to reproduce the results in Petty et al., 2013. 

global_data.f90
- global constants etc

ml_procs.f90
 - contains all the physics etc used in the ML model. This is the most important

ml_run.f90
- code to select and read in forcing file etc and run the model (described in ml_procs.f90) given a specified configuration.

ml_run_etc.f90
- a whole load of files to run the various sensitivity tests used in Petty et al., 2013, JPO.  

Forcings
- Should contain the forcing sets used by the model. Think it’s missing some of the various climatological (not idealized) forcing sets used in the sensitivity study, so I will try and track those down.

Python_scripts
- A simple script to plot out a 3 panel, 10 year time series of the ice concentration/thickness, water column temp/salinity. 

I can add more scripts in this folder over time, but didn’t want to go overboard.

Output
- Where i’ve set the data to be saved.




