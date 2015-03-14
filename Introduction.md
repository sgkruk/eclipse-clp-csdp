Interface ECLiPSe - CSDP

# Introduction #

Interface between ECLiPSe-CLP the Constraint Programming system and CSDP the Semidefinite program solver.


# Details #

The idea behind this code is to allow ECLiPSe to interface to Semidefinite Program solvers.  Currently there are interfaces to Linear Program and Mixed Integer Program solvers but there were no SDP solver interface.  I needed to write Prolog code that solved some SDPs so I decided to write an interface to CSDP, Brian Borcher's SDP solvers.

The code is beta or even gamma quality, beware.