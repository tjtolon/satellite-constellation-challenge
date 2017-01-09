# satellite-constellation-challenge

Point of this was to learn how to parse things with `attoparsec`. Code turned out
to be quite clean, which makes me happy. Also I practiced on using code from
random hackage repository, in here it was `wreq` for accessing data from the web
and `astar` for using A-star algorithm for satellite constellation route solution.

The point of the challenge was to solve a route between two points on a sphere,
"Earth", such that the route goes through satellite points above and around the
sphere. Satellites are connected when they are not obstructed by the sphere,
i.e. they should have visibilty to one another.

In the challenge, input was given as a randomly generated file, contaiing the following information
* Starting location, in latitude and longitude
* Goal location, in latitude and longitude
* a set of satellites, one in each line, with latitude, longitude, altitude, and
  an ID, separated by commas.
* Maybe something extra, was it the seed number used to generate the input file

Goal was to give the list of satellites which creates the route between the
start and the goal, matching with the input file seed. This code manages to do
it.
