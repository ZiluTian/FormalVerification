To run sat4j benchmarks, you need to download the file
http://download.forge.ow2.org/sat4j/sat4j-sat4j-sat-v20130419.zip
and unpack so that sat4j-sat.jar would be located alongside the zamsat repository folder.
Then run python3 run_benchmarks.jar sat4j

To run cafesat, you need to clone the repository
https://github.com/regb/cafesat
alongside this zamsat repository, and then run the following command inside it
sbt cafesat.
Then run python3 run_benchmarks.jar cafesat