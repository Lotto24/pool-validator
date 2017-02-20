
GUI-tool for offline validation of participation pools
=================================================================
- [Introduction](#introduction)
- [Build](#build)
- [Build Windows executable)](#win-build)
 
# Introduction 
This project provides a GUI tool for validating downloadable participation pool archives
of a lottery draw.
It applies multiple checks to validate the json documents concerned to a lottery product
order (e.g. check the signatures, timestamps etc.).
For more details about the domain, the performed checks and a description of the structure of 
downloadable participation pool archives 
see [lotteries-io.github.io](https://github.com/lotteries-io/lotteries-io.github.io).

 
# Build
To build the project run
```
sbt clean compile stage
```
This will compile the project and create a directory target/universal/stage 
in the project-directory. The stage directory contains a subfolder 'bin' 
with start-scripts for Windows and Linux and a subfolder 'lib' containing
the required depedencies (jar-files). 
To deploy the application copy the stage directory to the desired location 
and rename it as needed.
The Java heap size can be adjusted via variable CFG_OPTS in the start scripts, e.g.
```
set CFG_OPTS=-Xmx2G
```
A heap size of 2 GB (2G) or more is recommended!

# Build Windows executable

The sbt file also includes a task to create a Windows x64 executable wrapper containing all
jars.

Prerequisites:

* install Ant and launch4j if so
* define system variable LAUNCH4J_HOME
* run..
    ```
    sbt launch4j
    ```

The poolvalidator.exe is created in the target directory.
In order to get a working distribution the poolvalidator.exe should be copied in a 
directory like this:

```
[poolvalidator]
    [credentials]
    [jre]
    poolvalidator.exe
    poolvalidator.l4j.ini
    settings.properties
```

The bundled jre (at least jre1.8.0_74) is expected to be in the jre directory.
The heapsize can be configured in the poolvalidator.l4j.ini:
```
# Launch4j runtime config
-Xms1g
-Xmx4g
```