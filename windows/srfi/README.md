# Windows SRFI 138
This library provides SRFI 138 for larceny on Windows. 
It builds a self-extracting .exe file that contains a batch file to 
run the program along with a compiled version of the program.

## Usage
The `compile-r7rs` procedure is used just as it is when compiling 
files for POSIX systems.

## Notes
This library currently requires that the script to start larceny 
lies on your path and is called `larceny`.
It also requires that the built-in tool `iexpress.exe` works. 

## Implementation Notes
Much like how the POSIX version provides an executable shell script
to run the file, this version creates a shell script. The biggest 
difference is that it packages the shell script along with the
compiled file, so that you don't have to deal with a seperate 
compiled version of your code. These executables should be somewhat
portable, as long as the `larceny` command exists on the system in question.
