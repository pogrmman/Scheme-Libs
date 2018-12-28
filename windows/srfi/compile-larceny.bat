@echo off

set pgm=%~1
set config=compile.config
@echo Program to compile: %pgm%
@echo Writing to config file: %config%
@echo -pgm %pgm%> %config%

:loop
shift
set param=%~1
set arg=%~2
if not "%param%" == "" (
	 if not "%arg%" == "" (
	 		@echo %param% %arg%>> %config%
			@echo Found argument: %param% %arg%
			shift
			goto loop
	) else (
		@echo Finished writing file %config%
		goto compile
	)
) else (
	@echo Finished writing file %config%
	goto compile
)

:compile
@echo Calling compile-larceny.scm
call larceny -r7rs compile-larceny.scm
@echo Finished compiling %pgm%
@echo Deleting %config%
del %config%
