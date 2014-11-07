Legobase
======

An efficient query engine in Scala programming language

Testing
=======
For testing the interpreter, go to `lego-core` project using `project lego-core`.
Then for running query X with scaling factor N you have to run the following command:
`run DATA_FOLDER N QX`

To generate code the first command that must be run is:
`embedAll`
in project legocompiler

To generate C code, you can run the following command in the legocompiler project in sbt:
`run DATA_FOLDER N QX_C`
where N is the TPCH scaling factor and X in [0,22]
The generated code will appear in the generator-out folder, which can then be compiled using a traditional C compiler (CLANG/GCC).

For testing the compiler (Scala generated code), first you have to generate the code. 
For that purpose you have to go to `legocompiler` project using `project legocompiler`.
Then for generating query X with scaling factor N you have to run the following command:
`generate-test DATA_FOLDER N QX`
Then for testing the correctness you have copy the generated file into `test` folder of `legocompiler` project.
Then you have to run the following command:
`test:run DATA_FOLDER N QX`

For testing all TPCH queries with Scala code generator, in `legocompiler` project, 
you should run `generate-test DATA_FOLDER N testsuite-scala`.
Then you should publish `lego-core` project using `lego-core/publish-local`.
Afterwards, you have set your the environment `SCALA_PATH` to the folder which contains `scalac`.
Finally, you have to go to `generator-out` folder and run `./run_scala.sh DATA_FOLDER N`.
