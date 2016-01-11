# Parallel Dynamic Programming in the Scala compiler
This is a proof-of-concept implementation of a compiler plugin automating 
the concepts from my [Master's thesis](https://reitzig.github.io/publications/Reitzig2012).

These are the steps to build and run the plugin:

 0. Check that your environment variable `SCALA_HOME` points to your Scala
    installation's root directory. Alternatively, adapt property `scala.home`
    from `build.xml` (line 3).
    
 1. Compile and package the plugin with `ant`.
 
 2. Compile (one of) the examples with
    
    ```bash
    scalac -Xplugin:dist/dpplugin.jar -classpath dist/dpplugin.jar src/de/unikl/reitzig/paralleldynprog/automation/examples/[example].scala &> out
    ```
        
    As an alternative to `-Xplugin` you can also specify use `-Xpluginsdir`.
    
 3. Investigate the output which has been written into file `out`.

In order to use the plugin in your own context, you will need to point your IDE
to `dpuser.jar` so you can use our annotation and markers. You compile as above.
The compiled code needs `dpruntime.jar` (or `dpplugin.jar`, but that is overkill)
in the classpath to run.

*Hints:*

 * You need Scala 2.9 and ant 1.6
 * You can use `ant clean` to remove all files by `ant`.
 * If you want to investigate the AST, add option `-Ybrowse:parser` to see it
   before our plugin, and `-Ybrowse:dynprog` after.
 * If you prefer a text dump similar to the original source code, add
   `-Xprint:dynprog -Ycompact-trees`.
 * In order to see how the plugin nestles into the compiler run, add
   `-Xshow-phases`
