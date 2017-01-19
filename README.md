# Advent of Code 2016

I'm starting this pretty late, but it still seems like an interesting challenge.
I have a number of goals to improve my coding whilst I do this:

- Improve my testing
- Improve the readability of my code
- Write up the process of completing the tasks
- Improve my ability to quickly produce a solution to a specific problem

I'm pretty good at writing working code. The maintainability of my projects,
especially personal ones is not at the same level. I have done some testing here
and there, and when I have it has been a great boon to that project. It is not
something I have as a habit, nor do I use it regularly enough to be able to
reason properly about when and how to test my projects.

I'll be using Scala to write the code. I pondered also using it to learn Rust or
Go, but I think given my other goals using a language I'm fairly familiar with
will help.

## Running my code

If you want to run some or all of the project you'll need:

- A JDK for java 1.8, I'm currently using [Java SE Development Kit
  8u121](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
- [SBT](http://www.scala-sbt.org/) which will make sure any other dependencies, 
  including Scala itself, are available.

With those setup, from the root of the project `sbt test` will run all the
tests, and `sbt console` will start the Scala REPL with the classpath including
everything needed to run the tasks. All the code that I'm running in worksheets
in IntelliJ will also run as is in the REPL.

To read the write ups, see the 
[GitHub Pages for the project](https://kamioftea.github.io/advent-of-code-2016/)
