# Advent of Code 2016 - Day 3

[<< Day 2](./day2)

## Triangles

Today's task seems simpler than the previous challenges, but that may just be
that scala was well suited to the problem. I was given a list of potential
triangles, represented as sets of three numbers. The task was to count the sets
that could be triangles, i.e. where all sides were shorter than the sum of the
other two sides.

There was little in the way of examples today. To fill out the tests, I threw
together some basic examples, and also took the first 9 lines of the challenge
data and calculated the solution to that by hand. I split the tests into two
parts categorising individual triangles, and correctly counting a full list in
string format.

To validate the sets of numbers we only need to compare a maximum to the other
two sides. I started a solution that found the maximum in a arbitrary sequence,
then summed the rest, but I quickly realised that was overly generic. I started
writing a three way if/else statement to cover the three possible maximums, but
settled on a method that returned the solution if the first number was a
maximum, otherwise rotated the triangle and tried again.

```
def validateTriangle(a: Int, b: Int, c: Int): Boolean = {
  if (a >= b && a >= c) a < b + c
  else validateTriangle(b, c, a)
}
```

Parsing the rows and counting the valid triangles was made very simple by scala's collections library. Each step already had a method to help

- **String.lines**: Split a string into a lazy evaluated iterator of lines.
- **Seq.collect**: Map a collection using a partial function, filtering out
  elements that don't have a mapping in the function. Note that scala can automatically convert an incomplete pattern match into a PartialFunction.
- **Seq.count**: Return the number of elements that match a predicate.

```
val ParsableLine: Regex ="""\s*(\d+)\s+(\d+)\s+(\d+)\s*""".r

def countPossibleTriangles(data: String): Int =
  data
    .lines
    .collect { case ParsableLine(a, b, c) => (a.toInt, b.toInt, c.toInt) }
    .count { case (a, b, c) => validateTriangle(a, b, c) }
```

The tests pretty much passed first time, though they did catch a typo comparing
the side lengths, so that was useful. Applying `countPossibleTriangles` to the
problem set gave the correct answer.

## And more triangles

The second task was again a rehash of the first problem. The rows were now to be
grouped into sets of three rows, each column of those 3 x 3 grids was to be
considered a triangle.

For testing I worked out the expected values for the test cases from part one if
read using part two's spec and left it at that.

I first considered clumping sets of three rows, and manually accessing the data
points, but that seemed messy. A quick lookup in the collections library
provided:

- **Iterator.sliding**: Gives a iterator of sliding windows over the iterator,
  with customisable window size and step length, along with modifier methods to
  deal with edge cases.
- **Seq.transpose**: Rotate a Seq of evenly sized Seqs, so that rows become
  columns.
- **Seq.flatMap**: Merge the chunks of three Seqs back into a single iterator

I extracted the code that parsed the lines into a shared method. This then
required mapping the 3-Tuples of edge lengths into the sequences that transpose
needed to work with. The count was also updated to match the changed format of
the data.

```
def countPossibleTrianglesVertically(data: String): Int =
  parseData(data)
    .map { case (a, b, c) => List(a, b, c) }
    .sliding(3, 3).withPartial(false)
    .flatMap(_.transpose)
    .count { case a :: b :: c :: _ => validateTriangle(a, b, c) }
```

All the tests were green, and the correct answer was generated. All in all a
pretty easy day.

[<< Day 2](./day2)
