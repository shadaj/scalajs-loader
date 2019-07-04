# scalajs-loader
A proof of concept of a class-level Scala.js splitting linker that uses Webpack to operate completely in memory.

# THIS IS A HACK AND BARELY HANDLES ANYTHING MORE THAN NUMBERS
The purpose is to start a conversation about new ways that Scala.js could enable bundle splitting support.

First, build the loader
```
$ sbt fastOptJS
```

Next, compile the demo code
```
$ cd demo/minimal-scalajs
$ sbt compile
```

Then, use Webpack to link the code into a bundle
```
$ cd ..
$ npm run build
```

Finally, serve the dist folder, open up the site in a browser, and view the console!
