# scalajs-loader
A proof of concept of a class-level Scala.js splitting linker that uses Webpack to operate completely in memory.

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
