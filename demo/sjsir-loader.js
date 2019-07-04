const generator = require("../target/scala-2.12/scalajs-loader-fastopt").generator;
const path = require("path");

module.exports = function(source, foo) {
  const className = this.request.split("?")[1];
  const maybePath = generator.path(className);
  if (maybePath) {
    this.addDependency(path.resolve(maybePath));
  }

  console.error(className);

  this.callback(null,
    generator.generate(className)
  );
};
