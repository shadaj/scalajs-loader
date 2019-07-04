var merge = require('webpack-merge');
var core = require('./webpack-core.config.js')
var path = require('path');

module.exports = merge(core, {
  devtool: "none",
  entry: "./scalajs-entry.js",
  module: {
    noParse: (content) => {
      return content.endsWith("-fastopt.js");
    },
    rules: [
      {
        test: /\-fastopt.js$/,
        use: [ require.resolve('./fastopt-loader.js') ]
      }
    ]
  },
  devServer: {
    contentBase: path.join(__dirname, 'dist'),
    compress: true,
    port: 9000
  }
})
