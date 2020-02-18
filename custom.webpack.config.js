var webpack = require('webpack');

module.exports = require('./scalajs.webpack.config');

module.exports.target = 'node';

module.exports.node = {
    canvas: 'empty'
}