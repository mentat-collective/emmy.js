const fs = require('node:fs/promises')
const gulp = require('gulp')
const  { spawn } = require('child_process')

function shadow(...args) {
  return function shadow_cljs() {
    return spawn('npx', ['shadow-cljs'].concat(args), { stdio: 'inherit' })
  }
}

exports.clean = clean = () => fs.rm('build', { force: true, recursive: true })
exports.watch = shadow('watch', ':emmy')
exports.compile = compile = shadow('compile', ':emmy')
exports.release = release = shadow('release', ':emmy')

exports.default = gulp.series(clean, release)
