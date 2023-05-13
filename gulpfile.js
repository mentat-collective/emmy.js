const gulp = require('gulp')
const ts = require('gulp-typescript')
const flatten = require('gulp-flatten')
const  { exec } = require('child_process')

function shadow(cmd, target, a) {
  const arg = a || ''
  return function cljs(cb) {
    exec(
      `npx shadow-cljs ${cmd} ${target} ${arg}`,
      (error, stdout, stderr) => {
        stdout && console.log(stdout)
        stderr && console.error(stderr)
        error && console.error(error)
        cb()
      })
  }
}

exports.cljs = cljs = shadow('compile', ':dev')
exports.watch_cljs = shadow('watch', ':dev', '--verbose')

const tsp = ts.createProject('tsconfig.json')

exports.tsc = tsc = () => {
  let result = tsp.src().pipe(tsp())
  return result.js.pipe(flatten()).pipe(gulp.dest('build'))
}

exports.default = gulp.series(cljs, tsc)
