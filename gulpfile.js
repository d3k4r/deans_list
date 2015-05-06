var gulp = require('gulp');
var purescript = require('gulp-purescript');
var nodemon = require('gulp-nodemon');
var del = require('del');
var source = require('vinyl-source-stream')
var browserify = require('browserify')
var rename = require('gulp-rename')

var paths = {
  server: 'src/server/**/*.purs',
  client: 'src/client/**/*.purs',
  dependencies: 'bower_components/**/src/**/*.purs'
};

gulp.task('clean', function(cb) {
  del(['build', 'public/js'], cb);
});

gulp.task('client-compile', function(){
  return gulp.src([paths.client, paths.dependencies])
    .pipe(purescript.psc({
      main: 'DeanList.Client.Main', 
      output: 'client_nobrowserify.js', 
      module: ['DeanList.Client.Main']
    }))
    .on('error', function(e){
      console.error('task client-compile:');
      console.error(e.message);
      this.emit('end');
    })
    .pipe(gulp.dest('build'));
});

gulp.task('client-browserify', ['client-compile'], function() {
  var bundleStream = browserify('./build/client_nobrowserify.js').bundle()
  bundleStream
    .pipe(source('client.js'))
    .pipe(rename('client.js'))
    .pipe(gulp.dest('./public/js'))
    .on('error', function(e) {
      console.error('task client-browserify:');
      console.error(e.message);
      this.emit('end');
    });
})

gulp.task('server', function(){
  return gulp.src([paths.server, paths.dependencies])
    .pipe(purescript.psc({
      main: 'DeanList.Server.Main', 
      output: 'server.js',
      module: ['DeanList.Server.Main']
    }))
    .on('error', function(e){
      console.error('task server:');
      console.error(e.message);
      this.emit('end');
    })
    .pipe(gulp.dest('build'));
});

gulp.task('watch', function() {
  gulp.watch(paths.server, ['server']);
  gulp.watch(paths.client, ['client']);
});

gulp.task('run-server', function () {
  return nodemon({
    script: 'build/server.js',
    ext: 'js',
    env: {
      'NODE_ENV': 'development'
    }
  }).on('restart', function () {
    console.log('Restarting server');
  });
});

gulp.task('client', ['client-browserify']);
gulp.task('default', ['server', 'client', 'watch', 'run-server']);
