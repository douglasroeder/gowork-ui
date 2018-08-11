var gulp = require('gulp');
var sass = require('gulp-sass');
var elm = require('gulp-elm');
var gutil = require('gulp-util');
var plumber = require('gulp-plumber');
var connect = require('gulp-connect');

// File paths
var paths = {
  dest: 'dist',
  elm: 'src/**/*.elm',
  sass: 'src/scss/index.scss',
  static: 'src/**/*.html'
};

// Init Elm
gulp.task('elm-init', elm.init);

var elmOptions = {
    debug: true
};

gulp.task('css', function () {
    return gulp.src(paths.sass)
      .pipe(sass().on('error', sass.logError))
      .pipe(gulp.dest(paths.dest));
  });

// Compile Elm to HTML
gulp.task('elm', ['elm-init'], function(){
    return gulp.src(paths.elm)
        .pipe(plumber())
        .pipe(elm(elmOptions))
        .pipe(gulp.dest(paths.dest));
});

// Move static assets to dist
gulp.task('static', function() {
    return gulp.src(paths.static)
        .pipe(plumber())
        .pipe(gulp.dest(paths.dest));
});

// Watch for changes and compile
gulp.task('watch', function() {
    gulp.watch(paths.elm, ['elm']);
    gulp.watch(paths.static, ['static']);
    gulp.watch(paths.sass, ['css']);
});

// Local server
gulp.task('connect', function() {
    connect.server({
        root: 'dist',
        port: 3000
    });
});

// Main gulp tasks
gulp.task('build', ['elm', 'static', 'css']);
gulp.task('default', ['connect', 'build', 'watch']);
