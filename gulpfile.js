var concat = require('gulp-concat');
var gulp = require('gulp');
var jade = require('gulp-jade');
var bower = require('gulp-bower');
var rimraf = require('gulp-rimraf');

var SRC = {
    jade: 'private/*.jade',
    css: 'private/css/*.css'
};

var TARGET = {
    public: 'public/',
    css: 'public/css'
};

gulp.task('jade', function () {
    return gulp.src(SRC.jade)
         .pipe(jade())
         .pipe(gulp.dest(TARGET.public));
});

gulp.task('css', function () {
    return gulp.src(SRC.css)
               .pipe(concat('main.css'))
               .pipe(gulp.dest(TARGET.css));
});

gulp.task('clean', function () {
    return gulp.src(TARGET.public)
               .pipe(rimraf());
});

gulp.task('watch', function () {
    gulp.watch(SRC.jade, ['jade']);
    gulp.watch(SRC.css, ['css']);
});

gulp.task('default', [
    'jade',
    'css'
]);
