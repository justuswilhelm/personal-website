var concat = require('gulp-concat');
var deploy = require('gulp-gh-pages');
var gulp = require('gulp');
var jade = require('gulp-jade');
var bower = require('gulp-bower');
var del = require('del');

var SRC = {
    jade: 'private/*.jade',
    css: 'private/css/*.css',
    cname: 'private/CNAME*'
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

gulp.task('cname', function () {
    return gulp.src(SRC.cname)
               .pipe(gulp.dest(TARGET.public));
});

gulp.task('build', [
    'jade',
    'css',
    'cname'
]);
gulp.task('clean', function () {
    return gulp.src(TARGET.public)
               .pipe(del());
});

gulp.task('watch', function () {
    gulp.watch(SRC.jade, ['jade']);
    gulp.watch(SRC.css, ['css']);
});

gulp.task('deploy', ['build'], function () {
    return gulp.src(TARGET.public + '/**/*')
               .pipe(deploy());
});

gulp.task('default', ['build']);
