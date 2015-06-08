/* jshint node: true */
"use strict";

var gulp = require("gulp");
var jshint = require("gulp-jshint");
var jscs = require("gulp-jscs");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var run = require("gulp-run");
var rimraf = require("rimraf");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("lint", function() {
  return gulp.src("src/**/*.js")
    .pipe(jshint())
    .pipe(jshint.reporter())
    .pipe(jscs());
});

gulp.task("make", ["lint"], function() {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscMake({ ffi: foreigns }));
});

gulp.task("docs", ["clean-docs"], function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscDocs({
      docgen: {
        "Data.Array": "docs/Data.Array.md",
        "Data.Array.ST": "docs/Data.Array.ST.md",
        "Data.Array.Unsafe": "docs/Data.Array.Unsafe.md"
      }
    }));
});

gulp.task("dotpsci", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("test", ["make"], function() {
  return gulp.src(sources.concat(["test/**/*.purs"]))
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main", ffi: foreigns }))
    .pipe(run("node"));
});

gulp.task("default", ["make", "docs", "dotpsci", "test"]);
