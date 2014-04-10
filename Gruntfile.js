module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],

    clean: {
      dedupe: ["bower_components/purescript-arrays"],
      tests: ["tmp"],
      lib: ["js", "externs"]
    },

    "purescript-make": {
      lib: {
        src: "<%=libFiles%>"
      }
    },

    purescript: {
      tests: {
        options: {
          main: "SimpleTests",
          module: ["SimpleTests"]
        },
        src: ["tests/Simple.purs", "<%=libFiles%>"],
        dest: "tmp/tests.js"
      }
    },

    execute: {
      tests: {
        src: "tmp/tests.js"
      }
    }

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("test", ["clean:dedupe", "clean:tests", "purescript:tests", "execute:tests"]);
  grunt.registerTask("lib",  ["clean:dedupe", "purescript-make:lib"]);
  grunt.registerTask("default", ["test", "lib"]);
};
