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
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
  
    psc: {
      tests: {
        options: {
          module: "SimpleTests",
          main: "SimpleTests"
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
  
  grunt.registerTask("test", ["clean:dedupe", "clean:tests", "psc:tests", "execute:tests"]);
  grunt.registerTask("make", ["clean:dedupe", "pscMake", "dotPsci"]);
  grunt.registerTask("default", ["test", "make"]);
};
