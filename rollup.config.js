import typescript from "rollup-plugin-typescript2"
import nodeResolve from "rollup-plugin-node-resolve"
import commonJS from "rollup-plugin-commonjs"

export default {
  input: "./src/index.ts",
  external: ["lezer-tree"],
  output: [{
    format: "cjs",
    file: "./dist/index.cjs",
    sourcemap: true,
    externalLiveBindings: false
  }, {
    format: "es",
    file: "./dist/index.es.js",
    sourcemap: true,
    externalLiveBindings: false
  }],
  plugins: [
    nodeResolve(),
    commonJS(),
    typescript({
      check: false,
      tsconfigOverride: {
        compilerOptions: {
          lib: ["es5", "es6"],
          sourceMap: true,
          target: "es5",
          strict: false,
          declaration: true
        }
      },
      include: ["src/*.ts"]
    })
  ]
}
