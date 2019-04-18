import typescript from "rollup-plugin-typescript2"
import nodeResolve from "rollup-plugin-node-resolve"

export default {
  input: "./src/index.ts",
  output: {
    format: "commonjs",
    file: "./dist/index.ts",
    sourcemap: true
  },
  plugins: [
    nodeResolve(),
    typescript({
      check: false,
      tsconfigOverride: {
        compilerOptions: {lib: ["es5", "es6", "dom"], sourceMap: true, target: "es5", strict: false},
        include: null
      }
    })
  ]
}
