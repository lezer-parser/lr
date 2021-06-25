import typescript from "rollup-plugin-typescript2"
import {nodeResolve} from "@rollup/plugin-node-resolve"
import commonJS from "@rollup/plugin-commonjs"

export default {
  input: "./src/index.ts",
  external: ["@lezer/common"],
  output: [{
    format: "cjs",
    file: "./dist/index.cjs",
    externalLiveBindings: false
  }, {
    format: "es",
    file: "./dist/index.js",
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
          target: "es6",
          strict: false,
          declaration: true
        }
      },
      include: ["src/*.ts"]
    })
  ]
}
