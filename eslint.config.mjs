import { defineConfig, globalIgnores } from "eslint/config";
import typescriptEslint from "@typescript-eslint/eslint-plugin";
import globals from "globals";
import tsParser from "@typescript-eslint/parser";
import path from "node:path";
import { fileURLToPath } from "node:url";
import js from "@eslint/js";
import { FlatCompat } from "@eslint/eslintrc";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const compat = new FlatCompat({
    baseDirectory: __dirname,
    recommendedConfig: js.configs.recommended,
    allConfig: js.configs.all
});

export default defineConfig([globalIgnores([
    "src/types/typeStrings/typeString_parser_header.ts",
    "src/types/typeStrings/typeString_parser.ts",
    "src/compile/inference/file_level_definitions_parser_header.ts",
    "src/compile/inference/file_level_definitions_parser.ts",
    "test/samples/**"
]), {
    extends: compat.extends(
        "eslint:recommended",
        "plugin:@typescript-eslint/eslint-recommended",
        "plugin:@typescript-eslint/recommended",
        "plugin:prettier/recommended",
        "prettier",
    ),

    plugins: {
        "@typescript-eslint": typescriptEslint,
    },

    languageOptions: {
        globals: {
            ...globals.node,
        },

        parser: tsParser,
    },

    rules: {
        "no-var": "error",
        "no-constant-condition": "off",
        "comma-dangle": ["error", "never"],
        "no-prototype-builtins": "off",
        "@typescript-eslint/no-explicit-any": "off",
        "@typescript-eslint/no-var-requires": "off",
        "@typescript-eslint/no-this-alias": "off",
        "@typescript-eslint/explicit-function-return-type": "off",

        "@typescript-eslint/array-type": ["error", {
            default: "array-simple",
        }],

        "@typescript-eslint/explicit-module-boundary-types": ["error", {
            allowArgumentsExplicitlyTypedAsAny: true,
        }],
    },
}]);
