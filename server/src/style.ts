import { DiagnosticSeverity } from "vscode-languageserver";
import * as ast from "./ast";
import { StyleError } from "./error";
import { GlobalEnv } from "./typecheck/globalenv";

export function checkProgram(genv: GlobalEnv, decls: ast.Declaration[]): Set<StyleError> {
    return new Set<StyleError>();
}
