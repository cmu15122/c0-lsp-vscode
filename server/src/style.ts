import { DiagnosticSeverity } from "vscode-languageserver";
import * as ast from "./ast";
import { StyleError } from "./error";
import { GlobalEnv } from "./typecheck/globalenv";

// TODO:
// - [~]      if (c) { return true; } else { return false; }
// - [ ]      c == true
// - [ ]      write without a read
// - [ ]      unused vars?
// - [ ]      // @assert [we don't have comments in the AST!]

class FindBadReturns extends ast.Visitor<Set<StyleError>> {
    protected visitIfStatement(ifStatement: ast.IfStatement, state: Set<StyleError>): void {
        if (ifStatement.alternate
            && this.isOnlyReturnBool(ifStatement.consequent, true)
            && this.isOnlyReturnBool(ifStatement.alternate, false)) {
            // should just be `return condition;`
            state.add(new StyleError(
                DiagnosticSeverity.Warning,
                // TODO: use the actual condition
                ifStatement, "Unnecessary if statement", "consider replacing this if statement with 'return condition;'"
            ));
        } else if (ifStatement.alternate
            && this.isOnlyReturnBool(ifStatement.consequent, false)
            && this.isOnlyReturnBool(ifStatement.alternate, true)) {
            // should just be `return !condition;`
            state.add(new StyleError(
                DiagnosticSeverity.Warning,
                // TODO: use the actual condition
                ifStatement, "Unnecessary if statement", "consider replacing this if statement with 'return !condition;'"
            ));
        }

        this.visit(ifStatement.test, state);
        this.visit(ifStatement.consequent, state);
        this.visit(ifStatement.alternate, state);
    }


    private isOnlyReturnBool(stmt: ast.Statement, bool: boolean): boolean {
        return (stmt.tag === "ReturnStatement" && stmt.argument?.tag === "BoolLiteral" && stmt.argument.value === bool)
            || (stmt.tag === "BlockStatement" && stmt.body.length === 1 && this.isOnlyReturnBool(stmt.body[0], bool));
    }
}


export function checkProgram(genv: GlobalEnv, decls: ast.Declaration[]): Set<StyleError> {
    const issues = new Set<StyleError>();

    for (const decl of decls) {
        (new FindBadReturns()).visit(decl, issues);
    }

    return issues;
}
