import { DiagnosticSeverity } from "vscode-languageserver";
import * as ast from "./ast";
import { StyleError } from "./error";
import { GlobalEnv } from "./typecheck/globalenv";

// TODO:
// - [~]      if (c) { return true; } else { return false; }
// - [~]      c == true
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


class FindBoolCompares extends ast.Visitor<Set<StyleError>> {
    protected visitBinaryExpression(binaryExpression: ast.BinaryExpression, state: Set<StyleError>): void {
        if ((binaryExpression.left.tag === "BoolLiteral"
            || binaryExpression.right.tag === "BoolLiteral")
            && (binaryExpression.operator === "==" ||
                binaryExpression.operator === "!=")
        ) {
            state.add(new StyleError(
                DiagnosticSeverity.Warning,
                binaryExpression, `Unneeded ${binaryExpression.operator === "!=" ? "in" : ""}equality comparison with bool literal`,
                // TODO: use the actual other operand
                "consider rewriting this to just 'x/!x'"
            ));
        }

        this.visit(binaryExpression.left, state);
        this.visit(binaryExpression.right, state);
    }
}

class FindIntBoundsChecks extends ast.Visitor<Set<StyleError>> {
    protected visitBinaryExpression(binaryExpression: ast.BinaryExpression, state: Set<StyleError>): void {
        const error = this.checkBinExpr(binaryExpression);
        if (error !== null) {
            state.add(error);
        }

        this.visit(binaryExpression.left, state);
        this.visit(binaryExpression.right, state);
    }

    private checkBinExpr(binaryExpression: ast.BinaryExpression): StyleError | null {
        const alwaysTrue: [
            (e: ast.Expression) => boolean,
            (op: ast.BinaryOperator) => boolean,
            (e: ast.Expression) => boolean,
        ][] = [
                [_ => true, x => x === "<=", e => this.isFunctionCall(e, "int_max")],
                [e => this.isFunctionCall(e, "int_max"), x => x === ">=", _ => true],
                [_ => true, x => x === ">=", e => this.isFunctionCall(e, "int_min")],
                [e => this.isFunctionCall(e, "int_min"), x => x === "<=", _ => true],
            ];

        for (const [l, op, r] of alwaysTrue) {
            if (l(binaryExpression.left)
                && op(binaryExpression.operator)
                && r(binaryExpression.right)) {
                return new StyleError(
                    DiagnosticSeverity.Warning,
                    binaryExpression,
                    "This comparison is always true"
                );
            }
        }

        const alwaysFalse: [
            (e: ast.Expression) => boolean,
            (op: ast.BinaryOperator) => boolean,
            (e: ast.Expression) => boolean,
        ][] = [
                [_ => true, x => x === ">", e => this.isFunctionCall(e, "int_max")],
                [e => this.isFunctionCall(e, "int_max"), x => x === "<", _ => true],

                [_ => true, x => x === "<", e => this.isFunctionCall(e, "int_min")],
                [e => this.isFunctionCall(e, "int_min"), x => x === ">", _ => true],
            ];

        for (const [l, op, r] of alwaysFalse) {
            if (l(binaryExpression.left)
                && op(binaryExpression.operator)
                && r(binaryExpression.right)) {
                return new StyleError(
                    DiagnosticSeverity.Warning,
                    binaryExpression,
                    "This comparison is always false"
                );
            }
        }

        return null;
    }

    private isFunctionCall(expr: ast.Expression, functionName: string): boolean {
        return expr.tag === "CallExpression" && expr.callee.name === functionName;
    }
}


export function checkProgram(genv: GlobalEnv, decls: ast.Declaration[]): Set<StyleError> {
    const issues = new Set<StyleError>();

    for (const decl of decls) {
        (new FindBadReturns()).visit(decl, issues);
        (new FindBoolCompares()).visit(decl, issues);
        (new FindIntBoundsChecks()).visit(decl, issues);
    }

    return issues;
}
