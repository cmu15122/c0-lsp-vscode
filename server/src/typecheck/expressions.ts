/**
 * Code for typechecking expressions
 */
import {
    GlobalEnv,
    getFunctionDeclaration,
    getStructDefinition,
    actualType,
    concreteType,
    fullTypeName,
    isPrintfLike
} from "./globalenv";
import { Env, Synthed, isSubtype, leastUpperBoundSynthedType, actualSynthed, ActualSynthed } from "./types";
import * as ast from "../ast";
import { ImpossibleError, TypingError } from "../error";
import { typeToString } from "../print";
import * as util from "../util";
import { DiagnosticSeverity } from "vscode-languageserver";
import { uriToWorkspace } from "../server";

export type mode =
    | null
    | { tag: "@requires" }
    | { tag: "@ensures"; returns: ast.Type }
    | { tag: "@loop_invariant" }
    | { tag: "@assert" };

function valueDescription(genv: GlobalEnv, tp: Synthed): string {
    const t = actualSynthed(genv, tp);
    switch (t.tag) {
        case "IntType":
            return "an integer";
        case "BoolType":
            return "a boolean";
        case "StringType":
            return "a string";
        case "CharType":
            return "a character";
        case "VoidType":
            return "a void expression";
        case "PointerType":
            return "a pointer";
        case "ArrayType":
            return "an array";
        case "StructType":
            return "a struct";
        case "AmbiguousNullPointer":
            return "a pointer";
        case "AnonymousFunctionTypePointer":
            return "a pointer";
        case "NamedFunctionType":
            return "a function";
        default:
            return "not possible";
    }
}

/** Asserts that a synthesized type has small type */
export function synthLValue(genv: GlobalEnv, env: Env, mode: mode, exp: ast.LValue, sourceFile: string | undefined): ast.ValueType {
    const synthedType = synthExpression(genv, env, mode, exp, sourceFile);
    switch (synthedType.tag) {
        case "AmbiguousNullPointer":
            throw new ImpossibleError("lvalue cannot be null");
        case "AnonymousFunctionTypePointer":
            throw new ImpossibleError("lvalue cannot be address-of");
        case "NamedFunctionType":
            throw new TypingError(
                exp,
                `cannot assign expression with function type ${synthedType.definition.id.name}`,
                "use pointers to functions"
            );
        case "VoidType":
            throw new TypingError(exp, "cannot assign to an expression with type 'void'");
    }
    const actualSynthedType = actualType(genv, synthedType);
    switch (actualSynthedType.tag) {
        case "StructType": {
            throw new TypingError(
                exp,
                `cannot assign expression with type 'struct ${actualSynthedType.id.name}'`,
                "Assign the parts of the struct individually"
            );
        }
        case "NamedFunctionType":
            throw new TypingError(
                exp,
                `cannot assign expression with function type ${actualSynthedType.definition.id.name}`,
                "use pointers to functions"
            );
        default:
            return synthedType;
    }
}

export function synthExpression(genv: GlobalEnv, env: Env, mode: mode, exp: ast.Expression, sourceFile: string | undefined): Synthed {
    switch (exp.tag) {
        case "Identifier": {
            const t = env.get(exp.name);
            if (t === undefined) {
                throw new TypingError(exp, `variable ${exp.name} not declared`);
            } else {
                return t;
            }
        }
        case "IntLiteral":
            return { tag: "IntType" };
        case "StringLiteral":
            return { tag: "StringType" };
        case "CharLiteral":
            return { tag: "CharType" };
        case "BoolLiteral":
            return { tag: "BoolType" };
        case "NullLiteral":
            return { tag: "AmbiguousNullPointer" };
        case "ArrayMemberExpression": {
            const objectType = actualSynthed(genv, synthExpression(genv, env, mode, exp.object, sourceFile));
            if (objectType.tag !== "ArrayType") {
                throw new TypingError(
                    exp,
                    `subject of indexing '[...]' is ${valueDescription(genv, objectType)}, not an array`
                );
            }
            checkExpression(genv, env, mode, exp.index, { tag: "IntType" }, sourceFile);
            exp.size = concreteType(genv, objectType.argument); // INSERTING TYPE INFORMATION HERE
            return objectType.argument;
        }
        case "StructMemberExpression": {
            let objectType = actualSynthed(genv, synthExpression(genv, env, mode, exp.object, sourceFile));
            if (exp.deref) {
                if (objectType.tag === "StructType") {
                    throw new TypingError(
                        exp,
                        `cannot dereference non-pointer struct with e->${exp.field.name}`,
                        `try e.${exp.field.name}`
                    );
                }
                if (objectType.tag !== "PointerType") {
                    throw new TypingError(
                        exp,
                        `subject of dereference '->${exp.field.name}' is ${valueDescription(
                            genv,
                            objectType
                        )}, not a pointer to a struct`
                    );
                }
                objectType = actualType(genv, objectType.argument);
                if (objectType.tag !== "StructType") {
                    throw new TypingError(
                        exp,
                        `subject of dereference '->${exp.field.name}' is a pointer to ${valueDescription(
                            genv,
                            objectType
                        )}, not a pointer to a struct`
                    );
                }
            } else {
                if (objectType.tag !== "StructType") {
                    throw new TypingError(
                        exp,
                        `subject of access '.${exp.field.name}' is ${valueDescription(
                            genv,
                            objectType
                        )}, not a struct`
                    );
                }
            }
            const structDef = getStructDefinition(genv, objectType.id.name);
            if (structDef === null) {
                throw new TypingError(
                    exp,
                    `subject of access '${exp.deref ? "->" : "."}${exp.field.name}' is 'struct ${
                        objectType.id.name
                    }', which is not defined`
                );
            }
            if (structDef.definitions === null) {
                throw new TypingError(
                    exp,
                    `subject of access '${exp.deref ? "->" : "."}${exp.field.name}' is 'struct ${
                        objectType.id.name
                    }', which is declared but not defined`
                );
            }
            exp.struct = structDef.id.name; // INSERTING TYPE INFORMATION HERE
            for (const field of structDef.definitions) {
                if (field.id.name === exp.field.name) {
                    exp.size = concreteType(genv, field.kind); // INSERTING TYPE INFORMATION HERE
                    return field.kind;
                }
            }
            throw new TypingError(
                exp,
                `field '${exp.field.name}' not declared in 'struct ${objectType.id.name}'`
            );
        }
        case "CallExpression": {
            // Variables cannot be declared with the same name as a 
            // function 
            if (env.has(exp.callee.name)) {
                throw new TypingError(
                    exp,
                    `local '${exp.callee.name}' is ${valueDescription(
                        genv,
                        actualType(genv, env.get(exp.callee.name)!)
                    )}, not a function`
                );
            }

            const fname = exp.callee.name;

            if (isPrintfLike(genv, fname)) {
                if (exp.arguments.length === 0) { throw new TypingError(exp, `${fname} requires at least 1 argument`); }
                if (exp.arguments[0].tag !== "StringLiteral") { 
                    throw new TypingError(exp.arguments[0], "argument must be a string constant");
                }
                
                function parseFormatString(format: string): string[] {
                    const n = format.length;
                    const specifiers = [];
                    for (let i = 0; i < n; i++) {
                        const c = format[i];
                        if (c == '%') {
                            // Check next character
                            if (i + 1 == n) {
                                // Unmatched '%'
                                throw new TypingError(
                                    (<ast.CallExpression>exp).arguments[0], 
                                    "'%' must be followed by a format specifier.\nTry '%%' to print a percent sign");
                            }

                            const c2 = format[i + 1];
                            if (c2 != '%') {
                                // push specifier including the %
                                specifiers.push(`%${c2}`);
                            }

                            // Skip char following 'c'
                            // Otherwise %% would raise an error
                            i++;
                        }
                    }

                    return specifiers;
                }

                const formatString = exp.arguments[0].value;
                const formatArguments = exp.arguments.slice(1);
                const formatSpecifiers = parseFormatString(formatString);

                if (formatArguments.length !== formatSpecifiers.length) {
                    throw new TypingError(
                        exp, 
                        `found ${formatSpecifiers.length} format specifiers, but got ${formatArguments.length} arguments`);
                }

                for (let i = 0; i < formatArguments.length; i++) {
                    const arg = formatArguments[i];
                    const specifier = formatSpecifiers[i];

                    switch (specifier) {
                        case '%d': checkExpression(genv, env, mode, arg, { tag: "IntType" }, sourceFile); break;
                        case '%s': checkExpression(genv, env, mode, arg, { tag: "StringType" }, sourceFile); break;
                        case '%c': checkExpression(genv, env, mode, arg, { tag: "CharType" }, sourceFile); break;
                        default: throw new TypingError(exp.arguments[0], `invalid format specifier '${specifier}'. options are %d, %s, %c, or %% for a literal % sign`);
                    }
                }

                return { tag: fname === "printf" ? "VoidType" : "StringType" };
            }
            else {
                const func = getFunctionDeclaration(genv, exp.callee.name);
                if (func === null) { 
                    // Find similarly named functions
                    const functions = <ast.FunctionDeclaration[]>genv.decls
                        .filter(decl => decl.tag === "FunctionDeclaration");
                    
                    const functionNames = functions.map(func => func.id.name);
                    const possibleAlternatives = util.bestMatches(exp.callee.name, functionNames);
                    let hint = "perhaps you meant one of: ";
                    hint += possibleAlternatives
                        .map(str => `'${str}'`)
                        .join(", ");

                    if (possibleAlternatives.length === 0) {
                        throw new TypingError(exp, `function ${exp.callee.name} not declared`); 
                    }
                    else {
                        throw new TypingError(
                            exp, 
                            `function ${exp.callee.name} not declared`,
                            hint);
                    }
                }

                // Check if the function is an interface function
                if (func.isLocalTo && func.isLocalTo !== sourceFile) {
                    const error = new TypingError(
                        exp,
                        `function ${exp.callee.name} is not part of the interface of ${uriToWorkspace(func.isLocalTo)}`
                    );

                    error.severity = DiagnosticSeverity.Warning;
                    throw error;
                }

                // Check type and number of arguments
                if (exp.arguments.length !== func.params.length) {
                    throw new TypingError(
                        exp,
                        `function ${exp.callee.name} requires ${func.params.length} argument${
                            func.params.length === 1 ? "" : "s"
                        } but was given ${exp.arguments.length}`
                    );
                }
                exp.arguments.forEach((e, i) => checkExpression(genv, env, mode, e, func.params[i].kind, sourceFile));
                return func.returns;
            }
        }
        case "IndirectCallExpression": {
            const callType = synthExpression(genv, env, mode, exp.callee, sourceFile);
            if (callType.tag === "AnonymousFunctionTypePointer") {
                throw new TypingError(
                    exp,
                    "function pointers must be stored in locals before they are called"
                );
            }
            if (callType.tag === "AmbiguousNullPointer") {
                throw new TypingError(exp, "cannot call 'NULL' as a function");
            }
            if (callType.tag === "NamedFunctionType") {
                throw new TypingError(
                    exp,
                    `Can only call pointers to functions, the function type '${
                        callType.definition.id.name
                    }' is not a pointer`
                );
            }
            const actualCallType = actualType(genv, callType);
            if (actualCallType.tag !== "PointerType") {
                throw new TypingError(exp, "only pointers to functions can be called");
            }
            const actualFunctionType = actualType(genv, actualCallType.argument);
            if (actualFunctionType.tag !== "NamedFunctionType") {
                throw new TypingError(exp, "only pointers to functions can be called");
            }
            if (exp.arguments.length !== actualFunctionType.definition.params.length) {
                throw new TypingError(
                    exp,
                    `function pointer call requires ${actualFunctionType.definition.params.length} argument${
                        actualFunctionType.definition.params.length === 1 ? "" : "s"
                    } but was given ${exp.arguments.length}`
                );
            }
            exp.arguments.forEach((e, i) =>
                checkExpression(genv, env, mode, e, actualFunctionType.definition.params[i].kind, sourceFile)
            );
            return actualFunctionType.definition.returns;
        }
        case "CastExpression": {
            const castType = actualType(genv, exp.kind);
            if (castType.tag !== "PointerType") {
                throw new TypingError(
                    exp,
                    `casts must be pointer types, not ${valueDescription(genv, castType)}`
                );
            }

            const argumentType = actualSynthed(genv, synthExpression(genv, env, mode, exp.argument, sourceFile));
            if (argumentType.tag === "AmbiguousNullPointer") {
                // NULL cast always ok
                // We don't know (or care) if it's to or from void*
                return exp.kind;
            }
            if (
                argumentType.tag === "NamedFunctionType" ||
                argumentType.tag === "AnonymousFunctionTypePointer"
            ) {
                throw new TypingError(
                    exp,
                    "only function pointers with assigned types can be cast to 'void*'",
                    "assign to a variable and then cast to 'void*'"
                );
            }
            if (argumentType.tag !== "PointerType") {
                throw new TypingError(
                    exp,
                    `casts must be pointer types, not ${valueDescription(genv, argumentType)}`
                );
            }

            if (castType.argument.tag === "VoidType") {
                exp.direction = "TO_VOID";
                exp.typename = fullTypeName(genv, argumentType); // INSERTING TYPE INFORMATION HERE
                if (argumentType.argument.tag === "VoidType") {
                    throw new TypingError(exp, "Casting a 'void*' as a 'void*' not permitted\n");
                }
            } else if (argumentType.argument.tag !== "VoidType") {
                throw new TypingError(exp, "only casts to or from 'void*' allowed");
            } else {
                exp.direction = "FROM_VOID";
                exp.typename = fullTypeName(genv, exp.kind); // INSERTING TYPE INFORMATION HERE
            }
            return exp.kind;
        }
        case "UnaryExpression": {
            switch (exp.operator) {
                case "!": {
                    checkExpression(genv, env, mode, exp.argument, { tag: "BoolType" }, sourceFile);
                    return { tag: "BoolType" };
                }
                case "&": {
                    if (exp.argument.tag !== "Identifier") {
                        throw new TypingError(
                            exp,
                            "address-of operation '&' can only be applied directly to a function name"
                        );
                    }
                    const definition = getFunctionDeclaration(genv, exp.argument.name);
                    if (definition === null) {
                        throw new TypingError(exp, `There is no function named ${exp.argument.name}`);
                    }
                    if (env.has(exp.argument.name)) {
                        throw new TypingError(
                            exp,
                            `cannot take the address of function ${
                                exp.argument.name
                            } when it is also the name of a local`
                        );
                    }
                    return { tag: "AnonymousFunctionTypePointer", definition: definition };
                }
                case "~":
                case "-": {
                    checkExpression(genv, env, mode, exp.argument, { tag: "IntType" }, sourceFile);
                    return { tag: "IntType" };
                }
                case "*": {
                    const pointerType = actualSynthed(genv, synthExpression(genv, env, mode, exp.argument, sourceFile));
                    switch (pointerType.tag) {
                        case "AmbiguousNullPointer": {
                            throw new TypingError(exp, "cannot dereference 'NULL'");
                        }
                        case "AnonymousFunctionTypePointer": {
                            throw new TypingError(
                                exp,
                                "cannot dereference a function pointer immediately",
                                "assign it to a local first"
                            );
                        }
                        case "PointerType": {
                            if (pointerType.argument.tag === "VoidType") {
                                throw new TypingError(
                                    exp,
                                    "cannot dereference value of type 'void*'",
                                    "cast to another pointer type with '(t*)'"
                                );
                            }
                            exp.size = concreteType(genv, pointerType.argument); // INSERTING TYPE INFORMATION HERE
                            return pointerType.argument;
                        }
                        default:
                            throw new TypingError(
                                exp,
                                `only pointers can be dereferenced ${valueDescription(genv, pointerType)}`
                            );
                    }
                }
                default:
                    throw new ImpossibleError("Impossible");
            }
        }
        case "BinaryExpression": {
            switch (exp.operator) {
                case "*":
                case "/":
                case "%":
                case "+":
                case "-":
                case "<<":
                case ">>":
                case "&":
                case "^":
                case "|": {
                    checkExpression(genv, env, mode, exp.left, { tag: "IntType" }, sourceFile);
                    checkExpression(genv, env, mode, exp.right, { tag: "IntType" }, sourceFile);
                    return { tag: "IntType" };
                }

                case "<":
                case "<=":
                case ">=":
                case ">": {
                    const leftType = actualSynthed(genv, synthExpression(genv, env, mode, exp.left, sourceFile));
                    switch (leftType.tag) {
                        case "IntType":
                        case "CharType": {
                            // INSERTING TYPE INFORMATION HERE
                            exp.size = leftType;
                            checkExpression(genv, env, mode, exp.right, leftType, sourceFile);
                            return { tag: "BoolType" };
                        }
                        case "StringType": {
                            throw new TypingError(
                                exp,
                                `cannot compare strings with '${exp.operator}'`,
                                "use the 'string_compare' function from the library <string>"
                            );
                        }
                        default: {
                            throw new TypingError(
                                exp,
                                `cannot compare ${valueDescription(genv, leftType)} with '${exp.operator}'`,
                                `only values of type 'int' and 'char' can be used with '${exp.operator}'`
                            );
                        }
                    }
                }

                case "==":
                case "!=": {
                    const left = synthExpression(genv, env, mode, exp.left, sourceFile);
                    const right = synthExpression(genv, env, mode, exp.right, sourceFile);
                    const lub = leastUpperBoundSmallSynthedType(genv, exp, left, right, false);
                    if (lub.tag === "StringType") {
                        throw new TypingError(
                            exp,
                            `cannot compare strings with '${exp.operator}'`,
                            "use the 'string_equal' function from the library <string>"
                        );
                    }
                    // INSERTING TYPE INFORMATION HERE
                    if (lub.tag !== "AmbiguousNullPointer" && lub.tag !== "AnonymousFunctionTypePointer") {
                        exp.size = concreteType(genv, lub);
                    }
                    return { tag: "BoolType" };
                }
                default:
                    throw new ImpossibleError("Impossible");
            }
        }
        case "LogicalExpression": {
            const left = actualSynthed(genv, synthExpression(genv, env, mode, exp.left, sourceFile));
            if (left.tag === "IntType") {
                throw new TypingError(
                    exp,
                    `cannot perform logical-${exp.operator === "&&" ? "and" : "or"} on integers`,
                    `use the bitwise-${exp.operator === "&&" ? "and" : "or"} operation ${
                        exp.operator === "&&" ? "&" : "|"
                    } instead?`
                );
            }
            if (left.tag !== "BoolType") {
                throw new TypingError(
                    exp,
                    `cannot perform logical-${exp.operator === "&&" ? "and" : "or"} on type ${typeToString(
                        left
                    )}`
                );
            }
            checkExpression(genv, env, mode, exp.right, { tag: "BoolType" }, sourceFile);
            return { tag: "BoolType" };
        }
        case "ConditionalExpression": {
            checkExpression(genv, env, mode, exp.test, { tag: "BoolType" }, sourceFile);
            const left = synthExpression(genv, env, mode, exp.consequent, sourceFile);
            const right = synthExpression(genv, env, mode, exp.alternate, sourceFile);
            const lub = leastUpperBoundSmallSynthedType(genv, exp, left, right, true);
            return lub;
        }
        case "AllocExpression": {
            const kind = actualType(genv, exp.kind);
            if (kind.tag === "NamedFunctionType") { throw new TypingError(exp, "cannot allocate functions"); }
            if (kind.tag === "StructType") {
                const decl = getStructDefinition(genv, kind.id.name);
                if (decl === null || decl.definitions === null) {
                    throw new TypingError(
                        exp,
                        `cannot allocate struct that has not been defined`,
                        `give a definition for 'struct ${kind.id.name}'`
                    );
                }
            }
            exp.size = concreteType(genv, exp.kind); // INSERTING TYPE INFORMATION HERE
            return { tag: "PointerType", argument: exp.kind };
        }
        case "AllocArrayExpression": {
            const kind = actualType(genv, exp.kind);
            if (kind.tag === "NamedFunctionType") { throw new TypingError(exp, "cannot allocate functions"); }
            if (kind.tag === "StructType") {
                const decl = getStructDefinition(genv, kind.id.name);
                if (decl === null || decl.definitions === null) {
                    throw new TypingError(
                        exp,
                        `cannot allocate struct that has not been defined`,
                        `give a definition for 'struct ${kind.id.name}'`
                    );
                }
            }
            checkExpression(genv, env, mode, exp.argument, { tag: "IntType" }, sourceFile);
            exp.size = concreteType(genv, exp.kind); // INSERTING TYPE INFORMATION HERE
            return { tag: "ArrayType", argument: exp.kind };
        }
        case "ResultExpression": {
            if (mode === null) {
                throw new TypingError(
                    exp,
                    "\\result illegal in ordinary expressions",
                    "use only in @ensures annotations"
                );
            }
            if (mode.tag !== "@ensures") {
                throw new TypingError(
                    exp,
                    `\\result illegal in @${mode.tag} annotations`,
                    "use only in @ensures annotations"
                );
            }
            if (mode.returns.tag === "VoidType") {
                throw new TypingError(exp, "\\result illegal in functions that return 'void'");
            }
            return mode.returns;
        }
        case "LengthExpression": {
            if (mode === null) {
                throw new TypingError(
                    exp,
                    "\\length illegal in ordinary expressions",
                    "use only in annotations"
                );
            }
            const tp = actualSynthed(genv, synthExpression(genv, env, mode, exp.argument, sourceFile));
            if (tp.tag !== "ArrayType") {
                throw new TypingError(
                    exp,
                    `argument to \\length is ${valueDescription(genv, tp)} not an array`
                );
            }

            return { tag: "IntType" };
        }
        case "HasTagExpression": {
            if (mode === null) {
                throw new TypingError(
                    exp,
                    "\\hastag illegal in ordinary expressions",
                    "use only in annotations"
                );
            }

            // Validate tag
            const kind = actualType(genv, exp.kind);
            if (kind.tag !== "PointerType") {
                throw new TypingError(
                    exp,
                    `type argument to \\hastag is ${valueDescription(genv, kind)}, but must be a pointer`,
                    `try '\\hastag(${typeToString(kind)}*, ...)`
                );
            }
            if (kind.argument.tag === "VoidType") { throw new TypingError(exp, "tag cannot be 'void*'"); }

            // Validate expression
            checkExpression(genv, env, mode, exp.argument, { tag: "PointerType", argument: { tag: "VoidType" } }, sourceFile);
            return { tag: "BoolType" };
        }
        default:
            throw new ImpossibleError("Impossible");
    }
}

export function checkExpression(
    genv: GlobalEnv,
    env: Env,
    mode: mode,
    exp: ast.Expression,
    tp: ast.Type,
    sourceFile: string | undefined
): void {
    const synthed = synthExpression(genv, env, mode, exp, sourceFile);
    if (!isSubtype(genv, synthed, tp)) {
        throw new TypingError(
            exp,
            `expected to find a '${typeToString(
                tp
            )}', but this expression has an incompatible type: '${typeToString(synthed)}'`
        );
    }
}

/**
 * Factoring out the mess of least-upper-bound checking for equality and comparision
 */
function leastUpperBoundSmallSynthedType(
    genv: GlobalEnv,
    exp: ast.Expression,
    t1: Synthed,
    t2: Synthed,
    cond: boolean
): ActualSynthed {
    const lubWithIdents = leastUpperBoundSynthedType(genv, t1, t2);
    const lub = lubWithIdents && actualSynthed(genv, lubWithIdents);
    const doThatThingTo = () =>
        cond ? "use the conditional expression 'e ? e1 : e2' on" : "check equality of";
    if (lub === null) {
        throw new TypingError(
            exp,
            `cannot ${doThatThingTo()} expressions with different types \n  ${
                cond ? "first branch" : "left-hand side"
            } has type ${typeToString(t1)}\n  ${
                cond ? "second branch" : "right-hand side"
            } has type ${typeToString(t2)}`
        );
    }
    if (lub.tag === "NamedFunctionType") {
        throw new TypingError(exp, `cannot ${doThatThingTo()} functions`, "use pointers to functions");
    }
    if (lub.tag === "StructType") {
        throw new TypingError(exp, `cannot ${doThatThingTo()} structs`, "use pointers to structs");
    }
    if (lub.tag === "VoidType") {
        throw new TypingError(exp, `cannot ${doThatThingTo()} expressions of type 'void'`);
    }

    return lub;
}
