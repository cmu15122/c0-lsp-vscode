/**
 * This file contains the code for typechecking top-level declarations.
 */

import * as ast from "../ast";
import {
    GlobalEnv,
    getTypeDef,
    getFunctionDeclaration,
    addDecl,
    isLibraryFunction,
    isLibraryStruct,
    getStructDefinition,
    actualType,
    isPrintfLike,
} from "./globalenv";
import { Env, equalFunctionTypes, checkTypeInDeclaration, checkFunctionReturnType, EnvEntry } from "./types";
import { checkExpression } from "./expressions";
import { checkStatement } from "./statements";
import { expressionFreeVars, checkStatementFlow, checkExpressionUsesGetFreeFunctions } from "./flow";
import { TypingError, ImpossibleError } from "../error";

/**
 * Simple function to transform 
 */
function getDefinedFromParams(params: ast.VariableDeclarationOnly[]): Set<string> {
    return new Set<string>(params.map(p => p.id.name));
}

export function getEnvironmentFromParams(genv: GlobalEnv, params: ast.VariableDeclarationOnly[]): Env {
    const env = new Map<string, EnvEntry>();
    for (const param of params) {
        checkTypeInDeclaration(genv, param.kind, true);
        if (env.has(param.id.name)) {
            // TODO: Previous location
            // We can get the previous location from the environment now
            throw new TypingError(param.id, `Parameter ${param.id.name} declared a second time`);
        } else {
            env.set(param.id.name, { ...param.kind, position: param.id.loc });  
        }
    }
    return env;
}

/**
 * Typechecks the given gdecl and returns 
 * a set of all function names used
 * 
 * @param genv Global context so far
 * @param decl Declaration to check
 * @param errors Set to add typing errors to as they are encountered
 */
// tslint:disable-next-line: max-line-length
function checkDeclaration(genv: GlobalEnv, decl: ast.Declaration, errors: Set<TypingError>): Set<ast.Identifier> {
    const sourceFile = decl.loc?.source;

    switch (decl.tag) {
        // Libs and other files are loaded during parsing,
        // not typechecking, because otherwise the lexer
        // gets triggered 
        case "PragmaUseLib": 
        case "PragmaUseFile": 
            return new Set();
        
        case "StructDeclaration": {
            if (decl.definitions === null) { return new Set(); }
            if (isLibraryStruct(genv, decl.id.name)) {
                // TODO: Previous location
                errors.add(new TypingError(
                    decl,
                    `struct ${decl.id.name} is declared in a library and cannot be defined here`
                ));
            }
            const previousStruct = getStructDefinition(genv, decl.id.name);
            if (previousStruct !== null && previousStruct.definitions !== null) {
                // TODO: Previous location
                errors.add(new TypingError(
                    decl,
                    `struct ${decl.id.name} is defined twice`,
                    "structs can only be defined once"
                ));
            }

            const fields = new Set<string>();
            for (const definition of decl.definitions) {
                if (fields.has(definition.id.name)) {
                    // TODO: Previous location
                    errors.add(new TypingError(
                        decl,
                        `field '${definition.id.name}' used more than once in definition of struct '${
                            decl.id.name
                        }'`
                    ));
                }
                const kind = actualType(genv, definition.kind);
                if (kind.tag === "NamedFunctionType") {
                    errors.add(new TypingError(
                        definition,
                        "cannot put a function directly in a struct",
                        "use a function pointer"
                    ));
                }
                if (kind.tag === "StructType") {
                    const structdef = getStructDefinition(genv, kind.id.name);
                    if (structdef === null || structdef.definitions === null) {
                        errors.add(new TypingError(
                            definition,
                            "struct fields must be defined",
                            `define 'struct ${kind.id.name}' or make the field a pointer to a 'struct ${
                                kind.id.name
                            }'`
                        ));
                    }
                }
                fields.add(definition.id.name);
            }
            return new Set();
        }
        case "TypeDefinition": {
            const previousTypeDef = getTypeDef(genv, decl.definition.id.name);
            const previousFunction = getFunctionDeclaration(genv, decl.definition.id.name);
            if (previousTypeDef !== null) {
                // TODO: Previous location
                errors.add(new TypingError(
                    decl,
                    `type name '${decl.definition.id.name}' already defined as a type`
                ));
            }
            if (previousFunction !== null) {
                // TODO: Previous location
                errors.add(new TypingError(
                    decl,
                    `type name '${decl.definition.id.name}' already used as a function name`
                ));
            }
            return new Set();
        }
        case "FunctionTypeDefinition": {
            // Check for previous typedefs
            const previousTypeDef = getTypeDef(genv, decl.definition.id.name);
            const previousFunction = getFunctionDeclaration(genv, decl.definition.id.name);
            if (previousTypeDef !== null) {
                // TODO: Previous location
                errors.add(new TypingError(
                    decl,
                    `function type name '${decl.definition.id.name}' already defined as a type`
                ));
            }
            if (previousFunction !== null) {
                // TODO: Previous location
                errors.add(new TypingError(
                    decl,
                    `function type name '${decl.definition.id.name}' already used as a function name`
                ));
            }

            // Check declaration
            try {
                checkFunctionReturnType(genv, decl.definition.returns);
            } 
            catch (err) {
                errors.add(err as TypingError);
            }
            try {
                const env = getEnvironmentFromParams(genv, decl.definition.params);
                const defined = getDefinedFromParams(decl.definition.params);
                const functionsUsed = new Set<ast.Identifier>();
                for (const anno of decl.definition.preconditions) {
                    checkExpression(genv, env, { tag: "@requires" }, anno, { tag: "BoolType" }, sourceFile);
                    checkExpressionUsesGetFreeFunctions(defined, defined, anno).forEach(x =>
                        functionsUsed.add(x)
                    );
                }
                for (const anno of decl.definition.postconditions) {
                    checkExpression(genv, env, { tag: "@ensures", returns: decl.definition.returns }, anno, {
                        tag: "BoolType"
                    }, sourceFile);
                    checkExpressionUsesGetFreeFunctions(defined, defined, anno).forEach(x =>
                        functionsUsed.add(x)
                    );
                }
                return functionsUsed;
            } catch (err) {
                errors.add(err as TypingError);
                return new Set();
            }
        }
        case "FunctionDeclaration": {
            // No need to check for previous typedefs (this would cause a parse error)
            try {
                checkFunctionReturnType(genv, decl.returns);
            } 
            catch (err) {
                errors.add(err as TypingError);
            }

            const functionsUsed = new Set<ast.Identifier>();
            try {
                const env = getEnvironmentFromParams(genv, decl.params);
                const defined = getDefinedFromParams(decl.params);
                for (const anno of decl.preconditions) {
                    try {
                        checkExpression(genv, env, { tag: "@requires" }, anno, { tag: "BoolType" }, sourceFile);
                        checkExpressionUsesGetFreeFunctions(defined, defined, anno).forEach(x =>
                            functionsUsed.add(x)
                        );
                    } catch (err) {
                        errors.add(err as TypingError);
                    }
                }
                for (const anno of decl.postconditions) {
                    try {
                        checkExpression(genv, env, { tag: "@ensures", returns: decl.returns }, anno, {
                            tag: "BoolType"
                        }, sourceFile);
                        checkExpressionUsesGetFreeFunctions(defined, defined, anno).forEach(x =>
                            functionsUsed.add(x)
                        );
                    } catch (err) {
                        errors.add(err as TypingError);
                    }
                }

                // Check previous functions match
                try {
                    const previousFunction = getFunctionDeclaration(genv, decl.id.name);
                    if (previousFunction !== null && previousFunction !== decl) {
                        if (previousFunction.body !== null && decl.body !== null) {
                            // TODO: Previous location
                            errors.add(new TypingError(decl.id, `function ${decl.id.name} defined more than once`));
                        }
                        if (!equalFunctionTypes(genv, previousFunction, decl)) {
                            const oldone = previousFunction.body === null ? "declaration" : "definition";
                            const newone = decl.body === null ? "declaration" : "definition";
                            // TODO: Previous location
                            errors.add(new TypingError(
                                decl.id,
                                `function ${newone} for '${decl.id.name}' does not match previous function ${oldone}`
                            ));
                        }
                    }
                } catch (err) {
                    errors.add(err as TypingError);
                }
                
                // Check body, if necessary
                if (decl.body === null) { return functionsUsed; }

                if (isLibraryFunction(genv, decl.id.name)) {
                    // TODO: Previous location
                    errors.add(new TypingError(
                        decl.id,
                        `function ${decl.id.name} is declared in a library header and cannot be defined`
                    ));
                }

                // TODO: It's a hack that we _permanently_ add the recursive type declaration
                // even if it's a harmless hack.
                // As of Dec 29, 2019 it should get deleted after typechecking the current
                // function is done
                addDecl(false, genv, {
                    tag: "FunctionDeclaration",
                    id: decl.id,
                    returns: decl.returns,
                    params: decl.params,
                    preconditions: [],
                    postconditions: [],
                    // Don't give a location
                    // or it will confuse ast search code
                    // (By adding genv.decls.pop() this shouldn't matter anymore)
                    loc: undefined, 
                    body: null,
                    doc: ""
                });

                try {
                    checkStatement(genv, env, decl.body, decl.returns, false, errors, sourceFile);
                    const constants: Set<string> = new Set();
                    decl.postconditions.forEach(anno => {
                        expressionFreeVars(anno).forEach(x => {
                            if (defined.has(x.name)) { constants.add(x.name); }
                        });
                    });

                    const functionAnalysis = checkStatementFlow(defined, constants, defined, decl.body);
                    if (decl.returns.tag !== "VoidType" && !functionAnalysis.returns) {
                        errors.add(new TypingError(
                            decl.id,
                            `function ${decl.id.name} has non-void return type but does not return along every path`
                        ));
                    }
                    functionAnalysis.functions.forEach(f => functionsUsed.add(f));
                } catch (err) {
                    errors.add(err as TypingError);
                }

                // Delete the temp entry 
                genv.decls.pop();
            } catch (err) {
                errors.add(err as TypingError);
            }

            return functionsUsed;
        }
        /* instanbul ignore next */
        default: {
            throw new ImpossibleError("Impossible");
        }
    }
}

export interface TypecheckResult {
    errors: Set<TypingError>;
    genv: GlobalEnv;
}

export function checkProgram(genv: GlobalEnv, decls: ast.Declaration[]): TypecheckResult {
    const functionsUsed = new Set<ast.Identifier>();
    const errors = new Set<TypingError>();

    for (const decl of decls) {
        const declErrors = new Set<TypingError>();
        checkDeclaration(genv, decl, declErrors).forEach(f => functionsUsed.add(f));

        // Indicate where each error came from 
        for (const error of declErrors) {
            if (decl.loc && error.loc)
                error.loc.source = decl.loc.source;

            errors.add(error);
        }

        addDecl(false, genv, decl);
    }

    for (const f of functionsUsed) {
        const def = getFunctionDeclaration(genv, f.name);
        if (def === null) {
            // We should always be able to look up a declaration of a function.
            // Otherwise, we would have already produced a diagnostic for it earlier
            // The only exception is printf and format, which are magically declared
            // by the compiler when the appropriate library is #use'd
            if (!isPrintfLike(genv, f.name)) { 
                console.error(`No declaration for ${f.name}`); 
            }
        }
        else if (def.body === null && !isLibraryFunction(genv, def.id.name)) {
            const msg = `function ${f.name} was declared but never defined`;

            errors.add(new TypingError(f, msg));

            // The version of the LSP library we are using does not support
            // returning diagnostics for multiple files. This is an issue if
            // for example function F is declared in file A and used in file B,
            // but never defined. Then an error should show up in file A for the declaration
            // and in file B for the usage, but that's not possible. So we don't
            // want to generate a diagnostic for the declaration if it's in a different file
            if (f.loc?.source === def.loc?.source) {
                errors.add(new TypingError(def.id, msg));
            }
        }
    }

    return { errors, genv };
}
