import * as vscode from "vscode-languageserver";
/**
 * C1 AST
 *
 * Aims to mostly-faithfully capture the the C1 Grammar as described in
 * http://c0.typesafety.net/doc/c0-reference.pdf
 *
 * Changes/Exceptions:
 *  - Does not distinguish <sid>, <vid>, <fid>, and <aid> categories. These are used by the parser to
 *    disambiguate syntactic forms (especially the unfortunate <aid> vs. <tid> distinction needed to parse the
 *    statement `x * y;` as a binary expression or variable declaration). Within a full syntax tree
 *    they are unambiguous and can all be represented with Identifier.
 *  - The restrictions that a variable declaration not appear in the update of a ForStatement is expressed
 *    (this is a property of static semantics in the spec, see C0.23, "The step statement in a for loop may
 *    not be a declaration".).
 *  - SimpleStatement does not include variable declarations, which facilitates the above exception.
 *  - The placement restrictions on requires, ensures, loop_invariant, and assert contracts are
 *    expressed. These are properties of static semantics in the spec, see C0.23, "@requires and @ensures can
 *    only annotate functions," etc.
 *  - Use of void in declarations like "typedef void <tid>" and "void a = 12" is precluded in the syntax
 *    by the use of ValueType. These are properties of static semantics in the spec, see C0.23
 *    "Type void is used only as the return type of functions".
 *  - Arbitrary pragmas are accepted (this matches the actual behavior of the C0 compiler, which only warns on
 *    unknown pragmas)
 *
 * At the same time, this AST attempts to capture all the non-whitespace formatting that a user provided.
 *
 * Exceptions:
 *  - Does not keep annotations in groups or remember whether an annotation is single or multiline
 *    Example: cannot distinguish:
 *      {
 *        //@assert e1;
 *        //@assert e2
 *        x++;
 *      }
 *    and
 *      {
 *        //@assert e1; @assert e2
 *        x++;
 *      }
 *  - Assert contracts as statements, which means that asserts hanging off an if or else statement cannot
 *    be represented. This means that this AST cannot faithfully represent
 *      if (e)
 *        //@assert e;
 *        x++;
 *
 *    and so the parsing infrastructure must rewrite such an expression as
 *      if (e) {
 *         //@assert e;
 *         x++;
 *      }
 *
 * Loosely based on Esprima, with the notable and stubborn distinction of using "tag" instead of "type."
 * Esprima Spec: https://esprima.readthedocs.io/en/latest/syntax-tree-format.html
 * Esprima Demo: http://esprima.org/demo/parse.html
 */

export interface Syn {
    readonly tag: string;
    readonly range?: [number, number];
    loc?: SourceLocation;
}

export interface CreatesScope {
    environment?: Map<string, Type>;
}

/** Represents a 1-indexed source code position */
export interface Position {
    readonly line: number;
    readonly column: number;
}

export interface SourceLocation {
    readonly start: Position;
    readonly end: Position;
    
    /**
     * The filename this location is in
     */
    source?: string;
}

export interface Identifier extends Syn {
    readonly tag: "Identifier";
    readonly name: string;
}

export type AnyType = 
    | Type
    | { tag: "AmbiguousNullPointer" }
    | { tag: "NamedFunctionType", definition: FunctionDeclaration }
    | { tag: "AnonymousFunctionTypePointer", definition: FunctionDeclaration }
    | { tag: "FunctionType", definition: FunctionDeclaration };

export type Type =
    | IntType
    | BoolType
    | StringType
    | CharType
    | VoidType
    | PointerType
    | ArrayType
    | StructType
    | Identifier;

export type ValueType =
    | IntType
    | BoolType
    | StringType
    | CharType
    | PointerType
    | ArrayType
    | StructType
    | Identifier;

export type BinaryOperator = 
    | "*"
    | "/"
    | "%"
    | "+"
    | "-"
    | "<<"
    | ">>"
    | "<"
    | "<="
    | ">="
    | ">"
    | "=="
    | "!="
    | "&"
    | "^"
    | "|";

export type LogicalOperator = "||" | "&&";

export type UnaryOperator = "&" | "!" | "~" | "-" | "*";

export type ExpressionOperator = BinaryOperator | LogicalOperator | UnaryOperator;

export interface IntType extends Syn {
    readonly tag: "IntType";
}

export interface BoolType extends Syn {
    readonly tag: "BoolType";
}

export interface StringType extends Syn {
    readonly tag: "StringType";
}

export interface CharType extends Syn {
    readonly tag: "CharType";
}

export interface VoidType extends Syn {
    readonly tag: "VoidType";
}

export interface PointerType extends Syn {
    readonly tag: "PointerType";
    readonly argument: Type;
}

export interface ArrayType extends Syn {
    readonly tag: "ArrayType";
    readonly argument: Type;
}

export interface StructType extends Syn {
    readonly tag: "StructType";
    readonly id: Identifier;
}

export type Expression =
    | Identifier
    | IntLiteral
    | StringLiteral
    | CharLiteral
    | BoolLiteral
    | NullLiteral
    | ArrayMemberExpression
    | StructMemberExpression
    | CallExpression
    | IndirectCallExpression
    | CastExpression
    | UnaryExpression
    | BinaryExpression
    | LogicalExpression
    | ConditionalExpression
    | AllocExpression
    | AllocArrayExpression
    | ResultExpression
    | LengthExpression
    | HasTagExpression;

export interface IntLiteral extends Syn {
    readonly tag: "IntLiteral";
    readonly value: number;
    readonly raw: string;
}

export interface StringLiteral extends Syn {
    readonly tag: "StringLiteral";
    readonly value: string;
    readonly raw: string;
}

export interface CharLiteral extends Syn {
    readonly tag: "CharLiteral";
    readonly value: string;
    readonly raw: string;
}

export interface BoolLiteral extends Syn {
    readonly tag: "BoolLiteral";
    readonly value: boolean;
}

export interface NullLiteral extends Syn {
    readonly tag: "NullLiteral";
}

/**
 * Array access `e[e]`
 */
export interface ArrayMemberExpression extends Syn {
    readonly tag: "ArrayMemberExpression";
    readonly object: Expression;
    readonly index: Expression;
    size?: ConcreteType;
}

/**
 * Struct field access:
 *  - `e.f` (deref === false)
 *  - `e->f` (deref === true)
 */
export interface StructMemberExpression extends Syn {
    readonly tag: "StructMemberExpression";
    readonly deref: boolean;
    readonly object: Expression;
    readonly field: Identifier;
    struct?: string;
    size?: ConcreteType;
}

/**
 * Regular function calls `f(e1,e2,...,en)`
 */
export interface CallExpression extends Syn {
    readonly tag: "CallExpression";
    readonly callee: Identifier;
    readonly arguments: Expression[];
}

/**
 * Function pointer calls `(*e)(e1,e2,...,en)`
 */
export interface IndirectCallExpression extends Syn {
    readonly tag: "IndirectCallExpression";
    readonly callee: Expression;
    readonly arguments: Expression[];
}

/**
 * Prefix cast operation `(ty)e`.
 */
export interface CastExpression extends Syn {
    readonly tag: "CastExpression";
    readonly kind: ValueType;
    readonly argument: Expression;
    typename?: string;
    direction?: "TO_VOID" | "FROM_VOID";
}

/**
 * Prefix unary operations `~e` and friends.
 */
export interface UnaryExpression extends Syn {
    readonly tag: "UnaryExpression";
    readonly operator: UnaryOperator;
    readonly argument: Expression;
    size?: ConcreteType;
}

/**
 * Eager binary operations `e+e` and friends
 */
export interface BinaryExpression extends Syn {
    readonly tag: "BinaryExpression";
    readonly operator: BinaryOperator;
    readonly left: Expression;
    readonly right: Expression;
    size?: ConcreteType;
}

/**
 * Short-circuiting binary operations `e && e` and `e || e`.
 */
export interface LogicalExpression extends Syn {
    readonly tag: "LogicalExpression";
    readonly operator: LogicalOperator;
    readonly left: Expression;
    readonly right: Expression;
}

/**
 * `e ? e : e`
 */
export interface ConditionalExpression extends Syn {
    readonly tag: "ConditionalExpression";
    readonly test: Expression;
    readonly consequent: Expression;
    readonly alternate: Expression;
}

/**
 * `alloc(ty)`
 */
export interface AllocExpression extends Syn {
    readonly tag: "AllocExpression";
    readonly kind: ValueType;
    size?: ConcreteType;
}

/**
 * `alloc(ty)`
 */
export interface AllocArrayExpression extends Syn {
    readonly tag: "AllocArrayExpression";
    readonly kind: ValueType;
    readonly argument: Expression;
    size?: ConcreteType;
}

/**
 * `\result`
 */
export interface ResultExpression extends Syn {
    readonly tag: "ResultExpression";
}

/**
 * `\length(e)`
 */
export interface LengthExpression extends Syn {
    readonly tag: "LengthExpression";
    readonly argument: Expression;
}

/**
 * `\hastag(ty,e)`
 */
export interface HasTagExpression extends Syn {
    readonly tag: "HasTagExpression";
    readonly kind: ValueType;
    readonly argument: Expression;
    typename?: string;
}

/**
 * LValues are a refinement of Expressions
 */
export type LValue = 
    | Identifier 
    | StructMemberLValue 
    | DereferenceLValue 
    | ArrayMemberLValue 
    | CastExpressionLValue;

export interface StructMemberLValue extends StructMemberExpression {
    readonly object: LValue;
}

export interface DereferenceLValue extends UnaryExpression {
    readonly operator: "*";
    readonly argument: LValue;
}

export interface ArrayMemberLValue extends ArrayMemberExpression {
    readonly object: LValue;
}

export interface CastExpressionLValue extends CastExpression {
    readonly argument: LValue;
}

export type SimpleStatement = AssignmentStatement | UpdateStatement | ExpressionStatement;
export type Statement =
    | SimpleStatement
    | VariableDeclaration
    | IfStatement
    | WhileStatement
    | ForStatement
    | ReturnStatement
    | BlockStatement
    | AssertStatement
    | ErrorStatement
    | BreakStatement
    | ContinueStatement;

export interface AssignmentStatement extends Syn {
    readonly tag: "AssignmentStatement";
    readonly operator: "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "^=" | "|=";
    readonly left: LValue;
    readonly right: Expression;
    size?: ConcreteType;
}

export interface UpdateStatement extends Syn {
    readonly tag: "UpdateStatement";
    readonly operator: "++" | "--";
    readonly argument: LValue;
}

export interface ExpressionStatement extends Syn {
    readonly tag: "ExpressionStatement";
    readonly expression: Expression;
}

export interface VariableDeclaration extends Syn {
    readonly tag: "VariableDeclaration";
    readonly kind: ValueType;
    readonly id: Identifier;
    readonly init: Expression | null;
}

export interface IfStatement extends Syn {
    readonly tag: "IfStatement";
    readonly test: Expression;
    readonly consequent: Statement;
    readonly alternate?: Statement;
}

export interface WhileStatement extends Syn {
    readonly tag: "WhileStatement";
    readonly invariants: Expression[];
    readonly test: Expression;
    readonly body: Statement;
}

export interface ForStatement extends Syn {
    readonly tag: "ForStatement";
    readonly invariants: Expression[];
    readonly init: SimpleStatement | VariableDeclaration | null;
    readonly test: Expression;
    readonly update: SimpleStatement | null;
    readonly body: Statement;
}

export interface ReturnStatement extends Syn {
    readonly tag: "ReturnStatement";
    readonly argument: Expression | null;
}

export interface BlockStatement extends Syn, CreatesScope {
    readonly tag: "BlockStatement";
    readonly body: Statement[];
}

export interface AssertStatement extends Syn {
    readonly tag: "AssertStatement";
    readonly contract: boolean;
    readonly test: Expression;
}

export interface ErrorStatement extends Syn {
    readonly tag: "ErrorStatement";
    readonly argument: Expression;
}

export interface BreakStatement extends Syn {
    readonly tag: "BreakStatement";
}

export interface ContinueStatement extends Syn {
    readonly tag: "ContinueStatement";
}

export type Declaration =
    | StructDeclaration
    | FunctionDeclaration
    | TypeDefinition
    | FunctionTypeDefinition
    | PragmaUseLib
    | PragmaUseFile;

/**
 * Struct definitions must have 1 or more definitions.
 * `struct id {};`
 * is not allowed.
 *
 * If definitions.length === 0, this represents a struct declaration
 * `struct id;`
 */
export interface StructDeclaration extends Syn {
    readonly tag: "StructDeclaration";
    readonly id: Identifier;
    readonly definitions: null | VariableDeclarationOnly[];
    readonly doc: string;
}

export interface VariableDeclarationOnly extends Syn {
    readonly tag: "VariableDeclaration";
    readonly kind: ValueType;
    readonly id: Identifier;
}

export interface FunctionDeclaration extends Syn {
    readonly tag: "FunctionDeclaration";
    readonly returns: Type;
    readonly id: Identifier;
    readonly params: VariableDeclarationOnly[];
    readonly preconditions: Expression[];
    readonly postconditions: Expression[];
    readonly body: null | BlockStatement;
    readonly doc: string;

    /**
     * If this string is set, then this function
     * is intended to only be used in the file
     * with the given name.
     * 
     * This is used for library functions defined
     * in .o0 files, so we can produce a warning
     * for interface violations
     */
    isLocalTo?: string;
}

export interface TypeDefinition extends Syn {
    readonly tag: "TypeDefinition";
    readonly definition: VariableDeclarationOnly;
    readonly doc: string;
}

export interface FunctionTypeDefinition extends Syn {
    readonly tag: "FunctionTypeDefinition";
    readonly definition: FunctionDeclaration;
}

export interface PragmaUseLib extends Syn {
    readonly tag: "PragmaUseLib";
    readonly name: string;
}

export interface PragmaUseFile extends Syn {
    readonly tag: "PragmaUseFile";
    readonly path: string;
}

/**
 * Inelegantly, some information synthesized by the typechecker needs
 * to be present in order to faithfully compile. This information is inserted during
 * typechecking.
 */
export type ConcreteType =
    | CharType
    | BoolType
    | IntType
    | StringType
    | StructType
    | { tag: "ArrayType" }
    | { tag: "PointerType" }
    | { tag: "TaggedPointerType" };

export type AstNode =
    | AnyType
    | Expression
    | Statement
    | Declaration
    | VariableDeclarationOnly;

/**
 * Converts a 0-indexed position to a 1-indexed position
 * @param pos 
 */
export function fromVscodePosition(pos: vscode.Position): Position {
    return {
        column: pos.character + 1,
        line: pos.line + 1
    };
}

/**
 * Converts a 1-indexed position to a 0-indexed position
 * @param pos 
 */
export function toVscodePosition(pos: Position): vscode.Position {
    return {
        character: pos.column - 1,
        line: pos.line - 1
    };
}

export abstract class Visitor<T> {
    public visit(it: AstNode | null | undefined, state: T): void {
        switch (it?.tag) {
            case "Identifier": this.visitIdentifier(it, state); break;
            case "AmbiguousNullPointer": this.visitAmbiguousNullPointer(it, state); break;
            case "NamedFunctionType": this.visitNamedFunctionType(it, state); break;
            case "AnonymousFunctionTypePointer": this.visitAnonymousFunctionTypePointer(it, state); break;
            case "FunctionType": this.visitFunctionType(it, state); break;
            case "IntType": this.visitIntType(it, state); break;
            case "BoolType": this.visitBoolType(it, state); break;
            case "StringType": this.visitStringType(it, state); break;
            case "CharType": this.visitCharType(it, state); break;
            case "VoidType": this.visitVoidType(it, state); break;
            case "PointerType": this.visitPointerType(it, state); break;
            case "ArrayType": this.visitArrayType(it, state); break;
            case "StructType": this.visitStructType(it, state); break;
            case "IntLiteral": this.visitIntLiteral(it, state); break;
            case "StringLiteral": this.visitStringLiteral(it, state); break;
            case "CharLiteral": this.visitCharLiteral(it, state); break;
            case "BoolLiteral": this.visitBoolLiteral(it, state); break;
            case "NullLiteral": this.visitNullLiteral(it, state); break;
            case "ArrayMemberExpression": this.visitArrayMemberExpression(it, state); break;
            case "StructMemberExpression": this.visitStructMemberExpression(it, state); break;
            case "CallExpression": this.visitCallExpression(it, state); break;
            case "IndirectCallExpression": this.visitIndirectCallExpression(it, state); break;
            case "CastExpression": this.visitCastExpression(it, state); break;
            case "UnaryExpression": this.visitUnaryExpression(it, state); break;
            case "BinaryExpression": this.visitBinaryExpression(it, state); break;
            case "LogicalExpression": this.visitLogicalExpression(it, state); break;
            case "ConditionalExpression": this.visitConditionalExpression(it, state); break;
            case "AllocExpression": this.visitAllocExpression(it, state); break;
            case "AllocArrayExpression": this.visitAllocArrayExpression(it, state); break;
            case "ResultExpression": this.visitResultExpression(it, state); break;
            case "LengthExpression": this.visitLengthExpression(it, state); break;
            case "HasTagExpression": this.visitHasTagExpression(it, state); break;
            case "AssignmentStatement": this.visitAssignmentStatement(it, state); break;
            case "UpdateStatement": this.visitUpdateStatement(it, state); break;
            case "ExpressionStatement": this.visitExpressionStatement(it, state); break;
            case "VariableDeclaration": this.visitVariableDeclaration(it, state); break;
            case "IfStatement": this.visitIfStatement(it, state); break;
            case "WhileStatement": this.visitWhileStatement(it, state); break;
            case "ForStatement": this.visitForStatement(it, state); break;
            case "ReturnStatement": this.visitReturnStatement(it, state); break;
            case "BlockStatement": this.visitBlockStatement(it, state); break;
            case "AssertStatement": this.visitAssertStatement(it, state); break;
            case "ErrorStatement": this.visitErrorStatement(it, state); break;
            case "BreakStatement": this.visitBreakStatement(it, state); break;
            case "ContinueStatement": this.visitContinueStatement(it, state); break;
            case "StructDeclaration": this.visitStructDeclaration(it, state); break;
            case "FunctionDeclaration": this.visitFunctionDeclaration(it, state); break;
            case "TypeDefinition": this.visitTypeDefinition(it, state); break;
            case "FunctionTypeDefinition": this.visitFunctionTypeDefinition(it, state); break;
            case "PragmaUseLib": this.visitPragmaUseLib(it, state); break;
            case "PragmaUseFile": this.visitPragmaUseFile(it, state); break;
            case "ArrayType": this.visitArrayType(it, state); break;
        }
    }

    protected visitIdentifier(_: Identifier, state: T): void { return; }

    protected visitAmbiguousNullPointer(_: { tag: "AmbiguousNullPointer" }, state: T): void { return; }

    protected visitNamedFunctionType(namedFunctionType: {
        tag: "NamedFunctionType", definition: FunctionDeclaration
    }, state: T): void {
        this.visit(namedFunctionType.definition, state);
    }

    protected visitAnonymousFunctionTypePointer(anonymousFunctionTypePointer: {
        tag: "AnonymousFunctionTypePointer", definition: FunctionDeclaration
    }, state: T): void {
        this.visit(anonymousFunctionTypePointer.definition, state);
    }

    protected visitFunctionType(functionType: {
        tag: "FunctionType", definition: FunctionDeclaration
    }, state: T): void {
        this.visit(functionType.definition, state);
    }

    protected visitIntType(_: IntType, state: T): void { return; }

    protected visitBoolType(_: BoolType, state: T): void { return; }

    protected visitStringType(_: StringType, state: T): void { return; }

    protected visitCharType(_: CharType, state: T): void { return; }

    protected visitVoidType(_: VoidType, state: T): void { return; }

    protected visitPointerType(p: PointerType, state: T): void {
        this.visit(p.argument, state);
    }

    protected visitArrayType(arr: ArrayType, state: T): void {
        this.visit(arr.argument, state);
    }

    protected visitStructType(struct: StructType, state: T): void {
        this.visit(struct.id, state);
    }

    protected visitIntLiteral(_: IntLiteral, state: T): void { return; }

    protected visitStringLiteral(_: StringLiteral, state: T): void { return; }

    protected visitCharLiteral(_: CharLiteral, state: T): void { return; }

    protected visitBoolLiteral(_: BoolLiteral, state: T): void { return; }

    protected visitNullLiteral(_: NullLiteral, state: T): void { return; }

    protected visitArrayMemberExpression(arrayMemberExpression: ArrayMemberExpression, state: T): void {
        this.visit(arrayMemberExpression.object, state);
        this.visit(arrayMemberExpression.index, state);
    }

    protected visitStructMemberExpression(structMemberExpression: StructMemberExpression, state: T): void {
        this.visit(structMemberExpression.object, state);
        this.visit(structMemberExpression.field, state);
    }

    protected visitCallExpression(callExpression: CallExpression, state: T): void {
        this.visit(callExpression.callee, state);
        callExpression.arguments.forEach(x => this.visit(x, state));
    }

    protected visitIndirectCallExpression(indirectCallExpression: IndirectCallExpression, state: T): void {
        this.visit(indirectCallExpression.callee, state);
        indirectCallExpression.arguments.forEach(x => this.visit(x, state));
    }

    protected visitCastExpression(castExpression: CastExpression, state: T): void {
        this.visit(castExpression.kind, state);
        this.visit(castExpression.argument, state);
    }

    protected visitUnaryExpression(unaryExpression: UnaryExpression, state: T): void {
        this.visit(unaryExpression.argument, state);
    }

    protected visitBinaryExpression(binaryExpression: BinaryExpression, state: T): void {
        this.visit(binaryExpression.left, state);
        this.visit(binaryExpression.right, state);
    }

    protected visitLogicalExpression(logicalExpression: LogicalExpression, state: T): void {
        this.visit(logicalExpression.left, state);
        this.visit(logicalExpression.right, state);
    }

    protected visitConditionalExpression(conditionalExpression: ConditionalExpression, state: T): void {
        this.visit(conditionalExpression.test, state);
        this.visit(conditionalExpression.consequent, state);
        this.visit(conditionalExpression.alternate, state);
    }

    protected visitAllocExpression(allocExpression: AllocExpression, state: T): void {
        this.visit(allocExpression.kind, state);
    }

    protected visitAllocArrayExpression(allocArrayExpression: AllocArrayExpression, state: T): void {
        this.visit(allocArrayExpression.kind, state);
        this.visit(allocArrayExpression.argument, state);
    }

    protected visitResultExpression(_: ResultExpression, state: T): void { return; }

    protected visitLengthExpression(lengthExpression: LengthExpression, state: T): void {
        this.visit(lengthExpression.argument, state);
    }

    protected visitHasTagExpression(hasTagExpression: HasTagExpression, state: T): void {
        this.visit(hasTagExpression.kind, state);
        this.visit(hasTagExpression.argument, state);
    }

    protected visitAssignmentStatement(assignmentStatement: AssignmentStatement, state: T): void {
        this.visit(assignmentStatement.left, state);
        this.visit(assignmentStatement.right, state);
    }

    protected visitUpdateStatement(updateStatement: UpdateStatement, state: T): void {
        this.visit(updateStatement.argument, state);
    }

    protected visitExpressionStatement(expressionStatement: ExpressionStatement, state: T): void {
        this.visit(expressionStatement.expression, state);
    }

    // Partial to allow for VariableDeclarationOnly
    protected visitVariableDeclaration(variableDeclaration: Partial<VariableDeclaration>, state: T): void {
        this.visit(variableDeclaration.kind, state);
        this.visit(variableDeclaration.id, state);
        this.visit(variableDeclaration.init, state);
    }

    protected visitIfStatement(ifStatement: IfStatement, state: T): void {
        this.visit(ifStatement.test, state);
        this.visit(ifStatement.consequent, state);
        this.visit(ifStatement.alternate, state);
    }

    protected visitWhileStatement(whileStatement: WhileStatement, state: T): void {
        whileStatement.invariants.forEach(x => this.visit(x, state));
        this.visit(whileStatement.test, state);
        this.visit(whileStatement.body, state);
    }

    protected visitForStatement(forStatement: ForStatement, state: T): void {
        forStatement.invariants.forEach(x => this.visit(x, state));
        this.visit(forStatement.init, state);
        this.visit(forStatement.test, state);
        this.visit(forStatement.update, state);
        this.visit(forStatement.body, state);
    }

    protected visitReturnStatement(returnStatement: ReturnStatement, state: T): void {
        this.visit(returnStatement.argument, state);
    }

    protected visitBlockStatement(blockStatement: BlockStatement, state: T): void {
        blockStatement.body.forEach(x => this.visit(x, state));
    }

    protected visitAssertStatement(assertStatement: AssertStatement, state: T): void {
        this.visit(assertStatement.test, state);
    }

    protected visitErrorStatement(errorStatement: ErrorStatement, state: T): void {
        this.visit(errorStatement.argument, state);
    }

    protected visitBreakStatement(_: BreakStatement, state: T): void { return; }

    protected visitContinueStatement(_: ContinueStatement, state: T): void { return; }

    protected visitStructDeclaration(structDeclaration: StructDeclaration, state: T): void {
        this.visit(structDeclaration.id, state);
        structDeclaration.definitions?.forEach(x => this.visit(x, state));
    }

    protected visitVariableDeclarationOnly(variableDeclarationOnly: VariableDeclarationOnly, state: T): void {
        this.visit(variableDeclarationOnly.kind, state);
        this.visit(variableDeclarationOnly.id, state);
    }

    protected visitFunctionDeclaration(functionDeclaration: FunctionDeclaration, state: T): void {
        this.visit(functionDeclaration.returns, state);
        this.visit(functionDeclaration.id, state);
        functionDeclaration.params.forEach(x => this.visit(x, state));
        functionDeclaration.preconditions.forEach(x => this.visit(x, state));
        functionDeclaration.postconditions.forEach(x => this.visit(x, state));
        this.visit(functionDeclaration.body, state);
    }

    protected visitTypeDefinition(typeDefinition: TypeDefinition, state: T): void {
        this.visit(typeDefinition.definition, state);
    }

    protected visitFunctionTypeDefinition(functionTypeDefinition: FunctionTypeDefinition, state: T): void {
        this.visit(functionTypeDefinition.definition, state);
    }

    protected visitPragmaUseLib(_: PragmaUseLib, state: T): void { return; }

    protected visitPragmaUseFile(_: PragmaUseFile, state: T): void { return; }
}
