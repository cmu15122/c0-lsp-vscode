/**
 * This is rather annoying code to compute code completions.
 * It isn't perfect and is somewhat "heuristic-based" in that it
 * tries to guess where expression boundaries are
 */
import * as nearley from "nearley";
import * as exp from "./expression-rules";
import * as parsed from "./parse/parsedsyntax";

export const enum CompletionContextKind {
  StructAccess,
  FunctionCall,
  ContractDecl
}

export type CompletionResult =
  | StructAccess
  | FunctionCall
  | ContractDecl
  | null;

export interface StructAccess {
  tag: CompletionContextKind.StructAccess;
  expr: parsed.Expression;
  derefenced: boolean;
}

export interface FunctionCall {
  tag: CompletionContextKind.FunctionCall;
  name: string;
  argumentNumber: number;
}

export interface ContractDecl {
  tag: CompletionContextKind.ContractDecl;
}

function scanExpression(source: string, index: number) {
  let parenStack = 0;
  let braceStack = 0;

  let pos;
  for (pos = index; pos >= 0; pos--) {
    const c = source[pos];

    // Special case
    if (source[pos - 1] === "-" && c === ">") {
      pos--; continue;
    }

    if (c === ")") parenStack++;
    if (c === "(") {
      if (parenStack === 0) break; else parenStack--;
    }
    if (parenStack === 0) {
      if (c === "(") break;
      if (c === ";") break;
      if (c === ",") break;
      if (c === "{") break;
      // break on any operator 
      if ("!~-*+/%><&^|?:=".includes(c)) {
        break;
      }
    }
    else {
      if (c === "(") parenStack--;
    }

    // Technically it's valid to break a field access over a newline
    // but whatever
    if (c === "\n") break; 
    if (c === "]") braceStack++;
    if (c === "[") {
      if (braceStack === 0) break; else braceStack--;
    }

    if (source.startsWith("return", pos - 6)) break;
  }
  return pos < 0 ? "" : source.slice(pos + 1, index + 1).trim();
}

function scanFunctionName(source: string, index: number) {
  let pos;
  for (pos = index; pos >= 0; pos--) {
    if (!/[A-Za-z0-9_]/.test(source[pos])) break;
  }

  // Add one, `index` is the last character of the function name 
  return pos < 0 ? "" : source.slice(pos + 1, index + 1).trim();
}

export function getCompletionContext(source: string, index: number): CompletionResult {
  // We will try to grab the expression that the user is typing
  // by scanning backwards from the cursor position. Then we can
  // figure out what kind of expression it is, and return completions based on that. 

  const parser = new nearley.Parser(nearley.Grammar.fromCompiled(exp.default));
  // Ignore errors 
  (<any>parser).reportError = () => "";
  
  // Meaningful character to use are either:
  // - left paren (function call or casted expression)
  // - comma (function argument, we can skip to the left paren and then use that)
  // - struct dereference (-> or .)

  let pos = index;
  // Skip whitespace
  while (source[pos] === " ") pos--;

  if (source.startsWith("//@", pos - 3) || 
      source.startsWith("/*@", pos - 3)) 
  {
    return { tag: CompletionContextKind.ContractDecl };
  }

  // Struct access must be at the cursor (barring whitespace)
  if (source.startsWith("->", pos - 2) || source[pos - 1] === ".") {
    const derefenced = source[pos - 1] !== ".";

    // Scan backwards for as much expression as we can get, either
    // to a left paren (function call), left bracket (array index),
    // comma (function argument), or equals sign (assignment)
    const expressionText = scanExpression(source, pos - (derefenced ? 3 : 2));

    try {
      parser.feed(expressionText);
      const results = parser.finish();
      if (!(results && results.length === 1)) return null;

      return {
        tag: CompletionContextKind.StructAccess,
        expr: results[0],
        derefenced: derefenced
      };
    }
    catch (e) { /* pass */ }
  }

  // If we are in a function call we need to know what argument we are in
  let parenStack = 0;
  let argumentNumber = 0;

  // Iterate to the start of the function call 
  for (pos = index; pos >= 0; pos--) {
    if (source[pos] === ";") return null;
    if (source[pos] === "," && parenStack === 0) argumentNumber++;
    if (source[pos] === ")") parenStack++;
    if (source[pos] === "(") {
      if (parenStack === 0) {
        // Possible function call 
        const functionName = scanFunctionName(source, pos - 1);
        return functionName === "" ? null : { 
          tag: CompletionContextKind.FunctionCall,
          name: functionName,
          argumentNumber: argumentNumber
        };
      }
      else parenStack--;
    }
  }

  return null;
}
