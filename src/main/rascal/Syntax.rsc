module Syntax

extend lang::std::Layout;
extend lang::std::Id;

// ============================================================================
// Start Symbol
// ============================================================================
start syntax Module = Variables? variables (Function | Data)* items;

// ============================================================================
// Variables
// ============================================================================
syntax Variables = Identifier head ("," Identifier)* tail;

// ============================================================================
// Functions and Data Definitions
// ============================================================================
syntax Function = Assignment? assignment "function" "(" Variables params ")" "do" Body body "end" Identifier name
                | Assignment? assignment "function" "do" Body body "end" Identifier name;

syntax Data = Assignment? assignment "data" "with" Variables vars DataBody body "end" Identifier name;

syntax Assignment = Identifier name "=";

// ============================================================================
// Data Bodies
// ============================================================================
syntax DataBody = Constructor
                | Function;

syntax Constructor = Identifier name "=" "struct" "(" Variables fields ")";

// ============================================================================
// Statements and Body
// ============================================================================
syntax Body = Statement* statements;

syntax Statement = Expression expr
                 | Variables vars
                 | Range range
                 | Iterator iterator
                 | Loop loop
                 | "if" Expression condition "then" Body thenBody "else" Body elseBody "end"
                 | "cond" Expression condition "do" PatternBody patterns "end"
                 | Invocation invocation;

syntax Range = Assignment? assignment "from" Principal start "to" Principal end;

syntax Iterator = Assignment assignment "iterator" "(" Variables params ")" "yielding" "(" Variables yields ")";

syntax Loop = "for" Identifier var Range range "do" Body body;

syntax PatternBody = Expression pattern "-\>" Expression result;

// ============================================================================
// Expressions
// ============================================================================
syntax Expression = Principal principal
                  | Invocation invocation
                  | bracket "(" Expression ")"
                  | bracket "[" Expression "]"
                  > right "-" Expression
                  > right Expression "**" Expression
                  > left ( Expression "*" Expression
                         | Expression "/" Expression
                         | Expression "%" Expression )
                  > left ( Expression "+" Expression
                         | Expression "-" Expression )
                  > non-assoc ( Expression "\<" Expression
                              | Expression "\>" Expression
                              | Expression "\<=" Expression
                              | Expression "\>=" Expression
                              | Expression "\<\>" Expression
                              | Expression "=" Expression )
                  > left Expression "and" Expression
                  > left Expression "or" Expression
                  > right Expression "-\>" Expression
                  > right Expression ":" Expression;

// ============================================================================
// Invocations
// ============================================================================
syntax Invocation = Identifier name "$" "(" Variables args ")"
                  | Identifier obj "." Identifier method "(" Variables args ")";

// ============================================================================
// Principals (Literals and Identifiers)
// ============================================================================
syntax Principal = "true"
                 | "false"
                 | CharLiteral char
                 | IntLiteral int
                 | FloatLiteral float
                 | Identifier id;

// ============================================================================
// Lexical Tokens
// ============================================================================
lexical Identifier = [a-z]+ !>> [a-z] \ Reserved;

lexical CharLiteral = [a-z];

lexical IntLiteral = [0-9]+ !>> [0-9];

lexical FloatLiteral = IntLiteral "." IntLiteral;

// ============================================================================
// Reserved Keywords
// ============================================================================
keyword Reserved = "cond" | "do" | "data" | "end" | "for" | "from" | "then" 
                 | "function" | "else" | "if" | "in" | "iterator" | "sequence" 
                 | "struct" | "to" | "tuple" | "type" | "with" | "yielding"
                 | "and" | "or" | "neg" | "true" | "false";