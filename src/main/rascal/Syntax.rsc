module Syntax

extend lang::std::Layout;
extend lang::std::Id;

// ============================================================================
// Start Symbol
// ============================================================================
start syntax Module = Variables? variables (Function | Data)* items;

// ============================================================================
// Types - NEW: Type annotations for the type system
// ============================================================================
syntax Type = intType: "int"
            | boolType: "bool"
            | charType: "char"
            | stringType: "string"
            | floatType: "float"
            | structType: Identifier name
            | arrayType: Type element "[" "]"
            | tupleType: "tuple" "\<" {Type ","}+ elements "\>"
            | functionType: "function" "\<" Type returnType "," {Type ","}* params "\>"
            | customType: Identifier name
            | inferredType: "_";  // For type inference

// ============================================================================
// Variables with Type Annotations - UPDATED
// ============================================================================
syntax Variables = Identifier head ("," Identifier)* tail;

syntax TypedVariable = Type type Identifier name;

syntax TypedVariables = TypedVariable head ("," TypedVariable)* tail;

// ============================================================================
// Functions and Data Definitions - UPDATED with type annotations
// ============================================================================
syntax Function = Assignment? assignment "function" Type? returnType "(" TypedVariables? params ")" "do" Body body "end" Identifier name
                | Assignment? assignment "function" Type? returnType "do" Body body "end" Identifier name;

syntax Data = Assignment? assignment "data" Type? dataType "with" TypedVariables? vars DataBody body "end" Identifier name;

syntax Assignment = Identifier name "=";

// ============================================================================
// Data Bodies - UPDATED with typed constructors
// ============================================================================
syntax DataBody = Constructor
                | Function;

syntax Constructor = Identifier name "=" "struct" "(" TypedVariables? fields ")";

// ============================================================================
// Statements and Body - UPDATED with type declarations
// ============================================================================
syntax Body = Statement* statements;

syntax Statement = Expression expr
                 | Variables vars
                 | TypeDeclaration decl
                 | TypedDeclaration typedDecl
                 | Range range
                 | Iterator iterator
                 | Loop loop
                 | "if" Expression condition "then" Body thenBody "else" Body elseBody "end"
                 | "cond" Expression condition "do" PatternBody patterns "end"
                 | Invocation invocation
                 | TypeAssertion assertion;

// NEW: Type declarations in statements
syntax TypeDeclaration = "type" Identifier name "=" Type type;

syntax TypedDeclaration = Type type Identifier name "=" Expression init;

syntax TypeAssertion = "assert" Expression expr ":" Type type;

syntax Range = Assignment? assignment "from" Principal start "to" Principal end;

syntax Iterator = Assignment assignment "iterator" Type? returnType "(" TypedVariables? params ")" "yielding" "(" TypedVariables? yields ")";

syntax Loop = "for" Type? varType Identifier var Range range "do" Body body;

syntax PatternBody = Pattern pattern "-\>" Expression result;

// NEW: Patterns with type annotations
syntax Pattern = Expression expr
               | Type type ":" Expression expr;

// ============================================================================
// Expressions - UPDATED with type casting and annotations
// ============================================================================
syntax Expression = Principal principal
                  | Invocation invocation
                  | TypeCast cast
                  | TypedExpression typed
                  | arrayLiteral: "[" {Expression ","}* elements "]"
                  | tupleLiteral: "(" {Expression ","}+ elements ")"
                  | structLiteral: Identifier structName "{" {FieldInit ","}* fields "}"
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

// NEW: Type casting and typed expressions
syntax TypeCast = "cast" "\<" Type type "\>" "(" Expression expr ")";

syntax TypedExpression = Expression expr "::" Type type;

// ============================================================================
// Invocations - UPDATED with type parameters
// ============================================================================
syntax Invocation = Identifier name "$" "(" Variables? args ")"
                  | Identifier name "\<" {Type ","}+ typeArgs "\>" "$" "(" Variables? args ")"
                  | Identifier obj "." Identifier method "(" Variables? args ")"
                  | Identifier obj "." Identifier method "\<" {Type ","}+ typeArgs "\>" "(" Variables? args ")";

// ============================================================================
// Principals (Literals and Identifiers) - Core literals only
// ============================================================================
syntax Principal = boolLiteral: "true"
                 | boolLiteral: "false"
                 | charLiteral: CharLiteral char
                 | intLiteral: IntLiteral int
                 | floatLiteral: FloatLiteral float
                 | stringLiteral: StringLiteral str
                 | Identifier id;

// NEW: Field initialization for structs
syntax FieldInit = Identifier field "=" Expression val;

// ============================================================================
// Lexical Tokens - UPDATED with string literals
// ============================================================================
lexical Identifier = [a-z][a-z0-9]* !>> [a-z0-9] \ Reserved
                   | [A-Z][a-zA-Z0-9]* !>> [a-zA-Z0-9];  // Allow uppercase for types

lexical CharLiteral = "\'" ![\'\\] "\'";

lexical IntLiteral = [0-9]+ !>> [0-9];

lexical FloatLiteral = IntLiteral "." IntLiteral;

lexical StringLiteral = "\"" (![\"\\] | "\\" )* "\"";

// ============================================================================
// Reserved Keywords - UPDATED with type-related keywords
// ============================================================================
keyword Reserved = "cond" | "do" | "data" | "end" | "for" | "from" | "then" 
                 | "function" | "else" | "if" | "in" | "iterator" | "sequence" 
                 | "struct" | "to" | "tuple" | "type" | "with" | "yielding"
                 | "and" | "or" | "neg" | "true" | "false"
                 | "int" | "bool" | "char" | "string" | "float"
                 | "cast" | "assert";