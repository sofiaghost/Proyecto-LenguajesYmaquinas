module Parser

import Syntax;
import AST;
import ParseTree;
import String;

// ============================================================================
// Main Parsing Functions
// ============================================================================

// Parse a string into an AST
AModule parseALU(str input) = cst2ast(parse(#start[Module], input));

// Parse a file into an AST
AModule parseALUFile(loc file) = cst2ast(parse(#start[Module], file));

// ============================================================================
// CST to AST Conversion
// ============================================================================

// Convert Module
AModule cst2ast(start[Module] tree) = cst2ast(tree.top);

AModule cst2ast((Module)`<Function* funcs> <Data* datas>`) {
    list[AItem] items = [afunction(cst2ast(f)) | f <- funcs] + 
                        [adata(cst2ast(d)) | d <- datas];
    return amodule([], items);
}

AModule cst2ast((Module)`<Variables vars> <Function* funcs> <Data* datas>`) {
    list[str] moduleVars = getVariables(vars);
    list[AItem] items = [afunction(cst2ast(f)) | f <- funcs] + 
                        [adata(cst2ast(d)) | d <- datas];
    return amodule(moduleVars, items);
}

// ============================================================================
// Type Conversion - NEW
// ============================================================================

AType cst2ast((Type)`int`) = aintType();
AType cst2ast((Type)`bool`) = aboolType();
AType cst2ast((Type)`char`) = acharType();
AType cst2ast((Type)`string`) = astringType();
AType cst2ast((Type)`float`) = afloatType();
AType cst2ast((Type)`<Identifier name>`) = astructType("<name>");
AType cst2ast((Type)`<Type element> [ ]`) = aarrayType(cst2ast(element));
AType cst2ast((Type)`tuple \< <{Type ","}+ elements> \>`) = atupleType([cst2ast(t) | t <- elements]);
AType cst2ast((Type)`function \< <Type returnType> , <{Type ","}* params> \>`) = 
    afunctionType(cst2ast(returnType), [cst2ast(p) | p <- params]);
AType cst2ast((Type)`_`) = ainferredType();

// Convert Variables (untyped)
list[str] getVariables((Variables)`<Identifier head> <{Identifier ","}* tail>`) {
    return ["<head>"] + ["<id>" | id <- tail];
}

// Convert TypedVariables - NEW
list[ATypedVariable] getTypedVariables((TypedVariables)`<TypedVariable head> <{TypedVariable ","}* tail>`) {
    return [cst2ast(head)] + [cst2ast(tv) | tv <- tail];
}

ATypedVariable cst2ast((TypedVariable)`<Type t> <Identifier name>`) = atypedVar(cst2ast(t), "<name>");

// ============================================================================
// Functions - UPDATED with type support
// ============================================================================

// Typed function with parameters
AFunction cst2ast((Function)`function <Type returnType> ( <TypedVariables params> ) do <Body body> end <Identifier name>`) {
    list[ATypedVariable] parameters = getTypedVariables(params);
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    return function(funcName, parameters, cst2ast(returnType), statements);
}

AFunction cst2ast((Function)`<Assignment assign> function <Type returnType> ( <TypedVariables params> ) do <Body body> end <Identifier name>`) {
    list[ATypedVariable] parameters = getTypedVariables(params);
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return function(funcName, parameters, cst2ast(returnType), statements, assignedTo);
}

// Typed function without parameters
AFunction cst2ast((Function)`function <Type returnType> do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    return function(funcName, [], cst2ast(returnType), statements);
}

AFunction cst2ast((Function)`<Assignment assign> function <Type returnType> do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return function(funcName, [], cst2ast(returnType), statements, assignedTo);
}

// Legacy untyped functions (backward compatibility)
AFunction cst2ast((Function)`function ( <Variables params> ) do <Body body> end <Identifier name>`) {
    list[str] parameters = getVariables(params);
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    return functionUntyped(funcName, parameters, statements);
}

AFunction cst2ast((Function)`<Assignment assign> function ( <Variables params> ) do <Body body> end <Identifier name>`) {
    list[str] parameters = getVariables(params);
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return functionUntyped(funcName, parameters, statements, assignedTo);
}

AFunction cst2ast((Function)`function do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    return functionUntyped(funcName, [], statements);
}

AFunction cst2ast((Function)`<Assignment assign> function do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return functionUntyped(funcName, [], statements, assignedTo);
}

// ============================================================================
// Data Definitions - UPDATED with type support
// ============================================================================

// Typed data definition
AData cst2ast((Data)`data <Type dataType> with <TypedVariables vars> <DataBody body> end <Identifier name>`) {
    list[ATypedVariable] variables = getTypedVariables(vars);
    list[ADataBody] bodyItems = [cst2ast(body)];
    str dataName = "<name>";
    return dataDef(dataName, cst2ast(dataType), variables, bodyItems);
}

AData cst2ast((Data)`<Assignment assign> data <Type dataType> with <TypedVariables vars> <DataBody body> end <Identifier name>`) {
    list[ATypedVariable] variables = getTypedVariables(vars);
    list[ADataBody] bodyItems = [cst2ast(body)];
    str dataName = "<name>";
    str assignedTo = "<assign.name>";
    return dataDef(dataName, cst2ast(dataType), variables, bodyItems, assignedTo);
}

// Legacy untyped data (backward compatibility)
AData cst2ast((Data)`data with <Variables vars> <DataBody body> end <Identifier name>`) {
    list[str] variables = getVariables(vars);
    list[ADataBody] bodyItems = [cst2ast(body)];
    str dataName = "<name>";
    return dataDefUntyped(dataName, variables, bodyItems);
}

AData cst2ast((Data)`<Assignment assign> data with <Variables vars> <DataBody body> end <Identifier name>`) {
    list[str] variables = getVariables(vars);
    list[ADataBody] bodyItems = [cst2ast(body)];
    str dataName = "<name>";
    str assignedTo = "<assign.name>";
    return dataDefUntyped(dataName, variables, bodyItems, assignedTo);
}

ADataBody cst2ast((DataBody)`<Constructor cons>`) = cst2ast(cons);
ADataBody cst2ast((DataBody)`<Function func>`) = dataFunction(cst2ast(func));

// Typed constructor
ADataBody cst2ast((Constructor)`<Identifier name> = struct ( <TypedVariables fields> )`) {
    return constructor("<name>", getTypedVariables(fields));
}

// Untyped constructor (backward compatibility)
ADataBody cst2ast((Constructor)`<Identifier name> = struct ( <Variables fields> )`) {
    return constructorUntyped("<name>", getVariables(fields));
}

// ============================================================================
// Statements - UPDATED with type declarations
// ============================================================================

AStatement cst2ast((Statement)`<Expression expr>`) = expression(cst2ast(expr));
AStatement cst2ast((Statement)`<Variables vars>`) = variables(getVariables(vars));
AStatement cst2ast((Statement)`<TypeDeclaration decl>`) = cst2ast(decl);
AStatement cst2ast((Statement)`<TypedDeclaration decl>`) = cst2ast(decl);
AStatement cst2ast((Statement)`<TypeAssertion assertion>`) = cst2ast(assertion);
AStatement cst2ast((Statement)`<Range range>`) = rangeStmt(cst2ast(range));
AStatement cst2ast((Statement)`<Iterator iter>`) = cst2ast(iter);
AStatement cst2ast((Statement)`<Loop loop>`) = cst2ast(loop);
AStatement cst2ast((Statement)`<Invocation invoke>`) = invocation(cst2ast(invoke));

AStatement cst2ast((Statement)`if <Expression condition> then <Body thenBody> else <Body elseBody> end`) {
    return ifThenElse(
        cst2ast(condition),
        [cst2ast(s) | s <- thenBody.statements],
        [cst2ast(s) | s <- elseBody.statements]
    );
}

AStatement cst2ast((Statement)`cond <Expression condition> do <PatternBody patterns> end`) {
    return conditional(cst2ast(condition), [cst2ast(patterns)]);
}

// NEW: Type declarations
AStatement cst2ast((TypeDeclaration)`type <Identifier name> = <Type t>`) {
    return typeDeclaration("<name>", cst2ast(t));
}

AStatement cst2ast((TypedDeclaration)`<Type t> <Identifier name> = <Expression init>`) {
    return typedDeclaration(cst2ast(t), "<name>", cst2ast(init));
}

AStatement cst2ast((TypeAssertion)`assert <Expression expr> : <Type t>`) {
    return typeAssertion(cst2ast(expr), cst2ast(t));
}

// Range
ARange cst2ast((Range)`from <Principal from> to <Principal to>`) {
    return arange(cst2ast(from), cst2ast(to));
}

ARange cst2ast((Range)`<Assignment assign> from <Principal from> to <Principal to>`) {
    return arangeAssigned(cst2ast(from), cst2ast(to), "<assign.name>");
}

// Iterator - UPDATED with types
AStatement cst2ast((Iterator)`<Assignment assign> iterator <Type returnType> ( <TypedVariables params> ) yielding ( <TypedVariables yields> )`) {
    return iterator(
        "<assign.name>",
        getTypedVariables(params),
        getTypedVariables(yields),
        cst2ast(returnType)
    );
}

// Legacy untyped iterator
AStatement cst2ast((Iterator)`<Assignment assign> iterator ( <Variables params> ) yielding ( <Variables yields> )`) {
    return iteratorUntyped(
        "<assign.name>",
        getVariables(params),
        getVariables(yields)
    );
}

// Loop - UPDATED with type
AStatement cst2ast((Loop)`for <Type varType> <Identifier var> <Range range> do <Body body>`) {
    return loop(
        cst2ast(varType),
        "<var>",
        cst2ast(range),
        [cst2ast(s) | s <- body.statements]
    );
}

// Legacy untyped loop
AStatement cst2ast((Loop)`for <Identifier var> <Range range> do <Body body>`) {
    return loopUntyped(
        "<var>",
        cst2ast(range),
        [cst2ast(s) | s <- body.statements]
    );
}

// Pattern - UPDATED with typed patterns
APattern cst2ast((PatternBody)`<Pattern pat> -\> <Expression result>`) {
    return cst2ast(pat, cst2ast(result));
}

APattern cst2ast((Pattern)`<Expression expr>`, AExpression result) {
    return pattern(cst2ast(expr), result);
}

APattern cst2ast((Pattern)`<Type t> : <Expression expr>`, AExpression result) {
    return typedPattern(cst2ast(t), cst2ast(expr), result);
}

// ============================================================================
// Expressions - UPDATED with type casting and new literals
// ============================================================================

AExpression cst2ast((Expression)`<Principal p>`) = principal(cst2ast(p));
AExpression cst2ast((Expression)`<Invocation invoke>`) = invocation(cst2ast(invoke));
AExpression cst2ast((Expression)`<TypeCast cast>`) = cst2ast(cast);
AExpression cst2ast((Expression)`<TypedExpression typed>`) = cst2ast(typed);

// Array, tuple and struct literals
AExpression cst2ast((Expression)`[ <{Expression ","}* elements> ]`) = 
    arrayLiteral([cst2ast(e) | e <- elements]);

AExpression cst2ast((Expression)`( <{Expression ","}+ elements> )`) {
    list[AExpression] elems = [cst2ast(e) | e <- elements];
    if (size(elems) == 1) {
        return parenthesis(elems[0]);  // Single element is just parentheses
    }
    return tupleLiteral(elems);
}

AExpression cst2ast((Expression)`<Identifier structName> { <{FieldInit ","}* fields> }`) = 
    structLiteral("<structName>", [cst2ast(f) | f <- fields]);

AExpression cst2ast((Expression)`( <Expression expr> )`) = parenthesis(cst2ast(expr));
AExpression cst2ast((Expression)`[ <Expression expr> ]`) = listBracket(cst2ast(expr));
AExpression cst2ast((Expression)`- <Expression expr>`) = unaryMinus(cst2ast(expr));

// NEW: Type casting and typed expressions
AExpression cst2ast((TypeCast)`cast \< <Type t> \> ( <Expression expr> )`) {
    return typeCast(cst2ast(t), cst2ast(expr));
}

AExpression cst2ast((TypedExpression)`<Expression expr> :: <Type t>`) {
    return typedExpression(cst2ast(expr), cst2ast(t));
}

// Binary operations
AExpression cst2ast((Expression)`<Expression lhs> ** <Expression rhs>`) = power(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> * <Expression rhs>`) = multiply(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> / <Expression rhs>`) = divide(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> % <Expression rhs>`) = modulo(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> + <Expression rhs>`) = add(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> - <Expression rhs>`) = subtract(cst2ast(lhs), cst2ast(rhs));

// Comparison operations
AExpression cst2ast((Expression)`<Expression lhs> \< <Expression rhs>`) = lessThan(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> \> <Expression rhs>`) = greaterThan(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> \<= <Expression rhs>`) = lessOrEqual(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> \>= <Expression rhs>`) = greaterOrEqual(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> \<\> <Expression rhs>`) = notEqual(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> = <Expression rhs>`) = equal(cst2ast(lhs), cst2ast(rhs));

// Logical operations
AExpression cst2ast((Expression)`<Expression lhs> and <Expression rhs>`) = and(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> or <Expression rhs>`) = or(cst2ast(lhs), cst2ast(rhs));

// Special operations
AExpression cst2ast((Expression)`<Expression lhs> -\> <Expression rhs>`) = arrow(cst2ast(lhs), cst2ast(rhs));
AExpression cst2ast((Expression)`<Expression lhs> : <Expression rhs>`) = colon(cst2ast(lhs), cst2ast(rhs));

// ============================================================================
// Invocations - UPDATED with generic type parameters
// ============================================================================

// Function calls with type parameters
AInvocation cst2ast((Invocation)`<Identifier name> \< <{Type ","}+ typeArgs> \> $ ( <Variables args> )`) {
    return functionCallTyped("<name>", [cst2ast(t) | t <- typeArgs], getVariables(args));
}

// Standard function calls
AInvocation cst2ast((Invocation)`<Identifier name> $ ( <Variables args> )`) {
    return functionCall("<name>", getVariables(args));
}

AInvocation cst2ast((Invocation)`<Identifier name> $ ( )`) {
    return functionCall("<name>", []);
}

// Method calls with type parameters
AInvocation cst2ast((Invocation)`<Identifier obj> . <Identifier method> \< <{Type ","}+ typeArgs> \> ( <Variables args> )`) {
    return methodCallTyped("<obj>", "<method>", [cst2ast(t) | t <- typeArgs], getVariables(args));
}

// Standard method calls
AInvocation cst2ast((Invocation)`<Identifier obj> . <Identifier method> ( <Variables args> )`) {
    return methodCall("<obj>", "<method>", getVariables(args));
}

AInvocation cst2ast((Invocation)`<Identifier obj> . <Identifier method> ( )`) {
    return methodCall("<obj>", "<method>", []);
}

// ============================================================================
// Principals - UPDATED with new literal types
// ============================================================================

APrincipal cst2ast((Principal)`true`) = boolean(true);
APrincipal cst2ast((Principal)`false`) = boolean(false);

APrincipal cst2ast((Principal)`<CharLiteral char>`) {
    str charStr = "<char>";
    // Remove quotes: 'a' -> a
    if (size(charStr) >= 3 && charStr[0] == "\'" && charStr[size(charStr)-1] == "\'") {
        return character(substring(charStr, 1, size(charStr)-1));
    }
    return character(charStr);
}

APrincipal cst2ast((Principal)`<IntLiteral i>`) {
    str intStr = "<i>";
    return integer(toInt(intStr));
}

APrincipal cst2ast((Principal)`<FloatLiteral f>`) {
    str floatStr = "<f>";
    return floating(toReal(floatStr));
}

APrincipal cst2ast((Principal)`<StringLiteral s>`) {
    str strVal = "<s>";
    // Remove quotes: "hello" -> hello
    if (size(strVal) >= 2 && strVal[0] == "\"" && strVal[size(strVal)-1] == "\"") {
        return string(substring(strVal, 1, size(strVal)-1));
    }
    return string(strVal);
}

APrincipal cst2ast((Principal)`<Identifier id>`) = identifier("<id>");

// Note: typedLiteral pattern removed due to type conflicts
// Complex literals are handled by cst2astPrincipalExpr function




// Field initialization
AFieldInit cst2ast((FieldInit)`<Identifier field> = <Expression val>`) {
    return fieldInit("<field>", cst2ast(val));
}

// ============================================================================
// Default cases for debugging
// ============================================================================

default AModule cst2ast(Module m) { 
    throw "Cannot convert Module: <m>"; 
}

default AFunction cst2ast(Function f) { 
    throw "Cannot convert Function: <f>"; 
}

default AData cst2ast(Data d) { 
    throw "Cannot convert Data: <d>"; 
}

default AStatement cst2ast(Statement s) { 
    throw "Cannot convert Statement: <s>"; 
}

default AExpression cst2ast(Expression e) { 
    throw "Cannot convert Expression: <e>"; 
}

default APrincipal cst2ast(Principal p) { 
    throw "Cannot convert Principal: <p>"; 
}

default AInvocation cst2ast(Invocation i) { 
    throw "Cannot convert Invocation: <i>"; 
}

default AType cst2ast(Type t) {
    throw "Cannot convert Type: <t>";
}

default ATypedVariable cst2ast(TypedVariable tv) {
    throw "Cannot convert TypedVariable: <tv>";
}