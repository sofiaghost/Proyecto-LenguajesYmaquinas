module Parser

import Syntax;
import AST;
import ParseTree;
import String;

// ============================================================================
// Main Parsing Functions
// ============================================================================

// Parse a string into an AST
AModule parseAUL(str input) = cst2ast(parse(#start[Module], input));

// Parse a file into an AST
AModule parseAULFile(loc file) = cst2ast(parse(#start[Module], file));

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

// Convert Variables
list[str] getVariables((Variables)`<Identifier head> <{Identifier ","}* tail>`) {
    return ["<head>"] + ["<id>" | id <- tail];
}

// ============================================================================
// Functions
// ============================================================================

AFunction cst2ast((Function)`function ( <Variables params> ) do <Body body> end <Identifier name>`) {
    list[str] parameters = getVariables(params);
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    return function(funcName, parameters, statements);
}

AFunction cst2ast((Function)`<Assignment assign> function ( <Variables params> ) do <Body body> end <Identifier name>`) {
    list[str] parameters = getVariables(params);
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return function(funcName, parameters, statements, assignedTo);
}

AFunction cst2ast((Function)`function do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    return function(funcName, [], statements);
}

AFunction cst2ast((Function)`<Assignment assign> function do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [cst2ast(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return function(funcName, [], statements, assignedTo);
}

// ============================================================================
// Data Definitions
// ============================================================================

AData cst2ast((Data)`data with <Variables vars> <DataBody body> end <Identifier name>`) {
    list[str] variables = getVariables(vars);
    list[ADataBody] bodyItems = [cst2ast(body)];
    str dataName = "<name>";
    return dataDef(dataName, variables, bodyItems);
}

AData cst2ast((Data)`<Assignment assign> data with <Variables vars> <DataBody body> end <Identifier name>`) {
    list[str] variables = getVariables(vars);
    list[ADataBody] bodyItems = [cst2ast(body)];
    str dataName = "<name>";
    str assignedTo = "<assign.name>";
    return dataDef(dataName, variables, bodyItems, assignedTo);
}

ADataBody cst2ast((DataBody)`<Constructor cons>`) = cst2ast(cons);
ADataBody cst2ast((DataBody)`<Function func>`) = dataFunction(cst2ast(func));

ADataBody cst2ast((Constructor)`<Identifier name> = struct ( <Variables fields> )`) {
    return constructor("<name>", getVariables(fields));
}

// ============================================================================
// Statements
// ============================================================================

AStatement cst2ast((Statement)`<Expression expr>`) = expression(cst2ast(expr));
AStatement cst2ast((Statement)`<Variables vars>`) = variables(getVariables(vars));
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

// Range
ARange cst2ast((Range)`from <Principal from> to <Principal to>`) {
    return arange(cst2ast(from), cst2ast(to));
}

ARange cst2ast((Range)`<Assignment assign> from <Principal from> to <Principal to>`) {
    return arangeAssigned(cst2ast(from), cst2ast(to), "<assign.name>");
}

// Iterator
AStatement cst2ast((Iterator)`<Assignment assign> iterator ( <Variables params> ) yielding ( <Variables yields> )`) {
    return iterator(
        "<assign.name>",
        getVariables(params),
        getVariables(yields)
    );
}

// Loop
AStatement cst2ast((Loop)`for <Identifier var> <Range range> do <Body body>`) {
    return loop(
        "<var>",
        cst2ast(range),
        [cst2ast(s) | s <- body.statements]
    );
}

// Pattern
APattern cst2ast((PatternBody)`<Expression pat> -\> <Expression result>`) {
    return AST::pattern(cst2ast(pat), cst2ast(result));
}

// ============================================================================
// Expressions
// ============================================================================

AExpression cst2ast((Expression)`<Principal p>`) = principal(cst2ast(p));
AExpression cst2ast((Expression)`<Invocation invoke>`) = invocation(cst2ast(invoke));
AExpression cst2ast((Expression)`( <Expression expr> )`) = parenthesis(cst2ast(expr));
AExpression cst2ast((Expression)`[ <Expression expr> ]`) = listBracket(cst2ast(expr));
AExpression cst2ast((Expression)`- <Expression expr>`) = unaryMinus(cst2ast(expr));

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
// Invocations
// ============================================================================

AInvocation cst2ast((Invocation)`<Identifier name> $ ( <Variables args> )`) {
    return functionCall("<name>", getVariables(args));
}

AInvocation cst2ast((Invocation)`<Identifier obj> . <Identifier method> ( <Variables args> )`) {
    return methodCall("<obj>", "<method>", getVariables(args));
}

// ============================================================================
// Principals
// ============================================================================

APrincipal cst2ast((Principal)`true`) = boolean(true);
APrincipal cst2ast((Principal)`false`) = boolean(false);
APrincipal cst2ast((Principal)`<CharLiteral char>`) = character("<char>");

APrincipal cst2ast((Principal)`<IntLiteral i>`) {
    str intStr = "<i>";
    return integer(toInt(intStr));
}

APrincipal cst2ast((Principal)`<FloatLiteral f>`) {
    str floatStr = "<f>";
    return floating(toReal(floatStr));
}

APrincipal cst2ast((Principal)`<Identifier id>`) = identifier("<id>");

// ============================================================================
// Default case for debugging
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