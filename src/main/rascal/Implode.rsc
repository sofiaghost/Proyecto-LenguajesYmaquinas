module Implode

import AST;
import Syntax;
import ParseTree;
import String;

// ============================================================================
// Main Implode Function - ParseTree to AST
// ============================================================================

// Implode a start[Module] ParseTree into an AModule AST
AModule implode(start[Module] tree) = implodeModule(tree.top);

// ============================================================================
// Module Implosion
// ============================================================================

AModule implodeModule((Module)`<Function* funcs> <Data* datas>`) {
    list[AItem] items = [afunction(implodeFunction(f)) | f <- funcs] + 
                        [adata(implodeData(d)) | d <- datas];
    return amodule([], items);
}

AModule implodeModule((Module)`<Variables vars> <Function* funcs> <Data* datas>`) {
    list[str] moduleVars = implodeVariables(vars);
    list[AItem] items = [afunction(implodeFunction(f)) | f <- funcs] + 
                        [adata(implodeData(d)) | d <- datas];
    return amodule(moduleVars, items);
}

// ============================================================================
// Variables Implosion
// ============================================================================

list[str] implodeVariables((Variables)`<Identifier head> <{Identifier ","}* tail>`) {
    return ["<head>"] + ["<id>" | id <- tail];
}

// ============================================================================
// Function Implosion
// ============================================================================

AFunction implodeFunction((Function)`function ( <Variables params> ) do <Body body> end <Identifier name>`) {
    list[str] parameters = implodeVariables(params);
    list[AStatement] statements = [implodeStatement(s) | s <- body.statements];
    str funcName = "<name>";
    return function(funcName, parameters, statements);
}

AFunction implodeFunction((Function)`<Assignment assign> function ( <Variables params> ) do <Body body> end <Identifier name>`) {
    list[str] parameters = implodeVariables(params);
    list[AStatement] statements = [implodeStatement(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return function(funcName, parameters, statements, assignedTo);
}

AFunction implodeFunction((Function)`function do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [implodeStatement(s) | s <- body.statements];
    str funcName = "<name>";
    return function(funcName, [], statements);
}

AFunction implodeFunction((Function)`<Assignment assign> function do <Body body> end <Identifier name>`) {
    list[AStatement] statements = [implodeStatement(s) | s <- body.statements];
    str funcName = "<name>";
    str assignedTo = "<assign.name>";
    return function(funcName, [], statements, assignedTo);
}

// ============================================================================
// Data Definition Implosion
// ============================================================================

AData implodeData((Data)`data with <Variables vars> <DataBody body> end <Identifier name>`) {
    list[str] variables = implodeVariables(vars);
    list[ADataBody] bodyItems = [implodeDataBody(body)];
    str dataName = "<name>";
    return dataDef(dataName, variables, bodyItems);
}

AData implodeData((Data)`<Assignment assign> data with <Variables vars> <DataBody body> end <Identifier name>`) {
    list[str] variables = implodeVariables(vars);
    list[ADataBody] bodyItems = [implodeDataBody(body)];
    str dataName = "<name>";
    str assignedTo = "<assign.name>";
    return dataDef(dataName, variables, bodyItems, assignedTo);
}

ADataBody implodeDataBody((DataBody)`<Constructor cons>`) = implodeConstructor(cons);
ADataBody implodeDataBody((DataBody)`<Function func>`) = dataFunction(implodeFunction(func));

ADataBody implodeConstructor((Constructor)`<Identifier name> = struct ( <Variables fields> )`) {
    return constructor("<name>", implodeVariables(fields));
}

// ============================================================================
// Statement Implosion
// ============================================================================

AStatement implodeStatement((Statement)`<Expression expr>`) = expression(implodeExpression(expr));
AStatement implodeStatement((Statement)`<Variables vars>`) = variables(implodeVariables(vars));
AStatement implodeStatement((Statement)`<Range range>`) = rangeStmt(implodeRange(range));
AStatement implodeStatement((Statement)`<Iterator iter>`) = implodeIterator(iter);
AStatement implodeStatement((Statement)`<Loop loop>`) = implodeLoop(loop);
AStatement implodeStatement((Statement)`<Invocation invoke>`) = invocation(implodeInvocation(invoke));

AStatement implodeStatement((Statement)`if <Expression condition> then <Body thenBody> else <Body elseBody> end`) {
    return ifThenElse(
        implodeExpression(condition),
        [implodeStatement(s) | s <- thenBody.statements],
        [implodeStatement(s) | s <- elseBody.statements]
    );
}

AStatement implodeStatement((Statement)`cond <Expression condition> do <PatternBody patterns> end`) {
    return conditional(implodeExpression(condition), [implodePattern(patterns)]);
}

// ============================================================================
// Range Implosion
// ============================================================================

ARange implodeRange((Range)`from <Principal from> to <Principal to>`) {
    return arange(implodePrincipal(from), implodePrincipal(to));
}

ARange implodeRange((Range)`<Assignment assign> from <Principal from> to <Principal to>`) {
    return arangeAssigned(implodePrincipal(from), implodePrincipal(to), "<assign.name>");
}

// ============================================================================
// Iterator Implosion
// ============================================================================

AStatement implodeIterator((Iterator)`<Assignment assign> iterator ( <Variables params> ) yielding ( <Variables yields> )`) {
    return iterator(
        "<assign.name>",
        implodeVariables(params),
        implodeVariables(yields)
    );
}

// ============================================================================
// Loop Implosion
// ============================================================================

AStatement implodeLoop((Loop)`for <Identifier var> <Range range> do <Body body>`) {
    return loop(
        "<var>",
        implodeRange(range),
        [implodeStatement(s) | s <- body.statements]
    );
}

// ============================================================================
// Pattern Implosion
// ============================================================================

APattern implodePattern((PatternBody)`<Expression pat> -\> <Expression result>`) {
    return AST::pattern(implodeExpression(pat), implodeExpression(result));
}

// ============================================================================
// Expression Implosion
// ============================================================================

AExpression implodeExpression((Expression)`<Principal p>`) = principal(implodePrincipal(p));
AExpression implodeExpression((Expression)`<Invocation invoke>`) = invocation(implodeInvocation(invoke));
AExpression implodeExpression((Expression)`( <Expression expr> )`) = parenthesis(implodeExpression(expr));
AExpression implodeExpression((Expression)`[ <Expression expr> ]`) = listBracket(implodeExpression(expr));
AExpression implodeExpression((Expression)`- <Expression expr>`) = unaryMinus(implodeExpression(expr));

// Binary arithmetic operations
AExpression implodeExpression((Expression)`<Expression lhs> ** <Expression rhs>`) = power(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> * <Expression rhs>`) = multiply(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> / <Expression rhs>`) = divide(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> % <Expression rhs>`) = modulo(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> + <Expression rhs>`) = add(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> - <Expression rhs>`) = subtract(implodeExpression(lhs), implodeExpression(rhs));

// Comparison operations
AExpression implodeExpression((Expression)`<Expression lhs> \< <Expression rhs>`) = lessThan(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> \> <Expression rhs>`) = greaterThan(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> \<= <Expression rhs>`) = lessOrEqual(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> \>= <Expression rhs>`) = greaterOrEqual(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> \<\> <Expression rhs>`) = notEqual(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> = <Expression rhs>`) = equal(implodeExpression(lhs), implodeExpression(rhs));

// Logical operations
AExpression implodeExpression((Expression)`<Expression lhs> and <Expression rhs>`) = and(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> or <Expression rhs>`) = or(implodeExpression(lhs), implodeExpression(rhs));

// Special operations
AExpression implodeExpression((Expression)`<Expression lhs> -\> <Expression rhs>`) = arrow(implodeExpression(lhs), implodeExpression(rhs));
AExpression implodeExpression((Expression)`<Expression lhs> : <Expression rhs>`) = colon(implodeExpression(lhs), implodeExpression(rhs));

// ============================================================================
// Invocation Implosion
// ============================================================================

AInvocation implodeInvocation((Invocation)`<Identifier name> $ ( <Variables args> )`) {
    return functionCall("<name>", implodeVariables(args));
}

AInvocation implodeInvocation((Invocation)`<Identifier obj> . <Identifier method> ( <Variables args> )`) {
    return methodCall("<obj>", "<method>", implodeVariables(args));
}

// ============================================================================
// Principal Implosion
// ============================================================================

APrincipal implodePrincipal((Principal)`true`) = boolean(true);
APrincipal implodePrincipal((Principal)`false`) = boolean(false);
APrincipal implodePrincipal((Principal)`<CharLiteral char>`) = character("<char>");

APrincipal implodePrincipal((Principal)`<IntLiteral i>`) {
    str intStr = "<i>";
    return integer(toInt(intStr));
}

APrincipal implodePrincipal((Principal)`<FloatLiteral f>`) {
    str floatStr = "<f>";
    return floating(toReal(floatStr));
}

APrincipal implodePrincipal((Principal)`<Identifier id>`) = identifier("<id>");

// ============================================================================
// Default Error Handlers
// ============================================================================

default AModule implodeModule(Module m) { 
    throw "Cannot implode Module: <m>"; 
}

default AFunction implodeFunction(Function f) { 
    throw "Cannot implode Function: <f>"; 
}

default AData implodeData(Data d) { 
    throw "Cannot implode Data: <d>"; 
}

default AStatement implodeStatement(Statement s) { 
    throw "Cannot implode Statement: <s>"; 
}

default AExpression implodeExpression(Expression e) { 
    throw "Cannot implode Expression: <e>"; 
}

default APrincipal implodePrincipal(Principal p) { 
    throw "Cannot implode Principal: <p>"; 
}

default AInvocation implodeInvocation(Invocation i) { 
    throw "Cannot implode Invocation: <i>"; 
}