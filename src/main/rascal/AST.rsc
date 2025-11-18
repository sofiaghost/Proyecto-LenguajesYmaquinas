module AST

// ============================================================================
// Module - Root of the AST
// ============================================================================
data AModule 
    = amodule(list[str] variables, list[AItem] items);

data AItem
    = afunction(AFunction func)
    | adata(AData def);

// ============================================================================
// Functions
// ============================================================================
data AFunction
    = function(str name, list[str] params, list[AStatement] body)
    | function(str name, list[str] params, list[AStatement] body, str assignedTo);

// ============================================================================
// Data Definitions
// ============================================================================
data AData
    = dataDef(str name, list[str] variables, list[ADataBody] body)
    | dataDef(str name, list[str] variables, list[ADataBody] body, str assignedTo);

data ADataBody
    = constructor(str name, list[str] fields)
    | dataFunction(AFunction func);

// ============================================================================
// Statements
// ============================================================================
data AStatement
    = expression(AExpression expr)
    | variables(list[str] vars)
    | rangeStmt(ARange range)
    | iterator(str assignedTo, list[str] params, list[str] yields)
    | loop(str variable, ARange range, list[AStatement] body)
    | ifThenElse(AExpression condition, list[AStatement] thenBody, list[AStatement] elseBody)
    | conditional(AExpression condition, list[APattern] patterns)
    | invocation(AInvocation invoke);

data ARange
    = arange(APrincipal from, APrincipal to)
    | arangeAssigned(APrincipal from, APrincipal to, str assignedTo);

data APattern
    = pattern(AExpression patternExpr, AExpression resultExpr);

// ============================================================================
// Expressions
// ============================================================================
data AExpression
    = principal(APrincipal p)
    | invocation(AInvocation invoke)
    | parenthesis(AExpression expr)
    | listBracket(AExpression expr)
    | unaryMinus(AExpression expr)
    | power(AExpression lhs, AExpression rhs)
    | multiply(AExpression lhs, AExpression rhs)
    | divide(AExpression lhs, AExpression rhs)
    | modulo(AExpression lhs, AExpression rhs)
    | add(AExpression lhs, AExpression rhs)
    | subtract(AExpression lhs, AExpression rhs)
    | lessThan(AExpression lhs, AExpression rhs)
    | greaterThan(AExpression lhs, AExpression rhs)
    | lessOrEqual(AExpression lhs, AExpression rhs)
    | greaterOrEqual(AExpression lhs, AExpression rhs)
    | notEqual(AExpression lhs, AExpression rhs)
    | equal(AExpression lhs, AExpression rhs)
    | and(AExpression lhs, AExpression rhs)
    | or(AExpression lhs, AExpression rhs)
    | arrow(AExpression lhs, AExpression rhs)
    | colon(AExpression lhs, AExpression rhs);

// ============================================================================
// Invocations
// ============================================================================
data AInvocation
    = functionCall(str name, list[str] args)
    | methodCall(str object, str method, list[str] args);

// ============================================================================
// Principals (Literals and Identifiers)
// ============================================================================
data APrincipal
    = boolean(bool bval)
    | character(str charval)
    | integer(int ival)
    | floating(real fval)
    | identifier(str name);

// ============================================================================
// Utility Functions for AST Construction
// ============================================================================

// Convert string to boolean
bool toBool("true") = true;
bool toBool("false") = false;
default bool toBool(str s) = false;

// Convert string to integer
int toInt(str s) = toInt(s);

// Convert string to real
real toReal(str s) = toReal(s);

// ============================================================================
// Annotations for Source Locations (optional but recommended)
// ============================================================================
anno loc AModule@location;
anno loc AItem@location;
anno loc AFunction@location;
anno loc AData@location;
anno loc ADataBody@location;
anno loc AStatement@location;
anno loc AExpression@location;
anno loc AInvocation@location;
anno loc APrincipal@location;
anno loc ARange@location;
anno loc APattern@location;