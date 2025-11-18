module Generator1

import AST;
import Implode;
import Syntax;
import ParseTree;
import String;
import List;
import Map;
import IO;
import util::Math;

// ============================================================================
// Runtime Environment
// ============================================================================

alias Env = map[str, Value];

data Value
    = vint(int n)
    | vfloat(real r)
    | vbool(bool b)
    | vchar(str c)
    | vstring(str s)
    | vlist(list[Value] elements)
    | vfunction(str name, list[str] params, list[AStatement] body, Env closure)
    | vdata(str name, map[str, Value] fields)
    | vnull()
    ;

// ============================================================================
// Main Generation/Interpretation Functions
// ============================================================================

// Generate and execute code from a string
Value generate(str input) {
    AModule ast = implode(parse(#start[Module], input));
    return executeModule(ast);
}

// Generate and execute code from a file
Value generateFile(loc file) {
    AModule ast = implode(parse(#start[Module], file));
    return executeModule(ast);
}

// Generate code as string (code generation mode)
str generateCode(str input) {
    AModule ast = implode(parse(#start[Module], input));
    return moduleToCode(ast);
}

// ============================================================================
// Module Execution
// ============================================================================

Value executeModule(amodule(list[str] vars, list[AItem] items)) {
    Env env = ();
    
    // Initialize module variables
    for (v <- vars) {
        env[v] = vnull();
    }
    
    // Process all items (functions and data definitions)
    for (item <- items) {
        env = processItem(item, env);
    }
    
    // Execute main if exists
    if ("main" in env) {
        return executeFunction(env["main"], [], env);
    }
    
    return vnull();
}

Env processItem(afunction(AFunction func), Env env) {
    return processFunction(func, env);
}

Env processItem(adata(AData def), Env env) {
    return processData(def, env);
}

// ============================================================================
// Function Processing
// ============================================================================

Env processFunction(function(str name, list[str] params, list[AStatement] body), Env env) {
    env[name] = vfunction(name, params, body, env);
    return env;
}

Env processFunction(function(str name, list[str] params, list[AStatement] body, str assignedTo), Env env) {
    Value func = vfunction(name, params, body, env);
    env[name] = func;
    env[assignedTo] = func;
    return env;
}

Value executeFunction(vfunction(str name, list[str] params, list[AStatement] body, Env closure), list[Value] args, Env env) {
    Env localEnv = closure;
    
    // Bind parameters to arguments
    if (size(params) != size(args)) {
        throw "Function <name> expects <size(params)> arguments, got <size(args)>";
    }
    
    for (int i <- [0..size(params)]) {
        localEnv[params[i]] = args[i];
    }
    
    // Execute function body
    return executeStatements(body, localEnv).val;
}

default Value executeFunction(Value v, list[Value] args, Env env) {
    throw "Cannot execute non-function value: <v>";
}

// ============================================================================
// Data Processing
// ============================================================================

Env processData(dataDef(str name, list[str] vars, list[ADataBody] body), Env env) {
    for (bodyItem <- body) {
        env = processDataBody(bodyItem, name, vars, env);
    }
    return env;
}

Env processData(dataDef(str name, list[str] vars, list[ADataBody] body, str assignedTo), Env env) {
    for (bodyItem <- body) {
        env = processDataBody(bodyItem, name, vars, env);
    }
    if (name in env) {
        env[assignedTo] = env[name];
    }
    return env;
}

Env processDataBody(constructor(str name, list[str] fields), str dataName, list[str] vars, Env env) {
    // Create constructor function
    list[AStatement] constructorBody = [
        expression(principal(identifier("self")))
    ];
    env[name] = vfunction(name, fields, constructorBody, env);
    return env;
}

Env processDataBody(dataFunction(AFunction func), str dataName, list[str] vars, Env env) {
    return processFunction(func, env);
}

// ============================================================================
// Statement Execution
// ============================================================================

tuple[Value val, Env env] executeStatements(list[AStatement] stmts, Env env) {
    Value lastVal = vnull();
    
    for (stmt <- stmts) {
        tuple[Value val, Env e] result = executeStatement(stmt, env);
        lastVal = result.val;
        env = result.e;
    }
    
    return <lastVal, env>;
}

tuple[Value, Env] executeStatement(expression(AExpression expr), Env env) {
    Value val = evalExpression(expr, env);
    return <val, env>;
}

tuple[Value, Env] executeStatement(variables(list[str] vars), Env env) {
    for (v <- vars) {
        env[v] = vnull();
    }
    return <vnull(), env>;
}

tuple[Value, Env] executeStatement(rangeStmt(ARange range), Env env) {
    tuple[Value from, Value to] r = evalRange(range, env);
    return <vlist([vint(i) | int i <- [getInt(r.from)..getInt(r.to)+1]]), env>;
}

tuple[Value, Env] executeStatement(ifThenElse(AExpression cond, list[AStatement] thenBody, list[AStatement] elseBody), Env env) {
    Value condVal = evalExpression(cond, env);
    
    if (getBool(condVal)) {
        return executeStatements(thenBody, env);
    } else {
        return executeStatements(elseBody, env);
    }
}

tuple[Value, Env] executeStatement(conditional(AExpression cond, list[APattern] patterns), Env env) {
    Value condVal = evalExpression(cond, env);
    
    for (p <- patterns) {
        AExpression patExpr = getPatternExpr(p);
        AExpression resExpr = getResultExpr(p);
        
        if (matchPattern(patExpr, condVal, env)) {
            return <evalExpression(resExpr, env), env>;
        }
    }
    
    return <vnull(), env>;
}

tuple[Value, Env] executeStatement(loop(str varName, ARange range, list[AStatement] body), Env env) {
    tuple[Value from, Value to] r = evalRange(range, env);
    Value lastVal = vnull();
    
    for (int i <- [getInt(r.from)..getInt(r.to)+1]) {
        env[varName] = vint(i);
        tuple[Value val, Env e] result = executeStatements(body, env);
        lastVal = result.val;
        env = result.e;
    }
    
    return <lastVal, env>;
}

tuple[Value, Env] executeStatement(iterator(str name, list[str] params, list[str] yields), Env env) {
    // Iterator creation - simplified implementation
    env[name] = vfunction(name, params, [], env);
    return <vnull(), env>;
}

tuple[Value, Env] executeStatement(invocation(AInvocation inv), Env env) {
    Value val = evalInvocation(inv, env);
    return <val, env>;
}

// ============================================================================
// Expression Evaluation
// ============================================================================

Value evalExpression(principal(APrincipal p), Env env) = evalPrincipal(p, env);
Value evalExpression(invocation(AInvocation inv), Env env) = evalInvocation(inv, env);
Value evalExpression(parenthesis(AExpression expr), Env env) = evalExpression(expr, env);
Value evalExpression(listBracket(AExpression expr), Env env) = vlist([evalExpression(expr, env)]);
Value evalExpression(unaryMinus(AExpression expr), Env env) {
    Value val = evalExpression(expr, env);
    return (vint(n) := val) ? vint(-n) : vfloat(-getFloat(val));
}

// Arithmetic operations
Value evalExpression(power(AExpression lhs, AExpression rhs), Env env) {
    Value l = evalExpression(lhs, env);
    Value r = evalExpression(rhs, env);
    return vfloat(pow(getFloat(l), getFloat(r)));
}

Value evalExpression(multiply(AExpression lhs, AExpression rhs), Env env) {
    Value l = evalExpression(lhs, env);
    Value r = evalExpression(rhs, env);
    if (vint(n1) := l && vint(n2) := r) return vint(n1 * n2);
    return vfloat(getFloat(l) * getFloat(r));
}

Value evalExpression(divide(AExpression lhs, AExpression rhs), Env env) {
    Value l = evalExpression(lhs, env);
    Value r = evalExpression(rhs, env);
    return vfloat(getFloat(l) / getFloat(r));
}

Value evalExpression(modulo(AExpression lhs, AExpression rhs), Env env) {
    Value l = evalExpression(lhs, env);
    Value r = evalExpression(rhs, env);
    return vint(getInt(l) % getInt(r));
}

Value evalExpression(add(AExpression lhs, AExpression rhs), Env env) {
    Value l = evalExpression(lhs, env);
    Value r = evalExpression(rhs, env);
    if (vint(n1) := l && vint(n2) := r) return vint(n1 + n2);
    return vfloat(getFloat(l) + getFloat(r));
}

Value evalExpression(subtract(AExpression lhs, AExpression rhs), Env env) {
    Value l = evalExpression(lhs, env);
    Value r = evalExpression(rhs, env);
    if (vint(n1) := l && vint(n2) := r) return vint(n1 - n2);
    return vfloat(getFloat(l) - getFloat(r));
}

// Comparison operations
Value evalExpression(lessThan(AExpression lhs, AExpression rhs), Env env) {
    return vbool(getFloat(evalExpression(lhs, env)) < getFloat(evalExpression(rhs, env)));
}

Value evalExpression(greaterThan(AExpression lhs, AExpression rhs), Env env) {
    return vbool(getFloat(evalExpression(lhs, env)) > getFloat(evalExpression(rhs, env)));
}

Value evalExpression(lessOrEqual(AExpression lhs, AExpression rhs), Env env) {
    return vbool(getFloat(evalExpression(lhs, env)) <= getFloat(evalExpression(rhs, env)));
}

Value evalExpression(greaterOrEqual(AExpression lhs, AExpression rhs), Env env) {
    return vbool(getFloat(evalExpression(lhs, env)) >= getFloat(evalExpression(rhs, env)));
}

Value evalExpression(notEqual(AExpression lhs, AExpression rhs), Env env) {
    return vbool(evalExpression(lhs, env) != evalExpression(rhs, env));
}

Value evalExpression(equal(AExpression lhs, AExpression rhs), Env env) {
    return vbool(evalExpression(lhs, env) == evalExpression(rhs, env));
}

// Logical operations
Value evalExpression(and(AExpression lhs, AExpression rhs), Env env) {
    return vbool(getBool(evalExpression(lhs, env)) && getBool(evalExpression(rhs, env)));
}

Value evalExpression(or(AExpression lhs, AExpression rhs), Env env) {
    return vbool(getBool(evalExpression(lhs, env)) || getBool(evalExpression(rhs, env)));
}

// Special operations
Value evalExpression(arrow(AExpression lhs, AExpression rhs), Env env) {
    // Arrow operation (could be used for pairs, mappings, etc.)
    return vlist([evalExpression(lhs, env), evalExpression(rhs, env)]);
}

Value evalExpression(colon(AExpression lhs, AExpression rhs), Env env) {
    // Colon operation (list cons or similar)
    Value l = evalExpression(lhs, env);
    Value r = evalExpression(rhs, env);
    if (vlist(elements) := r) {
        return vlist([l] + elements);
    }
    return vlist([l, r]);
}

// ============================================================================
// Principal Evaluation
// ============================================================================

Value evalPrincipal(boolean(bool b), Env env) = vbool(b);
Value evalPrincipal(character(str c), Env env) = vchar(c);
Value evalPrincipal(integer(int n), Env env) = vint(n);
Value evalPrincipal(floating(real r), Env env) = vfloat(r);

Value evalPrincipal(identifier(str id), Env env) {
    if (id in env) {
        return env[id];
    }
    throw "Undefined variable: <id>";
}

// ============================================================================
// Invocation Evaluation
// ============================================================================

Value evalInvocation(functionCall(str name, list[str] argNames), Env env) {
    if (!(name in env)) {
        throw "Undefined function: <name>";
    }
    
    list[Value] args = [env[arg] | arg <- argNames, arg in env];
    
    return executeFunction(env[name], args, env);
}

Value evalInvocation(methodCall(str obj, str method, list[str] argNames), Env env) {
    if (!(obj in env)) {
        throw "Undefined object: <obj>";
    }
    
    Value objVal = env[obj];
    list[Value] args = [env[arg] | arg <- argNames, arg in env];
    
    // Method dispatch
    if (vdata(str dataName, map[str, Value] fields) := objVal) {
        if (method in fields && vfunction(_, _, _, _) := fields[method]) {
            return executeFunction(fields[method], args, env);
        }
    }
    
    throw "Method <method> not found on object <obj>";
}

// ============================================================================
// Range Evaluation
// ============================================================================

tuple[Value from, Value to] evalRange(arange(APrincipal from, APrincipal to), Env env) {
    return <evalPrincipal(from, env), evalPrincipal(to, env)>;
}

tuple[Value from, Value to] evalRange(arangeAssigned(APrincipal from, APrincipal to, str name), Env env) {
    Value fromVal = evalPrincipal(from, env);
    Value toVal = evalPrincipal(to, env);
    env[name] = vlist([vint(i) | int i <- [getInt(fromVal)..getInt(toVal)+1]]);
    return <fromVal, toVal>;
}

// ============================================================================
// Pattern Matching Helper Functions
// ============================================================================

// Extraer el patrón de un APattern
AExpression getPatternExpr(pattern(patExpr, resExpr)) = patExpr;

// Extraer el resultado de un APattern
AExpression getResultExpr(pattern(patExpr, resExpr)) = resExpr;

// Verificar si un patrón coincide con un valor
bool matchPattern(AExpression patExpr, Value val, Env env) {
    Value patVal = evalExpression(patExpr, env);
    return patVal == val;
}

// ============================================================================
// Helper Functions
// ============================================================================

// Convert Value to int
int getInt(vint(int n)) = n;
int getInt(vfloat(real r)) = toInt(r);
default int getInt(Value v) { 
    throw "Cannot convert <v> to int"; 
}

// Convert Value to float/real
real getFloat(vint(int n)) = toReal(n);
real getFloat(vfloat(real r)) = r;
default real getFloat(Value v) { 
    throw "Cannot convert <v> to float"; 
}

// Convert Value to bool
bool getBool(vbool(bool b)) = b;
default bool getBool(Value v) { 
    throw "Cannot convert <v> to bool"; 
}

// Power function using Rascal's Math utilities
real pow(real base, real exponent) {
    if (base <= 0.0 && exponent != toReal(toInt(exponent))) {
        throw "Cannot compute fractional power of non-positive base";
    }
    return exp(ln(base) * exponent);
}

// ============================================================================
// Code Generation (AST to String)
// ============================================================================

str moduleToCode(amodule(list[str] vars, list[AItem] items)) {
    str code = "";
    
    if (size(vars) > 0) {
        code += intercalate(", ", vars) + "\n\n";
    }
    
    for (item <- items) {
        code += itemToCode(item) + "\n\n";
    }
    
    return code;
}

str itemToCode(afunction(AFunction func)) = functionToCode(func);
str itemToCode(adata(AData def)) = dataToCode(def);

str functionToCode(function(str name, list[str] params, list[AStatement] body)) {
    str code = "function (";
    if (size(params) > 0) {
        code += intercalate(", ", params);
    }
    code += ") do\n";
    for (stmt <- body) {
        code += "  " + statementToCode(stmt) + "\n";
    }
    code += "end " + name;
    return code;
}

str functionToCode(function(str name, list[str] params, list[AStatement] body, str assignedTo)) {
    return assignedTo + " = " + functionToCode(function(name, params, body));
}

str dataToCode(dataDef(str name, list[str] vars, list[ADataBody] body)) {
    str code = "data with " + intercalate(", ", vars) + "\n";
    for (bodyItem <- body) {
        code += "  " + dataBodyToCode(bodyItem) + "\n";
    }
    code += "end " + name;
    return code;
}

str dataToCode(dataDef(str name, list[str] vars, list[ADataBody] body, str assignedTo)) {
    return assignedTo + " = " + dataToCode(dataDef(name, vars, body));
}

str dataBodyToCode(constructor(str name, list[str] fields)) {
    return name + " = struct (" + intercalate(", ", fields) + ")";
}

str dataBodyToCode(dataFunction(AFunction func)) = functionToCode(func);

str statementToCode(expression(AExpression expr)) = expressionToCode(expr);
str statementToCode(variables(list[str] vars)) = intercalate(", ", vars);
str statementToCode(rangeStmt(ARange range)) = rangeToCode(range);

str statementToCode(ifThenElse(AExpression cond, list[AStatement] thenBody, list[AStatement] elseBody)) {
    str code = "if " + expressionToCode(cond) + " then\n";
    for (stmt <- thenBody) {
        code += "  " + statementToCode(stmt) + "\n";
    }
    code += "else\n";
    for (stmt <- elseBody) {
        code += "  " + statementToCode(stmt) + "\n";
    }
    code += "end";
    return code;
}

str statementToCode(loop(str varName, ARange range, list[AStatement] body)) {
    str code = "for " + varName + " " + rangeToCode(range) + " do\n";
    for (stmt <- body) {
        code += "  " + statementToCode(stmt) + "\n";
    }
    return code;
}

str statementToCode(invocation(AInvocation inv)) = invocationToCode(inv);

str expressionToCode(principal(APrincipal p)) = principalToCode(p);
str expressionToCode(invocation(AInvocation inv)) = invocationToCode(inv);
str expressionToCode(parenthesis(AExpression expr)) = "(" + expressionToCode(expr) + ")";
str expressionToCode(listBracket(AExpression expr)) = "[" + expressionToCode(expr) + "]";
str expressionToCode(unaryMinus(AExpression expr)) = "-" + expressionToCode(expr);

str expressionToCode(power(AExpression lhs, AExpression rhs)) = 
    expressionToCode(lhs) + " ** " + expressionToCode(rhs);
str expressionToCode(multiply(AExpression lhs, AExpression rhs)) = 
    expressionToCode(lhs) + " * " + expressionToCode(rhs);
str expressionToCode(divide(AExpression lhs, AExpression rhs)) = 
    expressionToCode(lhs) + " / " + expressionToCode(rhs);
str expressionToCode(add(AExpression lhs, AExpression rhs)) = 
    expressionToCode(lhs) + " + " + expressionToCode(rhs);
str expressionToCode(subtract(AExpression lhs, AExpression rhs)) = 
    expressionToCode(lhs) + " - " + expressionToCode(rhs);

str principalToCode(boolean(bool b)) = "<b>";
str principalToCode(character(str c)) = "\'<c>\'";
str principalToCode(integer(int n)) = "<n>";
str principalToCode(floating(real r)) = "<r>";
str principalToCode(identifier(str id)) = id;

str invocationToCode(functionCall(str name, list[str] args)) = 
    name + "$(" + intercalate(", ", args) + ")";
str invocationToCode(methodCall(str obj, str method, list[str] args)) = 
    obj + "." + method + "(" + intercalate(", ", args) + ")";

str rangeToCode(arange(APrincipal from, APrincipal to)) = 
    "from " + principalToCode(from) + " to " + principalToCode(to);
str rangeToCode(arangeAssigned(APrincipal from, APrincipal to, str name)) = 
    name + " = from " + principalToCode(from) + " to " + principalToCode(to);