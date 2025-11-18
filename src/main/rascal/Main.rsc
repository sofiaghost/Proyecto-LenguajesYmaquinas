module Main

import Syntax;
import AST;
import Parser;
import ParseTree;
import IO;
import String;

// ============================================================================
// Main Functions
// ============================================================================

// Run an AUL program from a file
void runAULFile(loc file) {
    println("========================================");
    println("Running AUL program: <file>");
    println("========================================");
    
    try {
        // Parse the file
        AModule ast = parseAULFile(file);
        
        // Print the AST
        println("\n--- Abstract Syntax Tree ---");
        iprint(ast);
        
        // Execute the program
        println("\n--- Program Output ---");
        execute(ast);
        
        println("\n========================================");
        println("Program execution completed successfully");
        println("========================================");
    }
    catch ParseError(loc l): {
        println("Parse error at <l>");
    }
    catch e: {
        println("Error: <e>");
    }
}

// Run an AUL program from a string
void runAULString(str program) {
    println("========================================");
    println("Running AUL program from string");
    println("========================================");
    
    try {
        // Parse the string
        AModule ast = parseAUL(program);
        
        // Print the AST
        println("\n--- Abstract Syntax Tree ---");
        iprint(ast);
        
        // Execute the program
        println("\n--- Program Output ---");
        execute(ast);
        
        println("\n========================================");
        println("Program execution completed successfully");
        println("========================================");
    }
    catch ParseError(loc l): {
        println("Parse error at <l>");
    }
    catch e: {
        println("Error: <e>");
    }
}

// Parse and show AST without executing
void showAST(loc file) {
    println("========================================");
    println("Parsing AUL file: <file>");
    println("========================================");
    
    try {
        AModule ast = parseAULFile(file);
        println("\n--- Abstract Syntax Tree ---");
        iprint(ast);
    }
    catch ParseError(loc l): {
        println("Parse error at <l>");
    }
    catch e: {
        println("Error: <e>");
    }
}

// Validate syntax of an AUL file
bool validateSyntax(loc file) {
    println("Validating syntax of: <file>");
    
    try {
        parse(#start[Module], file);
        println("✓ Syntax is valid");
        return true;
    }
    catch ParseError(loc l): {
        println("✗ Parse error at <l>");
        return false;
    }
}

// ============================================================================
// Execution Engine (Interpreter)
// ============================================================================

// Environment for storing variables and functions
alias Env = map[str, Value];

// Values that can be computed
data Value
    = vint(int n)
    | vfloat(real r)
    | vbool(bool b)
    | vchar(str c)
    | vstring(str s)
    | vlist(list[Value] elements)
    | vfunction(str name, list[str] params, list[AStatement] body, Env closure)
    | vnull();

// Execute the module
void execute(AModule m) {
    Env env = ();
    
    // Initialize module variables
    for (v <- m.variables) {
        env[v] = vnull();
    }
    
    // Process all items (functions and data definitions)
    for (item <- m.items) {
        env = executeItem(item, env);
    }
    
    // Look for and execute main function if it exists
    if ("main" in env) {
        println("\n--- Executing main function ---");
        Value result = callFunction(env["main"], [], env);
        println("Result: <formatValue(result)>");
    } else {
        println("\nNote: No main function found in program");
    }
}

// Execute an item (function or data definition)
Env executeItem(afunction(AFunction func), Env env) {
    return executeFunction(func, env);
}

Env executeItem(adata(AData def), Env env) {
    // For now, just register data constructors
    // Full data type support would require more implementation
    return env;
}

// Execute a function definition
Env executeFunction(function(str name, list[str] params, list[AStatement] body), Env env) {
    env[name] = vfunction(name, params, body, env);
    return env;
}

Env executeFunction(function(str name, list[str] params, list[AStatement] body, str assignedTo), Env env) {
    Value func = vfunction(name, params, body, env);
    env[name] = func;
    env[assignedTo] = func;
    return env;
}

// Call a function
Value callFunction(vfunction(str name, list[str] params, list[AStatement] body, Env closure), list[Value] args, Env env) {
    // Create new environment with parameters bound to arguments
    Env localEnv = closure;
    
    if (size(params) != size(args)) {
        throw "Function <name> expects <size(params)> arguments, got <size(args)>";
    }
    
    for (i <- [0..size(params)]) {
        localEnv[params[i]] = args[i];
    }
    
    // Execute function body
    Value result = vnull();
    for (stmt <- body) {
        result = executeStatement(stmt, localEnv).val;
    }
    
    return result;
}

default Value callFunction(Value v, list[Value] args, Env env) {
    throw "Cannot call non-function value: <v>";
}

// Execute a statement and return updated environment and result value
tuple[Env env, Value val] executeStatement(AStatement stmt, Env env) {
    switch(stmt) {
        case expression(AExpression expr): {
            Value v = evaluateExpression(expr, env);
            return <env, v>;
        }
        case variables(list[str] vars): {
            for (v <- vars) {
                env[v] = vnull();
            }
            return <env, vnull()>;
        }
        case ifThenElse(AExpression cond, list[AStatement] thenBody, list[AStatement] elseBody): {
            Value condVal = evaluateExpression(cond, env);
            if (isTruthy(condVal)) {
                Value result = vnull();
                for (s <- thenBody) {
                    result = executeStatement(s, env).val;
                }
                return <env, result>;
            } else {
                Value result = vnull();
                for (s <- elseBody) {
                    result = executeStatement(s, env).val;
                }
                return <env, result>;
            }
        }
        default: {
            println("Warning: Unimplemented statement type: <stmt>");
            return <env, vnull()>;
        }
    }
}

// Evaluate an expression
Value evaluateExpression(AExpression expr, Env env) {
    switch(expr) {
        case principal(APrincipal p): return evaluatePrincipal(p, env);
        case parenthesis(AExpression e): return evaluateExpression(e, env);
        case listBracket(AExpression e): return vlist([evaluateExpression(e, env)]);
        case unaryMinus(AExpression e): {
            Value v = evaluateExpression(e, env);
            if (vint(int n) := v) return vint(-n);
            if (vfloat(real r) := v) return vfloat(-r);
            throw "Cannot negate non-numeric value";
        }
        case add(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            Value r = evaluateExpression(rhs, env);
            if (vint(int ln) := l && vint(int rn) := r) return vint(ln + rn);
            if (vfloat(real lf) := l && vfloat(real rf) := r) return vfloat(lf + rf);
            throw "Type error in addition";
        }
        case subtract(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            Value r = evaluateExpression(rhs, env);
            if (vint(int ln) := l && vint(int rn) := r) return vint(ln - rn);
            if (vfloat(real lf) := l && vfloat(real rf) := r) return vfloat(lf - rf);
            throw "Type error in subtraction";
        }
        case multiply(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            Value r = evaluateExpression(rhs, env);
            if (vint(int ln) := l && vint(int rn) := r) return vint(ln * rn);
            if (vfloat(real lf) := l && vfloat(real rf) := r) return vfloat(lf * rf);
            throw "Type error in multiplication";
        }
        case divide(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            Value r = evaluateExpression(rhs, env);
            if (vint(int ln) := l && vint(int rn) := r) {
                if (rn == 0) throw "Division by zero";
                return vint(ln / rn);
            }
            if (vfloat(real lf) := l && vfloat(real rf) := r) {
                if (rf == 0.0) throw "Division by zero";
                return vfloat(lf / rf);
            }
            throw "Type error in division";
        }
        case lessThan(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            Value r = evaluateExpression(rhs, env);
            if (vint(int ln) := l && vint(int rn) := r) return vbool(ln < rn);
            if (vfloat(real lf) := l && vfloat(real rf) := r) return vbool(lf < rf);
            throw "Type error in comparison";
        }
        case greaterThan(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            Value r = evaluateExpression(rhs, env);
            if (vint(int ln) := l && vint(int rn) := r) return vbool(ln > rn);
            if (vfloat(real lf) := l && vfloat(real rf) := r) return vbool(lf > rf);
            throw "Type error in comparison";
        }
        case equal(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            Value r = evaluateExpression(rhs, env);
            return vbool(l == r);
        }
        case and(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            if (!isTruthy(l)) return vbool(false);
            Value r = evaluateExpression(rhs, env);
            return vbool(isTruthy(r));
        }
        case or(AExpression lhs, AExpression rhs): {
            Value l = evaluateExpression(lhs, env);
            if (isTruthy(l)) return vbool(true);
            Value r = evaluateExpression(rhs, env);
            return vbool(isTruthy(r));
        }
        default: {
            throw "Unimplemented expression: <expr>";
        }
    }
}


Value evaluatePrincipal(APrincipal p, Env env) {
    switch(p) {
        case boolean(bool b): return vbool(b);
        case integer(int n): return vint(n);
        case floating(real r): return vfloat(r);
        case character(str c): return vchar(c);
        case identifier(str name): {
            if (name in env) {
                return env[name];
            }
            throw "Undefined variable: <name>";
        }
        default: 
            throw "Invalid principal value: <p>";
    }
}
 

// Check if a value is truthy
bool isTruthy(vbool(bool b)) = b;
bool isTruthy(vint(int n)) = n != 0;
bool isTruthy(vnull()) = false;
default bool isTruthy(Value v) = true;

// Format a value for output
str formatValue(vint(int n)) = "<n>";
str formatValue(vfloat(real r)) = "<r>";
str formatValue(vbool(bool b)) = "<b>";
str formatValue(vchar(str c)) = "\'<c>\'";
str formatValue(vstring(str s)) = "\"<s>\"";
str formatValue(vlist(list[Value] elements)) = "[<intercalate(", ", [formatValue(e) | e <- elements])>]";
str formatValue(vfunction(str name, _, _, _)) = "\<function <name>\>";
str formatValue(vnull()) = "null";

// ============================================================================
// Example Programs
// ============================================================================

void testSimpleProgram() {
    str program = "
    'function do
    '    x, y
    '    x = 5
    '    y = 10
    '    x + y
    'end main
    ";
    
    runAULString(program);
}

void testExample() {
    println("Testing simple AUL program...\n");
    
    str simpleProgram = 
        "function do
        '    true
        'end test
        ";
    
    runAULString(simpleProgram);
}

// ============================================================================
// Help
// ============================================================================

void help() {
    println("========================================");
    println("AUL Language - Rascal Implementation");
    println("========================================");
    println("");
    println("Available functions:");
    println("");
    println("  runAULFile(|file:///path/to/program.alu|)");
    println("    - Run an AUL program from a file");
    println("");
    println("  runAULString(\"program code\")");
    println("    - Run an AUL program from a string");
    println("");
    println("  showAST(|file:///path/to/program.alu|)");
    println("    - Parse and display the AST without executing");
    println("");
    println("  validateSyntax(|file:///path/to/program.alu|)");
    println("    - Check if the syntax is valid");
    println("");
    println("  testExample()");
    println("    - Run a simple test program");
    println("");
    println("  help()");
    println("    - Show this help message");
    println("");
    println("========================================");
}