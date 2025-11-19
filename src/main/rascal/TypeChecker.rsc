module TypeChecker

import AST;
import Message;
import ParseTree;
import IO;
import Set;
import Map;
import List;
import String;
import util::Maybe;

// ============================================================================
// Type Checking Infrastructure
// ============================================================================

// Main type checking function
TypeCheckResult typeCheckModule(AModule m) {
    TypeEnv globalEnv = ();
    StructEnv structEnv = ();
    FunctionEnv functionEnv = ();
    TypeAliasEnv aliasEnv = ();
    list[TypeError] errors = [];
    
    // First pass: collect all type definitions and declarations
    <errors1, globalEnv, structEnv, functionEnv, aliasEnv> = collectDeclarations(m);
    errors += errors1;
    
    // Second pass: type check all expressions and statements
    <errors2, typedModule> = typeCheckModuleBody(m, globalEnv, structEnv, functionEnv, aliasEnv);
    errors += errors2;
    
    // Third pass: validate struct field usage (Task 4)
    errors += validateStructReferences(m, structEnv, globalEnv, functionEnv);
    errors += validateDataStructureElements(m, structEnv);
    
    if (isEmpty(errors)) {
        return typeCheckSuccess(typedModule, globalEnv, structEnv, functionEnv);
    } else {
        return typeCheckFailure(errors);
    }
}

// ============================================================================
// Pass 1: Collect Declarations
// ============================================================================

tuple[list[TypeError], TypeEnv, StructEnv, FunctionEnv, TypeAliasEnv] 
collectDeclarations(AModule m) {
    list[TypeError] errors = [];
    TypeEnv env = ();
    StructEnv structs = ();
    FunctionEnv functions = ();
    TypeAliasEnv aliases = ();
    
    // Add module-level variables to environment
    for (str var <- m.variables) {
        env[var] = ainferredType(); // Will be inferred from usage
    }
    
    // Process all items (functions and data definitions)
    for (item <- m.items) {
        switch(item) {
            case afunction(function(name, params, returnType, body)): {
                if (name in functions) {
                    errors += duplicateDefinition(|unknown:///|, name, "function");
                } else {
                    list[AType] paramTypes = [param.varType | param <- params];
                    functions[name] = <returnType, paramTypes>;
                }
            }
            
            case afunction(function(name, params, returnType, body, assignedTo)): {
                if (name in functions) {
                    errors += duplicateDefinition(|unknown:///|, name, "function");
                } else {
                    list[AType] paramTypes = [param.varType | param <- params];
                    functions[name] = <returnType, paramTypes>;
                    env[assignedTo] = afunctionType(returnType, paramTypes);
                }
            }
            
            case afunction(functionUntyped(name, params, body)): {
                // Legacy untyped function - infer types
                if (name in functions) {
                    errors += duplicateDefinition(|unknown:///|, name, "function");
                } else {
                    list[AType] paramTypes = [ainferredType() | _ <- params];
                    functions[name] = <ainferredType(), paramTypes>;
                }
            }
            
            case afunction(functionUntyped(name, params, body, assignedTo)): {
                if (name in functions) {
                    errors += duplicateDefinition(|unknown:///|, name, "function");
                } else {
                    list[AType] paramTypes = [ainferredType() | _ <- params];
                    functions[name] = <ainferredType(), paramTypes>;
                    env[assignedTo] = afunctionType(ainferredType(), paramTypes);
                }
            }
            
            case adata(dataDef(name, dataType, variables, bodyItems)): {
                if (name in structs) {
                    errors += duplicateDefinition(|unknown:///|, name, "data");
                } else {
                    // Register the data type
                    aliases[name] = dataType;
                    
                    // Process constructors and nested functions
                    for (bodyItem <- bodyItems) {
                        switch(bodyItem) {
                            case constructor(consName, fields): {
                                structs[consName] = fields;
                            }
                            case constructorUntyped(consName, fields): {
                                // Convert untyped fields to inferred type
                                structs[consName] = [atypedVar(ainferredType(), f) | f <- fields];
                            }
                            case dataFunction(func): {
                                // Process nested function
                                <funcErrors, _, _, newFuncs, _> = 
                                    collectDeclarations(amodule([], [afunction(func)]));
                                errors += funcErrors;
                                functions = functions + newFuncs;
                            }
                        }
                    }
                }
            }
            
            case adata(dataDefUntyped(name, variables, bodyItems)): {
                // Legacy untyped data definition
                if (name in structs) {
                    errors += duplicateDefinition(|unknown:///|, name, "data");
                } else {
                    for (bodyItem <- bodyItems) {
                        switch(bodyItem) {
                            case constructorUntyped(consName, fields): {
                                structs[consName] = [atypedVar(ainferredType(), f) | f <- fields];
                            }
                        }
                    }
                }
            }
        }
    }
    
    return <errors, env, structs, functions, aliases>;
}

// ============================================================================
// Pass 2: Type Check Module Body
// ============================================================================

tuple[list[TypeError], AModule] typeCheckModuleBody(
    AModule m, 
    TypeEnv globalEnv, 
    StructEnv structEnv,
    FunctionEnv functionEnv,
    TypeAliasEnv aliasEnv
) {
    list[TypeError] errors = [];
    
    // Type check each function body
    for (item <- m.items) {
        switch(item) {
            case afunction(function(name, params, returnType, body)): {
                // Create local environment with parameters
                TypeEnv localEnv = globalEnv;
                for (param <- params) {
                    localEnv[param.name] = param.varType;
                }
                
                // Type check function body
                errors += typeCheckStatements(body, localEnv, structEnv, functionEnv, aliasEnv, returnType);
            }
            
            case afunction(function(name, params, returnType, body, _)): {
                TypeEnv localEnv = globalEnv;
                for (param <- params) {
                    localEnv[param.name] = param.varType;
                }
                errors += typeCheckStatements(body, localEnv, structEnv, functionEnv, aliasEnv, returnType);
            }
            
            case afunction(functionUntyped(name, params, body)): {
                // For untyped functions, use inferred types
                TypeEnv localEnv = globalEnv;
                for (param <- params) {
                    localEnv[param] = ainferredType();
                }
                errors += typeCheckStatements(body, localEnv, structEnv, functionEnv, aliasEnv, ainferredType());
            }
        }
    }
    
    return <errors, m>;
}

// ============================================================================
// Statement Type Checking
// ============================================================================

list[TypeError] typeCheckStatements(
    list[AStatement] stmts,
    TypeEnv env,
    StructEnv structs,
    FunctionEnv funcs,
    TypeAliasEnv aliases,
    AType expectedReturn
) {
    list[TypeError] errors = [];
    TypeEnv localEnv = env;
    
    for (stmt <- stmts) {
        <stmtErrors, localEnv> = typeCheckStatement(stmt, localEnv, structs, funcs, aliases, expectedReturn);
        errors += stmtErrors;
    }
    
    return errors;
}

tuple[list[TypeError], TypeEnv] typeCheckStatement(
    AStatement stmt,
    TypeEnv env,
    StructEnv structs,
    FunctionEnv funcs,
    TypeAliasEnv aliases,
    AType expectedReturn
) {
    list[TypeError] errors = [];
    TypeEnv newEnv = env;


    return <errors, newEnv>;
    
}

// ============================================================================
// Expression Type Inference
// ============================================================================

tuple[AType, list[TypeError]] inferType(
    AExpression expr,
    TypeEnv env,
    StructEnv structs,
    FunctionEnv funcs,
    TypeAliasEnv aliases
) {
    switch(expr) {
        case principal(p): return inferPrincipalType(p, env);
        
        case invocation(invoke): return typeCheckInvocation(invoke, env, structs, funcs, aliases);
        
        case typeCast(targetType, subExpr): {
            <subType, errors> = inferType(subExpr, env, structs, funcs, aliases);
            // Check if cast is valid
            if (!canCast(subType, targetType)) {
                errors += invalidCast(|unknown:///|, subType, targetType);
            }
            return <targetType, errors>;
        }
        
        case typedExpression(subExpr, exprType): {
            <inferredType, errors> = inferType(subExpr, env, structs, funcs, aliases);
            if (!typesCompatible(exprType, inferredType)) {
                errors += typeMismatch(|unknown:///|, exprType, inferredType,
                    "Typed expression annotation doesnt match inferred type");
            }
            return <exprType, errors>;
        }
        
        case arrayLiteral(elements): {
            if (isEmpty(elements)) {
                return <aarrayType(ainferredType()), []>;
            }
            
            // Infer element type from first element
            <firstType, errors> = inferType(elements[0], env, structs, funcs, aliases);
            
            // Check all elements have same type
            for (elem <- tail(elements)) {
                <elemType, elemErrors> = inferType(elem, env, structs, funcs, aliases);
                errors += elemErrors;
                if (!typesCompatible(firstType, elemType)) {
                    errors += incompatibleTypes(|unknown:///|, "array literal", firstType, elemType);
                }
            }
            
            return <aarrayType(firstType), errors>;
        }
        
        case tupleLiteral(elements): {
            list[AType] elementTypes = [];
            list[TypeError] errors = [];
            
            for (elem <- elements) {
                <elemType, elemErrors> = inferType(elem, env, structs, funcs, aliases);
                elementTypes += elemType;
                errors += elemErrors;
            }
            
            return <atupleType(elementTypes), errors>;
        }
        
        case structLiteral(structName, fields): {
            if (structName notin structs) {
                return <aunknownType(), [undefinedType(|unknown:///|, structName)]>;
            }
            
            list[TypeError] errors = [];
            list[ATypedVariable] structFields = structs[structName];
            
            // Check all required fields are present
            for (requiredField <- structFields) {
                bool found = false;
                for (fieldInit <- fields) {
                    if (fieldInit.fieldName == requiredField.name) {
                        found = true;
                        // Type check field value
                        <fieldType, fieldErrors> = inferType(fieldInit.val, env, structs, funcs, aliases);
                        errors += fieldErrors;
                        
                        if (!typesCompatible(requiredField.varType, fieldType)) {
                            errors += typeMismatch(|unknown:///|, requiredField.varType, fieldType,
                                "Field <requiredField.name> type mismatch");
                        }
                        break;
                    }
                }
                
                if (!found) {
                    errors += structFieldMissing(|unknown:///|, structName, requiredField.name);
                }
            }
            
            // Check no extra fields
            for (fieldInit <- fields) {
                bool valid = any(f <- structFields, f.name == fieldInit.fieldName);
                if (!valid) {
                    errors += undefinedField(|unknown:///|, structName, fieldInit.fieldName);
                }
            }
            
            return <astructType(structName), errors>;
        }
        
        case parenthesis(subExpr): return inferType(subExpr, env, structs, funcs, aliases);
        
        case listBracket(subExpr): return inferType(subExpr, env, structs, funcs, aliases);
        
        case unaryMinus(subExpr): {
            <subType, errors> = inferType(subExpr, env, structs, funcs, aliases);
            if (!isNumericType(subType) && subType != ainferredType()) {
                errors += incompatibleTypes(|unknown:///|, "unary minus", subType, aintType());
            }
            return <subType, errors>;
        }


        

}

tuple[AType, list[TypeError]] inferPrincipalType(APrincipal p, TypeEnv env) {
    switch(p) {
        case boolean(_): return <aboolType(), []>;
        case character(_): return <acharType(), []>;
        case integer(_): return <aintType(), []>;
        case floating(_): return <afloatType(), []>;
        case string(_): return <astringType(), []>;
        case identifier(name): {
            if (name in env) {
                return <env[name], []>;
            }
            return <aunknownType(), [undefinedVariable(|unknown:///|, name)]>;
        }
        case typedLiteral(literalType, val): {
            <valueType, errors> = inferPrincipalType(val, env);
            if (!typesCompatible(literalType, valueType)) {
                errors += typeMismatch(|unknown:///|, literalType, valueType, "Typed literal mismatch");
            }
            return <literalType, errors>;
        }
        default: return <aunknownType(), []>;
    }
}

// ============================================================================
// Invocation Type Checking
// ============================================================================

tuple[AType, list[TypeError]] typeCheckInvocation(
    AInvocation invoke,
    TypeEnv env,
    StructEnv structs,
    FunctionEnv funcs,
    TypeAliasEnv aliases
) {
    switch(invoke) {
        case functionCall(name, args): {
            if (name notin funcs) {
                return <aunknownType(), [undefinedFunction(|unknown:///|, name)]>;
            }
            
            <returnType, paramTypes> = funcs[name];
            
            if (size(args) != size(paramTypes)) {
                return <returnType, [wrongNumberOfArguments(|unknown:///|, name, size(paramTypes), size(args))]>;
            }
            
            list[TypeError] errors = [];
            for (i <- [0..size(args)]) {
                // Args are just identifiers, look them up
                if (args[i] in env) {
                    AType argType = env[args[i]];
                    if (!typesCompatible(paramTypes[i], argType)) {
                        errors += typeMismatch(|unknown:///|, paramTypes[i], argType, 
                            "Argument <i+1> type mismatch in function <name>");
                    }
                } else {
                    errors += undefinedVariable(|unknown:///|, args[i]);
                }
            }
            
            return <returnType, errors>;
        }
        
        case functionCallTyped(name, typeArgs, args): {
            // Generic function call - would need more complex type system
            return typeCheckInvocation(functionCall(name, args), env, structs, funcs, aliases);
        }
        
        case methodCall(object, method, args): {
            // Method calls would need object type information
            return <aunknownType(), []>;
        }
        
        case methodCallTyped(object, method, typeArgs, args): {
            return <aunknownType(), []>;
        }
        
        default: return <aunknownType(), []>;
    }
}

// ============================================================================
// Range Type Checking
// ============================================================================

list[TypeError] typeCheckRange(
    ARange range,
    TypeEnv env,
    StructEnv structs,
    FunctionEnv funcs,
    TypeAliasEnv aliases,
    AType expectedType
) {
    list[TypeError] errors = [];
    
    APrincipal fromVal, toVal;
    switch(range) {
        case arange(f, t): {
            fromVal = f;
            toVal = t;
        }
        case arangeAssigned(f, t, _): {
            fromVal = f;
            toVal = t;
        }
    }
    
    <fromType, fromErrors> = inferPrincipalType(fromVal, env);
    <toType, toErrors> = inferPrincipalType(toVal, env);
    errors += fromErrors + toErrors;
    
    if (!isNumericType(fromType) && fromType != ainferredType()) {
        errors += incompatibleTypes(|unknown:///|, "range start", fromType, aintType());
    }
    if (!isNumericType(toType) && toType != ainferredType()) {
        errors += incompatibleTypes(|unknown:///|, "range end", toType, aintType());
    }
    
    return errors;
}

// ============================================================================
// Task 4: Validate Struct References and Data Structure Elements
// ============================================================================

list[TypeError] validateStructReferences(
    AModule m,
    StructEnv structs,
    TypeEnv env,
    FunctionEnv funcs
) {
    list[TypeError] errors = [];
    
    // Visit all expressions in the module
    visit(m) {
        case structLiteral(structName, fields): {
            if (structName notin structs) {
                errors += undefinedType(|unknown:///|, structName);
            } else {
                // Check each field exists in the struct definition
                for (field <- fields) {
                    bool fieldExists = false;
                    for (structField <- structs[structName]) {
                        if (structField.name == field.fieldName) {
                            fieldExists = true;
                            break;
                        }
                    }
                    if (!fieldExists) {
                        errors += undefinedField(|unknown:///|, structName, field.fieldName);
                    }
                }
            }
        }
    }
    
    return errors;
}

list[TypeError] validateDataStructureElements(
    AModule m,
    StructEnv structs
) {
    list[TypeError] errors = [];
    
    // Check that all constructor fields are properly typed
    for (structName <- structs) {
        for (field <- structs[structName]) {
            // Ensure field type is valid
            if (field.varType == aunknownType()) {
                errors += typeInferenceFailure(|unknown:///|, 
                    "Cannot determine type for field <field.name> in struct <structName>");
            }
        }
    }
    
    return errors;
}

// ============================================================================
// Type Compatibility and Utility Functions
// ============================================================================

bool typesCompatible(AType t1, AType t2) {
    // Inferred types match anything
    if (t1 == ainferredType() || t2 == ainferredType()) return true;
    
    // Unknown types indicate an error occurred
    if (t1 == aunknownType() || t2 == aunknownType()) return false;
    
    // Use structural equality for complex types
    return typeEquals(t1, t2);
}

bool canCast(AType from, AType to) {
    // Can always cast to same type
    if (typesCompatible(from, to)) return true;
    
    // Numeric casts
    if (from == aintType() && to == afloatType()) return true;
    if (from == afloatType() && to == aintType()) return true;
    
    // String conversions
    if (to == astringType()) return true; // Can convert anything to string
    
    // Char to int
    if (from == acharType() && to == aintType()) return true;
    if (from == aintType() && to == acharType()) return true;
    
    return false;
}

