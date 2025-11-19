module AST

import util::Math;
import List;
// ============================================================================
// Module - Root of the AST
// ============================================================================
data AModule 
    = amodule(list[str] variables, list[AItem] items);

// ============================================================================
// Type System - NEW: AST representation of types
// ============================================================================
data AType
    = aintType()
    | aboolType()
    | acharType()
    | astringType()
    | afloatType()
    | astructType(str name)
    | aarrayType(AType elementType)
    | atupleType(list[AType] elementTypes)
    | afunctionType(AType returnType, list[AType] paramTypes)
    | acustomType(str name)
    | ainferredType()  // For type inference (_)
    | aunknownType()   // For error handling
    | avoidType();     // For functions with no return

// ============================================================================
// Typed Variables - NEW: Variables with type annotations
// ============================================================================
data ATypedVariable
    = atypedVar(AType varType, str name);

// ============================================================================
// Items (Functions and Data)
// ============================================================================
data AItem
    = afunction(AFunction func)
    | adata(AData def);

// ============================================================================
// Functions - UPDATED with type annotations
// ============================================================================
data AFunction
    = function(str name, list[ATypedVariable] params, AType returnType, list[AStatement] body)
    | function(str name, list[ATypedVariable] params, AType returnType, list[AStatement] body, str assignedTo)
    | functionUntyped(str name, list[str] params, list[AStatement] body)  // Legacy support
    | functionUntyped(str name, list[str] params, list[AStatement] body, str assignedTo);

// ============================================================================
// Data Definitions - UPDATED with typed fields
// ============================================================================
data AData
    = dataDef(str name, AType dataType, list[ATypedVariable] variables, list[ADataBody] body)
    | dataDef(str name, AType dataType, list[ATypedVariable] variables, list[ADataBody] body, str assignedTo)
    | dataDefUntyped(str name, list[str] variables, list[ADataBody] body)  // Legacy support
    | dataDefUntyped(str name, list[str] variables, list[ADataBody] body, str assignedTo);

data ADataBody
    = constructor(str name, list[ATypedVariable] fields)
    | constructorUntyped(str name, list[str] fields)  // Legacy support
    | dataFunction(AFunction func);

// ============================================================================
// Statements - UPDATED with type declarations
// ============================================================================
data AStatement
    = expression(AExpression expr)
    | variables(list[str] vars)
    | typedVariables(list[ATypedVariable] vars)  // NEW: typed variable declarations
    | typeDeclaration(str name, AType declaredType)  // NEW: type alias
    | typedDeclaration(AType varType, str name, AExpression init)  // NEW: typed initialization
    | typeAssertion(AExpression expr, AType assertedType)  // NEW: runtime type assertion
    | rangeStmt(ARange range)
    | iterator(str assignedTo, list[ATypedVariable] params, list[ATypedVariable] yields, AType returnType)
    | iteratorUntyped(str assignedTo, list[str] params, list[str] yields)  // Legacy
    | loop(AType varType, str variable, ARange range, list[AStatement] body)
    | loopUntyped(str variable, ARange range, list[AStatement] body)  // Legacy
    | ifThenElse(AExpression condition, list[AStatement] thenBody, list[AStatement] elseBody)
    | conditional(AExpression condition, list[APattern] patterns)
    | invocation(AInvocation invoke);

data ARange
    = arange(APrincipal from, APrincipal to)
    | arangeAssigned(APrincipal from, APrincipal to, str assignedTo);

data APattern
    = pattern(AExpression patternExpr, AExpression resultExpr)
    | typedPattern(AType patternType, AExpression patternExpr, AExpression resultExpr);  // NEW

// ============================================================================
// Expressions - UPDATED with type casting and typed expressions
// ============================================================================
data AExpression
    = principal(APrincipal p)
    | invocation(AInvocation invoke)
    | typeCast(AType targetType, AExpression expr)  // NEW: cast<Type>(expr)
    | typedExpression(AExpression expr, AType exprType)  // NEW: expr :: Type
    | parenthesis(AExpression expr)
    | listBracket(AExpression expr)
    | arrayLiteral(list[AExpression] elements)  // NEW: [elem1, elem2, ...]
    | tupleLiteral(list[AExpression] elements)  // NEW: (elem1, elem2, ...)
    | structLiteral(str structName, list[AFieldInit] fields)  // NEW: Point{x=1, y=2}
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

// NEW: Field initialization for struct literals
data AFieldInit
    = fieldInit(str fieldName, AExpression val);

// ============================================================================
// Invocations - UPDATED with generic type parameters
// ============================================================================
data AInvocation
    = functionCall(str name, list[str] args)
    | functionCallTyped(str name, list[AType] typeArgs, list[str] args)  // NEW: func<T>(args)
    | methodCall(str object, str method, list[str] args)
    | methodCallTyped(str object, str method, list[AType] typeArgs, list[str] args);  // NEW

// ============================================================================
// Principals (Literals and Identifiers) - UPDATED with strings
// ============================================================================
data APrincipal
    = boolean(bool bval)
    | character(str charval)
    | integer(int ival)
    | floating(real fval)
    | string(str sval)  // NEW: string literals
    | identifier(str name)
    | typedLiteral(AType literalType, APrincipal val);  // NEW: Type(value)

// ============================================================================
// Type Environment for Type Checking
// ============================================================================
alias TypeEnv = map[str, AType];
alias StructEnv = map[str, list[ATypedVariable]];
alias FunctionEnv = map[str, tuple[AType returnType, list[AType] paramTypes]];
alias TypeAliasEnv = map[str, AType];

// ============================================================================
// Type Checking Result
// ============================================================================
data TypeCheckResult
    = typeCheckSuccess(AModule typedModule, TypeEnv env, StructEnv structs, FunctionEnv functions)
    | typeCheckFailure(list[TypeError] errors);

data TypeError
    = typeMismatch(loc location, AType expected, AType actual, str message)
    | undefinedVariable(loc location, str varName)
    | undefinedType(loc location, str typeName)
    | undefinedField(loc location, str structName, str fieldName)
    | undefinedFunction(loc location, str funcName)
    | duplicateDefinition(loc location, str name, str kind)
    | invalidCast(loc location, AType fromType, AType toType)
    | incompatibleTypes(loc location, str operation, AType leftType, AType rightType)
    | structFieldMissing(loc location, str structName, str fieldName)
    | wrongNumberOfArguments(loc location, str funcName, int expected, int actual)
    | nonBooleanCondition(loc location, AType actualType)
    | invalidArrayIndex(loc location, AType indexType)
    | typeInferenceFailure(loc location, str message);

// ============================================================================
// Utility Functions for Type System
// ============================================================================

// Convert string to boolean
bool toBool("true") = true;
bool toBool("false") = false;
default bool toBool(str s) = false;

// Convert string to integer
int toInt(str s) = toInt(s);

// Convert string to real
real toReal(str s) = toReal(s);

// Type compatibility checking
bool isNumericType(aintType()) = true;
bool isNumericType(afloatType()) = true;
default bool isNumericType(AType t) = false;

bool isBooleanType(aboolType()) = true;
default bool isBooleanType(AType t) = false;

bool isComparableType(aintType()) = true;
bool isComparableType(afloatType()) = true;
bool isComparableType(acharType()) = true;
bool isComparableType(astringType()) = true;
default bool isComparableType(AType t) = false;

// Type equality (structural for complex types)
bool typeEquals(aintType(), aintType()) = true;
bool typeEquals(aboolType(), aboolType()) = true;
bool typeEquals(acharType(), acharType()) = true;
bool typeEquals(astringType(), astringType()) = true;
bool typeEquals(afloatType(), afloatType()) = true;
bool typeEquals(avoidType(), avoidType()) = true;
bool typeEquals(ainferredType(), _) = true;  // Inferred matches anything
bool typeEquals(_, ainferredType()) = true;
bool typeEquals(astructType(n1), astructType(n2)) = n1 == n2;
bool typeEquals(aarrayType(t1), aarrayType(t2)) = typeEquals(t1, t2);
bool typeEquals(atupleType(ts1), atupleType(ts2)) = 
    size(ts1) == size(ts2) && all(i <- [0..size(ts1)], typeEquals(ts1[i], ts2[i]));
bool typeEquals(afunctionType(r1, ps1), afunctionType(r2, ps2)) = 
    typeEquals(r1, r2) && size(ps1) == size(ps2) && 
    all(i <- [0..size(ps1)], typeEquals(ps1[i], ps2[i]));
default bool typeEquals(AType t1, AType t2) = false;

// Type to string conversion for error messages
str typeToString(aintType()) = "int";
str typeToString(aboolType()) = "bool";
str typeToString(acharType()) = "char";
str typeToString(astringType()) = "string";
str typeToString(afloatType()) = "float";
str typeToString(avoidType()) = "void";
str typeToString(ainferredType()) = "_";
str typeToString(aunknownType()) = "unknown";
str typeToString(astructType(name)) = name;
str typeToString(aarrayType(elemType)) = "<typeToString(elemType)>[]";
str typeToString(atupleType(types)) = "tuple\<<intercalate(", ", [typeToString(t) | t <- types])>\>";
str typeToString(afunctionType(ret, params)) = 
    "function\<<typeToString(ret)>(<intercalate(", ", [typeToString(p) | p <- params])>)\>";
str typeToString(acustomType(name)) = name;
default str typeToString(AType t) = "?";

// ============================================================================
// Annotations for Source Locations (for error reporting)
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
anno loc AType@location;
anno loc ATypedVariable@location;
anno loc AFieldInit@location;