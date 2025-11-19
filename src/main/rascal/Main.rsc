module Main

import Syntax;
import Parser;
import AST;
import TypeChecker;
import ParseTree;
import IO;
import Message;
import String;

// ============================================================================
// Main Entry Point
// ============================================================================

void main(list[str] args) {
    if (size(args) == 0) {
        println("Usage: main <filename.alu>");
        return;
    }
    
    str filename = args[0];
    loc file = |file:///| + filename;
    
    println("Processing ALU file: <filename>");
    println("=" * 50);
    
    try {
        // Step 1: Parse the file
        println("\n[1] Parsing...");
        AModule ast = parseALUFile(file);
        println("    ✓ Parsing successful");
        
        // Step 2: Type check
        println("\n[2] Type checking...");
        checkTypes(ast);
        
        // If type checking succeeds, we could continue to code generation
        // generateCode(ast);
        
    } catch ParseError(loc l): {
        println("    ✗ Parse error at <l>");
    } catch value v: {
        println("    ✗ Error: <v>");
    }
    
    println("\n" + "=" * 50);
}

// Alternative: Process a string directly
void processALUString(str input) {
    println("Processing ALU code:");
    println("=" * 50);
    
    try {
        // Step 1: Parse the string
        println("\n[1] Parsing...");
        AModule ast = parseALU(input);
        println("    ✓ Parsing successful");
        
        // Step 2: Type check
        println("\n[2] Type checking...");
        checkTypes(ast);
        
    } catch ParseError(loc l): {
        println("    ✗ Parse error at <l>");
    } catch value v: {
        println("    ✗ Error: <v>");
    }
    
    println("\n" + "=" * 50);
}

// ============================================================================
// Test Functions
// ============================================================================

void testValidProgram() {
    str program = 
        "// Test program with types
        function int fibonacci(int n) do
            if n \< 2 then
                n
            else
                fibonacci$(n - 1) + fibonacci$(n - 2)
            end
        end fib
        
        data Point with int x, int y
            constructor = struct(int x, int y)
        end point
        
        function int distance(Point p) do
            p.x * p.x + p.y * p.y
        end dist
        ";
    
    println("\nTesting VALID program:");
    processALUString(program);
}

void testInvalidProgram() {
    str program = 
        "// Test program with type errors
        function int wrongReturn() do
            true  // Returns bool instead of int!
        end wrong
        
        data Point with int x, int y
            constructor = struct(int x, int y)
        end point
        
        function test() do
            Point p = Point{x = 10, y = \"hello\"}  // Type error: string for int field
            int result = p.z  // Error: field z doesnt exist
        end test
        ";
    
    println("\nTesting INVALID program:");
    processALUString(program);
}

void testGradualTyping() {
    str program = 
        "// Test gradual typing (mixed typed and untyped)
        function add(a, b) do  // Untyped function
            a + b
        end add
        
        function int typedAdd(int a, int b) do  // Typed function
            a + b
        end typedAdd
        
        data Point with x, y  // Untyped data
            constructor = struct(x, y)
        end point
        ";
    
    println("\nTesting GRADUAL typing:");
    processALUString(program);
}

void testComplexTypes() {
    str program = 
        "// Test complex types
        type Matrix = int[][]
        type StringList = string[]
        
        function processArray(int[] numbers) do
            [1, 2, 3, 4, 5]
        end process
        
        function tuple\<int, string\> getPair() do
            (42, \"answer\")
        end pair
        
        data Person with string name, int age
            constructor = struct(string name, int age)
        end person
        
        function test() do
            Person p = Person{name = \"Alice\", age = 30}
            int[] nums = [1, 2, 3]
            cast\<float\>(42)
        end test
        ";
    
    println("\nTesting COMPLEX types:");
    processALUString(program);
}

// ============================================================================
// Run All Tests
// ============================================================================

void runAllTests() {
    println("Running ALU Type Checker Tests");
    println("=" * 60);
    
    testValidProgram();
    testInvalidProgram();
    testGradualTyping();
    testComplexTypes();
    
    println("\n" + "=" * 60);
    println("All tests completed!");
}

// ============================================================================
void repl() {
    println("ALU Language REPL - Type \"quit\" to exit");
    println(repeat("=", 50));
    
    while (true) {
        print("\nalu ");
        str input = readln();
        
        if (input == "quit" || input == "exit") {
            println("Goodbye!");
            break;
        }
        
        if (input == "help") {
            println("Commands:");
            println("  quit/exit - Exit the REPL");
            println("  help      - Show this help");
            println("  test      - Run all tests");
            println("  Or enter any ALU code to parse and type check");
            continue;
        }
        
        if (input == "test") {
            runAllTests();
            continue;
        }
        
        processALUString(input);
    }
}
