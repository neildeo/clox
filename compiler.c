#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct Parser
{
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum
{
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct ParseRule
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct Local
{
    Token name;
    int depth;
} Local;

typedef enum
{
    TYPE_FUNCTION,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler
{
    struct Compiler *enclosing;
    ObjFunction *function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

Parser parser;
Compiler *current = NULL;

static Chunk *currentChunk();
static void advance();
static void consume(TokenType type, const char *message);
static bool match(TokenType type);
static void initCompiler(Compiler *compiler, FunctionType type);
ObjFunction *endCompiler();

static void parsePrecedence(Precedence precedence);
static ParseRule *getRule(TokenType type);

// Expressions
static void expression();
static void grouping(bool canAssign);
static void unary(bool canAssign);
static void binary(bool canAssign);
static void variable(bool canAssign);
static void string(bool canAssign);
static void number(bool canAssign);
static void literal(bool canAssign);
static void and_(bool canAssign);
static void or_(bool canAssign);
static void call(bool canAssign);

// Statements
static void statement();
static void declaration();
static void block();
static void printStatement();
static void expressionStatement();
static void ifStatement();
static void whileStatement();
static void forStatement();
static void returnStatement();

// Parser and compiler behaviour

static void initCompiler(Compiler *compiler, FunctionType type)
{
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT)
    {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    Local *local = &current->locals[current->localCount++];
    local->depth = 0;
    local->name.start = "";
    local->name.length = 0;
}

/*
Compile bytecode from source
- Returns a pointer to the function or script which has been compiled.
*/
ObjFunction *compile(const char *source)
{
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF))
    {
        declaration();
    }

    ObjFunction *function = endCompiler();

    return parser.hadError ? NULL : function;
}

static Chunk *currentChunk()
{
    return &current->function->chunk;
}

/* Generate an error at a particular token with the specified message */
static void errorAt(Token *token, const char *message)
{
    if (parser.panicMode)
        return;
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF)
    {
        fprintf(stderr, " at end");
    }
    else if (token->type == TOKEN_ERROR)
    {
        // Nothing
    }
    else
    {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

/* Generate a compile time error at the parser's previous token with the specified message. */
static void error(const char *message)
{
    errorAt(&parser.previous, message);
}

/* Generate an error at the parser's current token with the specified message. */
static void errorAtCurrent(const char *message)
{
    errorAt(&parser.current, message);
}

/*
Advances the parser's current token by a single token.
- Updates the previous token
- If there is an error, performs synchronisation
*/
static void advance()
{
    parser.previous = parser.current;

    for (;;)
    {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR)
            break;

        errorAtCurrent(parser.current.start);
    }
}

/* Consumes a token of the specified type, raising an error if this doesn't match. */
static void consume(TokenType type, const char *message)
{
    if (parser.current.type == type)
    {
        advance();
        return;
    }

    errorAtCurrent(message);
}

/* Check the type of the parser's current token */
static bool check(TokenType type)
{
    return parser.current.type == type;
}

/*
Match a token type without necessarily consuming

`match` returns a bool of whether or not the parser's current token is of the specified type.
The parser is advanced only if there is a match.
*/
static bool match(TokenType type)
{
    if (!check(type))
        return false;
    advance();
    return true;
}

/* Write specified byte to the current chunk. */
static void emitByte(uint8_t byte)
{
    writeChunk(currentChunk(), byte, parser.previous.line);
}

/* Write two bytes in sequence to the current chunk. */
static void emitBytes(uint8_t byte1, uint8_t byte2)
{
    emitByte(byte1);
    emitByte(byte2);
}

/*
Write a jump-type instruction to the current chunk.
- The instruction has a two byte length. Initially, these are both set to
a holding value of `0xff`. The actual jump length is set by `patchJump`.
- Returns the offset of the first jump byte, so that the jump bytes can
be set later.
*/
static int emitJump(uint8_t instruction)
{
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

/*
Write a loop instruction to the current chunk.
*/
static void emitLoop(int loopStart)
{
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX)
        error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

/* In cases where an implicit return is required, write an OP_RETURN instruction byte to the current chunk. */
static void emitReturn()
{
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
}

/*
Write value to the value array and return its array index.
- Checks that the array has space to store value.
*/
static uint8_t makeConstant(Value value)
{
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX)
    {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

/*
Write a constant to the current chunk, consisting of:
1. The op code
2. The stack pointer for the value

This function indirectly calls `addConstant` so the stack is set appropriately.
*/
static void emitConstant(Value value)
{
    emitBytes(OP_CONSTANT, makeConstant(value));
}

/*
Set the jump distance starting at `offset`
*/
static void patchJump(int offset)
{
    // Then block instructions start at (offset + 2)
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX)
    {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

/* Compiler teardown at end of current chunk */
ObjFunction *endCompiler()
{
    emitReturn();
    ObjFunction *function = current->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError)
    {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

static void beginScope()
{
    current->scopeDepth++;
}

static void endScope()
{
    current->scopeDepth--;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth)
    {
        emitByte(OP_POP);
        current->localCount--;
    }
}

// Parsing

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

/*
Parse any expression with a precedence level at or higher than the given level.
*/
static void
parsePrecedence(Precedence precedence)
{
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL)
    {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence)
    {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL))
    {
        error("Invalid assignment target.");
    }
}

/* Fetches the parse rule for the given token type. */
static ParseRule *getRule(TokenType type)
{
    return &rules[type];
}

// EXPRESSIONS
static void expression()
{
    parsePrecedence(PREC_ASSIGNMENT);
}

/* Parse a binary expression */
static void binary(bool canAssign)
{
    // Note the left operand has already been consumed.
    TokenType operatorType = parser.previous.type;
    ParseRule *rule = getRule(operatorType);
    // This then stacks the right operand.
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType)
    {
    case TOKEN_BANG_EQUAL:
        emitBytes(OP_EQUAL, OP_NOT);
        break;
    case TOKEN_EQUAL_EQUAL:
        emitByte(OP_EQUAL);
        break;
    case TOKEN_GREATER:
        emitByte(OP_GREATER);
        break;
    case TOKEN_GREATER_EQUAL:
        emitBytes(OP_LESS, OP_NOT);
        break;
    case TOKEN_LESS:
        emitByte(OP_LESS);
        break;
    case TOKEN_LESS_EQUAL:
        emitBytes(OP_GREATER, OP_NOT);
        break;
    case TOKEN_PLUS:
        emitByte(OP_ADD);
        break;
    case TOKEN_MINUS:
        emitByte(OP_SUBTRACT);
        break;
    case TOKEN_STAR:
        emitByte(OP_MULTIPLY);
        break;
    case TOKEN_SLASH:
        emitByte(OP_DIVIDE);
        break;
    default:
        return; // Unreachable
    }
}

/* Parse a literal value expression */
static void literal(bool canAssign)
{
    switch (parser.previous.type)
    {
    case TOKEN_FALSE:
        emitByte(OP_FALSE);
        break;
    case TOKEN_NIL:
        emitByte(OP_NIL);
        break;
    case TOKEN_TRUE:
        emitByte(OP_TRUE);
        break;
    default:
        return; // Unreachable
    }
}

/* Parse a grouping expression */
static void grouping(bool canAssign)
{
    // Note the initial '(' has already been consumed.
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/* Parse a number literal */
static void number(bool canAssign)
{
    // We use previous here because the parser has already been advanced by one token.
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

/* Parse a string literal */
static void string(bool canAssign)
{
    // As usual, we use previous since the parser has gone "past" the current token.
    // The +1 and -2 trim the beginning and ending quotes.
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

/*
Create an identifier constant
- Returns the index of the identifier constant in the constant table of the chunk.
*/
static uint8_t identifierConstant(Token *name)
{
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token *a, Token *b)
{
    if (a->length != b->length)
        return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name)
{
    for (int i = compiler->localCount - 1; i >= 0; i--)
    {
        Local *local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name))
        {
            if (local->depth == -1)
            {
                error("Can't read local variable in its own initialiser.");
            }
            return i;
        }
    }

    return -1;
}

/*
Add a local variable to the compiler
- Adds local variable to the compiler's `locals` stack
*/
static void addLocal(Token name)
{
    if (current->localCount == UINT8_COUNT)
    {
        error("Too many local variables in function.");
        return;
    }

    Local *local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
}

/*
Declare a variable, i.e. add it to the current scope
- Checks we are in a non-global scope
- Calls `addLocal` with the correct identifier token
*/
static void declareVariable()
{
    if (current->scopeDepth == 0)
        return;

    Token *name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--)
    {
        Local *local = &current->locals[i];
        // A depth of -1 signifies a declared but undefined variable
        if (local->depth != -1 && local->depth < current->scopeDepth)
        {
            break;
        }

        if (identifiersEqual(name, &local->name))
        {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

/*
Parse a variable
- Returns the index of the variable's value in the constant table of the chunk.
*/
static uint8_t parseVariable(const char *errorMessage)
{
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0)
        return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialised()
{
    if (current->scopeDepth == 0)
        return;
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/*
Write bytes to chunk to define a variable
- `global` is the index of the variable name in the chunk's constant table.
*/
static void defineVariable(uint8_t global)
{
    if (current->scopeDepth > 0)
    {
        markInitialised();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void namedVariable(Token name, bool canAssign)
{
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) // This means the variable must be global (or undefined)
    {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    }
    else
    {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL))
    {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    }
    else
    {
        emitBytes(getOp, (uint8_t)arg);
    }
}

/* Parse a variable expression */
static void variable(bool canAssign)
{
    namedVariable(parser.previous, canAssign);
}

/* Parse a unary expression */
static void unary(bool canAssign)
{
    // We use previous here because the parser has been advanced once.
    TokenType operatorType = parser.previous.type;

    // Compile the operand first
    parsePrecedence(PREC_UNARY);

    // Emit the operator's instruction
    switch (operatorType)
    {
    case TOKEN_MINUS:
        emitByte(OP_NEGATE);
        break;
    case TOKEN_BANG:
        emitByte(OP_NOT);
        break;
    default:
        return; // Unreachable
    }
}

/* Parse an "and" expression */
static void and_(bool canAssign)
{
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

/* Parse an "or" expression */
static void or_(bool canAssign)
{
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

/*
Helper to parse a list of arguments in a function call.
- Returns the number of arguments parsed.
*/
static uint8_t argumentList()
{
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN))
    {
        do
        {
            expression();
            if (argCount == 255)
            {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

/* Parse a function call expression */
static void call(bool canAssign)
{
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

// STATEMENTS
static void statement()
{
    if (match(TOKEN_PRINT))
    {
        printStatement();
    }
    else if (match(TOKEN_FOR))
    {
        forStatement();
    }
    else if (match(TOKEN_IF))
    {
        ifStatement();
    }
    else if (match(TOKEN_RETURN))
    {
        returnStatement();
    }
    else if (match(TOKEN_WHILE))
    {
        whileStatement();
    }
    else if (match(TOKEN_LEFT_BRACE))
    {
        beginScope();
        block();
        endScope();
    }
    else
    {
        expressionStatement();
    }
}

static void block()
{
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF))
    {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type)
{
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN))
    {
        do
        {
            current->function->arity++;
            if (current->function->arity > 255)
            {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction *function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));
}

static void returnStatement()
{
    if (current->type == TYPE_SCRIPT)
    {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON))
    {
        emitReturn();
    }
    else
    {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void printStatement()
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void varDeclaration()
{
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL))
    {
        expression();
    }
    else
    {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

static void funDeclaration()
{
    uint8_t global = parseVariable("Expect function name.");
    markInitialised();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void expressionStatement()
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void ifStatement()
{
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE))
        statement();
    patchJump(elseJump);
}

static void whileStatement()
{
    int loopStart = currentChunk()->count;

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}

static void forStatement()
{
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    if (match(TOKEN_SEMICOLON))
    {
        // No initialiser.
    }
    else if (match(TOKEN_VAR))
    {
        varDeclaration();
    }
    else
    {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON))
    {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Exit loop if condition is false.
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN))
    {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);

    if (exitJump != -1)
    {
        patchJump(exitJump);
        emitByte(OP_POP); // Remove condition value from stack.
    }

    endScope();
}

/*
Synchronise the parser after an error

After a parse error, this function consumes tokens until the beginning of a new statement.
*/
static void synchronise()
{
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF)
    {
        if (parser.previous.type == TOKEN_SEMICOLON)
            return;
        switch (parser.current.type)
        {
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:
            return;

        default:; // Do nothing.
        }

        advance();
    }
}

static void declaration()
{
    if (match(TOKEN_FUN))
    {
        funDeclaration();
    }
    else if (match(TOKEN_VAR))
    {
        varDeclaration();
    }
    else
    {
        statement();
    }

    if (parser.panicMode)
        synchronise();
}
