const std = @import("std");

const Ptk = struct {
    const ptk = @import("ptk");

    pub const Token = Tokenizer.Token;

    pub const TokenType = enum {
        whitespace,

        token,
        kw_whitespace,
        identifier,
        @"::=",
        @";",
        literal,
    };

    pub const Pattern = ptk.Pattern(TokenType);

    pub const Tokenizer = ptk.Tokenizer(TokenType, &[_]Pattern{
        .create(.whitespace, ptk.matchers.whitespace),

        .create(.token, ptk.matchers.literal("token")),
        .create(.kw_whitespace, ptk.matchers.literal("whitespace")),
        .create(.identifier, ptk.matchers.identifier),
        .create(.@"::=", ptk.matchers.literal("::=")),
        .create(.@";", ptk.matchers.literal(";")),
        .create(.literal, ptk.matchers.sequenceOf(.{
            ptk.matchers.literal("\""),
            ptk.matchers.takeNoneOf("\""),
            ptk.matchers.literal("\""),
        })),
        .create(.literal, ptk.matchers.sequenceOf(.{
            ptk.matchers.literal("'"),
            ptk.matchers.takeNoneOf("'"),
            ptk.matchers.literal("'"),
        })),
    });

    pub const Parser = ptk.ParserCore(Tokenizer, .{.whitespace});

    pub const ruleset = ptk.RuleSet(TokenType);

    pub fn tokenize(comptime rules: []const u8) []const Token {
        comptime var tokenizer = Tokenizer.init(rules, null);

        return comptime tokenize_rec(&tokenizer, &.{}) orelse @compileError("Syntax error");
    }

    fn tokenize_rec(comptime tokenizer: *Tokenizer, comptime start: []const Token) ?[]const Token {
        const token = tokenizer.next() catch return null;

        if (token == null) {
            return start;
        }

        const res: []const Token = if (token.?.type != .whitespace) &.{token.?} else &.{};

        return comptime tokenize_rec(tokenizer, start ++ res);
    }
};

pub const Ast = struct {
    pub const Rule = struct {
        token: bool,
        name: []const u8,
        pattern: []const Expr,
    };
    pub const Expr = union(enum) {
        rule: Rule,
        lit: []const u8,
    };
};

fn parse_rules(comptime rules: []const u8) []const Ast.Rule {
    comptime var tokenizer = Ptk.Tokenizer.init(rules, null);

    comptime var parser = Ptk.Parser.init(&tokenizer);

    comptime var data = Data{};

    while (true) {
        const rule = comptime accept_rule(&parser, &data) catch break;
        data.rules = data.rules ++ &[_]Ast.Rule{rule};
    }

    if (parser.peek() catch @compileError("Syntax error")) |token| {
        @compileError("Syntax error: remaining token '" ++ token.text ++ "'");
    }

    return data.rules;
}

const Data = struct {
    rules: []const Ast.Rule = &.{},
};

const AcceptError = Ptk.Parser.AcceptError || error{
    Invalid,
};

fn accept_rule(comptime parser: *Ptk.Parser, data: *Data) AcceptError!Ast.Rule {
    const state = parser.saveState();
    errdefer parser.restoreState(state);

    // Will be set to false if 'token' is not found
    comptime var token = true;
    _ = parser.accept(Ptk.ruleset.is(.token)) catch |err| switch (err) {
        error.EndOfStream => return error.Invalid,
        error.UnexpectedToken => token = false,
        else => @compileError("Syntax error"),
    };

    const name = try parser.accept(Ptk.ruleset.is(.identifier));
    _ = try parser.accept(Ptk.ruleset.is(.@"::="));

    comptime var exprs: []const Ast.Expr = &.{};

    while ((parser.peek() catch @compileError("Syntax error") orelse @compileError("Syntax error")).type != .@";") {
        const expr = accept_expr(parser, data) catch |err| switch (err) {
            error.Invalid => @compileError("Invalid expression start"),
            else => return err,
        };
        exprs = exprs ++ &[_]Ast.Expr{expr};
    }

    _ = try parser.accept(Ptk.ruleset.is(.@";"));

    return Ast.Rule{
        .token = token,
        .name = name.text,
        .pattern = exprs,
    };
}

fn get_rule(comptime name: []const u8, data: *Data) Ast.Rule {
    return inline for (data.rules) |rule| {
        if (std.mem.eql(u8, name, rule.name)) {
            break rule;
        }
    } else @compileError("Undefined rule: " ++ name);
}

fn accept_expr(comptime parser: *Ptk.Parser, data: *Data) AcceptError!Ast.Expr {
    const state = parser.saveState();
    errdefer parser.restoreState(state);

    const next = parser.peek() catch @compileError("Syntax error") orelse return error.Invalid;

    switch (next.type) {
        .identifier => {
            const name = try parser.accept(Ptk.ruleset.is(.identifier));
            return .{ .rule = get_rule(name.text, data) };
        },
        .literal => {
            const lit = try parser.accept(Ptk.ruleset.is(.literal));
            return .{ .lit = lit.text[1..lit.text.len - 1] };
        },
        else => return error.Invalid,
    }
}

fn get_tokens(comptime rules: []const Ast.Rule) []const Ast.Rule {
    if (rules.len == 0) return &.{};

    if (rules[0].token) return &[_]Ast.Rule{rules[0]} ++ get_tokens(rules[1..])
    else return get_tokens(rules[1..]);
}

fn token_types(comptime tokens: []const Ast.Rule) []const std.builtin.Type.EnumField {
    comptime var fields = [_]std.builtin.Type.EnumField{undefined} ** (tokens.len + 1);

    inline for (tokens, 0..) |token, idx| {
        fields[idx] = .{
            .name = @ptrCast(token.name ++ &[_]u8{0}),
            .value = idx,
        };
    }

    fields[tokens.len] = .{
        .name = "whitespace",
        .value = tokens.len,
    };

    return &fields;
}

fn patterns(comptime TokenType: type, comptime tokens: []const Ast.Rule) []const Ptk.ptk.Pattern(TokenType) {
    const Pattern = Ptk.ptk.Pattern(TokenType);

    comptime var result: []const Pattern = &[_]Pattern{
        .create(.whitespace, Ptk.ptk.matchers.whitespace),
    };

    inline for (tokens) |token| {
        result = result ++ patterns_single(TokenType, token);
    }

    return result;
}

fn patterns_single(comptime TokenType: type, comptime token: Ast.Rule) []const Ptk.ptk.Pattern(TokenType) {
    const Pattern = Ptk.ptk.Pattern(TokenType);
    const token_type = std.meta.stringToEnum(TokenType, token.name).?;

    if (token.pattern.len == 1) {
        comptime var result: []const Pattern = &.{};
        inline for (matchers(token.pattern[0])) |matcher| {
            result = result ++ &[_]Pattern{.create(token_type, matcher)};
        }
        return result;
    } else {
        comptime var result_matchers: []const Ptk.ptk.Matcher = matchers(token.pattern[0]);

        for (token.pattern[1..]) |expr| {
            comptime var new_result: []const Ptk.ptk.Matcher = matchers(token.pattern[0]);
            const expr_matchers = matchers(expr);

            for (result_matchers) |prev_matcher| {
                for (expr_matchers) |new_matcher| {
                    new_result = new_result ++ &[_]Ptk.ptk.Matcher{Ptk.ptk.matchers.sequenceOf(.{
                        prev_matcher,
                        new_matcher,
                    })};
                }
            }

            result_matchers = new_result;
        }

        comptime var result: []const Pattern = &.{};
        inline for (result_matchers) |matcher| {
            result = result ++ &[_]Pattern{.create(token_type, matcher)};
        }
        return result;
    }
}

fn matchers(comptime expr: Ast.Expr) []const Ptk.ptk.Matcher {
    return switch (expr) {
        .lit => |lit| &[_]Ptk.ptk.Matcher{Ptk.ptk.matchers.literal(lit)},

        else => @compileError(@tagName(expr) ++ " is not supported in tokens"),
    };
}

pub fn Parser(comptime rules: []const u8) type {
    @setEvalBranchQuota(std.math.maxInt(u32));

    const result = comptime parse_rules(rules);

    const tokens = get_tokens(result);

    const enum_fields = token_types(tokens);
    const token_type = std.builtin.Type.Enum{
        .decls = &.{},
        .fields = enum_fields,
        .is_exhaustive = true,
        .tag_type = std.meta.Int(.unsigned, @intCast(enum_fields.len)),
    };
    const TokenTypeT = @Type(.{ .@"enum" = token_type });
    const TokenizerT = Ptk.ptk.Tokenizer(TokenTypeT, patterns(TokenTypeT, tokens));

    return struct {
        pub const TokenType = TokenTypeT;
        pub const Pattern = Ptk.ptk.Pattern(TokenType);
        pub const Tokenizer = TokenizerT;
    };
}
