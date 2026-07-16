// advent of code 2025-10, the buttons
// using Z3 solver directly
//
// zig 0.15.2
//

const std = @import("std");
const Io = std.Io;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const Prob = struct {
    lights: []const u8,
    wirings: []const []const u32,
    joltage: []const u32,

    pub fn deinit(self: *Prob) void {
        for (self.wirings) |w|
            allocator.free(w);
        allocator.free(self.lights);
        allocator.free(self.joltage);
    }
};

const c = @cImport({
    @cInclude("z3.h");
});

fn listOfInts(str: []const u8) ![]const u32 {
    var out: std.ArrayList(u32) = .empty;
    var it = std.mem.splitScalar(u8, str, ',');
    while (it.next()) |w| {
        try out.append(allocator, try std.fmt.parseInt(u32, w, 10));
    }
    return try out.toOwnedSlice(allocator);
}

fn parseLights(str: []const u8) ![]const u8 {
    return try allocator.dupe(u8, str);
}

fn parseJoltage(str: []const u8) ![]const u32 {
    return try listOfInts(str);
}

fn parseWiring(str: []const u8) ![]const u32 {
    return try listOfInts(str);
}

pub fn parseProblem(line: []const u8) !Prob {
    var pos: usize = 0;
    while (pos < line.len and std.ascii.isWhitespace(line[pos]))
        pos += 1;

    var lights: []const u8 = undefined;
    var wirings: std.ArrayList([]const u32) = .empty;
    var joltage: []const u32 = undefined;

    var it = std.mem.splitScalar(u8, line, ' ');
    while (it.next()) |w| {
        if (w.len == 0) continue;
        if (w[0] == '[') {
            lights = try parseLights(w[1 .. w.len - 1]);
        } else if (w[0] == '{') {
            joltage = try parseJoltage(w[1 .. w.len - 1]);
        } else if (w[0] == '(') {
            try wirings.append(allocator, try parseWiring(w[1 .. w.len - 1]));
        }
    }

    const res: Prob = .{
        .lights = lights,
        .wirings = try wirings.toOwnedSlice(allocator),
        .joltage = joltage,
    };
    return res;
}

pub fn readProblems(filename: []const u8) ![]const Prob {
    var problems: std.ArrayList(Prob) = .empty;

    const file = try std.fs.openFileAbsolute(filename, .{});
    defer file.close();

    var buf: [4096]u8 = undefined;
    var reader = file.reader(&buf);

    while (try reader.interface.takeDelimiter('\n')) |line| {
        const prob = try parseProblem(line);
        std.debug.print("lights: {any}\n", .{prob.lights});
        std.debug.print("joltage: {any}\n", .{prob.joltage});
        for (prob.wirings) |w| {
            std.debug.print(" W: {any}\n", .{w});
        }
        try problems.append(allocator, prob);
    }

    return try problems.toOwnedSlice(allocator);
}

fn extractSolution(ctx: c.Z3_context, model: c.Z3_model, vars: std.ArrayList(c.Z3_ast)) usize {
    var sum: usize = 0;

    for (vars.items) |v| {
        var value_ast: c.Z3_ast = undefined;
        var value: c_int = undefined;
        if (c.Z3_model_eval(ctx, model, v, true, &value_ast)) {
            if (c.Z3_get_numeral_int(ctx, value_ast, &value)) {
                sum += @intCast(value);
            }
        }
    }

    return sum;
}

fn z3solve(ctx: c.Z3_context, prob: Prob) !usize {
    const solver = c.Z3_mk_solver(ctx);
    const int_sort = c.Z3_mk_int_sort(ctx);

    c.Z3_solver_inc_ref(ctx, solver);
    defer c.Z3_solver_dec_ref(ctx, solver);

    // N variables, one for each wirings
    var vars: std.ArrayList(c.Z3_ast) = .empty;
    // times clicked

    const zero = c.Z3_mk_int(ctx, 0, int_sort);

    for (0..prob.wirings.len) |i| {
        const name = try std.fmt.allocPrintSentinel(allocator, "c{}", .{i}, 0);
        defer allocator.free(name);
        const sym = c.Z3_mk_string_symbol(ctx, name);
        const v = c.Z3_mk_const(ctx, sym, int_sort);
        try vars.append(allocator, v);

        c.Z3_solver_assert(ctx, solver, c.Z3_mk_ge(ctx, v, zero));
    }

    for (prob.joltage, 0..) |jolt, ji| {
        var terms: std.ArrayList(c.Z3_ast) = .empty;
        defer terms.deinit(allocator);

        for (prob.wirings, 0..) |w, i| {
            if (std.mem.indexOfScalar(u32, w, @intCast(ji)) != null) {
                try terms.append(allocator, vars.items[i]);
            }
        }

        const sum = c.Z3_mk_add(ctx, @intCast(terms.items.len), terms.items.ptr);
        c.Z3_solver_assert(ctx, solver, c.Z3_mk_eq(ctx, sum, c.Z3_mk_int(ctx, @intCast(jolt), int_sort)));
    }

    var solution: ?usize = null;

    while (c.Z3_solver_check(ctx, solver) == c.Z3_L_TRUE) {
        const model = c.Z3_solver_get_model(ctx, solver);
        c.Z3_model_inc_ref(ctx, model);
        defer c.Z3_model_dec_ref(ctx, model);
        std.debug.print("SAT {s}\n", .{c.Z3_model_to_string(ctx, model)});
        const s = extractSolution(ctx, model, vars);
        solution = s;
        std.debug.print("solution = {?}\n", .{solution});

        // add blocker:
        // x1+x2+...  < this_solution
        const sum = c.Z3_mk_add(ctx, @intCast(vars.items.len), vars.items.ptr);
        c.Z3_solver_assert(ctx, solver, c.Z3_mk_lt(ctx, sum, c.Z3_mk_int(ctx, @intCast(s), int_sort)));
    }

    if (solution) |best| {
        std.debug.print("SOLVED AS {}\n", .{best});
        return best;
    }
    return error.Unsolveable;
}

pub fn main() !void {
    const problems = try readProblems("/proj/mine/advent-of-code/resources/2025/day10.txt");
    // const problems = try readProblems("/proj/mine/advent-of-code/resources/2025/day10.sample.txt");
    defer allocator.free(problems);

    std.debug.print("got {} problems!\n", .{problems.len});

    const cfg = c.Z3_mk_config();
    const ctx = c.Z3_mk_context(cfg);

    var overall_score: usize = 0;
    for (problems, 0..) |p, i| {
        std.debug.print("Solving {}:\n", .{i});
        overall_score += try z3solve(ctx, p);
    }
    std.debug.print("Totes: {}\n", .{overall_score});
}
