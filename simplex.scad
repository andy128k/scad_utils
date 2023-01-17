include <./lists.scad>;

EPS = 0.0001;

function eq(a, b) = abs(a - b) < EPS;

function tableau_empty(variables, constraints) =
  assert(variables >= 0)
  assert(constraints >= 0)
  [for (r = [0 : constraints]) [for (c = [0 : variables]) 0]];

function tableau_width(tableau) =
  len(tableau[0]);

function tableau_height(tableau) =
  len(tableau);

function tableau_zero_column(tableau, column) =
  [for (row = tableau) list_set(row, column, 0)];

function tableau_goal_value(tableau) =
  let(last_column = tableau_width(tableau) - 1)
  tableau[0][last_column];

function tableau_set_goal_value(tableau, goal) =
  list_set(tableau, 0,
    list_set(tableau[0], -1, goal));

function tableau_run(tableau, start_row) =
  let(pivot_column = tableau_find_pivot_column(tableau, start_row))
  is_num(pivot_column)
    ? let(pivot_row = tableau_find_pivot_row(tableau, pivot_column, start_row))
      is_num(pivot_row)
        ? tableau_run(tableau_next(tableau, pivot_column, pivot_row), start_row)
        : [tableau, false]
    : [tableau, true];

function tableau_is_valid_pivot_column(tableau, column, start_row) =
  start_row < len(tableau)
    ? tableau[start_row][column] > 0 || tableau_is_valid_pivot_column(tableau, column, start_row + 1)
    : false;

function tableau_find_valid_pivot_column(tableau, start_row, start_col) =
  start_col >= tableau_width(tableau) - 1
    ? undef
    : tableau[0][start_col] < 0 && tableau_is_valid_pivot_column(tableau, start_col, start_row)
      ? start_col
      : tableau_find_valid_pivot_column(tableau, start_row, start_col + 1);

function tableau_find_pivot_column(tableau, start_row) =
  let(pivot_column = tableau_find_valid_pivot_column(tableau, start_row, 0))
    is_num(pivot_column)
      ? pivot_column
      : let(minc = list_min_by(
            [for(c = [0: tableau_width(tableau) - 2]) c],
            function(index) tableau[0][index]
          ))
          tableau[0][minc] < 0
            ? minc
            : undef;

function tableau_find_pivot_row(tableau, entry_var, start_row) =
  let(height = tableau_height(tableau))
  let(last_column = tableau_width(tableau) - 1)
  let(rows = [for (row = [start_row : height - 1]) if (tableau[row][entry_var] > 0) row])
  list_min_by(rows, function(row) tableau[row][last_column] / tableau[row][entry_var]);

function tableau_next(tableau, pivot_column, pivot_row) =
  let(pivot = tableau[pivot_row][pivot_column])
  let(width = tableau_width(tableau))
  let(height = tableau_height(tableau))
  let(pr = tableau[pivot_row] / pivot)
  [for (row = [0: height - 1])
    if (row == pivot_row)
      pr
    else
      tableau[row] - (tableau[row][pivot_column] * pr)
  ];

function tableau_get_base_row(tableau, variable) =
  let(rows = [
    for (row = [1: tableau_height(tableau) - 1])
      let(v = tableau[row][variable])
      if (!eq(v, 0))
        (eq(v, 1) ? row : undef)
  ])
  len(rows) == 1
    ? rows[0] // may be undef
    : undef;

function tableau_get_values(tableau) =
  let(last_column = tableau_width(tableau) - 1)
  let(rows = [for(index = [0: last_column - 1]) tableau_get_base_row(tableau, index)])
  let(rows_uniq = [for (index = [0: len(rows) - 1])
    is_num(list_index_of(rows, rows[index], start_index = 0, length = index))
      ? undef
      : rows[index]
  ])
  [for(row = rows_uniq) is_num(row) ? tableau[row][last_column] : 0];

function simplex_flip_comparison(cmp) =
  cmp == "<=" ? ">=" :
  cmp == ">=" ? "<=" :
                "=";

function simplex_constraint_lhs(constraint) =
  slice(constraint, 0, len(constraint) - 2);

function simplex_constraint_comparison(constraint) =
  list_get(constraint, -2);

function simplex_constraint_rhs(constraint) =
  list_get(constraint, -1);

function simplex_ensure_rhs_non_negative(constraint) =
  let(rhs = simplex_constraint_rhs(constraint))
    rhs >= 0
      ? constraint
      : concat(
          linear_form_negate(simplex_constraint_lhs(constraint)),
          [simplex_flip_comparison(simplex_constraint_comparison(constraint))],
          [-rhs]
        );

function simplex_constraint_extract_variables(linear_form) =
  [for (item = linear_form) item[1]];

function gen_variable(type) =
  str("__simplex_", type, floor(rands(0, 1000000, 1)[0]));

function simplex_normalize_constraint(constraint) =
  let(lhs = simplex_constraint_lhs(constraint))
  let(cmp = simplex_constraint_comparison(constraint))
  let(rhs = simplex_constraint_rhs(constraint))
    cmp == "<=" ? let(slack = gen_variable("slack"))
                  [
                    concat(lhs, [[1.0, slack]]),
                    rhs,
                    slack,
                    undef,
                  ] :
    cmp == ">=" ? let(slack = gen_variable("slack"))
                  let(surplus = gen_variable("surplus"))
                  [
                    concat(lhs, [[-1.0, slack], [1.0, surplus]]),
                    rhs,
                    slack,
                    surplus,
                  ] :
    /* = */       let(surplus = gen_variable("surplus"))
                  [
                    concat(lhs, [[1.0, surplus]]),
                    rhs,
                    undef,
                    surplus,
                  ];

function simplex_normalize_constraints(constraints) =
  [for (constraint = constraints)
    simplex_normalize_constraint(simplex_ensure_rhs_non_negative(constraint))];

function simplex_phase1_remove_variable(is_surplus_variable, tableau, col_index) =
  let(remove = is_surplus_variable
    ? is_undef(tableau_get_base_row(tableau, col_index))
    : tableau[0][col_index] > 0
  )
  remove ? tableau_zero_column(tableau, col_index) : tableau;

function simplex_phase1(variables, surplus_variables, tableau) =
  let(r1 = tableau_run(tableau, 2))
    !r1[1] || !eq(tableau_goal_value(r1[0]), 0)
      ? undef
      : tail(
          list_fold(
            indexes(variables),
            function(tableau, col_index) simplex_phase1_remove_variable(list_contains(surplus_variables, variables[col_index]), tableau, col_index),
            r1[0]
          )
        );

function linear_form_get_coeff(linear_form, var) =
  list_fold(linear_form,
    function(acc, item) acc + (item[1] == var ? item[0] : 0),
    0);

function linear_form_negate(linear_form) =
  [for(item = linear_form) [-item[0], item[1]]];

function make_row(lhs, variables, rhs) =
  concat(
    [for (var = variables) linear_form_get_coeff(lhs, var)],
    [rhs]
  );

function phase1_row(normalized_constraints, variables) =
  let(phase1_goal = [for(nc = normalized_constraints) if (!is_undef(nc[3])) each [for(m = nc[0]) if (m[1] != nc[3]) m]])
  let(goal_value = sum([for(nc = normalized_constraints) if (!is_undef(nc[3])) nc[1]]))
  make_row(phase1_goal, variables, goal_value);

function simplex_maximize(goal, constraints) =
  let(normal_variables = list_distinct(concat(
      simplex_constraint_extract_variables(goal),
      [for (constraint = constraints)
        each simplex_constraint_extract_variables(simplex_constraint_lhs(constraint))])))

  let(normalized_constraints = simplex_normalize_constraints(constraints))
  let(slack_variables = [for (nc = normalized_constraints) if (!is_undef(nc[2])) nc[2]])
  let(surplus_variables = [for (nc = normalized_constraints) if (!is_undef(nc[3])) nc[3]])

  let(variables = concat(normal_variables, slack_variables, surplus_variables))

  // initial tableau
  let(tableau = concat(
    [-make_row(goal, variables, 0)], // Maximize goal, so negate coeffs
    [for (nc = normalized_constraints) make_row(nc[0], variables, nc[1])]
  ))
  // first phase
  let(p1 = !is_empty(surplus_variables)
    ? let(phase1_tableau = concat(
        [-phase1_row(normalized_constraints, variables)],
        tableau
      ))
      simplex_phase1(variables, surplus_variables, phase1_tableau)
    : tableau)

  is_undef(p1)
    ? undef // first phase failed
    : // Second phase
      let(p2 = tableau_run(p1, 1))
      !p2[1]
        ? undef // second phase failed
        : let(t = p2[0])
          [
            tableau_goal_value(t),
            zip(normal_variables, tableau_get_values(t))
          ];

function simplex_minimize(goal, constraints) =
  let(r = simplex_maximize(linear_form_negate(goal), constraints))
  is_undef(r)
    ? undef
    : [-r[0], r[1]];

function test_tableau() =
  // tableau_empty
  assert(tableau_empty(0, 0) == [[0]])
  assert(tableau_empty(2, 3) == [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]])

  let(
    empty = [[0]],
    tableau1 = [
      [11, 12, 13, 14],
      [21, 22, 23, 24],
      [31, 32, 33, 34],
    ]
  )

  // tableau width/height
  assert(tableau_width(empty) == 1)
  assert(tableau_height(empty) == 1)
  assert(tableau_width(tableau1) == 4)
  assert(tableau_height(tableau1) == 3)

  // tableau_zero_column
  assert(tableau_zero_column(tableau1, 0) == [
    [0, 12, 13, 14],
    [0, 22, 23, 24],
    [0, 32, 33, 34],
  ])
  assert(tableau_zero_column(tableau1, 1) == [
    [11, 0, 13, 14],
    [21, 0, 23, 24],
    [31, 0, 33, 34],
  ])

  // tableau_goal_value
  assert(tableau_goal_value(empty) == 0)
  assert(tableau_goal_value(tableau1) == 14)

  // tableau_set_goal_value
  assert(tableau_set_goal_value(tableau1, 200) == [
    [11, 12, 13, 200],
    [21, 22, 23, 24],
    [31, 32, 33, 34],
  ])

  1;

function test_tableau_find_valid_pivot_column() =
  let(tableau = [
    [0, 0, 0, 1, -1, 1, 0, 0, 0, -9],
    [1, 0, 0, -1, 1, 0, 0, 0, 0, 2],
    [0, 1, 0, 0, -1, 0, 0, 0, 0, 3],
    [0, 0, 1, 0, 1, -1, 0, 0, 0, 4]
  ])
  let(pivot_column = tableau_find_valid_pivot_column(tableau, 1, 0))
  assert(pivot_column == 4)
  1;

function test_simplex_1() =
  let(solution = simplex_maximize(
    [[1, "x"], [1, "y"], [1, "z"]],
    [
      [[1, "x"], [1, "y"], "<=", 5],
      [[1, "y"],           "<=", 3],
      [[1, "y"], [1, "z"], "<=", 7],
    ]
  ))
  assert(solution == [12, [["x", 5], ["y", 0], ["z", 7]]])
  1;

function test_simplex_2() =
  let(solution = simplex_minimize(
    [[1, "x"], [1, "y"], [1, "z"]],
    [
      [[1, "x"], [1, "y"], ">=", 5],
      [[1, "y"],           ">=", 3],
      [[1, "y"], [1, "z"], ">=", 7],
    ]
  ))
  assert(solution == [7, [["x", 0], ["y", 5], ["z", 2]]])
  1;

function test_simplex_3() =
  let(solution = simplex_maximize(
    [[-1, "x"], [-1, "y"], [-1, "z"]],
    [
      [[1, "x"], ">=", 5],
      [[1, "y"], ">=", 3],
      [[1, "z"], ">=", 7],
      [[1, "z"], "<=", 700],
    ]
  ))
  assert(solution == [-15, [["x", 5], ["y", 3], ["z", 7]]])
  1;

function test_simplex_single() =
  let(solution = simplex_maximize(
    [[1, "x"]],
    [
      [[1, "x"], "<=", 3],
    ]
  ))
  assert(solution == [3, [["x", 3]]])
  1;

function test_simplex_degraged() =
  let(solution = simplex_maximize(
    [[1, "x"], [1, "y"]],
    [
      [[1, "x"], [1, "y"], "<=", 2],
    ]
  ))
  assert(solution == [2, [["x", 2], ["y", 0]]])
  1;

function test_simplex_unbound() =
  let(solution = simplex_maximize(
    [[1, "x"]],
    [
      [[1, "x"], ">=", 3],
    ]
  ))
  assert(is_undef(solution))
  1;

function test_simplex_cycle() =
  let(solution = simplex_maximize(
    [[10, "x1"], [-57, "x2"], [-9, "x3"], [-24, "x4"]],
    [
      [[0.5, "x1"], [-5.5, "x2"], [-2.5, "x3"], [9, "x4"], "<=", 0],
      [[0.5, "x1"], [-1.5, "x2"], [-0.5, "x3"], [1, "x4"], "<=", 0],
      [[1, "x1"],                                          "<=", 1],
    ]
  ))
  assert(solution == [1, [["x1", 1], ["x2", 0], ["x3", 1], ["x4", 0]]])
  1;

function test_simplex_cycle2() =
  let(solution = simplex_maximize(
    [[10, "x1"], [-57, "x2"], [-9, "x3"], [-24, "x4"]],
    [
      [[0.5, "x1"], [-5, "x2"], [-2, "x3"], [9, "x4"], "<=", 0],
      [[0.5, "x1"], [-1, "x2"], [ 1, "x3"], [1, "x4"], "<=", 0],
      [[1, "x1"],                                      "<=", 1],
    ]
  ))
  assert(solution == [0, [["x1", 0], ["x2", 0], ["x3", 0], ["x4", 0]]])
  1;

function test_simplex_unfeasible_solution() =
  let(solution = simplex_maximize(
    [[-1, "x1"]],
    [
      [[2, "x2"], "=", 0],
      [[10, "x2"], "=", 10],
    ]
  ))
  assert(is_undef(solution))
  1;

function test_simplex_ray() =
  let(solution = simplex_maximize(
    [[-1, "x1"]],
    [
      [[0.5, "x2"], "=", 10],
    ]
  ))
  assert(solution == [0, [["x1", 0], ["x2", 20]]])
  1;

function test_simplex_point() =
  let(solution = simplex_maximize(
    [[-1, "x1"], [-1, "x2"]],
    [
      [[1, "x1"], "=", 4],
      [[1, "x2"], "=", 5],
    ]
  ))
  assert(solution == [-9, [["x1", 4], ["x2", 5]]])
  1;

function test_simplex_eq() =
  let(solution = simplex_maximize(
    [[8, "x1"], [2, "x2"], [7, "x3"], [3, "x4"], [6, "x5"], [4, "x6"]],
    [
      [[1, "x1"], [1, "x3"], [1, "x5"], "=", 23],
      [[1, "x2"], [1, "x4"], [1, "x6"], "=", 23],
      [[1, "x1"],                       "=", 10],
      [[1, "x3"],                       "=", 8],
      [[1, "x5"],                       "=", 5],
    ]
  ))
  assert(solution == [258, [["x1", 10], ["x2", 23], ["x3", 8], ["x4", 0], ["x5", 5], ["x6", 0]]])
  1;

function test_simplex_degeneracy() =
  let(solution = simplex_maximize(
    [[-8, "x1"], [-7, "x2"]],
    [
      [[1, "x1"], [1, "x2"], "<=", 18],
      [[1, "x1"],            ">=", 10],
      [[1, "x2"],            ">=", 8],
    ]
  ))
  assert(solution == [-136, [["x1", 10], ["x2", 8]]])
  1;
