// Slice of the vector
function slice(list, start, length) =
  start < 0 || length <= 0 || start >= len(list)
    ? []
    : [for (i = [0 : min(length, len(list) - start) - 1]) list[start + i]];

function test_slice() =
  let(x = [[1, "x"], [1, "y"], "<=", 5])
  assert(slice(x, 0, len(x) - 2) == [[1, "x"], [1, "y"]])
  1;

// initial part of the vector (all but last element)
function init(list) = slice(list, 0, len(list) - 1);

// tail part of the vector (all but first)
function tail(list) = slice(list, 1, len(list) - 1);

// returns true when list is empty and false otherwise
function is_empty(list) =
  len(list) <= 0;

// adjusts an index so a negative indexes can be used
function adjust_index(index, length) =
  index < 0 && index + length >= 0
    ? index + length
    : index;

// gets a value at a given index
function list_get(list, index) =
  let(size = len(list), idx = adjust_index(index, size))
    idx >= 0 && idx < size
      ? list[idx]
      : undef;

// sets a value at a given index
function list_set(list, index, value) =
  let(size = len(list), idx = adjust_index(index, size))
    size <= 0
      ? []
      : [for (i = [0 : size - 1]) if (i == idx) value else list[i]];

// returns a vector with indexes of a given vector (e.g. [0, 1, 2] for ["a", "b", "c"]).
function indexes(list) =
  is_empty(list)
    ? []
    : [for (index = [0 : len(list) - 1]) index];

// zips two vectors. E.g. zip(["a", "b", "c", "d"], [1, 2, 3]) == [["a", 1], ["b", 2], ["c", 3]]
function zip(list1, list2) =
  is_empty(list1) || is_empty(list2)
    ? []
    : concat([[list1[0], list2[0]]], zip(tail(list1), tail(list2)));

// sum of all elements in a vector
function sum(list) =
  is_empty(list)
    ? 0
    : list[0] + sum(tail(list));

// returns true if all elements of a vector satisfy a given predicate
function list_every(list, predicate) =
  is_empty(list)
    ? true
    : predicate(list[0])
      ? list_every(tail(list), predicate)
      : false;

// returns true if at least one element of a vector satisfies a given predicate
function list_some(list, predicate) =
  is_empty(list)
    ? false
    : predicate(list[0])
      ? true
      : list_some(tail(list), predicate);

// Fold
function list_fold(list, func, acc = undef, index = 0) =
  index >= len(list)
    ? acc
    : list_fold(list, func, func(acc, list[index]), index + 1);

// finds an index
function list_index_of(list, element, start_index = 0, length = undef) =
  let(size = is_num(length) ? length : len(list) - start_index)
  size > 0
    ? list[start_index] == element
      ? start_index
      : list_index_of(list, element, start_index + 1, size - 1)
    : undef;

function test_index_of() =
  assert(list_index_of([1, 2, 3, 4, 5], 5) == 4)
  assert(list_index_of([1, 2, 3, 4, 5], 5, start_index = 1, length = 3) == undef)
  1;

function list_contains(list, element) =
  is_num(list_index_of(list, element));

//
// Min/max
//

// finds an element with minimal key
function list_min_by(list, key_func) =
  len(list) <= 0
    ? undef
    : list_fold(list,
        function(acc, item) key_func(acc) <= key_func(item) ? acc : item,
        list[0],
        1);

// finds an element with maximal key
function list_max_by(list, key_func) =
  list_min_by(list, function(v) -key_func(v));

function test_list_lookup() =
  assert(list_min_by([1, -2, 3, -4, 5], function(x) abs(x)) == 1)
  assert(list_max_by([1, -5, 3, -4, 5], function(x) abs(x)) == -5)
  1;

//
// Sorting
//

// Insertion sort
function __private_insert(x, list, key_func) =
  is_empty(list)
    ? [x]
    : key_func(x) < key_func(list[0])
      ? concat([x], list)
      : concat([list[0]], __private_insert(x, tail(list), key_func));

function list_sort_by_key(list, key_func) =
  is_empty(list)
    ? list
    : __private_insert(list[0], list_sort_by_key(tail(list), key_func), key_func);

function test_sort() =
  assert(list_sort_by_key([], function(x) x) == [])
  assert(list_sort_by_key([3, -4, -1, 2], function(x) abs(x)) == [-1, 2, 3, -4])
  1;

//
// Distinct
//

function list_distinct(list) =
  is_empty(list)
    ? []
    : [for (index = [0: len(list) - 1])
        if (is_undef(list_index_of(list, list[index], start_index = 0, length = index)))
          list[index]
      ];

function test_list_distinct() =
  assert(list_distinct([]) == [])
  assert(list_distinct([10, 10, 10, 10]) == [10])
  assert(list_distinct([3, -4, -1, 2]) == [3, -4, -1, 2])
  assert(list_distinct([3, -4, 3, 2, 3, -4, 1]) == [3, -4, 2, 1])
  1;
