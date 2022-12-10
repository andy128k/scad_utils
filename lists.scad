// Slice of the vector
function slice(list, start, length) =
  start < 0 || length <= 0 || start >= len(list)
    ? []
    : [for (i = [0 : min(length, len(list) - start) - 1]) list[start + i]];

// initial part of the vector (all but last element)
function init(list) = slice(list, 0, len(list) - 1);

// tail part of the vector (all but first)
function tail(list) = slice(list, 1, len(list) - 1);

// returns true when list is empty and false otherwise
function is_empty(list) =
  len(list) <= 0;

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

// returns true if all elements of a vector are equal to a given value and false otherwise
function all_equal(list, value) =
  is_empty(list)
    ? true
    : list[0] == value
      ? all_equal(tail(list), value)
      : false;

// if all elements of a vector are equal, the first element is returned and undef is returned otherwise
function same(list) =
  is_empty(list)
    ? undef
    : all_equal(tail(list), list[0])
      ? list[0]
      : undef;
