// Test multiple calls.
#{
  let f(b, c: "!") = b + c
  let g(a, ..sink) = a + f(..sink)
  test(g("a", "b", c: "c"), "abc")
}

