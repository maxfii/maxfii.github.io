// Ref: true
// Test break in set rule.
// Should output `Hi` in blue.
#for i in range(10) {
  [Hello]
  set text(blue, ..break)
  [Not happening]
}

