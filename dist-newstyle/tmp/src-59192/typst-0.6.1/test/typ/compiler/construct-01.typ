// Ensure that constructor styles win, but not over outer styles.
// The outer paragraph should be right-aligned,
// but the B should be center-aligned.
#set list(marker: [>])
#list(marker: [--])[
  #rect(width: 2cm, fill: green, inset: 4pt, list[A])
]

