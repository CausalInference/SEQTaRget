---
name: Bug report
about: Create a report to help us improve SEQuential
title: BUG
labels: bug
assignees: ryan-odea

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Minimal Working Example to reproduce the error in codeblock form:
```r
# My code to produce an error
df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
df$z <- df$x + df$y
```
**Console Output**
If applicable, add console output or errors given in codeblocked format:
```r
Error in df$x + df$y : non-numeric argument to binary operator
```

**Expected behavior**
A clear and concise description of what you expected to happen.

**Version Congruity:**
 - OS: [e.g. iOS]
 - R version [e.g. 4.2.1]

**Additional context**
Add any other context about the problem here.
