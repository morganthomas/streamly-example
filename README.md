I hypothesized this example would produce this output:

```
("R",1)
("G",1)
("B",1)
("R",2)
("G",2)
("B",2)
("R",3)
("G",3)
("B",3)
("R",4)
("G",4)
("B",4)
```

In fact it produces this output:

```
("R",1)
("G",2)
("B",3)
("R",4)
```

The branch `fix` shows one possible way to make it work as intended.
