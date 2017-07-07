IML
===

This is an experiment on implementing an interpreter for a dialect of the
Imperative Mini-Language (IML).

Example Program
---------------

The following code:

.. code-block:: text

  quo <- 0
  rem <- 42
  while(rem >= 6) {
    quo <- (quo + 1)
    rem <- (rem - 6)
  }

Produces the following (prettified) token stream:

.. code-block:: text

  IDENTIFIER quo, BECOMES, NUMERIC 0,
  IDENTIFIER rem, BECOMES, NUMERIC 42,
  WHILE, LEFTPAREN, IDENTIFIER rem, RELATIONALOPERATOR GreaterThanOrEqual, NUMERIC 6, LEFTCURLY,
    IDENTIFIER quo, BECOMES, LEFTPAREN, IDENTIFIER quo, ARITHMETICOPERATOR Plus, NUMERIC 1,
    IDENTIFIER rem, BECOMES, LEFTPAREN, IDENTIFIER rem, ARITHMETICOPERATOR Minus, NUMERIC 6,
  RIGHTCURLY

