
{--

6.1 execution traces and logical time

                                       +++++++++++++++++++++
                                       +  a = snoc empty 0 +    (future 3)
                                       +++++++++++++++++++++
                                               |
                                               V
                                       +++++++++++++++++++++
                                       +    b = snoc a 1   +    (future 3)
                                       +++++++++++++++++++++
                                           |          |
                                           V          V
                                 ++++++++++++++++  ++++++++++++++++++
                    (future 2)   +  c = tail b  +  +  c = snoc b 2  +   (future 2)shared cost
                                 ++++++++++++++++  ++++++++++++++++++
                                   /          \      /           \
                                  /            \    /             \
                  +++++++++++++++           ++++++++++++++         ++++++++++++++++
                  +  f = tail c +           + e = c ++ d +         + g = snoc d 3 +
                  +++++++++++++++           ++++++++++++++         ++++++++++++++++
                    (future 1)                (future 1)               (future 1)


    # ephemeral data sturcutre: out-degree of every node in a version grpah is restricted to be at most one
    # persistence data structure: in-degree of every node to be at most one. Nodes with in-degree greater than one correspond to operations that take more than one arugment, such as list catenation or set union.


    # reconsiling amortization and persistence: replacing the notion of accumulated savings with accumulated debt, where debt measures the cost of unevalucated lazy computations. Althouth savings can only be spent once, it does no harm to pay off debt more than once.


   # call-by-value (strict evaluation)
   # call-by-name (lazy evaluation without memoization)
   # call-by-need (lazy evaluation with memoization)
-}


{-
6.2 Framework for analyzing lazy data structures

                                         *******************
                                         *  unshared cost  *
                                     /   ******************* \
                                    /                         \
                                   /                           \
   *********************************                             *******************
   *  Costs of any given operation *                             *   complete cost *
   *********************************                             *******************
                                   \                                 actural cost if
                                    \                          /    strict evaluation
                                     \                        /
                                      \  ******************* /
                                         *    shared cost  *
                                         *******************
                                         * realized cost   *
                                         *******************
                                         * unrealized cost *
                                         *******************

   actual cost = unshared cost + realized cost

   shared cost = accumulated debt

   the amortized cost of an operation is the unshared cost of the operation plus the amount of accumulaed debt paid off by the operation.

-}


{-
6.3 the banker' method

monolithic: once begin, it runs to completion
incremental: decomposable into fragments that maybe executed independently


The amortized cost of an operation is the unshared cost of the operation plus the number of debits discharged by the operation.

6.3.1 Justifying the banker's method:

The total amortized cost = the total unshared cost + the total number of debits discharged (paid of)
The total actual cost    = the total unshared cost + the realized shared cost


the banker's method <========> a graph labelling problem

the problem is to label every execution trace node with three (multi) sets, s(v), a(v) and r(v), that meeting following condition:

    (1) v /= v' ==> s(v) /\ s(v') = 0      (no debit may be allocted more than once)
    (2) a(v)  <= U s(w)                    (no debit may be discharged before it is created)
    (3) r(v) <= U a(w)                     (no debit may be realized before it is discharged)
-}
