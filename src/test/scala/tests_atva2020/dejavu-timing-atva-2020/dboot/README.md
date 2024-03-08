
# DBOOT

## The property

```
prop noProblematicDoubleBoot:
  ! Exists i .
    (
      boot_e(i) &
      !P[<=20] boot_s(i) &
        @ (
        !boot_e(i) S
          (
            boot_s(i) &
            (
              !boot_e(i)
              S[<=5]
              (
                boot_e(i) &
                !P[<=20] boot_s(i) &
                @ (
                  !boot_e(i)
                  S
                  boot_s(i)
                )
              )
            )
         )
        )
    )
```

```
prop boots: ! Exists i . (boot_e(i) & !P[<=20] boot_s(i) & 
  @ (!boot_e(i) S (boot_s(i) & (!boot_e(i) S[<=5] (boot_e(i) &  
  !P[<=20] boot_s(i) & @ (!boot_e(i) S boot_s(i)))))))
```

## Evaluation 

- Trace has 10,012 events.

- verification with time 2 /5: 0.4 seconds, 70 errors detected   -> factor  2.0   (4 bits)
- verification with time 20/5: 0.8 seconds, 13 errors detected   -> factor  4.0   (6 bits)
- verification with time 50/5: 5.1 seconds, 3 errors detected    -> factor 25.5   (7 bits)
- verification with time 60/5: 7.2s seconds, 2 errors detected   -> factor 36.0   (7 bits)
- verification with time 70/5: 387.6s seconds, 1 error detected  -> factor 1938.0 (8 bits)

- verification without time: 0.2 seconds, 415 errors detected

## Discussion

I was spending some time thinking about the real boot property.
I'm almost sure you cannot express it in a logic that is similar to us.

The reason is that the logic LTL is constructed in a special way, that allows
it to be translated into first order monadic logic (first order logic where
instead of predicates p, q, you need to write (p(t1), q(t2) and can
quantify over the time variables t1, t2, ... and compare between them)
with 2.5 variables. Yes. 2.5!, this means that you can compare only two time variables
to each other at a time, but have a limited (hence the 0.5, which is due
to the lefthand side of the until operator) to a third variable.
Thus, and LTL formula can be translated into a first order monadic logic
with 3 variables (you may need to reuse variables, e.g., when nesting).
Of course you can relate events to each other in more complicated ways,
but using the fact that the time is linear, allows you to relate them, more or less in pairs
(again, its a bit more than pairs, the 2.5 variables buissness).:
Relating a, b, c and d, will, e.g., say that a comes before b, b comes before c and c before d.
Now, that's what you did in the property that you have sent today: you have related
them to each other in pairs.

But in the original double boot property, you related 4 (or 5) actions to
each other, saying that 2 (or 3) out of them, are within a pair of
actions that are within a duration of <=300.

This is not a proof. I may even be wrong.
This is more of the intuition. Moshe Vardi, Kousha Etessami
and Thomas Wilke are the experts on that.
