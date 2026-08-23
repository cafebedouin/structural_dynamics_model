# Superseded raw output — the double-emission defect

Kept deliberately, not as clutter. This directory holds the FIRST full sweep,
produced before two defects in this audit's own instruments were found:

1. **`emit_transition`'s double emission.** The seat filter was a bare
   disjunction `( MT0 \== MT1 ; FT0 \== FT1 )` inside `forall/2`, which yields
   ONCE PER SUCCEEDING BRANCH — so a seat whose MT *and* FT both changed was
   emitted twice, while an MT-invariant / FT-only seat was emitted once. The
   inflation was **differential, not uniform**, which is why "just halve it"
   would have been wrong: on `testsets`, 3228 emitted rows became 1863 after the
   fix, not 1614. Fixed to `MT0-FT0 \== MT1-FT1`.
2. **C4's precondition/failure conflation.** The carrier's suppression is a
   per-leg redraw property (haiku 0.35 / haiku2 0.28 / haiku3 0.42, but flash
   0.60 / kimi 0.60). On the 8 legs where it sits at or above the snare floor the
   control's premise is simply absent, and the code reported that as `FAIL`.
   An absent precondition is a SKIP; calling it a failure misattributes the
   instrument. Fixed to print `SKIPPED precondition-carrier-suppression-...`.

**Why keep it:** per `build_discipline.md` → *When a defect is found, its
before-commit is a free negative control*, the defective state is the naturally-
arising positive for any future check on this shape, and it is destroyed by
being fixed. Nothing in this directory is cited as a result. Every published
number comes from `../raw/`.
