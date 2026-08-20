# Falsifier A, harvested over the INCIDENT population (added 2026-08-20, post-review)

**Why this file exists.** The scored pass classified stratum 2 (141 integrity lines, 117 of them
value-checking) but reported no **incident** tally for it. In stratum 1 the value population is
**n = 1**, so *"falsifier A does not fire"* there is largely a fact about the denominator: you
cannot falsify a rule about value checks in a set containing one. Stratum 2 is where A had a real
population. Harvested here, from the incident side — which is the correct denominator for A, since
A asks *did a value check ever catch one*, not *did every line get exercised*.

**Population: every recorded apparatus self-instance in the record** — §7.4's twelve plus the
forward window's six, n = **18**. Shapes are taken from the frozen Phase 2 classification
(`classification.tsv` md5 `c6604cc6bd0625253788045179a6035e`,
`evidence/s2_classification.tsv` md5 `953766fc78a4dfbea360e0bae0156891`); no catcher was
re-classified for this harvest.

| # | what caught it | catcher kind | shape |
|---|---|---|---|
| 1 | a number moved between two runs | person | re-derivation, not exit-coded |
| 2 | reading the output rather than the exit code | person | — (**the exit code MISSED; the output caught it**) |
| 3 | running it against the reverted copy | person | before-commit pair |
| 4 | comparing the printed number against the directory | person | re-derivation |
| 5 | the same comparison | person | re-derivation |
| 6 | re-reading the command's output | person | — |
| 7 | cross-checking against version control | person | re-derivation |
| 8 | **a script that recounted the table** | **stratum-2 line** | **INVARIANT** — `recheck_predeclared_counts.py:148`, `ok = sorted(recomputed) == sorted(declared_sel)`, a re-derivation set-equality |
| 9 | a second reader | person | — |
| 10 | noticing the response files had changed | person | — |
| 11 | **`partition_check`** | **stratum-2 line** | **INVARIANT** — partition/set-equality; non-exiting |
| 12 | a person reading | person | — |
| W1 | **the `apparatus` gate row** | **stratum-1 row** | **INVARIANT** — three-way partition by construction; **exit-coded** |
| W2 | **`dangle_count.sh`'s control arm** | **stratum-2 line** | **INVARIANT** — every anchor must be PRESENT in the intact paper |
| W3 | **`standing_check.sh`'s selftest** | **stratum-2 line** | **INVARIANT** — the arms must DISCRIMINATE, not merely fire |
| W4 | a person, noticing the pointers post-dated the check | person | **MISSED by a stratum-2 VALUE line** (`checks.sh` row 3) |
| W5 | a person | person | an **invariant** stratum-2 line **FALSE-FIRED** |
| W6 | a person reading | person | — |

## Result

| | n |
|---|---|
| self-instances caught by a stratum-2 **value-checking** line | **0** |
| self-instances caught by a stratum-2 **invariant-asserting** line | 4 (8, 11, W2, W3) |
| self-instances caught by a stratum-1 row (invariant, exit-coded) | 1 (W1) |
| self-instances caught by a person | 13 |
| self-instances **missed** by a value-checking line | 1 (W4) |

**Falsifier A returns ZERO over both denominators**, and this time it is not a denominator
artifact: of the 18 recorded self-instances, **6 were caught by an integrity line of some kind, and
all 6 catchers classify invariant.** Not one value check has ever caught one, while a value check
is recorded **missing** one.

**Exposure, stated so the negative is read at its altitude.** The 117 value-checking stratum-2
lines are overwhelmingly **run-once** artifacts: an audit driver executes during its own pass and
is rarely run again. So the negative is *"no value check has ever been recorded catching one"*, not
*"117 value checks ran repeatedly against this class and stayed silent."* It is a real negative
with a thin exposure base — which is the same shape as, and no worse than, the evidence the rule
was built on.

## The finding this harvest actually produced

**§7.4's own "ten of eleven" catchers are re-derivations — and a re-derivation is
INVARIANT-asserting under OQ-310's frozen criteria.**

§7.4 states its property as *"caught by a person or a script comparing a claimed number against the
artifact it described — a diff, a directory listing, a file count, a re-read."* Every one of those
is *"the derived artifact must regenerate identically"*, which is an item in the frozen
invariant-asserting list, not the value-checking one.

So the eleven were **not** caught by something outside the invariant/value cut. They were caught by
**invariants that no gate was asserting and no exit code enforced** — performed by a human, by
hand, at read time.

That collapses the apparent gap between §7.4's property and its rule, and it relocates what the
rule is actually about. The scarce thing was never the invariant; the invariant was present in
every one of the eleven catches. **The scarce thing was the exit code.** Which is the promotion
this pass already made — *state an invariant AND exit on it* — arriving a second time, from the
retrospective side, and stronger: it is now the reading that makes §7.4's own eleven-instance
record and its rule say the same thing.
