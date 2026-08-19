# audit_log — OQ-303(a) class-B conversion rollout (Unit B)

Chronological. Every result line below the PREREGISTRATION md5 line was produced after that
freeze. Nothing above it is a result.

---

## OPEN — 2026-08-18

**HEAD stamp (OPEN):** `161707bb1832d40f3a418b615bee89a827fd6da0`, working tree clean.
Branch `oq303-classb-rollout` cut from it — this unit is multi-file and semantics-changing by
construction, which is the branch case in `CLAUDE.md` *Git autonomy*.

**Input:** `audits/2026-08-18_bound_caller_rewitness/partition.md` — 55 `converts-clean`,
1 `live-output-path`, 2 `converts-clean-minus-dataflow`.

### R1 (pre-freeze reads) — the `commentary` reachability question, answered by reading

Taken BEFORE the prereg was frozen, and written into it as the claim the pair will confirm or
refute. Recorded here as reads, not as results.

```
$ grep -rn "signature_grade" prolog/ python/ agent/ --include=*.pl --include=*.py | grep -v testsets
prolog/diagnostic_summary.pl:728:    (   signature_detection:signature_grade(C, SigGrade)
prolog/json_report.pl:1509:        format(S, '        "signature_grade": ', []),
prolog/signature_detection.pl:1952:    signature_grade(C, correction).
python/tensions_ledger.py:173:    grade = vj.get("signature_grade") if vj else None
python/shared/schemas.py:547:                       "signature_grade"):
```

```
$ sed -n '726,732p' prolog/diagnostic_summary.pl
    (   signature_detection:signature_grade(C, SigGrade)
    ->  true
    ;   SigGrade = none
    ),
    Join = verdict_join(Joined, Base, Cap, Alerts, GridProv, MeasProv,
                        SigGrade).
```

**The answer, and it is a distinction rather than a yes/no:** `commentary` is reachable
downstream as a **VALUE** — `verdict_join/3` queries `signature_grade/2` **unbound** at
`:728`, the result is serialized as `verdict_join.signature_grade`
(`json_report.pl:1509`) and read live by `python/tensions_ledger.py:173`. It is queried
**nowhere as a BOUND SELECTOR**; the engine's only bound caller is
`signature_detection.pl:1951`, at `correction`.

That distinction is what decides Ω_E vs Ω_P, so it is pre-registered with its consequences
(PREREGISTRATION §2) rather than left to be read off the diff afterwards.

---

## PREREGISTRATION FROZEN — 2026-08-18, before any conversion or six-leg run

```
$ md5sum audits/2026-08-18_classb_conversion_rollout/PREREGISTRATION.md
000742e0039894e98855c1187935aea4  audits/2026-08-18_classb_conversion_rollout/PREREGISTRATION.md
```

**Everything below this line is a result.**

---

## RESULTS — 2026-08-18

### R2. The clause-order census, and the control catching the census

Carry (ii). `audits/.../clause_order_census.pl` reuses `dispatch_head_check`'s ordered clause
reader through a new additive export `scan_file_clauses/2` — no second reader of the same
source (Pattern 2).

**The pre-registered control fired on the first version of the census, before any table was
read.** That version treated a variable-headed clause carrying a cut as an *unconditional*
commit and stopped scanning there:

```
COC_ROW: signature_detection.pl signature_grade/2 atom=commentary steal_risk=0 skipped=[] nclauses=3
COC_ROW: signature_detection.pl signature_grade/2 atom=correction steal_risk=0 skipped=[] nclauses=3
```

Zero at BOTH atoms — a clean-looking table, and wrong: Unit A measured `commentary` diverging
by 29–167 constraints per leg. A cut is not reached unless the body reaches it; a failing guard
falls through. Corrected model (head unification, not the cut, is what a bound query changes):

> `steal_risk(P, A)` = cut-bearing clauses of `P` whose head output arg is an atom ≠ `A`,
> appearing before the **last** clause that can yield `A` ("can yield" = output arg is `A` or a
> variable).

```
COC_ROW: signature_detection.pl signature_grade/2 atom=commentary steal_risk=1 skipped=[correction] nclauses=3
COC_ROW: signature_detection.pl signature_grade/2 atom=correction steal_risk=0 skipped=[] nclauses=3
```

Now matching the five-leg measurement exactly, in both directions. The driver **fails closed**
on this check rather than reporting, so the corrected split is a precondition of any zero in
the table.

### R3. Census result — NOT zero; 57 of 58, and the one exception is trivial

```
$ .venv/bin/python audits/2026-08-18_classb_conversion_rollout/clause_order_census.py
clause_order_census: control OK (signature_grade/2 correction=0, commentary=1)
  58 latent-B predicates, 218 (pred, atom) pairs
  57 predicate(s) with nonzero steal-risk at some atom
  165 (pred, atom) pair(s) with nonzero steal-risk
```

max steal-risk distribution across the 58:

```
  1 x 0     17 x 1     8 x 2     9 x 3     9 x 4     2 x 5     4 x 6
  2 x 7      1 x 8      1 x 10    1 x 11    1 x 13    1 x 14    1 x 17
```

The single zero is `signature_detection.pl resolve_with_perspectival_check/4`, and it is zero
for a trivial reason: **one atom**, so there is nothing for a bound query to skip. Worst rows:
`qualify_action/5` (17 atoms, max 17), `drift_severity/3` (14), `composition_rule/3` (13),
`cell_short/2` (11).

**Reading it honestly.** "May be zero" was the wrong prior, and the reason is structural rather
than surprising: these rows are in the registry *because* they carry the bound-probe shape, so
pre-conversion a nonzero steal-risk is close to definitional. What the census adds is the
magnitude and the granularity — **165 of 218 (predicate, atom) pairs are hazardous-if-called-bound
today**, and the `latent-B` label is holding all of that back on one fact alone: nobody calls
them bound. That is the exposure the conversion retires, and it is larger than "55 mechanical
edits" makes it sound.

**It also hands the conversion a by-construction witness stronger than any corpus diff.** After
conversion the census must read **0 of 218** — every head fresh-variable, nothing to skip. A
corpus diff can only say "no story exercised the difference"; the census says "the difference
is gone." Both are run.
