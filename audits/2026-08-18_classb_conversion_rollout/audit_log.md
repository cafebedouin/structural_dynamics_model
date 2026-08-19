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

### R4. Phase 1 — `signature_grade/2` converted; six-leg pair ZERO DIFF; prediction held

Conversion applied by `convert.py` (its own selftest green first). The diff:

```
-signature_grade(C, correction) :-                     +signature_grade(C, T) :-
     ... guard unchanged ...                                ... guard unchanged ...
-    !.                                                +    !,
-signature_grade(C, commentary) :-                     +    T = correction.
-    constraint_signature(C, _), !.                    +signature_grade(C, T) :-
                                                       +    constraint_signature(C, _), !,
                                                       +    T = commentary.
```

**By-construction witness** — the census on the converted predicate:

```
COC_PRED: signature_detection.pl signature_grade/2 atoms=0 max_steal_risk=0
```

**By-corpus witness** — the six-leg clean-vs-edited pair, 5,311 constraints:

```
$ .venv/bin/python audits/2026-08-18_classb_conversion_rollout/sixleg.py diff
testsets         n=  279  changed=0
testsets_haiku   n=  960  changed=0
testsets_flash   n=  960  changed=0
testsets_kimi    n= 1005  changed=0
testsets_sonnet  n= 1001  changed=0
kernel_v1        n= 1106  changed=0
sixleg diff: 0 changed constraint(s) across 6 legs
```

**The pair actually ran, and the halves are comparable** — the two things that make a
zero-diff a false pass if unchecked:

```
testsets         clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_haiku   clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_flash   clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_kimi    clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_sonnet  clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
kernel_v1        clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
```

Different code state per half; identical corpus md5 per leg across halves. Each leg's output
mtime was also asserted to advance past a pre-run marker (`sixleg.py`), so no half compares a
stale file against itself.

**The differ discriminates — a zero from a check that cannot fail witnesses nothing.** One
field planted in one record of one leg:

```
planted beneficiaries='__PLANTED__' on constraint ability_ceiling_reading
testsets         n=  279  changed=1
    first 10: ['ability_ceiling_reading']
      ability_ceiling_reading: beneficiaries: ['high_performing_learners', 'meritocratic_sorting_institutions'] -> '__PLANTED__'
sixleg diff: 1 changed constraint(s) across 6 legs
--- restored ---
sixleg diff: 0 changed constraint(s) across 6 legs
```

Fires on a single field in a single record, names the constraint and the field, declines when
restored. Same session, both directions.

**And the conversion did what it is FOR — the bound form now agrees with the engine.**
Re-running the Unit A agreement probe post-conversion:

```
                    BEFORE                              AFTER
testsets            commentary: bound=263 unb=234       commentary: bound=234 unb=234
testsets_haiku      commentary: bound=932 unb=899       commentary: bound=899 unb=899
                    correction: bound=45  unb=45        correction: bound=45  unb=45
                    correction: bound=61  unb=61        correction: bound=61  unb=61
```

The 29 and 33 over-permissive `commentary` answers are gone; `correction` is untouched — which
is exactly why the corpus diff is zero (nothing in the engine queries at `commentary`).

### R5. The pre-registered verdict, discharged as pre-registered

PREREGISTRATION §2 fixed the outcomes before the run:

- **Zero-diff across all six legs ⇒ Ω_E, discharged; the Ω_P escalation does NOT fire.**
  That is the result. It is a finding, not a caveat dropped: the escalation was live and
  declined.
- The predicted result was recorded as **ZERO DIFF** before the run, and it held. The
  reasoning it rested on (consumer-facing caller unbound + template output-preserving for
  unbound callers; the one bound caller at `correction`, measured identical) is therefore
  supported rather than merely consistent.

### R6. Registry and allowlist entries retired, per their own stated conditions

```
$ .venv/bin/python python/dispatch_head_check.py --check
dispatch_head_check: GREEN — 126 engine files, 69 shape hit(s) all declared (69 declared + 3 must-not-fire), 0 file(s) with read errors, selftest OK

$ .venv/bin/python python/codewalk_caller_check.py --check
codewalk_caller_check: GREEN — 73 registry spec(s) (57 latent-B), 99 loaded module(s), ...
```

70 → 69 declared shape hits; 58 → 57 `latent-B`. The allowlist row's REMOVE condition
("deleted when the class-B conversion of signature_grade/2 lands with its six-leg pair") was
met and the row is gone, so the allowlist is now empty of rows — retirement in the same change
as the repair, which is the orphaning rule.
