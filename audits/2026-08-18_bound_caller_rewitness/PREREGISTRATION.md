# PREREGISTRATION — bound-caller re-witness (OQ-303 arm (a))

**Frozen:** 2026-08-18, before any Phase-1 run. HEAD at freeze:
`0300be246bb28b5b173f41b5ce3f9d1636a9e7a4`. Not retro-edited; the writeup quotes this
file, never amends it. Its md5 is recorded in `audit_log.md` physically above the first
result line.

**Worklist size, read from the registry at Phase 0 (not from prose):** `N_latentB = 58`
(`python/dispatch_head_check.py` `DECLARED`; registry total 73). Every count below refers
to 58.

---

## 1. What is under test

OQ-303 arm (a) is licensed by a premise carried in the registry as the class label
`latent-B`: *"shape present, no live bound caller found in the 2026-08-17 caller sweep."*
That sweep is `audits/2026-08-17_bound_dispatch_hardening/caller_sweep.py` — a single-line
regex over the repo, whose blind spots are readable in its own source
(`caller_sweep.py:44,72-73`): one physical line at a time, no nested-term arguments
(conceded in its docstring), bare lowercase atoms only, name/arity textual matching with
no module resolution.

This unit re-witnesses that premise with a second instrument whose blind spots are
**different**, and reports the partition.

## 2. The verdict's shape — FROZEN TEXT

The writeup's one-line verdict is this sentence, at this altitude:

> **the bound-caller premise rests on an instrument pair with disjoint blind spots and
> one shared residue class; the partition is the evidence, the residue is the finding,
> and the count of clean-converting rows is neither.**

A writeup whose one-line verdict is a row tally **violates this prereg**. "58/58
converts-clean" is a result, not the verdict.

### Escalation clause

The frozen text is a **floor, not a ceiling**. If the partition outranks it — a large
`not-latent` or `dataflow-residue` stratum, or any result that makes the instrument-pair
finding the smaller of two — the verdict **may escalate**, provided (i) the frozen text
above is reproduced verbatim in the writeup alongside the escalated one, and (ii) the
escalation is named as an escalation. A freeze that suppresses a bigger finding is the
failure mode on the other side of the one this guards.

## 3. Dispositions — fixed before the run

Every one of the 58 `latent-B` rows receives exactly one:

| disposition | assignment rule | witness a conversion would owe |
|---|---|---|
| `converts-clean` | zero bound call sites under **both** instruments | template application; no six-leg run |
| `live-output-path` | bound caller found by the codewalk arm, and the predicate reaches a recorded output surface | full six-leg `classify_corpus` clean-vs-edited pair |
| `not-latent` | bound caller found by the codewalk arm that the regex arm missed | adjudicate before converting; may not be class-B at all |
| `dataflow-residue` | selector bound by unification in the caller's own clause body before the call (`T = rope, ..., p(C, T)`) | **neither of the two instruments sees these**; declared residue, counted separately |

Rules that bind the reporting:

- `dataflow-residue` is reported as its **own count** and is **never folded into
  `converts-clean`**. A row may carry `dataflow-residue` in addition to its primary
  disposition; the count is reported both ways (rows-with-residue, and residue sites).
- A row is `not-latent` on **disagreement**, whichever way it points. Disagreement in
  either direction is the finding, not an instrument to be tuned until it agrees.
- No row may be left unclassified. The partition's own row count is asserted equal to 58.

## 4. Controls — the zero rule

**A codewalk sweep returning zero for a predicate is NOT evidence until the positive
control has fired in the same run.** Concretely, the codewalk arm must, in the same
process that produces any cited zero:

- **fires:** `dr_type/3` reports a non-empty set of bound-last-argument call sites;
- **declines:** `constraint_signature/2` (converted 2026-08-17, no live bound callers)
  reports zero bound call sites while reporting a non-empty set of call sites overall —
  i.e. the arm looked and found the predicate, and declined on the bound question.

The decline half is the informative half. A run in which the fires-control is empty, or
in which the declines-control reports zero *total* call sites (meaning it did not look),
invalidates every zero in that run.

Additionally: the arm is RED on an empty scan (0 predicates, 0 modules resolved), per
`bound_selector_check.py:250-252` / `dispatch_head_check.pl:51`.

## 5. Declared limitations, fixed in advance

The codewalk arm inherits and declares:

- **Last argument is the output argument** — engine convention, inherited from
  `prolog/dispatch_head_check.pl:9-11`. A predicate whose output is not last escapes
  this arm exactly as it escapes the definition-site walker.
- **Loaded modules only.** `prolog_walk_code` walks the loaded program. A registry entry
  whose defining file is not loaded by `[stack]` cannot be resolved, and callers living
  in unloaded modules, in Python/shell goal strings, or in `.pl` files outside the load
  chain are invisible to it. These are reported explicitly as `unresolved`, not silently
  scored zero.
- **No dataflow.** The walker sees the call term as written. `T = rope, ..., p(C, T)` is
  a free last argument to it.
- **Line = clause start.** Site line numbers are the enclosing clause's first line
  (`clause_property/2 line_count`), not the goal's own line.

The `dataflow-residue` scanner is a **lower bound**: same-clause `Var = atom` followed by
a call with that same variable last. Residue reachable only through a helper predicate is
not counted and is declared unmeasured.

## 6. What would make this unit's own conclusion wrong

- **The instrument-pair claim is wrong if either instrument is a strict superset of the
  other.** Falsifier, checkable in this run: if the codewalk arm finds every site the
  regex arm finds *and* the regex arm finds none the codewalk arm misses, "disjoint blind
  spots" collapses to "one better instrument," and the finding downgrades to a tool
  upgrade. The unresolved-module count and the regex-only site list are the deciding
  evidence, and both are reported whatever they say.
- **The residue class is not shared if either arm sees a dataflow-bound selector.** If the
  codewalk arm reports a bound last argument at a site where the selector is bound by a
  preceding unification, the "one shared residue class" clause is false and must be struck.
- **The premise is not weakened if the partition is 58/58 `converts-clean` with both
  controls green and zero unresolved rows.** That outcome is a *confirmation* of arm (a)'s
  premise at a strictly higher grade, and must be reported as such — not as a finding.

## 7. Non-goals of this unit

No `prolog/*.pl` semantics change. No class-B conversion is performed. No gate row is
added. Unit A ends at a hard checkpoint: report the partition, write one ISSUES.md line,
stop.
