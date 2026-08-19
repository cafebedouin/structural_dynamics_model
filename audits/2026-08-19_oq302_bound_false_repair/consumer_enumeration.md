# OQ-302 Phase 0 — consumer enumeration for the bound-`false` site at `boltzmann_compliance.pl:577`

**Executed:** 2026-08-19 · **Code state:** `a84cb693` · **Phase:** 0 (read-only; no engine runs, no engine edits)
**Raw evidence:** `caller_enumeration_raw.txt`, `bound_false_scan.out`, `bound_false_scan.sh`, `run_scan.sh`

This artifact exists because the zero-consumer census is the load-bearing claim of the whole OQ:
it is what makes a pipeline clean-vs-edited pair vacuous. A load-bearing claim held only in a
plan or a memory is not a witness. Nothing here is a run of the engine.

---

## 1. `boltzmann_invariant_mountain/2` and its two consumers — the trace terminates at 0 callers

Live-code call sites (`prolog/**.pl` minus `prolog/archives/`; `python/` and `agent/` swept
separately and empty). Export lines, definition heads, and comments are excluded from "call site".

| Predicate | Defined | Called from | Callers of *that* |
|---|---|---|---|
| `boltzmann_invariant_mountain/2` | `boltzmann_compliance.pl:576, :579` | `boltzmann_compliance.pl:139` (inside `boltzmann_shadow_audit/2`) · `drl_boltzmann_analysis.pl:176` (inside `boltzmann_invariant_check/2`) | see the two rows below |
| `boltzmann_shadow_audit/2` | `boltzmann_compliance.pl:120` | **nothing** — exported at `:3`, *imported* at `diagnostic_summary.pl:47`, and that import is the only other occurrence in the module. An import is not a call. | **0** |
| `boltzmann_invariant_check/2` | `drl_boltzmann_analysis.pl:174` | **nothing** — exported at `:10`; no other occurrence anywhere. | **0** |

**Independent second source** (`audits/2026-06-30_oq38_orphan_xref/orphan_xref_census.tsv`, the
OQ-38 static xref built for a different question by a different instrument):

```
110  boltzmann_compliance.pl      boltzmann_shadow_audit     2  1  STATIC_ORPHAN  0
620  drl_boltzmann_analysis.pl    boltzmann_invariant_check  2  1  STATIC_ORPHAN  0
109  boltzmann_compliance.pl      boltzmann_invariant_mountain 2 1 LIVE 2  boltzmann_compliance.pl:boltzmann_shadow_audit/2;drl_boltzmann_analysis.pl:boltzmann_invariant_check/2
```

The census agrees with the grep on both the two-caller set and the two zeros.

**Downstream surfaces, all empty.** Zero Python/agent readers of any of the three names. Zero
occurrences of `boltzmann_invariant` / `boltzmann_shadow` / `boltzmann_audit` /
`invariant_mountain` in `outputs/pipeline_output.json` (manifest: `pipeline_run_at`
`2026-08-18T20:34:53Z`, `n_constraints` 279, `code_commit` `6523046`, `code_dirty` true).
**Positive control for that grep, same file, same invocation:** `purity_score` → 279 hits,
`h1_band` → 422 hits. The instrument that reports 0 for the Boltzmann keys reports non-zero for
keys that are there, so the zero is a measurement, not a failure to look.

**Blast radius: zero live consumers.** This *corrects* OQ-302's own recon line
("Blast radius (recon-scoped): diagnostic surfaces only"). There is no diagnostic surface: the
two consumers are zero-caller static orphans, nothing reaches an exporter, a test, a report, or
`outputs/`.

### 1a. Consequence — the pipeline pair is a consistency check that cannot fail

Because no path from `boltzmann_invariant_mountain/2` reaches any written artifact, a
`run_pipeline.py` clean-vs-edited pair over this repair is **byte-identical at `per_constraint`
by construction**. Running it would produce a green result that carries no information about the
change (`build_discipline.md` → *A consistency check is not a discrimination check*). **It is
therefore deliberately not run, and its absence is declared here rather than left as a gap.**
The behavioural witness for this repair is the Phase-2 probe, which reads the predicate directly.

---

## 2. Every `epistemic_access_check/2` call site, with its argument-2 shape

Authored mode line: `%% epistemic_access_check(+Constraint, -Sufficient)` — argument 2 is an
**output**. Definition: clause 1 `epistemic_access_check(C, true) :- …, N >= MinN, !.`
(`:478–497`), catch-all clause 2 `epistemic_access_check(_, false).` (`:498`).

| Site | Shape | Verdict |
|---|---|---|
| `boltzmann_compliance.pl:95` | `epistemic_access_check(C, true)` — bound `true` | **safe.** Clause-1 failure falls through to *no solution*, not a wrong one; the catch-all cannot unify with `true`. |
| `boltzmann_compliance.pl:577` | `epistemic_access_check(C, false), !.` — bound `false` | **THE DEFECT.** Clause 1's head cannot unify with `false`, so its guard and cut never execute; the catch-all matches every constraint. Clause 1 of `boltzmann_invariant_mountain/2` therefore fires for every constraint and the four-test body at `:579` has never executed. |
| `purity_scoring.pl:49` | `epistemic_access_check(C, true), !` | **safe**, same reason as `:95`. |
| `logical_fingerprint.pl:417` | unbound `EpistemicOk`, then `EpistemicOk == false` | **correct idiom.** |
| `signature_detection.pl:1309–1315` | unbound `Access`, then `Access == false, !` | **correct idiom** — this is the 2026-06-03 repair (`0bfd3b31`), carrying its own witness comment. |

Non-call occurrences (excluded, listed for completeness): export `:8`, header/warning comments
`:466–477`, `purity_scoring.pl:30` comment, two test-file comments.

**`:577` is the only reachable bound-`false` call site in the engine.**

### 2a. The file's own header warns about this exact site

`boltzmann_compliance.pl:470–477` carries the warning — *"call with an UNBOUND second argument…
Calling `epistemic_access_check(C, false)` with `false` bound ALWAYS succeeds"* — added by
`0bfd3b31` after the `structural_purity/2` episode. It sits **79 lines above** the call it
describes, in the same file. The warning was written and the sibling instance was not swept.

---

## 3. Positive control for the enumeration itself

The scan is `bound_false_scan.sh` (selection rule stated in the script; `/usr/bin/grep` pinned,
not `grep`). Its discrimination record is a **naturally-arising pair**, not a planted fixture:
`0bfd3b31` ("fix: structural_purity/2 bound-probe bug") repaired one of two then-live
bound-`false` sites and left the other.

```
## HEAD  (a84cb693)                       prolog/boltzmann_compliance.pl:577   count=1
## 0bfd3b31^ (669eab52)                   prolog/boltzmann_compliance.pl:536
                                          prolog/signature_detection.pl:975    count=2
## 0bfd3b31                               prolog/boltzmann_compliance.pl:545   count=1
```

**Fires at 2, declines to 1 across the pair.** A scan reporting the same count at both commits
did not look. Full output: `bound_false_scan.out`; re-runnable via `run_scan.sh`.

---

## 4. Correction to the OQ-302 text: `:577` has no clean parent

OQ-302 says *"Git-blame `:577` for the free before-commit control pair."* That is **wrong**.

```
$ git log -L 577,578:prolog/boltzmann_compliance.pl
a0e8d772  2026-02-17  Split 3 Prolog monoliths (5072 lines) into 12 focused modules + 3 facades
```

`a0e8d772` only *moved* the line. Scanning its parent shows the defect already present in the
pre-split monolith, twice:

```
## a0e8d772^ (74b41af4)   prolog/structural_signatures.pl:1224
                          prolog/structural_signatures.pl:1464   count=2
```

`git log -S'epistemic_access_check(C, false)' --reverse` puts first introduction at **`80aebdb3`
(2026-02-07)**. So this site never had a clean parent commit, and the free before-commit control
for *this* OQ is `0bfd3b31` / `0bfd3b31^` (§3), not a blame of `:577`. This correction goes back
into the OQ text at close.

---

## 5. What the repair does and does not buy — scoped now, measured in Phase 2

`epistemic_access_check` is not the only dark thing in this predicate. **Test 4 of the body is
itself dead-by-range**, and that is readable from the source, not only from a prior audit:

- `natural_law_signature/1` requires `HasAlternatives == false` (`signature_detection.pl:434`).
- `has_viable_alternatives/2` has exactly two clauses (`signature_detection.pl:288–292`): one
  emits `true`, the other emits `unknown`. **No clause can emit `false`.** The range is
  `{true, unknown}`; the `false` leg is builder-unreachable, not merely unauthored (OQ-113,
  closed 2026-06-18).
- Independently re-measured under OQ-296 on 2026-08-18: `has_viable_alternatives/2` is a constant
  function, `unknown` on all 8,688 constraints across seven legs
  (`signature_detection.pl:200–206`).

So un-deadening the body moves the constant verdict from `inconclusive(insufficient_data)` to
`variant([… fail(natural_law_signature) …])`. **The repair does not make the check informative.**
Whether the *payload* (`T1`–`T3`) stops being constant is exactly what Phase 2 measures, and it is
open — those three tests have never executed against any constraint on any corpus.

One live hazard the enumeration surfaces for Phase 2: `signature_detection.pl:1406–1410`
(`determine_pure_subtype/2` clause 1) **throws** `unreachable_pure_natural_law(C)` if
`natural_law_signature/1` ever succeeds. `boltzmann_invariant_mountain/2`'s T4 calls the same
predicate inside an if-then-else and would *not* throw — it would silently emit
`pass(natural_law_signature)`. That is why the prereg carries an escalation clause: a T4 pass
anywhere contradicts a same-week measurement and outranks the repair.
