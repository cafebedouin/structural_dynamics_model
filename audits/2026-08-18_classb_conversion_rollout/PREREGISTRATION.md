# PREREGISTRATION — OQ-303(a) class-B conversion rollout (Unit B)

**Frozen:** 2026-08-18, before any conversion or any six-leg run. HEAD at freeze:
`161707bb1832d40f3a418b615bee89a827fd6da0`. Branch `oq303-classb-rollout` (multi-file,
semantics-changing by construction — `CLAUDE.md` *Git autonomy*: branch for risky/multi-file).
Not retro-edited; the writeup quotes this file. Its md5 is recorded in `audit_log.md`
physically above the first result line.

**Input:** the Unit A partition, `audits/2026-08-18_bound_caller_rewitness/partition.md` —
58 `latent-B` rows: **55 `converts-clean`, 1 `live-output-path`, 2
`converts-clean-minus-dataflow`**.

---

## 1. Ordering — fixed, and the reason is evidential not administrative

1. **`signature_grade/2` six-leg pair FIRST**, before any batch conversion. It is the only row
   on a recorded output path and the only one carrying an Ω_P escalation condition. If its pair
   comes back non-zero-diff, that changes what the 55 are riding on, and the finding is worth
   more before 55 conversions land than after.
2. **Clause-order census over the 55**, before converting them.
3. **The 55**, batched on the template.
4. **The 2 `converts-clean-minus-dataflow` rows LAST.** They carry the least evidence (no
   `evaluate(true)` codewalk verdict; recovered at `evaluate(false)`), so any template surprise
   should surface on well-witnessed rows first.

## 2. Carry (i) — the `commentary` reachability question is answered HERE, not deferred

Unit A measured the bound/unbound divergence at `commentary` as real and large (263/234,
932/899, 886/834, 993/932, 952/785). What it did not establish is whether anything downstream
queries at that atom. **Pre-registered claim, from reads taken before the run (`audit_log.md`
R1), to be confirmed or refuted by the pair:**

> `commentary` is reachable downstream as a **value** but is queried nowhere as a **bound
> selector**. `diagnostic_summary:verdict_join/3:728` queries `signature_grade(C, SigGrade)`
> **unbound**, and that value is serialized as `verdict_join.signature_grade`
> (`json_report.pl:1509`) and read live by `python/tensions_ledger.py:173`. The only bound
> caller in the engine is `signature_detection.pl:1951`, at `correction`.

**What each outcome means, fixed now:**

- **Zero-diff across all six legs** ⇒ the conversion is **Ω_E**, discharged. The Ω_P escalation
  pre-declared on this row does NOT fire, and that is a result to state, not a caveat to drop.
- **Non-zero diff on `verdict_join.signature_grade`, `verdict_join.verdict`, or
  `verdict_join.cap_applied`** ⇒ the Ω_P escalation **fires**. Stop, do not convert the 55,
  surface to the operator: this is OQ-98's deferred re-rule (the moderate→yellow cap for
  correction-grade signatures is confirmed-but-never-stressed; the first firing on a base-GREEN
  constraint IS the re-rule evidence).
- **Non-zero diff anywhere else** ⇒ the template is not output-preserving for unbound callers,
  which contradicts the 2026-08-17 pilot. Stop and re-adjudicate the template itself.

**Predicted result, recorded BEFORE the run so the prediction can be wrong (`build_discipline`
→ derive the expected diff before running it): ZERO DIFF on all six legs.** Reasoning: the
consumer-facing caller is unbound and the template is output-preserving for unbound callers by
construction; the one bound caller is at `correction`, where bound and `once + ==` were measured
identical on all five live legs. A non-zero diff falsifies that reasoning and is the more
interesting outcome.

## 3. Carry (ii) — the clause-order question is a CENSUS, not 55 investigations

Unit A found that this row's hazard is **atom-specific**, and located the mechanism: a bound
query at atom `A` skips the cut of any earlier clause whose head binds a **different** atom `B`,
and if that clause's body would have succeeded, it steals the answer. `signature_grade/2` is
safe at `correction` because its first cut-bearing clause has a **fresh-variable** head (always
unifies, so its cut always runs) and the only clause it skips binds `commentary`, which cannot
match. It is unsafe at `commentary` because the clause it skips binds `correction` — the answer
being stolen.

That is a property of the **class**, not of the row. So one pass over the 55 answers it:

> For each of the 55 predicates, for each atom `A` in its output-position atom set: count the
> **earlier cut-bearing clauses whose output argument is an atom ≠ A**. Call it the *steal-risk
> count* at `A`. Zero ⇒ a bound query at `A` skips no committing clause that could have answered
> differently.

The template is *designed* to drive this to zero. This unit records what it is *witnessed* at —
the distinction the whole arc has been enforcing.

**The census's own discrimination record is naturally arising and must be checked in the same
run** — no planting:

- it must report **steal-risk 0** for `signature_grade/2` @ `correction`, and
- **steal-risk > 0** for `signature_grade/2` @ `commentary`,

matching the five-leg agreement measurement exactly. A census that does not reproduce that
split is measuring something other than what Unit A measured, and its zeros license nothing.

The census reuses `prolog/dispatch_head_check.pl`'s clause reader (an additive export, no
behaviour change) rather than minting a second walker (Pattern 2).

## 4. The conversion template — fixed here, applied uniformly

Per the 2026-08-17 pilot: **fresh-variable heads + unify-after-cut**.

```prolog
p(X, foo) :- guard1(X), !.            %  becomes   p(X, T) :- guard1(X), !, T = foo.
p(X, bar) :- guard2(X), !.            %            p(X, T) :- guard2(X), !, T = bar.
p(_, baz).                            %            p(_, T) :- T = baz.
```

Commit label, per the pilot: *output-preserving on the witness set, semantics-changing by
construction*. Each converted row's `dispatch_head_check.py` registry entry is **retired in the
converting change** — the gate row goes RED on a stale entry, which is the mechanism that keeps
the worklist honest.

## 5. Witness obligations, fixed

- **`signature_grade/2`:** full six-leg `classify_corpus` clean-vs-edited pair — the five live
  legs (`testsets`, `testsets_haiku`, `testsets_flash`, `testsets_kimi`, `testsets_sonnet`) plus
  the breadth archive `archives/datasets/kernel_v1/`. Diff on `per_constraint`, with
  `pipeline_run_at` normalized out.
- **The 55 and the 2:** template application, no six-leg run each — *that licence is the
  partition's, and it is scoped*: `converts-clean` means clean under two instruments with known,
  disjoint, non-empty blind spots. Batch-level witness instead: `./scripts/gate.sh` GREEN
  (including `dispatch head` with the retired entries and `codewalk caller`), the Prolog unit
  suites, and one whole-corpus `classify_corpus` diff over the batch on at least one leg.
- **Every batch:** md5-fingerprint each corpus leg around both halves of any diff pair (operator
  topic runs land stories mid-session, witnessed 2×) and confirm exit 0 AND output mtime
  advanced before trusting any diff.

## 6. What would make this unit's conclusions wrong

- **A zero-diff pair that the run never actually rewrote.** `run_pipeline`-family gates abort
  before writing, so a stale-file diff reads byte-identical. Assert exit 0 + mtime advance, or
  the pair witnesses nothing.
- **A census whose zeros are didn't-looks.** Guarded by §3's naturally-arising split; if the
  census cannot reproduce `signature_grade/2`'s known asymmetry, its zeros are void.
- **A conversion that is output-preserving on the corpus but not by construction.** The corpus
  is not the specification. Where the census reports steal-risk > 0 at some atom for some row,
  the conversion CHANGES the answer for a bound query at that atom, whether or not such a caller
  exists today. That is recorded per row, not averaged away.

## 7. Non-goals

OQ-303 arms (b), (c), (d) — untouched here. `bound_selector_check` retirement — (c)'s, with an
unmet precondition. `caller_sweep.py` retirement — explicitly not on the table; the arms are not
nested and the regex arm holds a demonstrated capability the codewalk arm lacks. MaxEnt catch
arms, OQ-302, OQ-307 — each owed its own pair.
