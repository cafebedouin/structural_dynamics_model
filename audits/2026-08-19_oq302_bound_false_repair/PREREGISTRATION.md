# OQ-302 — PREREGISTRATION

**Frozen:** 2026-08-19, before the Phase-2 probe is written or run. md5 recorded in
`audit_log.md` above the first result line (`oq277 freeze` gate row checks this).
**OQ:** OQ-302 · **Code state at freeze:** `a84cb693` · **Phase-0 artifact:** `consumer_enumeration.md`

---

## 0. Phase-3 precondition — checked, and RELEASED

The plan gated Phase 3 on *"OQ-303 arm (a) unit A's checkpoint line exists in `ISSUES.md`, and its
prereg freeze is released."* Checked at freeze time:

- Unit A's checkpoint is in `ISSUES.md` under OQ-303 (`**(a) RE-WITNESSED 2026-08-18 — the
  partition is audits/2026-08-18_bound_caller_rewitness/**`).
- That unit's `PREREGISTRATION.md` (md5 `900d4a42…`, `audit_log.md:105`) has been consumed: the
  unit's `WRITEUP.md` is landed.
- The whole of OQ-303 arm (a) is **LANDED 2026-08-19**; `latent-B` is recorded EMPTY.

**The precondition holds. The file-level collision the plan was sequencing against is gone** — no
receiver holds pins into `prolog/boltzmann_compliance.pl`, and the frozen worklist has been
released. Phases 0–3 may all run this session. The `bundled_with OQ-303` dependency edge is still
authored at close, because the *reason* for the ordering remains true and a future instance
re-deriving it from the frontier would not find it.

## 0a. DESIGN CORRECTION — the plan's Phase-2 mechanism cannot be executed as written

The plan specifies both arms *"via `probe_harness:with_overlay/3` … never hand-rolled
retract/assert."* **Executed as written, that produces exactly the vacuous pair the plan warns
against, silently.** Reported rather than silently repaired (`build_discipline.md` → *The
receiver's license to refuse*); the defect is in the design, not the prompt.

Two independent reasons, both witnessed at freeze time (`swipl -l stack.pl`, design check — not a
measurement of any observable below):

1. **`with_overlay/3` is FACT-ONLY by construction.** `probe_harness.pl:91–100` snapshots via
   `clause(M:Inst, true)` — clauses whose body is `true`. `boltzmann_invariant_mountain/2`
   clause 1 is a RULE (`epistemic_access_check(C,false), !`). It is never snapshotted, never
   retracted, and `warn_if_rule_clauses/1` emits a *warning*, not an error. An overlay clause
   asserted alongside it would land **after** it, so the defect clause would still fire first and
   the "repaired" arm would return `inconclusive(insufficient_data)` — **identical to the defect
   arm, for the wrong reason, with no error.**
2. **The predicate is static.** Measured: `bim/2 dynamic? no`; `assertz(boltzmann_compliance:
   boltzmann_invariant_mountain(zzz, test))` **throws**. So the overlay could not be installed at
   all even if (1) did not apply.

**Substituted mechanism, fidelity-preserving.** `clause/2` *does* work on the predicate
(measured: `clause/2 ok, first body: epistemic_access_check(_,false),!`). So the repaired arm is
executed from **the engine's own compiled clause-2 body**, fetched at run time and called — not a
hand-copied replica:

```prolog
arm_repaired(C, Result) :-
    once(boltzmann_compliance:epistemic_access_check(C, S)),
    (   S == false
    ->  Result = inconclusive(insufficient_data)
    ;   clause(boltzmann_compliance:boltzmann_invariant_mountain(HC, HR), Body),
        var(HR), !,                    % the 4-test clause: head arg 2 unbound
        HC = C, HR = Result,
        boltzmann_compliance:Body
    ).
```

This is precisely the program Commit 1 will produce. **Residual gap, declared:** the Phase-2 table
attests to a *composed* program, the commit lands a *source edit*, and those are different
programs. Commit 1's second witness (a probe re-run against the committed source with the
`arm(repaired)` column produced by the **real** clause) is what closes it, and is therefore
mandatory, not optional.

## 0b. Per-test columns are a transparent replica, and carry their own cross-check

`T1`–`T4` are not recoverable from the clause's `Result`: `variant(Failures)` lists only the
failures. The probe therefore computes `T1`–`T4` from the engine's own sub-predicates
(`boltzmann_compliant/2`, `scope_invariance_test/2`, `excess_extraction/2`,
`get_constraint_profile/2` + `natural_law_signature/1`) reproducing the four if-then-elses at
`boltzmann_compliance.pl:580–610`. That replica is a **claim**, so it carries a control:
**the probe's own aggregation of `T1`–`T4` must equal the `arm(repaired)` Result computed by
`clause/2`+call, row for row.** A mis-transcribed threshold or a wrong branch shows up as a
mismatch. **Any mismatch invalidates the per-test columns of that run** (the Result columns
survive; they do not depend on the replica).

---

## 1. Frozen verdict text

> The repair removes a live instance of the idiom the file's own header warns against
> (`boltzmann_compliance.pl:470–477`, seven lines of warning and 79 lines above the offending
> call). **The predicate's VERDICT remains constant**, because Test 4 is dead-by-range:
> `natural_law_signature/1` requires `HasAlternatives == false` and `has_viable_alternatives/2`
> has a two-clause range of `{true, unknown}` with no clause able to emit `false`. The verdict
> moves from a constant `inconclusive(insufficient_data)` to a constant
> `variant([… fail(natural_law_signature) …])`. **Whether the PAYLOAD (`T1`–`T3`) stops being
> constant is what this run measures, and it is open.** Blast radius is **zero live consumers**,
> so nothing downstream of the engine changes either way.

**A verdict that reads "the invariance check now works" violates this preregistration.**
So does any claim of per-constraint variation not shown in the `T1`–`T3` marginals.

## 2. `N_reaching` — declared per leg, before any branch is read

`N_reaching`(leg) = the number of constraints on that leg for which
`epistemic_access_check(C, S)` yields `S == true`, so the four-test body executes.

- The `T1`–`T3` variation branches in §3 apply to the **`N_reaching` complement only**, never to
  corpus size. Constraints that legitimately stop at clause 1 post-repair never reach `T1`–`T3`.
- **A leg with `N_reaching ≤ 1` is declared UNMEASURED for `T1`–`T3` variation.** It is not
  "uniform". Reporting a leg where epistemic access fails corpus-wide as *uniform* would
  manufacture the strongest available evidence for the OQ-317 bundling argument out of a probe
  that saw at most one row — the vacuous-pipeline-diff error, one level in.
- `N_reaching` is reported for every leg regardless of branch.

## 3. Disposition — contingent on the run; all three outcomes declared now

`T1`–`T3` have never executed against any constraint on any corpus, so all three are live.
Read against `N_reaching`, never against corpus size.

| Outcome | Meaning | What may be written |
|---|---|---|
| **`T1`–`T3` vary** (≥ 2 distinct value-tuples over the `N_reaching` set, on ≥ 1 leg) | the payload becomes per-constraint | Site comment MAY state that the payload becomes per-constraint. Disposition stands as ruled: land the repair. |
| **`T1`–`T3` uniform** (exactly 1 distinct tuple across every leg with `N_reaching ≥ 2`) | the payload is constant too | Site comment **MUST NOT** assert per-constraint variation. The bundling question (is this predicate worth keeping at all) is **escalated to the operator before commit**, not settled here. |
| **Any leg throws** | never-executed code, un-deadened, raises instantiation/existence errors | **OUTRANKS BOTH.** A predicate that errors is worse than one that is constant; the drafted site comment becomes unwritable and the OQ-317 bundling argument stops being a preference. **HALT and escalate. Do not repair the body on the way past.** |

Throw counts are reported per leg **and per test** regardless of outcome. Zero is a result, not
an assumption.

## 4. Escalation clause — outranks the repair

**If `natural_law_signature/1` fires (`T4 = pass`) on any constraint on any leg**, that
contradicts OQ-296's same-week measurement (`has_viable_alternatives/2` constant `unknown` on all
8,688 constraints across seven legs) and the by-construction range argument in §1. Halt,
escalate, cross-check against OQ-317 before anything lands. Note that the *sibling* consumer
`signature_detection.pl:1406–1410` **throws** `unreachable_pure_natural_law/1` on exactly this
event; `boltzmann_invariant_mountain/2`'s T4 would not — it would silently emit a pass. That
asymmetry is why this clause is stated rather than left to the existing tripwire.

## 5. Controls — two-sided, and graded honestly

### 5a. *fires* — A WIRING CHECK WITH A KNOWN ANSWER, NOT DISCRIMINATION

The `arm(defect)` column must be `inconclusive(insufficient_data)` for **100%** of constraints on
**every** leg. This follows from head unification, which *is* the finding, so as evidence it is
close to vacuous. It is still run, because it is the only check that the probe reaches the real
predicate with real constraints rather than producing rows some other way.

**The WRITEUP must label it a wiring check and must not grade it as discrimination.** The
declines-control carries the two-sided weight.

### 5b. *declines* — the two-sided control

Post-repair, a constraint with genuinely insufficient epistemic access must **still** report
`inconclusive(insufficient_data)`. Subject: whatever the probe finds per leg where
`epistemic_access_check(C, S)` yields `S == false`.

- **OQ-112's ~92 masked constraints is a RECALLED figure and is demoted to a prior.** It may not
  do existence work here. The subject is whatever this run finds, or there is none.
- **A leg on which the probe finds no such constraint has an UNTESTED GUARD.** Declare it as a
  scoped residue. Never green.

### 5c. *invalidation*

A run whose fires-control is not 100%, **or** whose declines-control has no available subject and
is not declared, invalidates **every number in that run**. A run whose §0b aggregation
cross-check mismatches invalidates that run's `T1`–`T4` columns.

---

## 6. Probe requirements (frozen)

1. **Arm flags name the clause, never its position.** Each row carries `arm(defect)` /
   `arm(repaired)` explicitly. In Phase 2 the source holds the defect; after Commit 1 the source
   holds the repair and the *no-overlay* arm becomes `arm(repaired)`. A probe deciding which
   column is which by arm order silently compares the wrong pair post-commit and reproduces the
   table for the wrong reason. **All comparisons are by arm flag.**
2. **`epistemic_access_check`'s value is PRINTED DIRECTLY as its own column** — head-unification
   failure is demonstrated, not inferred from differing verdicts.
3. **Membership is `corpus_loader:corpus_constraint/1`**, the authoritative predicate. Never a
   `constraint_metric`/classification union (those pick up engine demo constraints).
4. **Every per-constraint call is wrapped in `catch/3`; a thrown error is a column VALUE**
   (`error(Formal)` rendered into the cell), not a crash. A probe that dies on constraint 40
   measures nothing about 41+. Failure (no solution) is a **distinct** cell value from a throw.
5. **Six corpora** (engine-file change ⇒ CLAUDE.md's all-corpora rule): `testsets/`,
   `testsets_haiku/`, `testsets_flash/`, `testsets_kimi/`, `testsets_sonnet/`,
   `archives/datasets/kernel_v1/`. `corpus_path` overlaid with **`asserta`** — a plain `assertz`
   appends after `config.pl:489`'s default and silently loads `testsets`.
6. **TSV output**, one row per (leg, constraint), columns: `leg`, `constraint_id`, `arm`,
   `eac_value`, `result`, `T1`, `T2`, `T3`, `T4`, so per-test marginals and distinctness counts
   fall straight out.
7. **Pre-flight, per leg, before any number is trusted:** `corpus_loaded/0` asserted; the
   `[corpus] Loaded N testsets successfully.` count equals `ls <leg>/*.pl | wc -l`; leg directory
   md5-fingerprinted **around both arms** and identical (operator topic runs land stories
   mid-session, witnessed 2×).
8. **MaxEnt:** asserted at freeze that no goal in the `T1`–`T4` chain reads a MaxEnt observable
   (`maxent_dist/3`, `maxent_entropy/3`, `maxent_top_type/3`, `maxent_run_info/3`) — grep over
   `prolog/*.pl` minus `archives/`, `tests/`, `maxent_*`, non-comment lines: **empty**, and
   `drl_core.pl` carries no non-comment maxent reference. MaxEnt fails *soft* under `[stack]`, so
   a read there would measure nothing. If the run contradicts this, refit explicitly
   (`maxent_cleanup, maxent_multi_run(Ctxs,_)`) and assert `maxent_dist/3` non-empty **first**.
9. **`prolog/testsets/` carries no count.** Counted at run time, stamped with the date. No
   recalled figure for it appears anywhere in this audit.

## 7. Kill conditions for this preregistration

- Fires-control below 100% on any leg → the probe is not reaching the predicate; every number is
  void until that is explained.
- §0b aggregation cross-check mismatch → `T1`–`T4` columns void for that run.
- Corpus md5 differing across the halves on any leg → that leg's pair is void (mid-run corpus
  drift), re-run required.
- A `T4 = pass` anywhere → §4, halt.
- Any throw → §3 row 3, halt and escalate.
