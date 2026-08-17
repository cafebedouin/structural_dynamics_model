# OQ-285 — route-back on the brief, filed before the spend

**Executed:** 2026-08-17
**OQ:** OQ-285
**Purpose:** OQ-285's receiver's prompt states a license to refuse — *"If this brief is correct in
prose and wrong when executed … say so and route it back, at the volume of a completion."* Six
items below exercise it. None of them is "hard" or "I'd do it differently"; each is a step that,
executed as written, produces something the design forbids.

---

## 1. The step-2 binary is mis-partitioned: one empirically-empty cell, three phenomena in the other

The brief's step 2 says: *"Count, on the live corpus, how often each absence condition actually
occurs: seats where `dr_type_for_stakeholder/3` FAILS versus seats deriving a literal `unknown`.
If the second is empty, or the first is, the distinction this OQ proposes has no population."*

Executed literally that count is **1333 seats, 0 FAILS, 152 literal `unknown`** — and the brief's
own stopping rule then fires ("if the first is [empty] … the answer is already no arm"). But
stopping there discards the finding, because *`unknown`* is not one condition. It is four:

| group | meaning | pin | live on `testsets/` |
|---|---|---|---|
| **(i)** derivation FAILED | `stakeholder_seats.pl:336-341` | **0** |
| **(ii)** inputs unauthored — classification never started | `drl_core.pl:500-508` guard chain | **0** |
| **(iii)** metrics exist, no band fit; signature declines to rescue | `drl_core.pl:459` fallthrough | **152** |
| **(iv)** a real type computed and deliberately declined | `residual_route/2`, `config.pl:508` = 0 | **0** |

The live phenomenon is entirely group (iii). The brief's binary places it on the side it treats as
uninteresting, and its stopping rule discards it. **Use groups (i)–(iv), not the binary.**

## 2. The premise "unnoticed collapse" is wrong at the root — the collapse is registered

OQ-285's framing is that a distinction is silently lost. It is not silent.
`reading_registry.pl:142` registers the raw per-seat computation as `partial_by_design` and names
**both** wrapper tokens in the registration string — `seat_perceived_vs_real/4` (`untyped`,
census-facing) and `stakeholder_type_vector/2` (`unknown`, kernel-facing) — with the instruction
*"never unify the two tokens"*. The OQ-137 totality suite machine-checks the registration.

Live witness (chain taken from `tests/test_reading_totality.pl:27-28`, corpus-loaded):

```
$ cd prolog && swipl -g "[stack], [reading_registry], [commentary_census], \
    corpus_loader:load_all_testsets, [tests/test_reading_totality], \
    run_tests(reading_totality), halt" -t "halt(1)"
[corpus] Loaded 273 testsets successfully.
% Start unit: reading_totality
% [1/10] reading_totality:..pus_loaded_nonempty ..... passed (0.006 sec)
% [2/10] reading_totality:registry_nonempty ......... passed (0.000 sec)
% [3/10] reading_totality:..us_sources_nonempty ..... passed (0.000 sec)
% [4/10] reading_totality:..otality_all_entries ..... passed (0.713 sec)
% [5/10] reading_totality:..tub_flagged_at_hole ..... passed (0.000 sec)
% [6/10] reading_totality:..an_when_hole_closed ..... passed (0.000 sec)
% [7/10] reading_totality:..rces_all_registered ..... passed (0.000 sec)
% [8/10] reading_totality:..sign_reasons_stated ..... passed (0.000 sec)
% [9/10] reading_totality:..main_has_no_reading ..... passed (0.000 sec)
% [10/10] reading_totality:.._fallback_reachable .... passed (0.000 sec)
% End unit reading_totality: passed (0.723 sec CPU)
% All 10 tests passed in 0.726 seconds (0.726 cpu)
```

*(Test names are column-truncated by plunit; the block above is `witness_totality_suite.txt`
verbatim.)*

This is a **deliberate design whose cost is now visible**, not an oversight. That changes what a
fix would be: not "restore a lost distinction" but "revisit a documented ruling."

## 3. The determining reason is discarded; only a CO-OCCURRING classification ships

`dr_signature/2` (`drl_core.pl:721-722`) wraps `constraint_signature/2` and is serialized as
`per_constraint[].signature` (`json_report.pl:296-301`), so it is tempting to read the signature as
"the reason, already computed and shipped." It is not. Two measurements kill that reading; both are
in `phase1a_signature_layer.txt` and summarised in `WRITEUP.md` §2.

**But the plan that authorised this audit got the mechanism half-wrong, and the correction
strengthens the conclusion.** The plan asserted that *"every reachable
`resolve_modal_signature_conflict/3` clause with first argument `unknown` returns `unknown`."*
**False, on two counts:**

- `integrate_signature_with_modal/3` (`signature_detection.pl:812-814`) does **not** call
  `resolve_modal_signature_conflict/3`. It calls `resolve_with_perspectival_check/4`, whose
  `false_ci_rope` clause (`:846-861`) cuts before the `resolve_modal_signature_conflict/3`
  `false_ci_rope` clause at `:946` — the plan's citation is to an **unreachable** clause.
- Two reachable paths map an `unknown` modal type to a REAL type:
  `resolve_with_perspectival_check(C, _, false_ci_rope, piton)` via `piton_candidate/1`
  (`:843-845`) and `resolve_modal_signature_conflict(_, coupling_invariant_rope, rope)`
  (`:939`), reached through the fallthrough at `:862-863`. **29 live seats take them.**

Measured per seat rather than read off clauses (n = 1333 agent seats):

```
metric unknown -> final unknown  (signature left it)       152
metric unknown -> final REAL     (signature RESCUED it)     29   <- the plan said 0
metric REAL    -> final unknown  (signature DEMOTED it)      0
metric REAL    -> final REAL                              1152
```

**The corrected statement is sharper than the one it replaces: the signature layer is a one-way
valve.** It can only *reduce* abstention, never create it. So abstention is determined
**entirely** by `classify_from_metrics/6` falling through at `drl_core.pl:459` — and *that* fact is
serialized nowhere. The conclusion the plan reached stands; the route to it was wrong.

**Cite this as ONE observation, not two.** Group (iii)'s named sub-path counts (103 / 40 / 9) are
byte-identical to the per-signature unknown-seat counts, because they *are*
`constraint_signature/2` under other names — not an independent trace of which clause fired.

## 4. `reading_registry.pl:110` is an over-claim — latent, not live

`derive_directionality_for_stakeholder/3` is registered `total_on_domain`. It is total only
*contingent on the authored atom vocabulary*: a malformed `exit_options` atom under a **well-formed**
role fails at `stakeholder_seats.pl:76`, inside the `->` then-branch, so the canonical-power
fallback at `:79` does not catch it. A malformed *role* is harmless (it routes to `:79`). The
registration is currently true because 0 malformed atoms are authored anywhere in the live corpus —
which is a fact about the corpus, not about the predicate. **Latent, filed; not a live defect.**

## 5. Step 4 names a retired instrument

The brief's step 4 says to *"check the twin legs and OQ-277 cross-coding as candidates."* OQ-277
cross-coding was **CLOSED/RETIRED 2026-08-12** (`ISSUES.md:10564`) — the same day OQ-285 was
minted — because its reference taxonomy was 55% self-agreeing. Executed as written, step 4 would
have evaluated a foreign-vocabulary probe already known not to agree with itself. Its retirement is
the governing lesson for *any* foreign-vocabulary probe proposed here, including a new one.

## 6. Step 5's option set is missing the outcome the evidence actually supports

The brief allows exactly (A) arm exists / (B) no arm / (C) arm could be built. The evidence
supports none of those cleanly: the axis the brief names (FAILS vs `unknown`) is **empty and
registered-by-design**, while the live variance sits on an axis the brief does not name. A
**(D) — the question as posed is mis-addressed** option is added in `WRITEUP.md` and is the
recommendation. Filing it under (B) would report "no arm on the Mode-3 question" when the honest
statement is "no arm, *and* the question was aimed at an empty cell."

---

## Pin check

Every `file:line` this document or `WRITEUP.md` cites was re-read at HEAD on 2026-08-17. **One drift
found**, filed as incidental item 7: `commentary_census.pl:163` cites `stakeholder_seats.pl:182`
for the text *"out-of-domain (no seats to compare)"*, which is now at `stakeholder_seats.pl:252`.
No other cited pin has moved.
