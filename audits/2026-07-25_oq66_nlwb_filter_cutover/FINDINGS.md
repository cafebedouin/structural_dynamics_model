# OQ-66 — `natural_law_without_beneficiary/1` agent-filter cutover

**Execution date:** 2026-07-25
**Scope:** land the agent-kind filter on `drl_core:natural_law_without_beneficiary/1`
(ruling 63-A, operator Q1 2026-07-25), with the raw-vs-filtered diff as the check on what
landing it costs — not as the reason to land it. **Result stated at the right quantity: zero
observable diff on six legs, one predicate-truth flip. "Behaviourally free" is the wrong
label — see §4 and `RELEASE_NOTE.md`.**

Artifacts in this directory:

- `nlwb_diff_harness.pl` — the repaired six-leg diff harness
- `RELEASE_NOTE.md` — what changed, at the right quantity; consumer surface; open items
- `maxwell_shadow_probe.pl` — the first properly-fitted read of the reference constraint
- `RAW_OUTPUT.md` — pasted tool output for every run below

---

## 1. Corrected fact base (five live legs, not three)

CLAUDE.md's "THREE LIVE LEGS" text predates `testsets_sonnet` and `testsets_kimi`.
Disk-verified file counts, 2026-07-25:

| corpus | N files |
|---|---|
| `testsets` (live) | 199 |
| `testsets_haiku` | 960 |
| `testsets_flash` | 960 |
| `testsets_sonnet` | 1001 |
| `testsets_kimi` | 1005 |
| `archives/datasets/kernel_v1` | 1106 |

## 2. Why the live legs are zero — the membership query, not the diff

Sourced off a fact-existence query that never touches classification or the cache path,
so it is not circular with the diff it explains:

```
PROBE testsets           n=199  beneficiary_facts=489  registry_hits=0 registry_values=[] raw_nlwb=0  divergence_n=0
PROBE testsets_haiku     n=960  beneficiary_facts=2502 registry_hits=0 registry_values=[] raw_nlwb=8  divergence_n=0
PROBE testsets_flash     n=960  beneficiary_facts=2174 registry_hits=0 registry_values=[] raw_nlwb=7  divergence_n=0
PROBE testsets_sonnet    n=1001 beneficiary_facts=3121 registry_hits=0 registry_values=[] raw_nlwb=3  divergence_n=0
PROBE testsets_kimi      n=1005 beneficiary_facts=1838 registry_hits=0 registry_values=[] raw_nlwb=19 divergence_n=0
PROBE archives/datasets/kernel_v1 n=1106 beneficiary_facts=2314 registry_hits=2
      registry_values=[entropic_universe_hypothesis,international_humanitarian_law_framework]
      raw_nlwb=29 divergence_n=1 divergence=[maxwell_demon_impossibility]
```

**Positive control is built in:** the same probe returns `registry_hits=2` on `kernel_v1`.
The five `registry_hits=0` results are measured-empty, not didn't-look.

10,124 beneficiary facts across the five live legs, zero carrying either registered value.
(Counts run +1 against the plan's figures because this query counts the whole
`constraint_beneficiary/2` table, which includes the `catholic_church_1200` engine demo
from `constraint_instances.pl` — a known non-corpus entry, not a discrepancy.)

`natural_law_without_beneficiary` itself fires plenty (up to 19/1005). It is the
raw-vs-filtered **distinction** that is inert on the live legs, not the predicate.

That entails extensional identity only because the filter is *exactly* registry
membership — one clause, no `dynamic`/`multifile` declaration, no other clause in the
tree (`narrative_ontology.pl:445-447`), and no `assertz`/`retract` of
`non_agent_beneficiary/1` anywhere. So `registry_hits=0` is not a point-in-time read:
there is no runtime channel by which the filter's extension can drift. Both premises are
now **enforced** as tests (`agent_beneficiary_is_single_clause`, `registry_is_static`,
`filter_is_exactly_registry_membership`) rather than left as a code-read.

## 3. The prior zero-diff was a screen; its MaxEnt half was never measured

Three uncontrolled channels in the harness that produced the original
`full_corpus_diff_count=0`:

1. **No cache clear between arms.** Registered memos (incl.
   `boltzmann_compliance:cached_classification/3`) can serve pre-swap values; a
   stale-cache "no change" is byte-identical to a real null.
2. **MaxEnt is corpus-fitted state, deliberately outside `cache_registry`** — a cache
   clear does not touch it.
3. **No planted flip** proving the harness can see a change.

Channel 2 is worse than uncontrolled. Witnessed this session:

```
MAXENT maxent_dist_facts_after_stack_load=0
MAXENT sample=abrahamic_covenant__isaac_covenant_reading top=FAILED(no fit)
```

A plain `[stack]` + corpus load leaves the model **unfitted**. Every `maxent_top_type`
call in the old probe failed and was mapped to `no_top` — in *both* arms. The MaxEnt half
of the diff compared `[no_top,no_top,no_top,no_top]` against itself. The MaxEnt consumer
surface (`maxent_classifier.pl:182,186,201`) was never measured, while the result
presented as zero-diff. **Pattern 6, in the instrument.**

The same defect was live in the guard itself (`test_agent_beneficiary.pl:129-133`, old
version), so the tripwire's MaxEnt arm had been vacuous since it was written — a defect in
the OQ-66 guard of exactly the class OQ-66 existed to watch. Recorded at close.

## 4. The repaired diff — six legs plus a planted fixture leg

Repairs: `cache_registry:clear_all_caches/0` after each swap and after restore; explicit
`maxent_cleanup + maxent_multi_run/2` in **each** arm; a non-vacuity assertion that
`maxent_dist/3` is non-empty before any `maxent_top_type` is read (`throw(maxent_unfitted(Arm))`
otherwise).

The refit is not an empty control — MaxEnt consumes the predicate through three channels:
fit population (`compute_type_profile/4` selects training sets by `drl_core:dr_type`, which
depends on nlwb via the snare and tangled_rope blocks), priors
(`maxent_compute_priors/1`, same source), and per-constraint likelihood
(`boolean_log_likelihood/3` → `eval_boolean_feature(C, natural_law_without_beneficiary, _)`
against `boolean_spec(snare|tangled_rope, …, forbidden)`). The boolean term is baked into
the stored `maxent_dist/3` at `maxent_precompute/2` time, not read lazily — so the recompute
*is* the reclassify.

**Flip detector first** (a zero from a harness that cannot see a change is worthless):

```
LEG tests/fixtures/nlwb_controls n=4 maxent_dist_raw=16 maxent_dist_filtered=16 diff_count=1
  DIFF nlwb_ctl_nonagent_only
    raw:      snap([snare,snare,scaffold,snare],[snare,snare,scaffold,snare])
    filtered: snap([unknown,unknown,scaffold,unknown],[snare,snare,scaffold,snare])
```

Non-zero at **exactly** `nlwb_ctl_nonagent_only`, zero at the other three; `maxent_dist`
non-empty in both arms. Then the real legs:

```
LEG testsets                      n=199  maxent_dist_raw=724  maxent_dist_filtered=724  diff_count=0
LEG testsets_haiku                n=960  maxent_dist_raw=3840 maxent_dist_filtered=3840 diff_count=0
LEG testsets_flash                n=960  maxent_dist_raw=3840 maxent_dist_filtered=3840 diff_count=0
LEG testsets_sonnet               n=1001 maxent_dist_raw=4004 maxent_dist_filtered=4004 diff_count=0
LEG testsets_kimi                 n=1005 maxent_dist_raw=4020 maxent_dist_filtered=4020 diff_count=0
LEG archives/datasets/kernel_v1   n=1106 maxent_dist_raw=4424 maxent_dist_filtered=4424 diff_count=0
```

**Stated at the right quantity: ZERO OBSERVABLE DIFF on six legs; ONE PREDICATE-TRUTH FLIP
at `maxwell_demon_impossibility` (kernel_v1), downstream-invisible because it classifies
`rope` in both arms.** "diff 0 everywhere" collapses those two and is wrong — the measured
quantity is final classification observables, and the predicate's own truth value is a
different quantity that DID change.

**The no-op is STRUCTURAL on the five live legs and CONTINGENT on `kernel_v1`.** Live-leg
zero is forced by `registry_hits=0` (extensional identity). `kernel_v1`'s zero is not forced —
it holds because one constraint's metrics land in rope territory. So the honest forward
statement is NOT "behaviourally free": *no observable change on the checked corpora; the first
live constraint carrying a registered non-agent beneficiary with snare-range metrics will
classify differently than it would have pre-cutover.* Full statement + consumer surface +
declared residue: `RELEASE_NOTE.md`.

**METHOD NOTE — the stop point was keyed on the wrong quantity.** It was specced to fire on a
non-zero *diff*; what it protected (the operator's seat on the release note and the consumer
re-audit scope) is triggered by a *predicate flip*. The flip happened, the trigger did not
fire, the filter landed, and the release note was written after the commit instead of before.
No harm to the ruling (Q1 was land-regardless), but the sequencing property was lost.
Generalization: **key a stop point on the quantity that carries the meaning, not the one the
harness happens to emit.**

## 5. FINDING (contradicts the record) — the maxwell gate-2 evidence does not re-witness

The registry entry for `entropic_universe_hypothesis` carries this gate-2 justification
(`narrative_ontology.pl:428-430`, ruled 2026-06-03):

> host `maxwell_demon_impossibility` is the witnessed genuine law — **MaxEnt shadow 0.990
> mountain / entropy 0.031**, omegas authored empty, narrative asserts the Second Law.

The first properly-fitted read of that constraint, on `kernel_v1` (its home corpus),
2026-07-25:

```
MAXWELL maxent_dist_facts=4424 (non-vacuity control)
MAXWELL beneficiaries=[entropic_universe_hypothesis] agent_beneficiaries=[]
MAXWELL signature=coupling_invariant_rope
MAXWELL ctx=... dr_type=rope shadow_top=rope entropy=0.1557   (all four canonical contexts)
MAXWELL   dist=[mountain-0.010, rope-0.95, tangled_rope-0.010, snare-0.010, scaffold-0.010, piton-0.010]
```

Shadow is **rope 0.95 / entropy 0.156 / mountain 0.010**, signature
`coupling_invariant_rope`, `dr_type` `rope` at all four contexts — not
`0.990 mountain / entropy 0.031`, and not natural-law/mountain.

**Control against a degenerate fit** (an identical `rope-0.95` at all four contexts is
suspicious on its face):

```
CTRL shadow_top_distribution_ctx1=[mountain-39,piton-4,rope-77,scaffold-73,snare-272,tangled_rope-641]
CTRL entropy min=0.0011 max=0.6111 mean=0.2052 n=1106
CTRL mountain_shadow_exists=animal_moral_status__property_reading
```

The fit spans all six types and the full entropy range, and mountain shadows do exist
(39 of them). The maxwell reading is a real measurement, not a collapsed model.

**Scope of the claim — deliberately narrow.** This says the recorded numbers do not
reproduce *on `kernel_v1` at HEAD*. It does **not** establish that the 2026-06-03 read was
wrong: that measurement was taken on the then-live pre-reset corpus, which is not
byte-identical to the `kernel_v1` archive, MaxEnt is corpus-fitted, and the signature layer
has changed repeatedly since (OQ-60, OQ-62, OQ-138 among others). Attributing the gap
between the two numbers to corpus vs. engine regime needs a stage-hash diff, which was not
run — "it's the corpus" without that diff is a hypothesis where a witness goes.

**Not acted on.** Retiring or re-ruling a `non_agent_beneficiary/1` entry is a gate-2
ruling and the operator's seat, not a run's. Recorded here, carried into the
shadow-separability OQ as its opening datum, and flagged for the operator. The entry
remains in the registry; nothing in the cutover depends on it (the diff is zero either way).

## 6. Ledger disposition

Both gate-two items close as **moot-by-reset**:

- `press_reformation_causality__technological_inevitability` — absent from all five live legs.
- `statutory_debt_ceiling__constitutional_nullity_reading` — the surviving names in
  `haiku`/`flash` are new draws, not the measured story (generation is stochastic; a
  regenerated story is a new draw, not a re-measurement). The 2026-06-03 reads cannot be
  re-witnessed.

Substrate is already correct: both values are unlisted ⇒ default-agent ⇒ FSM keeps firing.
No code change. A future revisit is a fresh measurement, not a resumption.

## 7. Rider — a suspected Pattern-6 that is NOT real

The plan flagged ~17 catch-wrapped MaxEnt reads in `abductive_triggers.pl`, six mapping
failure to a *plausible value* (`HNorm = 0.0` at `:86,:135,:358,:711,:771`,
`ShadowTop = unknown` at `:188`) — an entropy of 0.0 reads as maximal certainty. Probed
before writing the claim, as the plan required. **The suspicion is refuted, twice over:**

1. `maxent_entropy/3` **fails** on a missing `maxent_dist/3` — it does not throw:
   `ENTROPY FAILED (no exception) -- catch/3 does NOT intercept`. These sites are bare
   `catch/3`, not if-then-else, so the recovery goal never runs; the clause just fails.
2. Every one of those clauses is gated at its **first** goal by
   `subsystem_available(maxent)` (`abductive_triggers.pl:75,126,177,231`), which checks
   `maxent_run_info/3` — empty under a plain `[stack]` load. The clause fails before
   reaching the fallback at all.

So `abductive_triggers.pl` already carries the provenance guard the discipline asks for.
The wider claim was not written. The narrow, true finding — *a plain `[stack]` load leaves
MaxEnt unfitted while reads fail soft* — is what goes to KNOWN_STATE; no OQ minted for
these sites.

## 8. The tangled_rope `\+ nlwb` guard is structurally DEAD (→ OQ-250)

Found while re-auditing the consumer surface for the release note. The tangled_rope clause opens
with `\+ natural_law_without_beneficiary(C)` (`drl_core.pl:426`) and later requires
`requires_active_enforcement(C)` (`:435`); `nlwb` requires `\+ requires_active_enforcement(C)`.
**Contradictory — the guard can never block anything, on any corpus.**

Positive control is `tr_body_control` (the clause body MINUS the guard), so the zeros below are
measured-empty rather than didn't-look:

```
TR testsets                    tr_body_control=952   tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=0
TR testsets_haiku              tr_body_control=5809  tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=8
TR testsets_kimi               tr_body_control=3040  tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=19
TR archives/datasets/kernel_v1 tr_body_control=5461  tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=30
TR tests/fixtures/nlwb_controls tr_body_control=0    tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=2
```

`nlwb_total` is non-zero on four legs, so the zeros are not "nlwb never fires." **Corroboration of
the predicate flip:** kernel_v1 reads `nlwb_total=30` here, against 29 in §2's pre-cutover probe —
the +1 is `maxwell_demon_impossibility`, the same flip §4 reports, surfacing in an independent
measurement.

**The snare guard is unaffected** — no enforcement conjunct, so its `\+ nlwb` is satisfiable and
live. That asymmetry is why the fixture leg can exercise the snare block and not this one.

**The MaxEnt mirror is LIVE, not dead.** `boolean_spec(tangled_rope, nlwb, forbidden)`
(`maxent_classifier.pl:186`) evaluates the feature with no enforcement conjunct gating it. Two-sided
on identical-metric fixtures:

```
MIRROR nlwb_ctl_nonagent_only nlwb=true  feature_val=true   boolLL(tangled_rope)=-12.0  boolLL(snare)=-4.0
MIRROR nlwb_ctl_agent_only    nlwb=false feature_val=false  boolLL(tangled_rope)=-8.0   boolLL(snare)=0.0
```

Deterministic guard dead, MaxEnt mirror live — against a repo convention of same-commit congruence
between the two. That is what makes the disposition a ruling (OQ-250) rather than a deletion, and
why "write the missing fixture" is not available: a fixture would have to satisfy the contradiction.
The live mirror half needed no ruling and is now covered in the gate.

## 9. Verification ledger

| # | check | status |
|---|---|---|
| 1 | Fixture reachability — raw arm classifies `nlwb_ctl_nonagent_only` snare | GREEN (§4, 3 of 4 contexts) |
| 2 | Flip detection — non-zero at exactly the flip fixture | GREEN (§4) |
| 3 | Harness non-vacuity — `maxent_dist/3` non-empty in both arms | GREEN (§4, all seven legs) |
| 4 | Six-leg diff with cache-clear and refit | **Zero observable diff on 6 legs; 1 predicate-truth flip at `maxwell_demon_impossibility`, downstream-invisible (rope in both arms)** (§4) |
| 5 | Suite green — `run_tests(agent_beneficiary)` | GREEN, 7/7 |
| 6 | Gate fires; deliberate-break control | GREEN; break throws `agency_nlwb_set([nlwb_ctl_no_beneficiary])` |
| 7 | `./scripts/gate.sh` | see RAW_OUTPUT.md |
