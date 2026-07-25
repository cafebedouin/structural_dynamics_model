# OQ-66 cutover — raw tool output

Phase-1 collection. Every claim in `FINDINGS.md` cites one of these blocks. Pasted verbatim;
noise lines (corpus loader banners, `discontiguous` warnings from testset files) filtered by
`grep` on the reported markers only.

---

## A. Disk-verified corpus file counts (2026-07-25)

```
testsets                           199
testsets_haiku                     960
testsets_flash                     960
testsets_sonnet                    1001
testsets_kimi                      1005
archives/datasets/kernel_v1        1106
```

## B. Membership probe — six legs

Fact-existence query only; never touches classification or the cache path.
Its own positive control is the `kernel_v1` row (`registry_hits=2`), which is what makes the
five zeros measured-empty rather than didn't-look.

```
PROBE testsets n=199 beneficiary_facts=489 registry_hits=0 registry_values=[] raw_nlwb=0 divergence_n=0 divergence=[]
PROBE testsets_haiku n=960 beneficiary_facts=2502 registry_hits=0 registry_values=[] raw_nlwb=8 divergence_n=0 divergence=[]
PROBE testsets_flash n=960 beneficiary_facts=2174 registry_hits=0 registry_values=[] raw_nlwb=7 divergence_n=0 divergence=[]
PROBE testsets_sonnet n=1001 beneficiary_facts=3121 registry_hits=0 registry_values=[] raw_nlwb=3 divergence_n=0 divergence=[]
PROBE testsets_kimi n=1005 beneficiary_facts=1838 registry_hits=0 registry_values=[] raw_nlwb=19 divergence_n=0 divergence=[]
PROBE archives/datasets/kernel_v1 n=1106 beneficiary_facts=2314 registry_hits=2 registry_values=[entropic_universe_hypothesis,international_humanitarian_law_framework] raw_nlwb=29 divergence_n=1 divergence=[maxwell_demon_impossibility]
```

## C. MaxEnt unfitted under a plain `[stack]` load

```
MAXENT maxent_dist_facts_after_stack_load=0
MAXENT sample=abrahamic_covenant__isaac_covenant_reading ctx=context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)) top=FAILED(no fit)
```

Throw-vs-fail probe (run before writing any claim about the `abductive_triggers.pl` fallbacks):

```
ENTROPY FAILED (no exception) -- catch/3 does NOT intercept
```

`subsystem_available(maxent)` (`abductive_helpers.pl:183`) checks `maxent_run_info/3`, which is
also empty under a plain `[stack]` load — so the four `abductive_triggers.pl` clauses at
`:75,:126,:177,:231` fail at their FIRST goal, before reaching any fallback.

## D. Old suite baseline (pre-rewrite)

```
% [154/159] agent_beneficiary..stry_exact_contents .. passed
% [155/159] agent_beneficiary..rtifies_natural_law .. **FAILED
% [156/159] agent_beneficiary.._canonical_contexts .. **FAILED
% [157/159] agent_beneficiary..eleased_for_maxwell .. passed      <-- VACUOUS (\+ over absent constraint)
% [158-1..11/159] agent_beneficiary:fsm_still_fires .. **FAILED   (all 11)
% [159/159] agent_beneficiary..87_inertness_direct .. **FAILED
ERROR: 38 tests failed
% 131 tests passed
```

15 of 17 red in the unit; 38 total = 15 + 23 pre-existing corpus-story failures
(`mountain_threshold_validation` / `nl_profile_validation`), which matches the plan's
carry-forward note exactly.

## E. Fixture reachability (raw arm, before the filter landed)

```
FIXCOUNT n=4 [nlwb_ctl_agent_only,nlwb_ctl_mixed,nlwb_ctl_no_beneficiary,nlwb_ctl_nonagent_only]
RAW nlwb_ctl_agent_only            nlwb=false types=[snare,snare,scaffold,snare]
RAW nlwb_ctl_mixed                 nlwb=false types=[snare,snare,scaffold,snare]
RAW nlwb_ctl_no_beneficiary        nlwb=true types=[unknown,unknown,rope,unknown]
RAW nlwb_ctl_nonagent_only         nlwb=false types=[snare,snare,scaffold,snare]
```

The snare gate genuinely opens on `nlwb_ctl_nonagent_only` — a fixture whose gate never opened
would produce a no-flip that reads exactly like a working control.

## F. Repaired diff — flip detector first, then the real legs

```
LEG tests/fixtures/nlwb_controls n=4 maxent_dist_raw=16 maxent_dist_filtered=16 diff_count=1
  DIFF nlwb_ctl_nonagent_only
    raw:      snap([snare,snare,scaffold,snare],[snare,snare,scaffold,snare])
    filtered: snap([unknown,unknown,scaffold,unknown],[snare,snare,scaffold,snare])

LEG testsets n=199 maxent_dist_raw=724 maxent_dist_filtered=724 diff_count=0
LEG testsets_haiku n=960 maxent_dist_raw=3840 maxent_dist_filtered=3840 diff_count=0
LEG testsets_flash n=960 maxent_dist_raw=3840 maxent_dist_filtered=3840 diff_count=0
LEG testsets_sonnet n=1001 maxent_dist_raw=4004 maxent_dist_filtered=4004 diff_count=0
LEG testsets_kimi n=1005 maxent_dist_raw=4020 maxent_dist_filtered=4020 diff_count=0
LEG archives/datasets/kernel_v1 n=1106 maxent_dist_raw=4424 maxent_dist_filtered=4424 diff_count=0
```

`maxent_dist_*` non-empty in BOTH arms on every leg — the non-vacuity control the prior run
lacked. The fixture leg proves the harness can see a change; only then is a zero on the real
legs measured-empty.

## G. The maxwell measurement (opening datum for OQ-248)

```
MAXWELL maxent_dist_facts=4424 (non-vacuity control)
MAXWELL raw_nlwb=false
MAXWELL beneficiaries=[entropic_universe_hypothesis] agent_beneficiaries=[]
MAXWELL signature=coupling_invariant_rope
MAXWELL ctx=context(agent_power(powerless),...)     dr_type=rope shadow_top=rope entropy=0.15570568692893474
MAXWELL ctx=context(agent_power(moderate),...)      dr_type=rope shadow_top=rope entropy=0.15570568692893474
MAXWELL ctx=context(agent_power(institutional),...) dr_type=rope shadow_top=rope entropy=0.15570568692893474
MAXWELL ctx=context(agent_power(analytical),...)    dr_type=rope shadow_top=rope entropy=0.15570568692893474
MAXWELL   dist=[mountain-0.010000000000000009,rope-0.95,tangled_rope-0.010000000000000009,snare-0.010000000000000009,scaffold-0.010000000000000009,piton-0.010000000000000009]
```

Control against a degenerate fit (an identical `rope-0.95` at all four contexts is suspicious
on its face):

```
CTRL shadow_top_distribution_ctx1=[mountain-39,piton-4,rope-77,scaffold-73,snare-272,tangled_rope-641]
CTRL entropy min=0.0011 max=0.6111 mean=0.2052 n=1106
CTRL mountain_shadow_exists=animal_moral_status__property_reading
```

The fit spans all six types and the full entropy range; mountain shadows exist. The maxwell
reading is a real measurement.

## H. Post-landing fixture truth table

```
LANDED nlwb_ctl_agent_only     nlwb=false dr_type=[snare,snare,scaffold,snare]     metric_type=[snare,snare,snare,snare]
LANDED nlwb_ctl_mixed          nlwb=false dr_type=[snare,snare,scaffold,snare]     metric_type=[snare,snare,snare,snare]
LANDED nlwb_ctl_no_beneficiary nlwb=true  dr_type=[unknown,unknown,rope,unknown]   metric_type=[unknown,unknown,unknown,unknown]
LANDED nlwb_ctl_nonagent_only  nlwb=true  dr_type=[unknown,unknown,scaffold,unknown] metric_type=[unknown,unknown,unknown,unknown]
```

`agent_only` and `nonagent_only` carry IDENTICAL metrics and land opposite — so the block is
caused by registry membership, not by weak metrics.

## I. Rewritten suite

```
% [1/7] agent_beneficiary..stry_exact_contents ...... passed
% [2/7] agent_beneficiary..e_defaults_to_agent ...... passed
% [3/7] agent_beneficiary..ry_is_single_clause ...... passed
% [4/7] agent_beneficiary:registry_is_static ........ passed
% [5/7] agent_beneficiary..registry_membership ...... passed
% [6/7] agent_beneficiary..agent_filtered_view ...... passed
% [7/7] agent_beneficiary..re_config_constants ...... passed
% All 7 tests passed
```

## J. Gate + deliberate-break control

```
AGENCY GATE: GREEN
```

Break control — `drl_core.pl` reverted to the raw `constraint_beneficiary/2` read:

```
REVERTED
BREAK: gate throws agency_nlwb_set([nlwb_ctl_no_beneficiary])  [expected [nlwb_ctl_no_beneficiary,nlwb_ctl_nonagent_only]]
RESTORED
323:    \+ narrative_ontology:agent_beneficiary(C, _).
POST-RESTORE AGENCY GATE: GREEN
```

The gate detects a revert, and detects it at the FIXTURE pass — the live-corpus suite stays
green through a revert, which is exactly why the fixture leg exists.

## K. Pipeline + project gate

```
EXIT=0
BEFORE mtime=1784957692
AFTER  mtime=1784960633
[pipeline] [PROLOG] agency gate...
  Total time: 29.4s
```

Exit 0 AND output mtime advanced — not a false pass on a stale file (a gate abort leaves the
baseline in place and a diff then reads byte-identical).

```
# Gate checks
  ✓ issues_status    249 parsed, 0 malformed
  ✓ omega check      0 problems
  ✓ omega selftest   selftest: all positive controls fired (10/10)
  ✓ omega index      index --check: fresh (249 rows, 86 active / 163 archive)
  ✓ known_state      251 entries parsed, 0 problems
  ✓ axis boundary    [AXIS-SELFTEST] ALL PASS
  ✓ audit cites      ERRORS: 0
  ✓ gap surfaces     gap surfaces check: 3/3 human surfaces distinguish no_gap vs undetermined (self-test OK)
  ✓ cli selftest     cli selftest: OK (231 commands across 17 groups)

GATE: GREEN
```

## L. tangled_rope guard reachability (→ OQ-250)

`tr_body_control` = the tangled_rope clause body MINUS the `\+ nlwb` guard. It is the positive
control: large everywhere, so the `tr_guard_blocks=0` column is measured-empty, not didn't-look.

```
TR testsets                    tr_body_control=952   tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=0
TR testsets_haiku              tr_body_control=5809  tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=8
TR testsets_kimi               tr_body_control=3040  tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=19
TR archives/datasets/kernel_v1 tr_body_control=5461  tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=30
TR tests/fixtures/nlwb_controls tr_body_control=0    tr_guard_blocks=0  nlwb_and_enforcement=0  nlwb_total=2
```

kernel_v1 `nlwb_total=30` vs §B's pre-cutover `raw_nlwb=29` — the +1 is
`maxwell_demon_impossibility`, the predicate flip surfacing in an independent measurement.

## M. MaxEnt tangled_rope mirror is LIVE (two-sided, identical metrics)

```
MIRROR nlwb_ctl_nonagent_only nlwb=true  feature_val=true   boolLL(tangled_rope)=-12.0  boolLL(snare)=-4.0
MIRROR nlwb_ctl_agent_only    nlwb=false feature_val=false  boolLL(tangled_rope)=-8.0   boolLL(snare)=0.0
```

Deterministic guard dead, MaxEnt mirror live. The mirror is now covered in the gate's fixture pass
(`agency_maxent_tr_mirror_inert`, asserted relatively so a penalty-weight change does not red it).
