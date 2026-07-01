# OQ-197 acceptance controls — kernel_v1 (i) + twins (ii), 2026-07-01

**Claim under test (the whole OQ-197 chain exists to make good on this):** the constraints
that motivated the fix surface as `undetermined`/labeled rather than **silent 0**. Two cases,
each with its **paired negative control in the same run**, and counts **reproduced from
substrate**, not inherited from the doc. Probe: `probe_oq197_controls.pl` (+ read-only load of
`detector_calibration.pl` for the net-new intersection). Raw: `controls_output.txt`.

## Case (i) — kernel_v1 total-absence

```
corpus_constraints=1106  stakeholder_facts=0  canonical_cross_seat_varying=944
source(a) stakeholder: gap=0 no_gap=0 undet(no_seats)=1106 undet(single_seat)=0 undet(single_power_position)=0
source(b) canonical:   gap=944 no_gap=152 undet(no_seats)=0 undet(single_seat)=10
canonical-varying under source(a): undetermined=944 gap=0 no_gap=0  (sum = 944)
```

- **Substrate reproduction:** `stakeholder_facts=0` (total absence confirmed) and
  `canonical_cross_seat_varying=944` — the doc's **944 reproduced exactly from the corpus**,
  not asserted.
- **POSITIVE (i):** all 944 canonical-structure constraints read `undetermined` under the live
  source (a) (`undetermined=944, gap=0, no_gap=0`) — never silent 0, never a false `no_gap`.
  (All 1106 → `undet(no_seats)`.)
- **NEGATIVE (i), same run:** source (b) canonical **discriminates** — `gap=944, no_gap=152` —
  so the probe is NOT the degenerate everything-`undetermined` fix; it fires where structure
  exists and reads `no_gap` where it doesn't. The undetermined-under-(a) is a genuine coverage
  fact (no stakeholders authored), not a stuck probe.

## Case (ii) — twins present-but-insufficient

```
                             net-new   net-new & stakeholders-present & detect_gap_pattern-fails (= doc "29/41")
testsets_haiku                 43                    29   → SPLIT under 3-valued: undetermined=4  no_gap=25
testsets_flash                 53                    41   → SPLIT under 3-valued: undetermined=12 no_gap=29
source(a) haiku: gap=365 no_gap=114 undet(no_seats)=466 single_seat=8 single_power_position=7
source(a) flash: gap=480 no_gap=232 undet(no_seats)=214 single_seat=17 single_power_position=17
```

- **Substrate reproduction:** `detector_calibration` net-new = **43 / 53** and the intersection
  "net-new ∧ stakeholders-present ∧ `detect_gap_pattern` fails" = **29 / 41** — the doc's exact
  `29/43` + `41/53` reproduced from substrate (needed the read-only `detector_calibration.pl`,
  which is where those figures were defined).
- **POSITIVE (ii), but a REFINEMENT the fix reveals:** the 29/41 are NOT monolithically
  undetermined. Under the three-valued contract they split — haiku 29 = **4 undetermined + 25
  no_gap**; flash 41 = **12 undetermined + 29 no_gap**. The 4/12 (`<2` power positions) are the
  genuinely-inexaminable ones the OLD `gap_coverage`≥1 emitted as a FALSE `[]`; they now correctly
  read `undetermined`. The 25/29 have ≥2 stakeholder seats spanning ≥2 power positions that
  genuinely **agree** → genuine `no_gap` (labeled "examined, agree"). **None read silent 0** — the
  "never silent 0" requirement is met at the labeled level for all 29/41.
  - Honest note: the doc's premise that the 29/41 were uniformly "present-but-**insufficient**"
    was imprecise. The fix shows most (25/29, 29/41) were present-AND-sufficient-but-agreeing
    (real `no_gap`); only 4/12 were truly insufficient. The literal control wording "the 29/41
    read undetermined" is therefore REFUTED for the majority — because those constraints were
    never insufficient, and the fix is more precise than the control's premise.
- **NEGATIVE (ii), same run:** source (a) produces all three values on both twins
  (haiku gap=365/no_gap=114; flash gap=480/no_gap=232) — real firings still fire, agreements
  still read `no_gap`. Not vacuously undetermined on the corpora where that failure would show.

## Verdict

Acceptance MET, with a refinement. The fix does what it was built for: on the two motivating
cases, nothing reads silent 0 — every constraint carries an explicit `gap_status` label. kernel_v1's
944 reproduced exactly and read `undetermined`; the twins' 29/41 reproduced exactly and split into
`undetermined` (the real false-`[]` rescues: 4/12) + genuine `no_gap` (25/29). Both negative controls
pass: the probe discriminates (produces gap AND no_gap) on both corpora, so the `undetermined` reads
are coverage facts, not a stuck detector.

## Caveats

- `detector_calibration.pl` is untracked/unwired WIP; loaded **read-only** here purely to reproduce
  its net-new set (the source of the 29/41 definition). Not wired into anything.
- "Never silent 0" is at the LABELED level: all 29/41 now carry `gap_status`. The old defect was that
  they emitted `[]`/absent indistinguishable from measured-no-gap; that distinction now exists.
- The refinement (25/29 are genuine no_gap, not insufficient) corrects the doc's premise, not the fix.
