# CI-rope limb: HEAD re-witness + inherited neutron_star RED adjudication (2026-07-02)

**Scope:** the two witness items Campaign 2 owed before the OQ-138 CI-rope route-purity ruling:
(1) staleness-ladder re-witness of the 5-consumer cost claim in `cirope_genuine_vs_washed.md`;
(2) the parked neutron_star RED adjudication (the FCR/OQ-70-threaded item: RED cap flattening
the ε=0.12 vs 0.68 magnitude gap against superheavy).

**Code state:** HEAD `f7b9db53` (2026-07-02), probes run from `prolog/` on the live `testsets/`
leg (119 files, `[corpus] Loaded 119 testsets successfully.`). Cross-check artifact:
`outputs/pipeline_output.json` manifest `pipeline_run_at=2026-07-03T00:05:43Z`,
`code_commit_short=5d6f219`, `code_dirty=true` (the dirty bit is the standing twin-manifest
caveat, OQ-75 preflight item 4; the two probe values below were reproduced live at clean-tree
HEAD by `red_adjudication_probe.pl`, so nothing in this doc rests on the dirty manifest alone).

## 1. The 5 rope-consumer sites — re-witnessed at HEAD (cost claim CURRENT)

All five sites from `cirope_genuine_vs_washed.md` are live at HEAD (grep 2026-07-02; line
numbers current as of `f7b9db53`, will rot — re-grep by predicate name):

| consumer | HEAD site | reading |
|---|---|---|
| dirac | `dirac_classification.pl:287,289` `type_to_dirac_class(rope, …, first_class)` | rope ⇒ first_class |
| purity | `drl_purity_network.pl:213` `type_immunity(rope, 1.0)`; `:200` contamination 0.1 | immunity 1.0 |
| cohomology | `grothendieck_cohomology.pl:506` `extraction_rank(rope, 1)` | rank 1 |
| boltzmann | `boltzmann_compliance.pl:363–364` `expected_power_divergence(_,_,rope,naturalized)` (+ inverse) | expected-divergence pair |
| maxent | `maxent_classifier.pl:352–353` `apply_override_for_sig(_, coupling_invariant_rope, …) → override_unconditional(rope, …)` | unconditional rope boost |

The KEEP-vs-convert cost structure is unchanged: converting CI-rope to route-purity still means
rewiring all five consumers (or losing first_class/immunity/rank-1 on genuinely-coordinating
seats).

## 2. neutron_star vs superheavy at HEAD — the parked RED is MOOT

Probe: `red_adjudication_probe.pl` (this dir); raw log `red_adjudication_probe.log`. Both seats
cross-checked against the canonical `pipeline_output.json` (values agree; superheavy's null
verdict is CLASS behavior — all 8 null-verdict seats in the 119-run are analytical-unknown, incl.
`organization_floor_c0`).

```
=== neutron_star_bombardment_reading ===
  claim=mountain  eps(extractiveness)=0.12
  metric_type=scaffold  dr_type=scaffold
  signatures=[false_ci_rope]  fcr_routed=yes
  signature_grade=commentary  signature_severity=informational
  boltzmann=compliant(0)  excess_extraction=0.099…
  base_verdict=yellow  join_verdict=yellow  cap=none  sig_grade=commentary
  alerts=[alert(signature_correction,informational,signature_grade),
          alert(type_1_false_summit,informational,claim_mismatch)]

=== superheavy_decay_reading ===
  claim=rope  eps(extractiveness)=0.68
  metric_type=unknown  dr_type=unknown
  signatures=[false_ci_rope]  fcr_routed=no
  signature_grade=commentary  signature_severity=none
  boltzmann=non_compliant(0.75,0.25)  excess_extraction=0.66
  diagnostic_verdict / verdict_join: ABSENT (unknown-typed seat; class behavior)
```

**Positive control (superheavy matches its post-FCR-9 state):** PASS — and the pass is grounded in
the substrate's explicit seat classification, not in "class behavior" (the earlier phrasing was a
control passing by absence, the byte-identical-to-never-looked shape; resolved below).

*Pre-registration (verbatim from the plan, `~/.claude/plans/i-d-like-to-make-cosmic-lecun.md`
Campaign 2 item 2):* "**Positive control (restated for HEAD): superheavy matches its post-FCR-9
state per FCR9_FINDINGS.md** — FCR-9 shipped milder discriminated grading, so 'still shows its
genuine FCR red path' would fail benignly if superheavy is among the routed seats."

*Substrate reconciliation (against the seat lists, not the report's characterization):*
- superheavy's post-FCR-9 seat classification is **documented explicitly**:
  `CONSTRUCTED3_FINDINGS.md:21` names `superheavy_decay` as "an **FCR inert seat** — caught by the
  sweep showing 4 routed when only 3 changed; §1 gotcha" (the bound-query gotcha that wrongly
  counted it as routed). So its `fcr_routed=no` is not a probe miss — it is the seat's authored
  post-conversion identity. My probe used the UNBOUND `constraint_signature/2` idiom (the §1 gotcha
  fix), so it reads the true cascade winner, not the bound-query artifact.
- **superheavy did NOT change under FCR-9**: 0 hits in `FCR9_live_diff.txt` (the 7 changed testsets
  seats). An inert seat's dr_type is preserved, not overwritten to tangled_rope — superheavy's
  preserved dr_type is `unknown`, so it sits in the 13-inert group's *unknown-surfaced* stratum.
- **Why the verdict is ABSENT, not present-and-mild:** superheavy's per-perspective types are
  `powerless/moderate/analytical = unknown`, `institutional = scaffold` (pipeline_output.json). At
  the default (analytical) context it is unknown-typed, and unknown-typed seats render no
  `diagnostic_verdict` — class behavior confirmed against the whole run (all 8 null-verdict seats
  in the 119-corpus are analytical-unknown).
- **The absence is DISCRIMINATED, not a probe that never fired:** neutron_star, in the SAME dump,
  renders a *present* verdict (yellow, two alerts) — so the probe's verdict machinery is proven
  live in this very run. superheavy's absence is therefore measured-empty, not didn't-look. The
  detector layer independently carries superheavy's genuine signal (boltzmann non_compliant coupling
  0.75, excess 0.66) — only the VERDICT surface is absent, per the OQ-37 honest-unknown convention.

**The parked item dissolves — two subsequent rulings each independently removed the RED:**

1. **OQ-128 discriminated type_1 severity (2026-06-17, `0a629077`; `drl_core.pl:620–644`):**
   mountain-claim degrade→snare stays SEVERE (red floor); degrade→ANY-OTHER type is
   INFORMATIONAL (no floor). neutron_star degrades mountain→scaffold ⇒ informational ⇒ no cap.
2. **FCR-9 route conversion (2026-06-21):** neutron_star is among the routed-9 — the
   tangled_rope overwrite no longer applies; dr_type routes to its metric type (scaffold), grade
   commentary/informational (vic=0), no signature floor.

Result at HEAD: neutron_star = **YELLOW, cap=none** (base yellow on its own metrics). The
"RED cap flattening a real magnitude gap" no longer exists — neither seat is RED, and the two
seats no longer render comparable verdicts at all (yellow vs verdict-absent). The magnitude gap
is now legible in the detector layer itself (excess 0.10-vs-0.66, compliant-vs-non_compliant),
which is where the 2026-06-14 secondary observation wanted it carried.

## 3. What this leaves for the ruling

- **Route-purity limb:** evidence complete and current. KEEP-as-written (recorded lean) vs
  convert+rewire-5. Nothing new at HEAD moves the lean; the kill condition (a consumer found
  treating rope as contested) stays live either way.
- **neutron_star sub-ruling:** the parked Position-A/discriminate-cap/leave-OPEN menu is moot —
  there is no RED to adjudicate. The residual question, if the operator wants it tracked, is the
  report-layer one already noted 2026-06-14: the convergence aggregate still flattens
  confidence/magnitude (0.10 vs 0.66) when two seats share `false_ci_rope`. That is
  presentation, not classification, and belongs in a report-refinement OQ if anywhere.
