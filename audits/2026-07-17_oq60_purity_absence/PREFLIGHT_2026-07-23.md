# OQ-60 Preflight (execution 2026-07-23, plan `can-you-review-oq-60-reflective-clarke.md`)

## (a) Corpus snapshot equality vs `pin.txt` (census 2026-07-17)

Method: per-leg file count + md5 over sorted concatenation; `git diff --stat f3e89f43..HEAD`
per leg; census TSV sha256 re-checked.

| leg | pinned files | live files (2026-07-23) | verdict |
|---|---|---|---|
| testsets | 145 | 189 | **DRIFTED** (+38 tracked new, +13 untracked new; insertions only, no modifications to pinned files) |
| testsets_haiku | 960 | 960 | unchanged (empty git diff since pin commit) |
| testsets_flash | 960 | 960 | unchanged (empty git diff since pin commit) |
| archives/datasets/kernel_v1 | 1106 | 1106 | unchanged (empty git diff since pin commit) |

Census TSV integrity: all four sha256 match `pin.txt` (re-run 2026-07-23).

### testsets census RE-RUN (per plan branch: drift ⇒ re-run probe, not re-derive)

Command: `swipl -g "[stack], consult('.../census_oq60.pl'), run_leg(testsets, '.../census_testsets_2026-07-23.tsv'), halt"` — engine = HEAD 2026-07-23 (pinned engine + inert 0a plumbing).

```
[census] leg=testsets population=189
[census:testsets] pop=189 gate_pass=155 | m1=17 m2=17 m3=17 m4=17 m5=28 any=28
[control:testsets] BARE gate=pass purity=1.0  m1=1 m2=1 m3=1 m4=1 m5=1 (expect all 1, purity=1.0 pre-fix)
[control:testsets] POSITIVE CONTROL OK — all 5 branches fire
```

**Load-bearing fact RE-ESTABLISHED on the drifted corpus: m1–m4 GATE-PASS victims = 0**
(raw m1–m4=17 are all gate-fail rows; disposition column already folds this: gate-fail ⇒
sentinel). Gate-pass m5 victims: **11** (was 9). Cross-checks: 0 MISMATCH rows.

### Updated expected flip set — testsets (disposition==unknown, census_testsets_2026-07-23.tsv)

```
ability_ceiling_reading         0.3541666666666667
access_barrier_reading          0.3541666666666667
conceptual_framework_reading    0.9720000000000001
deflationary_reading            0.988               (NEW since 2026-07-17)
epistemic_collapse              0.5
interactionist_reading          0.4061666666666667
pragmatic_action_reading        0.4061666666666667
pre_public_initiative_reading   0.4061666666666667
quantitative_growth_reading     0.3541666666666667
sufficiency_reading             0.9720000000000001  (NEW since 2026-07-17)
vocabulary_collision_reading    0.9480000000000001
```

Expected flips are now **11 / 2 / 80 / 2** (testsets / haiku / flash / kernel_v1). The
C-FLOOR join for testsets uses `census_testsets_2026-07-23.tsv`; the other three legs use
their pinned 2026-07-17 TSVs. Money pair unchanged and joined by two new near-pristine
fabricated-floor scores (`deflationary_reading` 0.988, `sufficiency_reading` 0.972).

## (b) Absence-token serialization — census half

One line per format, each checked against an actual census row:

- **Gate-fail sentinel:** literal string `-1.0` in the `purity` column (34/34 sentinel rows
  in census_testsets_2026-07-23.tsv carry exactly `-1.0`; example row:
  `actinide_replenishment_mechanism_contradictions  fail  -1.0`).
- **`unknown` atom:** appears in NO purity/subscore cell of any census TSV at HEAD (awk
  non-numeric sweep over all five TSVs matched only header rows) — consistent with "nothing
  produces `unknown` at HEAD"; the census encodes the *post-fix expectation* in the
  `disposition` column (`scored` / `unknown` / `sentinel`), not in the value columns.
- **Disposition tokens:** `scored` | `unknown` | `sentinel` (col 18); `sentinel` ⇔ gate-fail
  ⇔ purity `-1.0`; `unknown` ⇔ gate-pass ∧ any mechanism; `scored` otherwise.

**Engine half** — pinned by (c) below: at the real emitter (`json_report:write_per_constraint_entry/4`,
pipeline load chain), an `unknown` scalar serializes as `"purity_score": null, "purity_band": null`
(`write_json_number(S, unknown)` → `null` at `json_report.pl:2380`; band routed to `null` by the
`number(PScore)` guard at `json_report.pl:325-328`). Join assumption now verified at BOTH producers:
census TSV `-1.0`/disposition tokens vs engine JSON `null`.

## (c) Retro-witness of 0a/0a.2 — injected `unknown`, end-to-end (2026-07-23)

Method: gotchas §3 swap (`abolish` + re-assert with dispatch clause keyed on
`user:oq60_inject_target/1`), pipeline load chain (`stack, covering_analysis, maxent_classifier,
dirac_classification, diagnostic_summary, post_synthesis, json_report`), corpus loaded (189),
caches cleared per §7. Probe: scratchpad `preflight_c_probe.pl`; durable encoding:
`prolog/tests/test_purity_absence.pl` unit `purity_absence`.

```
[pre ] bare purity=1.0 (EX=1.0)  golden purity=0.3541666666666667
[mid ] EX subscore: bare=unknown golden=unknown (expect unknown/unknown)
[mid ] non-target epistemic_collapse EX=0.0 (expect number)
[step1] purity_score: bare=unknown golden=unknown (expect unknown/unknown)
[step2] purity_zone(unknown) = unknown (expect unknown)
[step3] emitter: "purity_score": null PRESENT
[step3] emitter fragment: "purity_score": null,
      "purity_band": null,
[step4] effective_purity: bare=unknown (purity_components(unknown,0.0,no_neighbors)) golden=unknown
[post] bare purity=1.0 golden purity=0.3541666666666667 (expect 1.0 / 0.3541666666666667)
[post] RESTORE VERIFIED
```

All three §3 dispatch controls present (pre original / mid visible-at-consumer + non-target
unaffected / post restore verified). **0a's propagation guard (`purity_scoring.pl:54-55`) and
0a.2's consumer guards (emitter, `effective_purity`) are LIVE, not shadowed** — the retroactive
positive control the null-diff witnesses lacked. `[pre]` also re-exhibits the OQ-60 defect at
HEAD: bare gate-passing constraint scores 1.0.
