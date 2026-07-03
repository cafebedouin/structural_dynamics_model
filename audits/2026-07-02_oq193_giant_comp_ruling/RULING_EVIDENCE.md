# OQ-193 giant_comp sibling edges — ruling evidence (2026-07-02)

**Question:** giant_component_analysis counts same-kernel sibling `affects_constraint` edges as
genuine topology. Correction (guard the explicit channel), loss (intended topology, document),
or additive split (report both numbers)?

**Code state:** HEAD `354663b2` (2026-07-02). Probes read-only w.r.t. substrate (reversible
strips, verified restore, caches cleared around each mutation). Corpus overlays via
`retractall(config:param(corpus_path,_)), assertz(...)`.

## 1. Ripple re-witness at HEAD (probe_giant_ripple.pl)

Strip = same-kernel `affects_constraint` edges (`cs_kernel_id` equality — the correct
discriminant per HOLD_FINDINGS.md; every same-kernel edge is a reading-sibling, 0 non-sibling
found in the prior census). Positive control PASSED every leg (raw edge count dropped by exactly
the strip count).

| leg | n | strip edges | giant size old→new | components old→new | gc edges old→new |
|---|---|---|---|---|---|
| testsets | 110* | 64 | 12 → 9 | 69 → 90 | 55 → 21 |
| testsets_haiku | 960 | 1810 | 549 → 47 | 123 → 670 | 1431 → 489 |
| kernel_v1 (archive) | 1106 | 1516 | 334 → 70 | 276 → 789 | 1438 → 607 |

*giant_comp counts loaded constraints minus phantoms; the manifest is 119 for testsets.

The ripple is LARGE and confirmed at HEAD: on kernel_v1 the headline giant drops 334→70 (the
2026-06-29 number reproduces exactly); on the never-before-measured haiku twin it is even more
dramatic (549→47, a 91% collapse). **The single headline number is dominated by sibling
plurality** — most of the "giant component" is readings of the same kernel linked to each other.

## 2. Per-consumer price (probe_consumer_price.pl) — the correction arm cost

Same-kernel strip, diff at strip-incident endpoints only. Both substrate controls PASSED
(strip applied and restored exactly).

| consumer | testsets (34 endpoints) | testsets_haiku (950 endpoints) |
|---|---|---|
| **FPN effective_purity** | **NO DIFF** | **NO DIFF** |
| json_report neighbor sets | CHANGED (34) | CHANGED (950) |
| network_dynamics degree/hub | CHANGED (34); **15 hub flips** | CHANGED (950); **282 hub flips** |
| network_drift_severity | CHANGED (8) | CHANGED (88) |

**Hypothesis confirmed exactly:** FPN purity is UNCHANGED (the OQ-23 same-kernel-donor guard in
`compute_edge_contamination/7` already zeroes sibling contamination), while the three
topology-reading consumers — neighbor lists, hub degrees/severity — DO change. So a correction
in `constraint_neighbors_existing/2` would be redundant for FPN but would move json_report's
neighbor output, network_dynamics hub classification (15/282 hub-status flips), and drift
severity.

### The FPN-unchanged claim is two-sided controlled (not a didn't-look)

The plan required proving the probe WOULD detect an FPN change (else NO-DIFF is vacuous).
- **testsets:** `PC_FPN_DETECTS ok` — a planted cross-kernel single-edge strip
  (`apoe4_mitochondrial_vulnerability → mitochondrial_demand_signal_deficiency`) moves the
  receiver's effective_purity 0.4104→0.4292 and restores. The probe demonstrably detects
  explicit-edge purity change; the same-kernel NO-DIFF is therefore MEASURED-empty.
- **testsets_haiku:** the alphabetical 20-edge sample did not move purity, so an ADVERSARIAL
  control (`probe_fpn_control_adversarial.pl`) selected cross-kernel edges by expected
  contamination (receiver purity − donor purity, donor type-factor > 0). Result: **0 such edges
  exist on the haiku leg** — no cross-kernel explicit edge carries any expected contamination
  there, so the FPN NO-DIFF is *trivially* consistent and unwitnessable-by-strip on this leg.
  The scorer is positive-controlled: on testsets it ranks the known-moving apoe4 edge first
  (score 0.075) and fires `PC_FPN_DETECTS_ADVERSARIAL ok`. So the FPN-unchanged witness rides
  the testsets leg; haiku is consistent-but-vacuous, stated honestly rather than claimed as a
  measurement.

## 3. Downstream-consumer fact (unchanged from exploration, re-confirmed by grep)

The giant_comp headline is DESCRIPTIVE-ONLY: `giant_component_analysis` writes
`outputs/giant_component_analysis.md` with zero downstream classification consumers. The
same-kernel asymmetry is already documented at `drl_purity_network.pl:59–78` (OQ-23 fix comment)
and `:289–294`, which explicitly say the giant_comp topology is left unchanged pending this
ruling and warn against extending the guard into `constraint_neighbors_existing/2` without it.

## 4. The three options, priced

(a) **correction** — add the same-kernel guard to `constraint_neighbors_existing/2` (mirror the
`:115` shared-agent intra-kernel guard onto the two explicit findalls). Cost: changes ALL FIVE
`constraint_neighbors/3` consumers, of which FPN is provably a no-op and json_report +
network_dynamics (×3) + giant_comp genuinely change (15/282 hub flips, severity shifts). Headline
restates to cross-kernel (~9 on testsets, ~47 on haiku, ~70 on kernel_v1). Owes the 5-consumer
old-vs-new pipeline diff before landing.

(b) **loss/document** — sibling edges are intended topology; record the asymmetry rationale at
the existing comment anchors. Zero code change. Leaves the headline conflating reading-plurality
with cross-topic structure.

(c) **additive provenance split** — leave topology untouched; make giant_comp report BOTH the
pooled number AND the cross-kernel stratum (the strip result is already computed here). Since
nothing downstream consumes the headline, the per-consumer cost of (a) is avoided and the
misleading single count is fixed at the one surface that is actually read. **(c) is NOT neutral
between (a) and (b): it is (b)-plus-a-report-fix.** Choosing it RULES that same-kernel sibling
edges are *intended topology for all five `constraint_neighbors/3` consumers* — including the
network_dynamics hub degrees this probe shows flipping 15 (testsets) / 282 (haiku) under a guard.
The "zero blast radius" describes only the report surface; the topology ruling underneath is
"siblings stay in the graph." That may well be right — the zero-consumer + FPN-unchanged facts
support it — but it must be ruled as a topology decision, not sold as costless.

**The witnesses point at (c):** the ripple is real and large (the single number IS misleading),
FPN — the only classification-feeding consumer — is provably unaffected, and the headline has no
downstream consumer. (a)'s consumer churn (282 haiku hub flips) buys correctness in surfaces
nobody classifies on. Operator's seat: rule whether same-kernel siblings are intended topology
(→ b or c; c additionally fixes the report count) or should be guarded engine-wide (→ a).
