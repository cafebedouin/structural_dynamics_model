# OQ-140 PROPOSAL — frozen pre-registration (awaiting operator ratification, esc. 1)

**Status: DRAFT — NOT YET FROZEN.** Freezes on operator ratification. Everything below is
committed BEFORE any hand-read spend, per the OQ-136 template. The blanks the operator sets
at ratification are flagged **[OPERATOR]**.

## 0. What recon settled (so the proposal doesn't re-open it)

- Population = 277 records / 96 constraints at HEAD `7762b2c0` (RECON §1).
- Primary cut = **granularity class** (G-A 56 / G-B 122 / G-C 99), Σ-checked.
- Three OQ-proposed axes (diagnostic mode, author mode, detector) are **degenerate**
  (100% one value each) — dropped as axes, recorded as silence findings.
- Confound = 100% of per-seat orbit variation is d-driven at fixed ε; confound-exposed
  share = **79.8%** (G-B+G-C). Committed for the reframe gate.
- Axis D is **powered** (49 natural raw≠final controls).

## 1. Sub-typing scheme (frozen)

**Primary cut — granularity class (Ω_E, neutral labels):** G-A "uniform-orbit divergence",
G-B "partial-match divergence", G-C "total-mismatch divergence". No genuine/artifact gloss
at Ω_E (that is the ruling's vocabulary, esc. 3).

**Secondary axes (per record):**

| axis | source | values |
|---|---|---|
| A. type-pair | membership.tsv (independent Prolog sourcing) | strata as (pair \| G-class) cells |
| B. seat + seat-shape | records grouped by constraint | seat ∈ {powerless, moderate, analytical, institutional}; shape ∈ {all_4, upper_3, institutional_only, other} |
| C. ε/χ band | pipeline_output `perspective_chi` + `config.pl` thresholds | `threshold_adjacent` (δ=**[OPERATOR]**, default 0.05) vs `deep` |
| D. signature involvement | raw `metric_based_type_indexed/3` vs final `dr_type/3` | **drop with "unpowered on divergence pop"** unless the Phase-2 scan finds >0 (positive control already witnessed: 49 baseline raw≠final seats) |
| E. mismatch tap | `provenance.mismatch` | carried; the 4 `type_3_snare_as_rope-severe` firings tied per-item to their firing clause in the writeup |

## 2. Ω_C candidate kinds — defined on (pair | G-class) cells, killed by structure

G-A is the confound-free population; kinds are named off G-A **first**. G-B/G-C cells are
**confound-until-shown**: no such kind is promoted unless the hand-read shows divergence the
ε/d-granularity mechanics do not explain.

| candidate kind | cell | n | burden |
|---|---|---|---|
| "permanence disagreement" (engine reads whole story transitional; author claims standing coordination) | rope→scaffold \| G-A | 36 | clean — name first |
| (same pair, different mechanism?) | rope→scaffold \| G-C | 25 | hand-read decides merge-or-split with G-A |
| small clean cell — read all | scaffold→piton \| G-A | 8 | clean |
| "conceded-entanglement, refused-coordination" (asserted per-cell, never over the 111) | tangled_rope→snare \| G-B / \| G-C | 63 / 48 | G-B carries confound-until-shown |
| "authored entanglement the engine absolves" (predicts ε near/below `tangled_rope_epsilon_floor`=0.30; C-band discriminates) | tangled_rope→rope \| G-B / \| G-C | 30 / 19 | confound-until-shown |
| pure-G-B tail — read all | snare→rope \| G-B (11), snare→tangled_rope \| G-B (7) | 18 | confound-until-shown |
| **clean G-A tail cells (recon-surfaced, plan did not name)** | rope→snare (4), piton→rope (4), rope→piton (4) — all G-A | 12 | confound-free; disposition at esc. 4 |

## 3. Kill structure (pre-registered per kind — FROZEN before hand-read)

- **Primary kill = segregation on a covariate.** If a kind's hand-read *failures* (reads
  where the authored text does NOT support the kind name) concentrate in one cell of a
  pre-registered covariate, split that cell off as a distinct stratum **regardless of
  count**. Covariates (frozen): **G-class** (where a kind spans classes), **C-band**
  (threshold_adjacent vs deep), **seat-shape**. Segregation rule (frozen): *all* failures in
  one covariate cell, OR ≥2× that cell's base rate.
- **Secondary floor = K≥3/10 diffuse failures** (failures that do NOT segregate) kills the
  kind name.
- Coverage is measured over **validated kinds only**.

## 4. Per-kind witness (frozen sample plan)

Stratified hand-read: **10 per dominant cell** (sampled across C-bands and seat-shapes,
seeded) **+ all members of cells n≤11 read whole** (scaffold→piton 8, snare→rope 11,
snare→tr 7, and the 3 clean G-A tail cells 4+4+4) ≈ **60–75 live-corpus reads** (subject to
the reframe gate). Each validated kind ships **≥2 exemplars** pairing in-file authored claim
text (`prolog/testsets/<id>.pl`) with the engine trace (seat, ε, χ, d, f_d, threshold
crossed).

## 5. Controls (pre-registered)

1. **Σ-checks (same-source, labeled):** class record-counts sum to 277 (56+122+99);
   per-axis sums; total 512. **Drop-one:** delete one record in-memory → every Σ-check must
   flip to fail (witnessed in `controls`).
2. **Independent mountain control:** count residual records with authored type `mountain`,
   author column sourced by direct `narrative_ontology:constraint_claim/2` (NOT the sink
   emit). Expected **0** (seat-blind ⟹ uniform mountain ⟹ routed to exit_table, never
   divergence). **Positive control:** plant a synthetic membership row with author=mountain →
   the probe must flag it. Nonzero *real* count = hard stop, escalate.
3. **ε-band classifier control:** planted records at `boundary − δ/2` and `boundary − 3δ`,
   same code path, two-sided.
4. **Axis-D ladder control:** the 49 baseline raw≠final firings already pasted
   (`d_ladder_control.log`) as the positive control; any Phase-2 divergence-population scan
   absence claim carries them.
5. **Same-run coherence:** one serialized `run_pipeline.py`; witness = exit 0 with BOTH
   `json_report` and `routing_sink` steps ok + equal `n_constraints` across manifests. mtime
   corroborates only. (NB routing_sink manifest lacks code_commit — coherence is the run,
   not the file; RECON §regime.)
6. **Emit-independence control:** membership.tsv author/engine columns re-derived by direct
   Prolog query, diffed against routing_sink.json fields — byte-agreement expected; any
   mismatch is itself a finding and **halts**.
7. **Statistics:** any enrichment claim gets a permutation test w/ pinned seed +
   planted-cluster + uniform-draw controls (OQ-136 pattern). If descriptive-only,
   pre-register here that no test family exists.

## 6. Reframe gate (esc. 2 — committed branches)

Committed confound-exposed share = **79.8%** (221/277). **[OPERATOR] sets threshold X.**

- **share ≥ X → REFRAME branch:** close on (a) the corrected primary cut (G-class + pair,
  Σ-checked), (b) the three silent-axis findings, (c) the confound as a committed quantity,
  + a residual OQ for the remaining real typing (n = confound-free 56 + any confound-shown
  G-B cells the operator wants pursued, stated explicitly). No 60–75-read spend.
- **share < X → FULL EXECUTION:** the §4 hand-read, kinds named + killed per §3.

Both branches are frozen here; the operator picks after reading the committed number.

## 7. Cross-corpus posture (frozen)

- **Twins (`testsets_haiku`/`testsets_flash`): Ω_E replication + bounded Ω_C spot-check.**
  Overlay `corpus_path` (asserta/retract-first), then
  `routing_sink:routing_sink_emit_to('../outputs/oq140_routing_sink_<twin>.json')`
  (`routing_sink.pl:289`; never `run_routing_sink/0` under overlay — hardcoded path at :325).
  **Honest two-level labeling:** a populated twin stratum witnesses only that the *Ω_E
  stratum* reproduces — never the kind. **Ω_C promotion to KNOWN_STATE requires ≥3 twin
  hand-reads at the kind's kill boundary passing on ≥1 twin.** A kind without twin Ω_C
  evidence records "Ω_E stratum reproduces; Ω_C replication not established" and stays OPEN.
  **[OPERATOR] may strike the twin Ω_C spend at ratification.**
- **Archives (`kernel_v1`): NO — residual OQ** (per-seat author_mode = different Ω_E base +
  re-key trap).

## 8. Validity scope (frozen)

Kind names are valid **relative to `route_address/5` at HEAD `7762b2c0`** — written into the
KNOWN_STATE entry and the compressed OQ-140 close, with the note that any sibling-clause edit
(OQ-138 territory) repopulates the residual and silently invalidates the taxonomy. The
wiring residual OQ carries this as a freshness condition.

## 9. Operator escalation points

1. **PROPOSAL ratification** (this doc) — kill structures, covariates, δ, X, twin spend.
2. **Reframe-vs-full** over the committed 79.8%.
3. **Kind names + Ω_C types** — evidence tables first (separably), candidate names second.
4. **Tail/killed-cell disposition** (absorb vs typed residual) — incl. the 3 clean G-A tail
   cells recon surfaced.
5. **Wiring spend-go** on the residual OQ (Ω_E descriptor tuple only).
6. **Any contested-origin hand-read member** — typed Ω per `docs/omega_variables.md`, listed,
   never binned.

## 10. Blanks for the operator to set at ratification

- **δ** (axis-C threshold_adjacent width) — default 0.05, arbitrary-but-frozen.
- **X** (reframe threshold on the 79.8% confound-exposed share).
- **Twin Ω_C spend** — keep (~3 reads × kinds × 1–2 twins) or strike.
- **Whether to pursue any confound-shown G-B cell in the reframe branch** (if reframe chosen).
