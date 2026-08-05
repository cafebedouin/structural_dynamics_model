# OQ-259 item 1 — Emphasis-aware discriminator (PROPOSAL ADDENDUM, pre-registered)

Date: 2026-08-05. Executor: Claude (Fable 5), local session. Plan:
`~/.claude/plans/serialized-hopping-yeti.md` (rev 3, post second operator review; plan
approval = spend-go). This addendum extends `audits/2026-08-03_kritik_ingest/PROPOSAL.md`
(committed `1bd57a84`) and is committed BEFORE any Phase-1 conversion or Phase-2 run. All
distribution evidence below was re-witnessed TODAY on the tracked `.docx` originals — not
carried from the prototype session.

Baseline anchors (md5, witnessed 2026-08-05, identical to `1bd57a84` blobs):

```
18f726ab17e77729765863fef9c65483  k_files/Capitalism K Aff And Neg - Northwestern 2026.md
722602a701d9e40dcc5836df71f3d072  k_files/Biopower K Aff And Neg - Northwestern 2026.md
8d2224c863a44a466ea2b94571d8055e  k_files/AT Fiat K - Michigan 2026 BCFP.md
```

## 1. Marker scheme (settled, with distribution evidence)

- **Markers:** `⟦HL⟧…⟦/HL⟧` (highlight = read-in-round layer), `⟦MIN⟧…⟦/MIN⟧`
  (minimized layer). U+27E6/U+27E7 collision check, witnessed today: absent from all four
  candidate `.docx` (`AT Fiat K`, `Cap K NW`, `Biopower NW`, `T Framework`) and from every
  baseline `.md` in `k_files/`.
- **ANNOTATE, not excise.** The excision variant (does read-in-round text ALONE recover
  tags?) is a DISTINCT unrun experiment — named here so it cannot be reached for post hoc
  (§9).
- **Yellow only** (`--highlight-colors yellow`). Witnessed highlight-color census:
  AT Fiat `{yellow: 405}`; Cap K NW `{yellow: 3728}`; Biopower NW `{yellow: 1152}` — the
  three specimens are yellow-pure. (`T Framework` is `{cyan: 943, yellow: 90, darkBlue: 1}`
  — cyan-dominant; that is item 2's parameterization, not this experiment's.)
- **MIN = effective size ≤ 16 half-points, style-chain-resolved** (run `w:sz` →
  `w:rStyle` chain with `basedOn` recursion → paragraph-style chain → docDefault).
  docDefault `w:sz` = 22 in all four files. Witnessed char-weighted effective-size
  distribution:
  - AT Fiat: sz=12 32.2%, sz=22 30.6%, sz=16 22.4%, sz=10 13.9% (total 64,960 chars)
  - Cap K NW: **sz=2 44.3%** (three giant runs of 327,603 / 122,310 / 11,979 chars —
    1pt near-hidden card-body mass), sz=16 23.6%, sz=22 21.8% (total 1,041,912 chars)
  - Biopower NW: sz=22 32.0%, sz=16 29.1%, sz=10 13.6%, sz=24 10.7% (total 280,705 chars)
  The ≤16 cutoff cleanly separates the minimized strata (16 and below, incl. the sz=2
  giants) from the read strata (22/24+) in every specimen.
- **HL wins over MIN** where a run is both highlighted and ≤16. Witnessed overlap is
  negligible: AT Fiat 13 chars, Cap K NW 68, Biopower NW 32 (of 6,413 / 48,671 / 51,328
  HL chars respectively) — the precedence choice cannot matter at these magnitudes.
  (The plan's prototype-session range "27–192 chars" is superseded by these re-witnessed
  numbers; conclusion unchanged.)

## 2. Predictions P1/P2, per-file split, independence check

**P1 (tag idiom).** Two halves, unequal risk:

- *Manipulation-check half (near-guaranteed by construction):* "tag idiom RAISED" —
  the evidence for the scaffold finding is the BASELINE precision a fortiori
  (4/6 and 5–6/6 under ~10× dilution), not the post-marking rise. The rise itself is a
  manipulation check; it carries no confirmatory weight.
- *Risky halves (the actual falsifiers):* (i) tag idiom DROPPING under emphasis-aware
  ingestion falsifies the whole framing; (ii) the four read-through readings — survive ⇒
  format property; vanish ⇒ conversion artifact.

**Per-file split of the read-through set (2+2, never pooled):**

- Cap K NW: `world_system_reading` (Ajl/Schmelzer unequal exchange),
  `growth_process_reading` (degrowth growth-imperative).
- Biopower NW: `coalition_governmentality_reading` (Foucault anti-state-phobia, Ilott),
  `insurance_as_risk_technology_axis` (Mei/Lobo-Guerrero; baseline altitude = DEFERRED
  axis).

**Independence check (witnessed today, line-anchored):**

- **Cap K pair CO-OCCURS — effective n nearer 1 than 2.** Ajl cards sit inside the same
  blocks that carry the degrowth material (`L---Green Transition` lines 345/353;
  `Alt---AT: Transition Offense` lines 963–1015, a "degrowth elides class analysis"
  block), and Schmelzer (line 1829, Aff `## Sustainability`) grounds both the
  accumulation and degrowth clusters. The two readings draw on one overlapping card
  cluster spread over a few shared blocks. **Ruling (pre-registered): the Cap K pair's
  survival outcome is reported at reduced weight — a 2/2 or 0/2 outcome there counts as
  ONE effective vote (weak per-file verdict), not two.**
- **Biopower pair INDEPENDENT — effective n = 2.** Coalition material: Aff `Perm---Do
  Both` (line 367) + Aff `## Alt` (473–487). Insurance material: Neg Framework
  `AT: Utilitarianism` (69–77) + Neg `2NC---AT: Perm Do Both` (261). Different sides,
  different sections, disjoint authors.

**P2 (granularity, independent of P1).** Link/Impact machinery stays absorbed at
`expected_structural_delta` altitude (WRITEUP Amendment 4). Surfacing as readings
falsifies position-vs-machinery. **Pinned machinery sections (2 per file, symmetric):**
Cap K Neg `## Link/Perm` (line 277) and Neg `## Impact` (line 411); Biopower Neg
`## Link` (line 113) and Neg `## Impact` (line 265). Marker-salience blind spot
(highlighted machinery might surface *because highlighted*) is covered by Arm 2.

## 3. Pre-specified ordinal thresholds, per file (exact numbers)

**Idiom scoring (unchanged from SCORING.md):** per manifest reading (kernel readings +
selected axes), idiom ∈ {tag, tag-leaning, mixed, card}; for counting, tag+tag-leaning
count as TAG, mixed counts as neither. **Pinned baseline tag counts:** Cap K 2/6
(accumulation tag-leaning, framework_competitiveness tag); Biopower 3/6 (totalizing,
empirical_falsification tag-leaning; counter_conduct tag).

**"Tag idiom RAISED" rule (per file; doubles as Arm-2 selection threshold):** the Arm-1
manifest shows TAG count ≥ baseline count + 1 **AND** TAG share > 1/2 of that file's
Arm-1 manifest readings. (Cap: ≥3 tag and majority; Biopower: ≥4 tag and majority.)
"Tag idiom DROPPED" (framing falsifier): TAG count ≤ baseline count − 1 AND TAG share
< baseline share. Between the two bands: idiom UNCHANGED — no claim either way.

**Read-through survival (per file, scored 0–2):** presence = a manifest entry (kernel
reading, selected axis, or deferred axis — ANY altitude) whose subject+stance matches the
read-through reading, name-blind; altitude changes recorded descriptively.

- Score = number of that file's WEIGHT-CARRYING read-through readings present in Arm 1.
  (Weight-carrying = survived the Arm-0 churn control, §4.)
- Effective n = 2 (both weight-carrying, pair independent): 2 → SURVIVE (format
  property, this file); 0 → VANISH (conversion artifact, this file); 1 → INDETERMINATE,
  no per-file ruling.
- Effective n = 2 but pair co-occurring (Cap K, per §2): 2 → weak-survive; 0 →
  weak-vanish; 1 → INDETERMINATE. Weak verdicts are stated as one-vote evidence.
- Effective n = 1: present → weak-survive; absent → weak-vanish.
- Effective n = 0: INDETERMINATE, no ruling for that file.
- Cross-file: "format property" / "conversion artifact" is claimed as the experiment's
  verdict only if every non-indeterminate per-file verdict points the same direction;
  otherwise per-file statements only, no pooled claim.

**P2 rule (per section × per replicate):** per file, score = number of that file's 2
pinned machinery sections whose machinery content surfaces as a manifest READING
(kernel reading or selected axis; deferred-axis or delta-altitude presence does NOT
count as surfacing).

- 0 → P2 supported in that file; 2 → P2 falsified in that file; 1 → INDETERMINATE,
  unless the same section also surfaced in either of that file's Arm-0 re-runs (then
  churn: recorded, discounted, file scored supported-with-note).
- Overall: P2 holds only if both files score supported; falsified if any file scores
  falsified; else indeterminate.

**Arm-2 selection rule (fixed NOW, before Arm 1 runs):** Arm 2 runs on the file with the
largest tag-idiom rise among files meeting the RAISED rule — largest increase in TAG
share (percentage points vs baseline); tie → larger absolute TAG-count increase; still
tied → Cap K NW (larger marker mass). If NO file meets the RAISED rule, Arm 2 is
**SKIPPED and recorded moot** — there is no positive rise for it to test, and the
manipulation-check half of P1 is simply not claimed. **Scramble seed pinned: 259.**
Scrambled output matches the real Arm-1 marking on marker count and span-length
distribution, placement randomized over eligible runs.

**Arm-2 verdict rule:** apply the same RAISED rule to the Arm-2 manifest. Scrambled run
ALSO raised → the rise is a marker-salience artifact, the claimed rise is WITHDRAWN
(P1's tag half then rests solely on the baseline a-fortiori evidence). Scrambled run not
raised → the rise is placement-specific and claimable.

**Selection-time caveat (stated, not hidden):** Arm-2 selection requires a preliminary
idiom count on the Arm-1 manifests BEFORE the Phase-3 blind. That preliminary count is
recorded in the audit dir at selection time; the FORMAL blinded Phase-3 scoring governs
the verdicts, and any preliminary-vs-formal disagreement is reported. The selection
itself is not revisited post hoc.

## 4. Quantified Arm-0 HALT rule

**Pinned baseline reading sets (subject+stance descriptors; name-blind matching):**

Cap K NW (6): (1) capitalism = accumulation/exploitation compulsion, harms structural,
reform legitimating; (2) capitalism = voluntary market-exchange institutions, harms
reform-correctable; (3) capitalism = imperial world-system of unequal exchange;
(4) capitalism = historically specific neoliberal financialized regime (reform targets
neoliberalism, not capitalism-as-such); (5) capitalism = growth imperative, separable
(degrowth); (6) framework axis: judges evaluate epistemology vs plan outcomes.

Biopower NW (6): (1) NHI as constitutive biopower (totalizing); (2) NHI as corrigible
institution (Illich/iatrogenesis); (3) autonomous counter-conduct vs state integration;
(4) anti-state-phobia coalition politics; (5) biopower apparatus empirically
underdetermined/unfalsifiable; (6) post-politics framing self-fulfilling/disabling.

**HALT rule:** for each Arm-0 re-run, reproduce-rate = (baseline readings with a
subject+stance match among the re-run's kernel readings + selected axes) / 6. If
reproduce-rate **< 2/3 (i.e., < 4 of 6)** in EITHER re-run of a file, HALT before any
Arm-1 spend and reassess — the predicted effects are inside the churn floor.

**Churn control (within-file only):** each of the four read-through readings must be
present (ANY altitude, §3 presence rule) in BOTH of its file's Arm-0 re-runs to carry
P1 weight. A churned reading carries no weight; it shrinks that file's effective n.

**Floor, not estimate:** n=2 per file bounds churn from below; a reading stable across
2 re-runs may still churn at rate up to ~1−(2/3)^(1/2) ≈ 18% per run without detection.
The discount rule is anti-conservative at this n; stated plainly, not repaired.

## 5. Denominators, predicted lists, coherence bar

Reused UNCHANGED from PROPOSAL.md (`1bd57a84`): N = Cap 10, Biopower 9, AT Fiat 6;
predicted reading lists verbatim; hit/idiom/precision/recall definitions verbatim.
Coherence bar: the EXISTING emotives control manifest
(`macintyre_after_virtue_emotivism_20260803_102123`) — no new control run.

## 6. Run order and exact commands (serial; ≤ 8 decompose calls; no `--auto-bypass-refusal`)

```bash
cd /home/scott/bin/structural_dynamics_model
# Arm 0 — churn floor (committed baseline .md, byte-identical to 1bd57a84 — md5s above)
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/Biopower K Aff And Neg - Northwestern 2026.md"   # A0-B1
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/Biopower K Aff And Neg - Northwestern 2026.md"   # A0-B2
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/Capitalism K Aff And Neg - Northwestern 2026.md" # A0-C1
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/Capitalism K Aff And Neg - Northwestern 2026.md" # A0-C2
# → per-file HALT check (§4) — no Arm-1 spend unless both files pass
# Arm 1 — emphasis-aware conversions (this audit dir)
python3 agent/c-orchestrator.py --dry-run --skip-search "audits/2026-08-05_oq259_emphasis_discriminator/AT_Fiat_K_emphasis.md"
python3 agent/c-orchestrator.py --dry-run --skip-search "audits/2026-08-05_oq259_emphasis_discriminator/Biopower_NW_emphasis.md"
python3 agent/c-orchestrator.py --dry-run --skip-search "audits/2026-08-05_oq259_emphasis_discriminator/Cap_K_NW_emphasis.md"
# Arm 2 — scrambled control on the §3-selected file (or SKIPPED as moot)
python3 agent/c-orchestrator.py --dry-run --skip-search "audits/2026-08-05_oq259_emphasis_discriminator/<selected>_scrambled.md"
```

Per-run checks (each pasted at Phase 2): `[ingest]` headroom line captured; whole-doc
single-prompt ingest confirmed from the log (any chunking/windowing → boundary
comparability logged explicitly); **no `*_brief.md` written — HALT if one appears**
(Cap K ≈ 340–400K tok vs the 975,616 decompose cap, asserted per run); markers present
in the exact file the positional arg names (Arm 1/2 only); corpus untouched
(`git status` clean on `prolog/testsets/`, no new `json/*.json`). Arm-0 inputs
re-md5ed immediately before each run (frozen-corpus discipline).

Spend: ≤ 8 Sonnet decompose calls, ≈1.4–1.8M input tokens (Arm 0 ≈ 886K is the bulk) →
order $4–6. Plan approval = spend-go (operator-approved plan, this session).

## 7. Marker-effect direction clause

The token re-dilution argument (markers add tokens mostly inside card bodies) makes
marker annotation CONSERVATIVE for the tag-idiom half only. For the read-through half,
MIN annotation has **UNKNOWN sign** — wrapping card bodies in `⟦MIN⟧` may raise their
salience (an explicit "here is minimized text" label) rather than suppress them. The
write-up may not lean on "conservative" globally.

## 8. Blinded scoring protocol (Phase 3) + commit-order evidence

- Per file: scoring packet = per-manifest reading lists (reading id, human_readable,
  commitment, authority_grounding, expected_structural_delta; deferred axes included)
  from all arms present (2026-08-03 baseline, Arm-0 pair, Arm 1, Arm 2 if run) under
  shuffled neutral labels (M1…Mk). Shuffle mapping generated programmatically (seeded
  from packet content hash), written to a mapping file NOT read until after the calls
  commit.
- **Redaction pass:** emphasis/marker/highlight/read-in-round vocabulary stripped from
  packet text before scoring.
- **Partial blind, stated:** file identity is unmaskable; the baseline/Arm-0
  near-identical cluster is identifiable as such; content-level tells may survive
  redaction; executor and scorer are the same agent. SCORING.md states all of this.
- **Commit order is the blind's evidence:** recorded idiom/hit/presence calls committed
  FIRST; the label→arm mapping committed SECOND; SCORING.md cites both hashes.

## 9. Scope limits (pre-committed)

- Item-1 manifests CANNOT serve as OQ-259 item 3's independent genre-flag replication —
  same sources; a genre flag here is a same-source rerun observation, not the
  pre-specified second independent arsenal.
- The EXCISION arm (read-in-round text alone) is named as UNRUN. Nothing in this
  experiment licenses claims about it.
- T Framework's committed baseline `.md` does not reproduce under the pinned pandoc
  recipe (prototype-session observation) — logged for item 2, out of scope here.
- Arm-0 results measure decompose stochasticity + model/API drift only; the conversion
  environment is excluded by the byte-exact restore check (Phase 1), not by Arm 0.
