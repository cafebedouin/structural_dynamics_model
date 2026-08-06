# PREREGISTRATION — OQ-259 item 3, Part B0: origin-stability measurement (free arm)

Frozen at commit time, before any adjudication call. Authored fresh this session (not
pasted from the plan file or chat). B1 (the spend arm) has its own go-gate and is NOT
pre-registered here; this document does pre-commit the interpretation table B1's verdict
will be read off, per the plan's ordering requirement (table pinned before any B1 call).

## Role and scope

B0 measures the origin file's STRICT reproduce-rate under blinded adjudication across
the two Arm-0 same-input redraws (n=2). It is NOT tier selection (n=2 cannot select
tiers) and NOT a quantity that disciplines B1 arithmetically (see table preamble (a)).
The origin manifest's own carrier is adjudicated blind alongside the redraws.

## Inputs (pinned)

- ORIGIN: `audits/2026-08-03_kritik_ingest/biopower_k_nhi_debate_2026_20260803_102652.manifest.json`
- RUN1: `audits/2026-08-05_oq259_emphasis_discriminator/biopower_healthcare_kernel_2026_20260805_144612.manifest.json`
- RUN2: `audits/2026-08-05_oq259_emphasis_discriminator/biopower_nhi_debate_2026_20260805_144823.manifest.json`

Mode-comparability (recon, RECON.md §2): all three were `--dry-run --skip-search`; a
Part B null cannot be a mode artifact.

## Carrier definition

Per OQ-259 item 3 ("an omega or fracture-note"), the candidate carriers are every
omega's `description` text and each manifest's `fracture_scan.notes` text — 14 items
total (ORIGIN 3+1, RUN1 4+1, RUN2 4+1). A manifest attains a tier iff AT LEAST ONE of
its items is called at that tier.

## Two-tier rubric (pinned before adjudication)

- **STRICT** = the item asserts BOTH
  (i) that the source's arguments/readings are selected for strategic/competitive
      utility rather than truth-seeking, AND
  (ii) a fidelity consequence — readings may be exaggerated/strawmanned relative to the
      underlying literature, or an equivalent verify-against-primary-sources warning.
  Coverage/completeness caveats do NOT satisfy (ii). This is OQ-259 item 3's own
  pre-specified standard; any detection language keys to STRICT only.
- **TERRITORY** = the item names the competitive-debate genre of the source + makes a
  selection-pressure claim + carries ANY epistemic-consequence clause (fidelity OR
  coverage OR structure-import). Context tier; expected near-saturated.
- STRICT ⇒ TERRITORY, so a manifest's T-count ≥ S-count always. Per-item calls are
  STRICT / TERRITORY-only / NEITHER, each with a quoted basis phrase.

## Matching rule

Omega matching keys on rubric criteria ONLY. Packet items are description text alone:
`id` stripped, manifest position discarded, adjacent fields (`source`, booleans)
discarded, item order randomized. A name-churned flag whose content meets a tier IS a
reproduction at that tier (the origin's own flag name churned in both redraws). The
same rule governs B1.

## Redaction–scorability check (executed before packet finalization)

Pinned redactions, each verified to leave the item scorable against (ii):
1. `(NDI 2026)` → `([tournament/institute identifier redacted])` — provenance
   identifier; no (ii)-relevant content removed.
2. Three omega-id cross-references inside fracture notes → `[omega-id redacted]` —
   reduces same-manifest grouping leak; ids carry no rubric-relevant content.

Deliberately NOT redacted (redaction would destroy the basis of the call — plan rule:
keep the phrase and widen the declared leak):
- RUN1's coverage clause ("should not be read as claiming these are the only four
  coherent positions…") — the sole candidate basis of an (ii) call on that item; kept
  verbatim.
- Reading names (e.g. `totalizing_capture_reading`) throughout — removing them would
  gut several items' assertive content; they permit same-manifest grouping (declared
  leak below) but not origin-vs-redraw identification by a fresh adjudicator.

## Blinding, honestly scoped (a degraded blind, not a clean one)

- Adjudicator: a fresh instance without this session's read; receives ONLY the rubric
  and PACKET.md text inline; instructed to use no tools and no repo access.
- Commit-order blinding: PACKET.md + this prereg are committed BEFORE adjudication;
  the adjudicator's calls are committed BEFORE the label→item mapping file is added.
  The mapping file's md5 is pinned here: **`baa682f423d4af55d87e09aef059bd42`**
  (`b0_mapping.json`, seed 259, held out of the tree until the calls commit).
- PACKET.md md5: **`ae8878d6a14e5e4f918c1edd834e536c`** (14 items, labels ITEM-A…N).
- DECLARED residual leaks: item texts are content-memorable and provenance is
  reconstructible from the repo by anyone with repo access (the adjudicator is denied
  tools, but this is an instruction, not a mechanism); reading-name overlap and
  omega-id-redaction placement permit same-manifest grouping of items. This session's
  own unblinded read exists and is filed in RECON.md as hypothesis, not adjudication.

## Tally rule

- B0 STRICT reproduce-rate = (# of {RUN1, RUN2} whose item-set contains ≥1 STRICT
  call) / 2. Same for TERRITORY. Origin's blind tier reported alongside, not in the
  rate.
- **B0 disclosure rule:** if the redraws measure STRICT 0/2, the prereg discloses that
  item 3's bar is one the origin file itself did not clear at n=2, and interpretation
  rows 1–3 are read against that disclosure (stated, not silently dropped). Likewise
  if the ORIGIN item itself fails STRICT blind, that is stated wherever the origin
  flag is cited.

## Interpretation table (pre-committed before any B1 call; B1 verdict reads off this)

**Preamble (pinned):**
(a) The origin's B0 rate is n=2 (reachable values 0, ½, 1) and the B1 result is n=3 —
the comparison between them is QUALITATIVE CONTEXT ONLY and carries no inferential
weight; no cell's verdict depends on it.
(b) STRICT ⇒ TERRITORY constrains reachable cells to T ≥ S.
(c) Any outcome not matching a row below → catch-all: "indeterminate, no ruling" —
reported descriptively; no framing available to Part D beyond quoting the tallies.

| # | B1 fresh-file outcome (S, T each /3) | Licenses | Does NOT license |
|---|---|---|---|
| 1 | S=3 (T=3 forced) | "the pre-specified (i)+(ii) flag reproduced on a second independent source" (item-3 detection framing; ceiling per Part D) | "detection" in any general/cross-file sense |
| 2 | S=2, any T | observation: "strict form appears, redraw-brittle on this file" | reproduction claim; non-reproduction claim |
| 3 | S=1, any T | observation: "strict form appeared once" | reproduction claim; non-reproduction claim |
| 4 | S=0, T=3 | "the strict conjunction is churn-brittle; genre territory reproduces on a second source" — quote stays observation with territory support | treating S=0 as evidence against the origin observation; any item-3 claim |
| 5 | S=0, T=2 | observation: "genre territory appears but churns on this file" — quote stays observation, weaker support than row 4 | territory-reproduction claim |
| 6 | S=0, T=1 | observation: "genre-adjacent omega appeared once; neither tier stable" | any reproduction language |
| 7 | S=0, T=0 | non-reproduction at both tiers on this specimen (one file — not generalizable to arsenals) | evidence against the origin observation itself |
| 8 | anything else / scoring dispute | indeterminate, no ruling | everything |

Standing note for all rows: T=3 alone licenses at most "arsenal ingests generally
produce a genre-adjacent omega" — context, much weaker than item 3's language.

## B1 specimen + thresholds (pinned here; spend gated on the operator checkpoint)

- Specimen: `agent/analysis/originals/k_files/Afropessimism K Aff And Neg -
  Northwestern 2026.md` (214,416 B). Selection rule: producer-independence (measured
  content overlap) outranks topic-independence; smallest arsenal passing.
- Thresholds pinned from the measured control, not guessed: specimen passes iff shared
  8-word shingles with EACH item-1 source < 0.1% of the smaller file's shingle count;
  the positive control (Biopower K - CNDI 2026 × Biopower NW) must measure ≥ 5%.
  Session measurement: control 21.12%, specimen 0.0000% both pairings; execution
  re-runs the probe verbatim as the committed witness (`shingle_probe.py`).
- Evidential-only ingest: the specimen does NOT join the K-file corpus (the (f)
  verdict bars expansion). Mode matches origin (`--dry-run --skip-search`).
  Both-direction reporting; rubric-criteria-only omega matching; 3 serialized runs,
  input md5 around each, corpus-untouched check after each.
