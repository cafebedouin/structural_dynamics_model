# OQ-136 — Census absence buckets: authoring gaps vs genuine structural categories

**Pre-registration (FROZEN before any provenance join runs).** The git-log ordering of this
file's commit vs the execution commits is the freeze witness. Date: 2026-07-02.

## Question

For each absence/out-of-domain/unnameable bucket the totalized commentary census surfaces, are
its members an **authoring artifact** of the generation pipeline (fix at generation, possibly
mint a generation OQ) or a **genuine structural category** of the situations (keep + candidate
first-class reporting)? Disposition per bucket is an operator ruling (Ω_C tail); this audit
delivers the evidence table.

## Buckets in scope

`q6_unmeasured`, `q6_signature_unknown`, `extraction_unnameable`, `no_agent_seats`,
`manufactured_consensus_candidate`. (The consensus buckets enter the census via the OQ-137
enabling slice, commentary_census source `consensus`, same-day change — membership was NOT
inspected before this freeze; only aggregate counts were observed, see the register below.)

## Pre-committed statistic

Per (bucket × axis), axis ∈ {model, prompt_commit, topic_family}:

- **K×2 raw-count contingency tables**: bucket members vs rest-of-corpus, per stratum of the
  axis. `provenance_unauthored` is an explicit stratum of EVERY axis (the 9 `*_contradictions`
  files with no `story_provenance/8` are never dropped or merged). Raw counts only — **no rates
  anywhere** (the OQ-136 denominator caveat: rates move by domain-shrink with no change in the
  finding).
- **Test**: Fisher exact for 2×2 tables; permutation test on the χ² statistic for K>2
  (N=10,000 permutations, seed=20260702, scipy 1.15.3 present).
- **Decision rule (artifact axes)**: a bucket is *clustered on an axis* iff Holm-corrected
  p < 0.05 AND the most-enriched stratum has ≥3 in-bucket members AND its enrichment is ≥2×
  (stratum share among bucket members ≥ 2× its share among the rest of the corpus, computed from
  the raw counts).
- **Holm family (defined by RULE, not a pinned count)**: all (powered bucket) × (artifact axis:
  model, prompt_commit) tests, where *powered* means bucket n ≥ 8 at the re-witnessed count.
  Bucket sizes are data-dependent (extraction_unnameable shifted 5→3 over corpus growth n=72→119),
  so a literal family size would force phantom tests or contradict execution. Expected size at
  freeze time: 4 powered buckets (q6_unmeasured=26, q6_signature_unknown=16, no_agent_seats=26,
  manufactured_consensus_candidate=9) × 2 axes = **8 tests** — noted, not pinned; RECON.md
  re-witnesses the counts and the family follows the rule.
- **Topic axis sits OUTSIDE the corrected decision family.** Topic p-values are reported
  descriptively; topic "clustered" for labeling purposes = the same enrichment gates (≥3 members,
  ≥2×) with uncorrected p < 0.05, reported as descriptive. Rationale under Semantics.
- **Unpowered buckets (n < 8)**: descriptive tables only, no test; disposition rides entirely on
  the hand-read. At freeze this is extraction_unnameable (n=3).

## Pre-registered semantics, split per axis

Topic is NOT a valid artifact discriminator: a genuine structural category concentrated in a
subject domain would ALSO cluster by topic — topic-enrichment is predicted by BOTH hypotheses.
Hence:

- **Clustered on model or prompt_commit** ⇒ authoring artifact ⇒ propose a generation OQ.
  (Nothing about the world should correlate with which sampler or prompt version wrote the file.)
- **Clustered on topic alone** ⇒ ambiguous; disposition rides entirely on the hand-read.
- **No clustering on any axis + hand-read confirms diffuse** ⇒ genuine category ⇒ propose
  first-class reporting.
- **Mixed/discordant** ⇒ both readings to the operator, no default.

**Axis-confounding check**: the outputs include a topic_family × prompt_commit cross-tab (the
filename-prefix fallback can echo generation batches into topic), so Phase-4 adjudication reads
the axes jointly, never as independent votes.

## Axes — frozen derivation rules

- **model**: `story_provenance/8` arg 6 (Model). Missing file ⇒ stratum `provenance_unauthored`.
- **prompt_commit**: `story_provenance/8` arg 1 (PromptCommit). Missing ⇒ `provenance_unauthored`.
  (No stored run_tag exists; prompt_commit+model+date is the run proxy.)
- **topic_family** (frozen rule, applied in this order):
  1. If the testset authors `cs_kernel_id(C, K)` ⇒ topic_family = K.
  2. Else if the file base name contains `__` ⇒ topic_family = the prefix before the first `__`.
  3. Else topic_family = the base name with one terminal generation-batch tag stripped from the
     fixed list {`_contradictions`, `_flat_control`, `_c0`} (at most one strip; no other
     transformation).
  Files with no provenance still get a topic_family (the rule reads the filename/facts, not
  `story_provenance`); `provenance_unauthored` is a stratum of the model/prompt_commit axes and,
  for the topic axis, those files keep their derived family.

## Membership extraction (Phase 3, after freeze)

One swipl run emits `MEMBER Source C Bucket` per `commentary_cell/3` solution for sources
{q6, extraction_reading, consensus}; the script checks Σ members == loaded-N per source and
exactly-one-bucket per (source, C). **Extractor positive control**: drop one parsed member
in-memory, confirm the Σ check fires, then run clean. The audit script records its own git rev +
dirty flag + loaded corpus count (`RECON.md` cites that, never the possibly-stale pipeline
manifest). Join on `story_provenance/8` parsed from `prolog/testsets/*.pl`, with the `json/`
twins as a cross-check.

**Statistic positive controls (before reading any real table)**: (1) a synthetic all-one-model
bucket must flag as clustered; (2) a seeded uniform draw must not. Both pasted in the outputs.

## Hand-read protocol

Per member of `extraction_unnameable` (n=3 at freeze) and `manufactured_consensus_candidate`
(each names its `Excl` seats): paste the in-file authored fields; ask — is the unnamed/excluded
party genuinely absent from the situation, or an authoring oversight? Cross-sibling check
(same-kernel readings, same-topic siblings) BEFORE marking any call undecidable; cross-sibling
transfer is hypothesis-generating only — an in-file witness makes it ruled, else the call is
marked INFERRED (CLAUDE.md Cross-Sibling Disambiguation).

## Known-before-freeze register

Facts already observed during recon/Phase-1, before this freeze — listed so the git-log ordering
witness is honest about what "before any join runs" means:

1. The 9 `*_contradictions` testsets carry NO `story_provenance/8` (110/119 do) — itself a
   finding about the contradictions generation path; escalated in Phase 4 regardless of stats.
2. Bucket counts at n=119 (standalone census run, 2026-07-02, post-consensus-source):
   q6_unmeasured=26, q6_signature_unknown=16, extraction_unnameable=3, no_agent_seats=26,
   manufactured_consensus_candidate=9. (extraction_unnameable was 5 at n=72 — sizes are
   data-dependent.)
3. 67/119 files author `cs_kernel_id`; 39/119 base names contain `__`; live corpus is 119 files.
4. NO per-bucket membership list, provenance join, or contingency table has been computed or
   inspected as of this freeze.

## Verdict semantics of the deliverable

The deliverable is a per-bucket evidence row: clustered-on-which-axis (artifact axes vs
topic-only-ambiguous, or unpowered) × hand-read verdict × proposed disposition — read jointly
with the confounding cross-tab. Every disposition is the operator's ruling, not the audit's.
