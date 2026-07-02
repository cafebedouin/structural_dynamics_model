# OQ-136 writeup — census absence buckets × generation provenance

Design: PROPOSAL.md (frozen at commit `0ba48b4c` BEFORE any join ran — git-log ordering is the
freeze witness). Execution: `python/audits/oq136_bucket_provenance.py` at git `0ba48b4c`
(dirty=True; the script's own stamp, RECON.md). Corpus n=119 (loader count from the extract
run). Evidence: membership.tsv, contingency_tables.md, stats_output.json, HANDREAD.md.

## Controls (all fired; RECON.md)

- Extractor Σ-check: dropped member → `[q6] sigma members (118) != n_corpus (119)`; duplicated
  member → `non-exactly-one cids` — both fire; clean run passes.
- Statistic pair: planted all-one-stratum bucket flagged (p=2.8e-07, gates pass); seeded uniform
  draw not flagged (p=1.0).
- json-twin cross-check: 110/110 provenance blocks match the .pl facts, 0 mismatches.

## Results (raw counts; Holm family = 8 = 4 powered buckets × 2 artifact axes)

| bucket | n | model axis | prompt_commit axis | topic (descriptive) | hand-read |
|---|---|---|---|---|---|
| q6_unmeasured | 26 | **CLUSTERED** p_holm=8.0e-4 | **CLUSTERED** p_holm=8.0e-4 | not clustered (p=.86) | — |
| no_agent_seats | 26 | **CLUSTERED** p_holm=8.0e-4 | **CLUSTERED** p_holm=8.0e-4 | not clustered (p=.75) | — |
| q6_signature_unknown | 16 | not (p_holm=.59) | not (p_holm=.59) | not (p=.93) | — |
| manufactured_consensus_candidate | 9 | not (p_holm=.95) | not (p_holm=.59) | not (p=.44) | 8/9 genuine, 1/9 authoring inconsistency |
| extraction_unnameable | 3 | UNPOWERED (descr.: 3/3 haiku, ratio 4.6) | UNPOWERED | UNPOWERED | seat limb artifact 3/3; victim limb genuine 2/3 RULED |

The driving strata for the two clustered buckets are identical:
**claude-haiku-4-5 (16 of 28 haiku files in each bucket)** and **provenance_unauthored (9/9 —
every `*_contradictions` file)**, with claude-sonnet-4-5 nearly absent (1/64 and 0/64). The
overlap is almost total: **25 of 26 q6_unmeasured members are also no_agent_seats members** —
one generation-path artifact expressing in two census buckets. Axis-confounding cross-tab: only
9/79 topic families span >1 prompt_commit; topic and prompt_commit are largely aliased at the
family level, which is why the topic axis was pre-registered as non-discriminating.

## Interpretation (per the pre-registered semantics)

1. **q6_unmeasured + no_agent_seats = AUTHORING ARTIFACT** (clustered on both artifact axes;
   nothing about the world correlates with which sampler wrote the file). Two generation paths
   under-author: the **haiku path** emits prose + constraint_beneficiary but no
   `founding_problem_status` and no `stakeholders[]` facts (HANDREAD: the prose *plans* the
   seats — "the payer seat (palestinian_presence_interpreters) should compute…" — the fact
   layer never emits them); the **contradictions path** authors neither, and also stamps no
   `story_provenance/8` (the known-before-freeze register, finding (i)).
2. **q6_signature_unknown = NOT an authoring artifact** (no clustering on any axis; spread
   ~proportionally over the sonnet-4-5 majority). Consistent with its OQ-136 framing as the
   config-VARIANT computational component (dr_type=unknown moves with thresholds, not with the
   generator). Genuine category of the corpus×config pair.
3. **manufactured_consensus_candidate = GENUINE CATEGORY** (no clustering + 8/9 hand-read
   genuine with in-file witnesses; the exclusions are deliberately authored and substantively
   argued). One member (radiative_levitation_stratification) is a per-member false positive BY
   ITS OWN TEXT ("exclusion is evidential, not structural") — naming a vocabulary gap: the
   `excluded` role atom cannot express evidential-vs-structural exclusion.
4. **extraction_unnameable = COMPOUND**: the extractor-seat limb is the same haiku seat-authoring
   artifact (fixing it migrates all 3 members to `extraction_fired`); the victim limb is
   genuine-to-the-reading (RULED in 2/3: "the reading does not recognize animals as victims";
   "under this reading's own terms, Palestinians are not victims"). Unpowered — descriptive only.

## Proposed dispositions (operator rulings — one per bucket; nothing below is self-ratified)

| # | bucket | proposed disposition |
|---|---|---|
| R1 | q6_unmeasured | artifact → mint a generation OQ: haiku + contradictions paths must author `founding_problem_status` (or the gap is declared per-path in design_gaps) |
| R2 | no_agent_seats | artifact → same generation OQ (stakeholders[] emission); keep the census bucket (it is the honest surface that CAUGHT the artifact) with its out-of-domain mapping as-is |
| R3 | q6_signature_unknown | genuine (config-variant) → keep; no first-class reporting change needed (already a census absence bucket) |
| R4 | manufactured_consensus_candidate | genuine → candidate first-class reporting (a seated stage on the census); if ruled first-class, the provisional `no_agent_seats` out-of-domain declaration is NOT affected (different bucket), but the `excluded`-role vocabulary gap (evidential vs structural) is worth its own small OQ |
| R5 | extraction_unnameable | compound → covered by the R1/R2 generation OQ on the seat limb; keep the bucket (post-fix membership becomes the genuine both-sides-unnamed residue) |
| R6 | `*_contradictions` provenance stamping | the contradictions generation path stamps no story_provenance/8 — fix-on-sight at the generator vs mint OQ (it is cross-cutting: it also blanks the provenance axes for EVERY future audit of these files) |

## Scope boundary

Findings are about the LIVE post-reset corpus at n=119 under the code state `0ba48b4c` (dirty;
corpus and code named per the corpus×code-state citation rule). Bucket sizes are data-dependent;
the artifact verdicts ride on provenance clustering + in-file witnesses, not on the counts.
Generation is stochastic: per-story provenance was the join key throughout; no cross-run name
identity was assumed.
