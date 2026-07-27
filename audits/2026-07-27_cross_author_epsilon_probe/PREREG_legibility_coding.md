# Pre-registration — channel-legibility coding of ε-spread deciles

**Date:** 2026-07-27 (committed BEFORE any coding output was read) · Part 2 of this audit.
Operator ruling (2026-07-27, this session): no human-authored ε leg will be run; the
reader-profile plan's steps 2–3 are dead unless revived. What remains authorized: "do what
legibility coding you'd like and see what it tells us."

## Question

The 4-author ε probe (Part 1) found a main effect (mean-ε span ~0.10 across authors) and
pervasive per-reading spread (33% of shared readings ≥ 0.20). The Claude-web thesis is an
**interaction**: LLM authors should diverge specifically where the extraction channel is
tacit/unstatable and agree where it is text-legible. Every Part-1 number is equally compatible
with pure calibration difference. This coding tests whether the spread SORTS by channel.

## Design

- Sample: top-30 and bottom-30 readings by 4-author ε spread (from Part 1's shared-957 set),
  plus 8 mid-spread controls with a-priori-obvious channel, expected labels recorded in
  `coding_batches.json` (`controls_expected`) before coding: 4 × text_legible (GDPR scope,
  takings clause, NPT withdrawal, licensing statute), 4 × tacit (jati practice, feud
  obligation, shinbutsu partition, near-miss competence retention).
- Substrate: the model-neutral seed pool (`never_generated_seeds.json` — pre-generation,
  author-free, no ε, no severity prose; coverage 957/957). No leg's authored file is shown
  to coders.
- Coders: 7 parallel subagents, each coding a shuffled batch of 9–10 items. Blind: coders see
  only id + seed description + domain + structural note; they are not told about spread
  groups, the thesis, its direction, or ε at all.
- Codes per item: `channel` ∈ {text_legible, tacit, mixed, none_apparent};
  `statable_party` ∈ {yes, no, unclear, none}; `hot_topic` ∈ {yes, no} (the rival sort
  Claude-web named: "topic heat"); one-line note.

## Hypotheses (Claude-web's pre-commitment, adopted verbatim)

- H1: top-30 enriched for tacit/diffuse channels; bottom-30 enriched for
  statutory/contractual (text_legible).
- Rival sort R1: spread sorts by hot_topic (topic heat), not channel.

## Outcome criteria (fixed now)

- **Control gate:** ≥ 6/8 controls coded to their expected label (mixed counts as a miss).
  Below that, the rubric does not discriminate → coding is INCONCLUSIVE, not evidence for
  any reading.
- **H1 supported:** tacit share (tacit + statable_party=no) differs top vs bottom in the
  predicted direction with Fisher exact two-sided p < 0.05 on the 2×2
  (top/bottom × tacit/text_legible, mixed and none_apparent excluded from the 2×2 but
  reported).
- **KILL (calibration wins):** no significant enrichment, or enrichment in the wrong
  direction → the four-author spread reads as authors disagreeing about severity in
  general; the inversion thesis has no purchase on ε via this instrument.
- R1 check: same 2×2 on hot_topic; if hot_topic separates the deciles and channel does not,
  the spread is topic-heat-sorted.
- Coder disagreement is not resolvable post hoc (single-coder per item by design); the
  control gate is the rubric's only calibration.

## Known limitations (declared now)

- The orchestrating instance (me) has seen the top-15 diverger names and the thesis
  direction; the CODERS have not. Blindness lives at the coder, sample construction is
  deterministic (spread ranking + fixed random seed 42), controls were named before coding.
- Haiku's exact-0.00 stratum was adjudicated in Part 1 addendum: authored reading (14/960
  tail, justification prose in-file), not a null token — but its mechanism (ε-referent
  scope: reading-endorsed arrangement vs situation addressed) is a THIRD sort the coding
  cannot see. If neither H1 nor R1 separates, referent-scope disagreement is the standing
  candidate, untested here.
