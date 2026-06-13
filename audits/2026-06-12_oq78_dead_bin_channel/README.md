# OQ-78 bin-withdrawal probe: HALTED pre-spend — the epsilon_bin channel is dead at the generation interface

**Date:** 2026-06-12. **Verdict:** the pinned probe's premise is falsified by code witness;
no API spend occurred. The probe assumed production generation feeds `epsilon_bin` to the
authoring model (so a withheld arm would isolate the instruction layer). It does not — on any
path. The withheld arm ≡ production; the contrast cannot run as designed.

## Question

OQ-78 fate 2 (idiom vs instruction for the ε grid: 0.68 point mass, .x8 rail) was to graduate
via a bin-withdrawal probe (15/arm, bin-only removal, two endpoints). Pre-flight recon for the
build asked: where does `epsilon_bin` enter the assembled generation prompt?

## Witnesses (all run 2026-06-12, repo at c2d49e8f)

**W1 — no generation path passes the bin.** All three prompt-assembly paths feed
`Hypothesis type` (the CLAIM side) and never `epsilon_bin`:

- `agent/story_generator_base.py:203` `axis_source_desc` (unified backend flat path):
  structural_delta / primary_observable / hypothesis / beneficiary / victim / CSR — no bin.
- `agent/generate_kernel_corpus.py:380` `build_cached_messages` (gkc kernel/reading/control
  path): kernel context (reading_id, delta, rules) — no bin.
- `agent/c-orchestrator.py:607` legacy inline source_desc: same field list as
  axis_source_desc — no bin.

Grep witness with positive control:

```
$ grep -rn "epsilon_bin" agent/*.py
agent/app.py:94:    f"(ε={ax['epsilon_bin']}) — {ax['structural_delta']}"   # streamlit UI display
agent/c-app.py:92:   f"(ε={ax['epsilon_bin']}) — {ax['structural_delta']}"  # streamlit UI display
$ grep -c "hypothesis" agent/story_generator_base.py
1   # control fires
```

The only consumers of the manifest's `epsilon_bin` are two streamlit display lines. The
generation prompt's UKE_SCOPE mapping table
(`prompts/constraint_story_generation_prompt_json.md:756`, "epsilon_bin →
base_properties.extractiveness") is instruction-without-data: the referenced field never
arrives in the payload.

**W2 — the historical channel was the prompt text itself, scrubbed at the de-leak.**
`git log -S "epsilon_bin" -- agent/` surfaces commit `b6c4e113` (2026-06-05 02:35,
"de-leak generator prompt — qualitative type criteria, prose-ified chi, no epsilon
anchors/bins"). The pre-de-leak prompt disclosed the full numeric type-bands (witnessed in
the commit diff: "Mountain… ε ≤ 0.25", "Snare… ε ≥ 0.46", "Tangled Rope… ε ≥ 0.30" — the
exact config thresholds, plus the f(d) sigmoid and canonical-d table). The de-leak removed
them; nothing re-fed numeric ε guidance or per-story bins afterward. Every post-reset story
(the entire OQ-78 evidence base, n=91 → n=60 → cohort zero) was authored with NO numeric ε
instruction and NO bin token in its prompt.

**W3 — the recorded `uke_scope` blocks are model-fabricated, not mechanical stamps.** No
code writes them (`grep -rn "uke_scope" agent/*.py python/*.py` → only `story_repair.py`
listing the key as preserved); the story schema's `uke_scope.epsilon_bin` is a free string
(no enum), which is how `moderate_high` / `moderate` / `negligible` got through; the same
blocks carry fabricated `generated_date` values (2024-01-09; 2026-06-15, the future). The
15/15 bin-ε "conformance" measured earlier in OQ-78's evidence pass is therefore model
SELF-LABELING (the model authored both the ε and the bin token in one act), not instruction
compliance.

## What this changes

1. **OQ-78 fate 2 is substantially answered by code inspection.** The numeric grid (which
   numbers inside a band: the .x8 rail, the 0.68 point mass) CANNOT be numeric instruction —
   no numeric channel exists in the live pipeline. The only ε-relevant instruction is
   qualitative (the hypothesis token + prose type criteria). Live cohort zero (hypothesis-free
   seeds) shows the rail at 4/5 — idiom surviving even qualitative withdrawal, n too small.
2. **The zero-spend graduation route already exists: OQ-109 Phase C.** The cohort-zero regen
   re-authors the SAME 60 archive seeds with title/domain/summary ONLY (no hypothesis, no
   metrics — `agent/cohort_zero_regen.py` seed spec). That is a withheld arm on matched seeds
   by construction; the archive n=60 is the fed arm (hypothesis-fed). The OQ-78 endpoints
   (rail share, exact-0.68 share) read cross-arm when Phase C completes. Caveat carried in
   the OQ: Phase C withdraws MORE than hypothesis (delta/beneficiary/victim/CSR too) — it is
   full-manifest withdrawal; persistence there establishes idiom a fortiori; collapse would
   motivate a finer hypothesis-only arm (which WOULD need new spend and a new design).
3. **OQ-117's mechanism is corrected.** The live co-authoring channel is HYPOTHESIS-feeding
   (claim side instructed in every manifest path; ε is the model's prior given the instructed
   claim). Manufactured concordance via bin-conformance does not operate in production. The
   SCOPE-stage boundary disclosure (0.10/0.30 = config thresholds) survives as fact, with only
   an indirect path to generation: bin and hypothesis are co-authored at SCOPE, so boundary
   knowledge can tip the hypothesis choice near a threshold; the hypothesis is what travels.
4. **`epsilon_bin` is a Pattern-1 dangling wire**, produced by SCOPE and consumed by nothing
   but UI display and a mapping table whose data never arrives (plus model-fabricated echo
   blocks downstream). Disposition (wire / retire / gap) folded into OQ-117 (c) — feeding it
   would CREATE the instruction channel OQ-117 interrogates, so this is an operator call, not
   a default re-wire.

## Spend

None. The halt fired during pre-flight recon, before any API call.
