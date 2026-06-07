# Step 4 — 4b opposition-authoring gate: RENAMED-NOT-ESCAPED (gate fired; STOP, hand to operator)

**Date:** 2026-06-07 · **Model (pinned):** gemini-2.5-pro · **Prompt:** neutral pilot
(`constraint_story_generation_prompt_stakeholder_pilot.md`, neutrality witnessed: zero
contention-directive matches). Topics + labels pinned BEFORE generation
(`step4_topics_pinned.json`). 4c was NOT run (gated on 4b). The prompt was NOT tuned toward
eliciting opposition (operator pin: a null here is a result, not a thing to engineer away).

## Gate result (raw, `step4b_gate.py` output banked)

| label | topic | `in_contention` |
|---|---|---|
| contention | pilot_app_store_commission | — (none) |
| contention | pilot_streaming_royalty_split | YES (non-headline pair) |
| contention | pilot_hospital_insurer_reimbursement | — (none) |
| non_contention | pilot_payday_lending | — (none) |
| non_contention | pilot_time_zones | YES |
| non_contention (mountain) | pilot_lightspeed_latency | YES |

emerges-in-all-contention = False; absent-in-all-non_contention = False → **gate fails both
halves.** But the binary hides the mechanism, and the mechanism is the finding.

## What actually happened (the honest reading, not the binary)

`in_contention` requires two agents at the **same power atom** with **opposed roles**
(beneficiary-side vs payer). Across the three contention topics, the **headline antagonists
never land in that shape** — by two distinct mechanisms:

- **Role-collapse (streaming, hospital):** the two headline antagonists were authored at the
  SAME power (both `institutional`) but the SAME role — **both `agenda_setter`**
  (`dominant_streaming_service` + `major_record_labels`; `large_hospital_systems` +
  `large_health_insurers`). Two co-equal administrators on opposite sides of a split share
  the role; their opposition survives only in the `situation` prose, invisible to a predicate
  keyed on role-difference.
- **Power-atom drift (app_store):** opposed roles WERE authored (`platform_operator`
  agenda_setter vs `large_app_publishers` payer) but at DIFFERENT power atoms (institutional
  vs powerful), so the same-atom requirement fails.

Meanwhile `in_contention` **did** fire on 3 topics — `streaming` (a non-headline pair:
`songwriters_and_publishers` beneficiary vs `major_label_artists` payer, both `organized`),
`time_zones` (public vs border-communities, both `powerless`), and `lightspeed` (developers
vs HFTs, both `powerful`). So the detector + step-3 mechanism are **live** (positive control
on the detector itself); they are simply **not aligned with the topic-level contention
label**, and one firing is on a mountain-profile topic.

## Verdict: renamed-not-escaped — with a precise, dual cause

The schema escaped the straitjacket (step 1) and the engine mechanism splits same-power
opposed-role pairs (step 3, re-witnessed firing here). But on **independently generated
arms**, the opposition the topics actually contain does **not** arrive in the shape the
mechanism splits:

1. **Generation layer:** under the neutral prompt, gemini-2.5-pro authors the canonical
   contention — two co-equal institutional players fighting over a split — as **two
   `agenda_setter`s** (co-administrators), or shifts one antagonist's power atom. It does not
   reliably render headline antagonists as same-power **opposed-role** agents.
2. **Vocabulary/operationalization layer:** `in_contention` (and the per-(C,Name) d split it
   rests on) only separates **agenda_setter/beneficiary vs payer**. Two opposed
   `agenda_setter`s — co-equal administrators with opposed interests — are **structurally
   invisible** to it. This is the A4 contender-residue (6.3%, "the dial-set backgrounds
   contention between co-equals") resurfacing at the generation layer with evidence.

The two compound: the generator authors the opposition in exactly the form the vocabulary
cannot see.

## This is the operator's to act on (escalation, not self-resolve)

Per the step-4 pins, a 4b null stops the run and hands back. The decision is the operator's;
candidates, **not** acted on here:

- **Re-examine the "derive contention, never author it" ruling (A4) against this evidence.**
  "Derive from opposed roles" assumed opposition shows up as role-difference. The dominant
  authored shape is opposed interests between two SAME-role co-administrators — which derived
  contention cannot read, and which is the exact case the no-contender ruling set aside.
- **OR** treat `agenda_setter`-vs-`agenda_setter` as a contention case in `in_contention` /
  the d-model (opposition between co-administrators) — but nothing in the structured fields
  marks two agenda_setters as opposed; the opposition is only in prose, so this needs a new
  authored signal (an opposed-pair edge?), which reopens "author vs derive."
- **OR** accept that the straitjacket is escaped only for asymmetric-role opposition and
  re-imposed for co-equal-administrator opposition, and scope the migration's claim to that.
- Prompt-craft is explicitly NOT chosen here (would manufacture the finding); if pursued, it
  is the operator's call and must avoid contention-directive language.

## Scope / honesty notes

- **Gemini-conditional** (single-model pilot): this is how gemini-2.5-pro fills the neutral
  prompt; the 2×2 model×framing perturbation (filed as the follow-on Ω) is unmeasured.
- The app_store power-atom assignment (publishers = `powerful` < platform `institutional`) is
  defensible; the pin keeps the label as ground truth and reports the output divergence rather
  than relabeling.
- Evidence banked: 6 `*.stakeholder.json`, `step4b_gate.py` + its output, `step4_topics_pinned.json`,
  `step4_scaffold_leak_witness.py` (PASS, both axes). No `json/` or live-`testsets/` writes;
  temp `.pl` removed; four-tuple prompt untouched.
