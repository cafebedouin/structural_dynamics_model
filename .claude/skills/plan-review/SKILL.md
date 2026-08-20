---
name: plan-review
description: Run the §4.1 plan-review loop with a spawned repo-blind reviewer instead of the operator relaying to Claude web. Use when asked to plan an OQ (or any task) "with review" or via /plan-review. Automates the relay; never the rulings.
---

# plan-review — the §4.1 cycle with the relay automated

This skill mechanizes the planning loop of `docs/amnesiac_institution/amnesiac_institution_v0_6.md`
§4.1/§9.4: CC plans, a repo-blind reviewer iterates it to ready, the operator gets one batched
checkpoint instead of N relays. **What is automated is the message-bus role only.** The operator's
two jurisdictions (§9.2) are untouched: rulings are batched and escalated, never self-resolved;
plan acceptance stays with the operator.

## Phase 0 — materials

1. Resolve the target: an OQ id (`/plan-review OQ-NNN`) or a task description. For an OQ: scan
   `issues/INDEX.md`, then `grep -n "OQ-NNN" ISSUES.md` and read that entry only — never read
   ISSUES.md whole. Capture the OQ text **verbatim** (byte-copy, not paraphrase).
2. Read what the entry's Deps/evidence point at. This context is for YOUR plan — it does not go to
   the reviewer (rule 3 below).

## Phase 1 — draft, then self-check before spending a round

Draft the plan and your clarification questions. Before any review round, run the sender-side
checklist yourself (§9.4 — the reviewer's known regularities graduate into the sender):
**shape** (decisions in the right entries), **record** (reads correctly cold; zeros classified),
**reviver** (triggers have named read-sites; falsifiers are not ratchets), and the
**specification test** (write the executor's prompt now, as part of the plan — enumerating what a
receiver must do surfaces what re-reading cannot).

## Phase 2 — review rounds

3. Spawn the `repo-blind-reviewer` agent. **Payload composition rule:** exactly (a) the OQ/task
   text verbatim, (b) your questions and/or plan verbatim, (c) on later rounds, the reviewer's
   prior round. Nothing else — no file excerpts, no command output, no planning-context summary.
   The blindness is the control; if the reviewer flags leakage, fix the payload, not the flag.
4. **Model rule (two independence axes, §4.1):** the reviewer must run on a different model family
   than this session. The agent definition defaults to `opus`; if this session IS Opus, pass a
   `model` override. Fresh context removes inherited framing; only a different model's read removes
   a model-shaped misreading.
5. Respond to findings **per item** — numbered dispositions, never "all addressed". Questions the
   reviewer marks `RULING:` go on the escalation list untouched; do not argue them down.
6. Iterate by SendMessage to the same reviewer (its context holds the negotiation). Witnessed
   practice is 4–6 rounds, occasionally 12+ (OQ-303): cap at 6 by default; if not converged, stop
   and report the sticking points to the operator rather than grinding.
7. **Fresh-eyes final pass:** when the loop reviewer says ready, spawn a NEW `repo-blind-reviewer`
   with only the OQ + final plan (not the negotiation). A reviewer that iterated with you has
   accumulated your context; its "ready" is partly recognition. Address or escalate what the fresh
   pass finds; it does not restart the loop.

## Phase 3 — the operator's checkpoint (one interaction)

Present, in the final message: the converged plan; the **executor prompt** (below); the
**RULINGS list** (each stated neutrally — the reviewer checks that no quality claim wears a
jurisdictional argument's clothes); and the reviewer's **CAUTIONS**. Then stop. Plan approval and
rulings are the operator's; implementation happens in a fresh session per §4.1.

## The executor prompt (part of the deliverable, not an afterthought)

It must contain: the plan; the stop-and-ask triggers (the CAUTIONS, made concrete); the license to
refuse, stated ("if an instruction is correct in prose and wrong when executed, say so rather than
comply — report the refusal at the volume of a completion"); and mid-run question routing:

- **Spec-interpretation question** → spawn `repo-blind-reviewer` with OQ + plan + the question
  (triage rule applies: it answers from the plan's own terms or marks it RULING).
- **Evidence question** → re-derive it yourself against the substrate; that is the executor's job,
  not a reviewer's (the reviewer's amendments never touch evidence, §9.4).
- **RULING** → stop and ask the operator.

## After implementation — evaluation is re-derivation, not reading

The evaluating instance (fresh, holding OQ + plan + the diff) must not review by reading prose:
judges anchor on confident closing language (§3.2; Advani 2026, ≤0.65 AUROC). Instead: re-run the
claimed commands; compare every claimed number/count/diff against the artifact it describes;
classify every zero (tested absence / untested instrument / unrecheckable, §7.8); paste witnesses.
A done-claim without a same-turn witness is OPEN (paste-or-untag).

## Standing limits

- This skill automates the relay. It may not resolve rulings, approve its own plans, or push.
- The reviewer verifies framing and specification, never evidence. Evidence witnesses are produced
  repo-side and never outsourced to the blind seat.
- **Falsifier (declared read-site, not a ratchet):** the operator, at plan-acceptance and at
  implementation, observes directly whether this loop earns its keep — if review rounds repeatedly
  amend nothing beyond the Phase-1 checklist, or implementations of skill-reviewed plans repeatedly
  surface gaps the reviewer approved, say so and propose retiring or revising this skill. Passive
  trigger, declared as such.
