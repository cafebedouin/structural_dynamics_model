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

The plan must carry a declared **assumed substrate** section: the repo facts (paths, predicates,
counts, invariants) the executor is expected to verify itself, each stated checkably. This is the
damper on blind-review false positives — the reviewer checks that the list exists and its entries
are checkable, instead of flagging every repo dependency as a gap and pressing the plan toward
inlining repo contents.

## Phase 2 — review rounds

3. Spawn the `repo-blind-reviewer` agent. **Payload composition rule:** exactly (a) the OQ/task
   text verbatim, (b) your questions and/or plan verbatim, (c) on later rounds, the reviewer's
   prior round. Nothing else — no file excerpts, no command output, no planning-context summary.
   The blindness is the control; if the reviewer flags leakage, fix the payload, not the flag.
4. **Model rule (two independence axes, §4.1), claimed at its altitude:** the reviewer must run
   on a different model than this session (definition defaults to `opus`; if this session IS
   Opus, pass a `model` override). A different Anthropic model buys fresh context plus a
   different capability profile — it does NOT remove lineage-shaped misreadings (same vendor,
   correlated failure modes). Where that axis matters (concept→surface mappings, framework
   readings), say so in the deliverable: the operator's relay to a non-Anthropic model remains
   the path for it, and this skill does not replace that.
5. Respond to findings **per item** — numbered dispositions, never "all addressed". Questions the
   reviewer marks `RULING:` go on the escalation list untouched; do not argue them down.
6. Iterate by SendMessage to the same reviewer (a send resumes it from its transcript — but this
   is witnessed by the agent file's FIRST-USE CHECK, not assumed; if continuation proved
   unavailable there, carry the full prior negotiation in each round's payload instead).
   Witnessed practice is 4–6 rounds, occasionally 12+ (OQ-303): cap at 6 by default; if not
   converged, stop and report the sticking points to the operator rather than grinding.
7. **Fresh-eyes final pass — MANDATORY, and it is a PRODUCTION step, not a test of the loop**
   (witnessed 2026-08-20, `audits/2026-08-20_plan_review_shakedown/`). When the loop reviewer says
   ready, spawn a NEW `repo-blind-reviewer` with only the OQ + final plan, never the negotiation.
   **A negotiated "Ready" is not gap-free.** Measured: a genuine five-round loop reached
   `VERDICT: Ready`; a fresh instance holding only that text found a **self-contradictory sentence
   that had survived all five rounds** — the negotiating reviewer had accumulated context about what
   was *meant* and stopped parsing it literally — plus a load-bearing gap **that same reviewer had
   flagged at round 4 and waived as "non-blocking, carried"** on its way to Ready. Negotiated
   convergence accumulates a charity a fresh read does not have. **Skipping this ships the charity.** Stopping rule, stated because it is otherwise
   implicit: an amendment made in response to the fresh pass ships unreviewed — mark it as such
   at the operator checkpoint rather than silently folding it in.

## Phase 3 — the operator's checkpoint (one interaction)

Present, in the final message: the converged plan; the **executor prompt** (below); the
**RULINGS list** (each stated neutrally — the reviewer checks that no quality claim wears a
jurisdictional argument's clothes); and the reviewer's **CAUTIONS**, plus any unreviewed
fresh-pass amendments, marked. Then stop. Plan approval and rulings are the operator's;
implementation happens in a fresh session per §4.1.

Before stopping, append one line to `.claude/skills/plan-review/RUNS.md` (create if absent):
`YYYY-MM-DD | run-id | target | rounds used | amendments beyond Phase-1 checklist y/n | rulings N |
searches per round | fresh-pass finds: N`. This is the falsifier's substrate: each session sees one
run, so "repeatedly" is unobservable to the amnesiac reader who owns the retirement decision unless
the runs leave a record.

**Two grammars live in this file, and a reader counting rows must know which it is parsing.** A
run row starts with a date; a shakedown row starts with the literal `shakedown:` and has no
columns. Say so in RUNS.md's own header when creating it, or a later reader parsing by column
hits a row that does not fit and has no way to tell a malformed run from a different record kind.
**`run-id` is `<date>-<n>`, `n` starting at 1 per day** — `date | target` collides when two runs
hit the same target in a day, and that pair is exactly how the post-implementation session
locates the line it must annotate. **`fresh-pass finds: N`** is shakedown item (7)'s column: it is
what makes step 7's contamination claim falsifiable rather than assumed.

## The executor prompt (part of the deliverable, not an afterthought)

It must contain: the plan; the stop-and-ask triggers (the CAUTIONS, made concrete); the license to
refuse, stated ("if an instruction is correct in prose and wrong when executed, say so rather than
comply — report the refusal at the volume of a completion"); **the RUNS.md annotation obligation
below, quoted into the prompt with the run's line identified**; and mid-run question routing:

- **Spec-interpretation question** → spawn `repo-blind-reviewer` with OQ + plan + the question
  (triage rule applies: it answers from the plan's own terms or marks it RULING).
- **Evidence question** → re-derive it yourself against the substrate; that is the executor's job,
  not a reviewer's (the reviewer's amendments never touch evidence, §9.4).
- **RULING** → stop and ask the operator.

> **Why the annotation obligation travels in the prompt (Pattern 1 — wire the consumer in the same
> change).** The Phase-3 line is written by a session running this skill, so the instruction reaches
> it. The `post-impl gaps` half is written **after implementation, by a fresh session that is not
> running this skill** — SKILL.md is not in its context, no hook delivers it, and no KNOWN_STATE
> entry names either file (checked 2026-08-20: 0 entries). **The executor prompt is the only channel
> that reaches that session**, so an obligation stated only here is a producer with no consumer, and
> the column that would carry it accumulates empty. An empty `post-impl gaps` column is
> indistinguishable from *no gaps were found* — the falsifier's other half reading as a clean
> record. Quote the obligation, and name the line to annotate (date + target), or it does not
> happen.

## After implementation — evaluation is re-derivation, not reading

The evaluating instance (fresh, holding OQ + plan + the diff) must not review by reading prose:
judges anchor on confident closing language rather than verified state change
(`amnesiac_institution_v0_6.md` §3.2, citing Advani 2026, ≤0.65 AUROC — the read-site for this
figure). Instead: re-run the claimed commands; compare every claimed number/count/diff against
the artifact it describes; classify every zero (tested absence / untested instrument /
unrecheckable, §7.8); paste witnesses. A done-claim without a same-turn witness is OPEN
(paste-or-untag). Annotate the run's line in RUNS.md: `post-impl gaps: N (what)` — gaps the
reviewer approved past are the falsifier's other half.

## Standing limits

- This skill automates the relay. It may not resolve rulings, approve its own plans, or push.
- The reviewer verifies framing and specification, never evidence. Evidence witnesses are produced
  repo-side and never outsourced to the blind seat. **This got HARDER to hold on 2026-08-20:** the
  reviewer now receives the project's rules by injection, so it knows the conventions and may think
  it knows facts. Watch for it opining on evidence.
- **"Blind" means TOOL-blind, not context-blind (OQ-334).** Every spawned subagent receives both
  `CLAUDE.md` files, `MEMORY.md` plus relevance-triggered memory files, and live `gitStatus`. So the
  **cold-reader jurisdiction survives and is arguably more faithful** — the receiver it simulates is
  a fresh instance that *will* hold `CLAUDE.md`. The **outside-the-framing jurisdiction is lost** and
  reverts to a human relay: `CLAUDE.md` **is** the framing, and a reviewer holding it is inside it.
  **Never cite a loop review as an outside-the-framing check**, and never route the method papers
  through this agent — Build Discipline is close to an abstract of the practice paper.
- **The loop REFINES; it does not replace the operator's own outside read.** Its product is a plan
  already cleaned up before a human sees it. Taking that plan afterwards to a genuinely context-free
  instance is a separate act with a different warrant — and it is standing practice here for essays,
  where a no-context read reliably finds what multi-model review, conversation, and the analysis
  suite do not.
- **Falsifier (direct, and it needs no instrument):** **do the plans the operator receives read as
  better?** The operator sees every plan and is the read-site; if the loop stops improving them, say
  so at the checkpoint and propose retiring or revising this skill. RUNS.md is retained for a
  different job — a **cost meter**, not a falsifier substrate. A loop living inside planning spends
  invisibly: the operator sees a plan, not the rounds and agent calls behind it, which is the
  apparatus-growth hazard at its quietest. One line per run — rounds, agents spawned, findings
  closed vs opened, defects the FIXER introduced — so the cost stays visible and cappable.
- **The fixer is a source of rounds, not just the reviewer (witnessed 2026-08-20).** In one measured
  round on a complex plan, 13 of 22 findings closed and **15 new ones opened**, two of them defects
  the revising instance introduced while fixing — including a fix-label attached to a non-fix. The
  loop can feed itself. **The round cap is therefore not a budget suggestion; it is the only bound on
  a process that can sustain itself.** Hit the cap, stop, report sticking points; do not grind.
- **Intended evolution, named:** Phase 1's checklist graduation is *designed* to erode the
  reviewer's marginal value on shape/record/reviver — that erosion is success, not failure, and
  RUNS.md is where it becomes visible. As it happens, narrow the reviewer's mandate toward the
  **specification test alone**: the one axis the sender structurally cannot run on itself,
  because it cannot un-know its own context.
