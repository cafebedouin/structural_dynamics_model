---
name: plan-review
description: Plan an OQ (or any task) with a spawned repo-blind reviewer iterating the plan before the operator sees it. Refines the plan; does not replace the operator's own outside read, and never resolves rulings. Invoked as /plan-review, or whenever a task is to be planned "with review".
---

# plan-review — refine the plan before it reaches a human

The planning instance drafts, self-checks, spawns a different-model reviewer, iterates to ready
(capped), takes a mandatory fresh-eyes pass, and hands the operator a refined plan plus a RULINGS
list. **It refines; it does not replace the operator's own outside read.** Rulings are batched and
escalated, never self-resolved (§9.2); plan acceptance stays with the operator.

## The organizing idea: three outside seats, and only one used to be named

The mechanism this skill runs on is a **change of seat**, not a repetition of effort. A party that
accumulated your context reads your plan as you meant it; a party that did not must read it as text.
Recursion inside one seat does not substitute for one change of seat — five rounds of negotiation
demonstrably did not (step 7 below). The workflow contains **three** such seats:

1. **The loop reviewer** — fresh of *your session*, though not of the project (see *TOOL-blind* in
   Standing limits). Runs the specification test round by round.
2. **The fresh-eyes reviewer** — fresh of the *negotiation*. Mandatory, step 7, and a production
   step rather than a check on the loop.
3. **The implementing instance** — fresh of everything, and the most consequential of the three,
   because it must **enumerate to comply**. Its warrant is not merely the write-lock (§4.1); it is
   the last and hardest outside read the plan gets, and **its refusals are findings about the plan.**

The operator's own no-context read is a fourth seat and is outside this skill's scope — it is
standing practice for essays here, and it is the one seat that is genuinely outside the project's
framing (`CWC:P2`). This skill feeds it a cleaner artifact; it does not substitute for it.

## Phase 0 — materials

1. Resolve the target: an OQ id (`/plan-review OQ-NNN`) or a task description. For an OQ: scan
   `issues/INDEX.md`, then `grep -n "OQ-NNN" ISSUES.md` and read that entry only — never read
   ISSUES.md whole. Capture the OQ text **verbatim** (byte-copy, not paraphrase).
2. Read what the entry's Deps/evidence point at. This context is for YOUR plan — it does not go to
   the reviewer (rule 3).

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

3. Spawn `repo-blind-reviewer`. **Payload composition rule:** exactly (a) the OQ/task text verbatim,
   (b) your questions and/or plan verbatim, (c) on later rounds, the reviewer's prior round. Nothing
   else — no file excerpts, no command output, no planning-context summary. If the reviewer flags
   leakage, fix the payload, not the flag.
4. **Model rule (two independence axes, §4.1), claimed at its altitude:** the reviewer must run on a
   different model than this session (definition defaults to `opus`; if this session IS Opus, pass a
   `model` override). A different Anthropic model buys fresh context plus a different capability
   profile — it does **not** remove lineage-shaped misreadings (same vendor, correlated failure
   modes). Where that axis matters (concept→surface mappings, framework readings), say so in the
   deliverable: the operator's relay to a non-Anthropic model remains the path for it.
5. Respond to findings **per item** — numbered dispositions, never "all addressed". Questions the
   reviewer marks `RULING:` go on the escalation list untouched; do not argue them down.
6. Iterate by SendMessage to the same reviewer (a send resumes it from its transcript — witnessed,
   not assumed; if continuation ever proves unavailable, carry the full prior negotiation in each
   round's payload instead). **Cap at 6 rounds.** If not converged, stop and report the sticking
   points to the operator rather than grinding — see *the fixer is a source of rounds* below, which
   is why the cap is a bound and not a target.
7. **Fresh-eyes pass — MANDATORY, and a PRODUCTION step, not a test of the loop** (witnessed
   2026-08-20, `audits/2026-08-20_plan_review_shakedown/`). When the loop reviewer says ready, spawn
   a **NEW** `repo-blind-reviewer` holding only the OQ + final plan, never the negotiation.
   **A negotiated "Ready" is not gap-free.** Measured: a genuine five-round loop reached
   `VERDICT: Ready`; a fresh instance holding only that text found a **self-contradictory sentence
   that had survived all five rounds** — the negotiating reviewer had accumulated context about what
   was *meant* and stopped parsing it literally — plus a load-bearing gap **that same reviewer had
   itself flagged at round 4 and waived as "non-blocking, carried"** on the way to Ready. Negotiated
   convergence accumulates a charity a fresh read does not have; **skipping this ships the charity.**
   Stopping rule, stated because it is otherwise implicit: an amendment made in response to the fresh
   pass **ships unreviewed** — mark it as such at the checkpoint rather than silently folding it in.

## Phase 3 — the operator's checkpoint (one interaction)

Present, in the final message: the converged plan; the **executor prompt** (below); the **RULINGS
list** (each stated neutrally — no quality claim wearing a jurisdictional argument's clothes); the
reviewer's **CAUTIONS**; and any post-fresh-pass amendments, marked unreviewed. Then stop.
Implementation happens in a fresh session (§4.1, and seat 3 above).

Append one line to `.claude/skills/plan-review/RUNS.md` (create if absent):

```
YYYY-MM-DD | run-id | target | rounds | agents spawned | findings closed/opened | fixer-introduced
defects | rulings N | fresh-pass finds: N | post-impl gaps: (left blank; the executor fills it)
```

**RUNS.md is a COST METER, not the falsifier.** The falsifier is direct and needs no instrument
(Standing limits). What RUNS.md exists for is that a loop living inside planning **spends invisibly**
— the operator sees a plan, not the rounds and agent calls behind it, which is the apparatus-growth
hazard at its quietest. The columns are chosen so cost stays visible and cappable, and so the two
self-feeding terms (`findings closed/opened`, `fixer-introduced defects`) are legible.

**One grammar lives in THIS file: run rows, each starting with a date.** `run-id` is `<date>-<n>`,
`n` from 1 per day — `date | target` collides when two runs hit one target in a day, and that pair is
how the post-implementation session finds the line it must annotate.

**A second grammar exists but NOT here** (corrected 2026-08-20; as first written this note warned a
future parser-writer about a row kind that may never appear in this file). Shakedown rows — prefix
`shakedown:`, no columns — are written into a **clean-room** `RUNS.md` by the first-use check, which
creates its own copy outside the repository. They reach the repository only as **audit evidence**,
never by being appended here: the completed one is at
`audits/2026-08-20_plan_review_shakedown/evidence/RUNS.md`. So a parser over *this* file may assume
every row is a run row; a parser over a clean-room copy or an evidence copy may not.

## The executor prompt (part of the deliverable, not an afterthought)

It must contain: the plan; the stop-and-ask triggers (the CAUTIONS, made concrete); the **license to
refuse**, stated ("if an instruction is correct in prose and wrong when executed, say so rather than
comply — report the refusal at the volume of a completion"); **the RUNS.md annotation obligation
below, quoted, with the run's line named**; and mid-run question routing:

- **Spec-interpretation question** → spawn `repo-blind-reviewer` with OQ + plan + the question
  (triage applies: it answers from the plan's own terms or marks it RULING).
- **Evidence question** → re-derive it yourself against the substrate; that is the executor's job,
  not a reviewer's (the reviewer's amendments never touch evidence, §9.4).
- **RULING** → stop and ask the operator.

**State the license explicitly, because an unstated licence is not exercised.** In the one measured
arc, receivers refused **five times** and every refusal was a real defect — a room that could not be
reached, a registry that could not be refreshed, a probe case the instrument's own system prompt
named, a pass criterion that was architecturally unpassable, and an instruction conflicting with its
authoritative spec. **None was caught by the sender re-reading.** Seat 3 is where a plan is executed
by someone who did not write it, which is the only place its specification is actually tested.

> **Why the annotation obligation travels in the prompt (Pattern 1 — wire the consumer in the same
> change).** The Phase-3 line is written by a session running this skill, so the instruction reaches
> it. The `post-impl gaps` column is written **after implementation, by a fresh session that is not
> running this skill** — SKILL.md is not in its context, no hook delivers it, and no KNOWN_STATE
> entry names either file (checked 2026-08-20: 0 entries). **The executor prompt is the only channel
> that reaches that session.** An obligation stated only here is a producer with no consumer, and an
> empty `post-impl gaps` column is indistinguishable from *no gaps were found*.

## After implementation — evaluation is re-derivation, not reading

The evaluating instance (fresh, holding OQ + plan + the diff) must not review by reading prose:
judges anchor on confident closing language rather than verified state change
(`amnesiac_institution_v0_6.md` §3.2, citing Advani 2026, ≤0.65 AUROC — the read-site for this
figure). Instead: re-run the claimed commands; compare every claimed number/count/diff against the
artifact it describes; classify every zero (tested absence / untested instrument / unrecheckable,
§7.8); paste witnesses. A done-claim without a same-turn witness is OPEN (paste-or-untag). Annotate
the run's line: `post-impl gaps: N (what)`.

## Standing limits

- **It may not resolve rulings, approve its own plans, or push.**
- **The reviewer verifies framing and specification, never evidence.** Evidence witnesses are
  produced repo-side and never outsourced to the blind seat. **This got harder to hold on
  2026-08-20:** the reviewer receives the project's rules by injection, so it knows the conventions
  and may think it knows facts. Watch for it opining on evidence.
- **"Blind" means TOOL-blind, not context-blind (OQ-334).** Every spawned subagent receives both
  `CLAUDE.md` files, `MEMORY.md` plus relevance-triggered memory files, and live `gitStatus`
  (including, witnessed, the *untracked-file list* — a channel that can leak the shape of a test
  without leaking any content). So the **cold-reader jurisdiction survives and is arguably more
  faithful** — the receiver it simulates is a fresh instance that *will* hold `CLAUDE.md`. The
  **outside-the-framing jurisdiction is lost** and reverts to the operator's own read: `CLAUDE.md`
  **is** the framing. **Never cite a loop review as an outside-the-framing check**, and never route
  the method papers through this agent — Build Discipline is close to an abstract of the practice
  paper, so it is maximally inside there.
- **Falsifier (direct, and it needs no instrument): do the plans the operator receives read as
  better?** The operator sees every plan and is the read-site. If the loop stops improving them, say
  so at the checkpoint and propose retiring or revising this skill.
- **The fixer is a source of rounds, not just the reviewer (witnessed 2026-08-20).** In one measured
  round on a complex plan, 13 of 22 findings closed and **15 new ones opened**, two of them defects
  the revising instance introduced while fixing — including a **fix-label attached to a non-fix**.
  The loop can feed itself, so **the cap is the only bound on it.** Hit the cap, stop, report.
- **Intended evolution, named:** Phase 1's checklist graduation is *designed* to erode the reviewer's
  marginal value on shape/record/reviver — that erosion is success, not failure. As it happens,
  narrow the reviewer's mandate toward the **specification test alone**: the one axis the sender
  structurally cannot run on itself, because it cannot un-know its own context.
