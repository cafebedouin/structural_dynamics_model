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

**Record the pre-review draft's size, and treat it as a budget (added 2026-08-25).** Before the
first spawn, note the byte size of the draft. **A plan that leaves review more than ~50% larger
owes one sentence per increment saying what would break without it** — carried in the checkpoint
deliverable, so the operator sees what the loop added rather than only what it produced.

Why there is a number here at all. Measured across all plans in `~/.claude/plans` (a natural
experiment, since a fingerprint probe for `fresh-eyes` / `repo-blind` / `allocated-at-append`
fires on 11 of 22 plans from 2026-08-20 on and on **0 of 29** before it):

| population | n | mean size |
|---|---|---|
| pre-skill (Aug 1–19) | 29 | 19,447 B |
| same period, **without** plan-review | 6 | 20,948 B |
| same period, **with** plan-review | 11 | **51,508 B** |

Contemporaneous plans that skipped the skill are indistinguishable from the pre-skill baseline,
so the 2.5× is the skill and not the period. Nine of the eleven exceed the pre-skill maximum
(32,569 B). **Not yet shown: that larger plans execute worse.** There is no pre-skill outcome
data and the `post-impl gaps` column was 40% empty when this was measured — which is why that
column is now gated (below). Until it fills, the budget is a discipline, not a verdict.

**Clocks and pilots are incompatible by construction — reconcile them in the plan, not at
execution (witnessed 2026-08-25, run `2026-08-25-2`).** A viability clock measured from FIRST
launch and a mandated pilot-first sequence cannot coexist: the pilot is deliberately one seed —
often the hard one — so it can consume the whole viability window while proving nothing about
the mechanism, and a literal executor then abandons a working phase at the deadline. If a plan
carries both, it must say which: either the viability clock starts at first POST-pilot launch,
or the pilot does not count toward viability. The self-check question: *can the plan's own
sequencing consume its own clock before the clock's subject has been tested?*

## Phase 2 — review rounds

3. Spawn `repo-blind-reviewer`. **Payload composition rule:** exactly (a) the OQ/task text verbatim,
   (b) your questions and/or plan verbatim, (c) on later rounds, the reviewer's prior round. Nothing
   else — no file excerpts, no command output, no planning-context summary. If the reviewer flags
   leakage, fix the payload, not the flag.

   **Require the falsifiability pass explicitly in the spawn prompt (added 2026-08-25).** Ask the
   reviewer to run, over every criterion, control, gate, printed label and pre-registered expected
   value in the plan: **(i) what value would make this FAIL?** and **(ii) is the NAME true of
   everything it counts?** The mandate is also in the agent's own definition, but state it in the
   payload too — an unstated instruction is not exercised. This is not a new idea; it is the
   find-criterion CLAUDE.md promoted on 2026-08-24, and the ledger's own reading is that **neither
   reviewer has ever asked either question unprompted**, across three recorded instances
   (`2026-08-21-3`, `2026-08-24-1`, `2026-08-25-3`). Every instance was a control-structure defect,
   invisible to review-for-correctness and immediate under review-for-falsifiability.
4. **Model rule (two independence axes, §4.1), claimed at its altitude:** the reviewer must run on a
   different model than this session (definition defaults to `opus`; if this session IS Opus, pass a
   `model` override). A different Anthropic model buys fresh context plus a different capability
   profile — it does **not** remove lineage-shaped misreadings (same vendor, correlated failure
   modes). Where that axis matters (concept→surface mappings, framework readings), say so in the
   deliverable: the operator's relay to a non-Anthropic model remains the path for it.
5. Respond to findings **per item** — numbered dispositions, never "all addressed". Questions the
   reviewer marks `RULING:` go on the escalation list untouched; do not argue them down.

   **The fixer's two obligations, added 2026-08-25 because the fixer is the component that
   actually failed.** A four-round run plus fresh pass produced a plan the operator cut by ~60%,
   and *every* cut item was loop-generated: coverage floors invented where "report `n`" would
   do; a copy-freeze that then spawned a ruling, a label form, a verification row and a reach
   into a later phase; a decoy control that risked a corpus denominator to buy a negative
   statable at liveness grade; a preregistration artifact and hash chain on a check the operator
   had called free. **Each finding was real. Each fix was two sizes too big.** Nothing in the
   loop asked whether a fix was proportionate, and no disposition existed except "changed".

   - **State the SMALLEST change that closes the finding, and take it.** If you take a larger
     one, say in one sentence what breaks without the extra. A fix that adds an artifact, a
     phase, a freeze, or a control-protecting-a-control needs that sentence or it is not
     warranted.
   - **DECLINE is an available disposition.** "This finding is real and the plan should not
     change" is a legitimate per-item answer — record it with its reason. Without an explicit
     decline the loop has no exit at the fix step, only at the round step, and the plan can only
     grow. (Which is the false-wager shape: no decline option, extraction invariant to whether
     the artifact improves.)
6. Iterate by SendMessage to the same reviewer (a send resumes it from its transcript — witnessed,
   not assumed; if continuation ever proves unavailable, carry the full prior negotiation in each
   round's payload instead). **Cap at 2 rounds** (was 6 until 2026-08-25). If not converged, stop
   and report the sticking points to the operator rather than grinding — see *the fixer is a source
   of rounds* below, which is why the cap is a bound and not a target.

   **Why 2, measured on run `2026-08-25-3`** (the first run to instrument per-round cost):

   | round | subagent tokens | findings | cost/finding |
   |---|---|---|---|
   | loop r1 | 72,583 | 29 | 2,503 |
   | loop r2 | 93,722 | 23 | 4,075 |
   | loop r3 | 131,155 | 12 | 10,930 |
   | loop r4 | 164,741 | 11 | 14,977 |
   | **fresh pass** | **84,002** | **27** | **3,111** |

   Rounds grow monotonically in cost while yield falls — a **6× degradation in cost-per-finding
   across four rounds** — and the fresh reviewer is ~5× more efficient than the fourth loop round
   while finding more than rounds 3 and 4 combined. The fresh-pass yield does **not** fall as loop
   rounds accumulate (corroborated: `2026-08-23-1` fresh 29 after 6 rounds; `2026-08-21-4` fresh 18
   after 45 loop findings; `2026-08-21-3` fresh 8 *including 2 blocking that had survived all six
   negotiated rounds*). So extra loop rounds do not buy a cleaner fresh pass — they buy expensive
   polish. **Spend the saved rounds on the fresh seat, not on the loop.**
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

**COMPOSE the run row here. Do NOT append it *while in plan mode* — you cannot.** A session running
this skill is in plan mode and structurally cannot write to `.claude/skills/plan-review/RUNS.md`.
Four appends to this ledger have now failed, three of them consecutive planning-time blocks, and
every one was discovered late or by a human re-derivation rather than by anything going red
(OQ-337). An obligation placed on a session that cannot discharge it is unenforceable by
construction, so Phase 3 **composes** the row and a write-capable session **lands** it.

**Landing-chain link 1 fired for the first time on 2026-08-25** (`2026-08-25-3`): plan mode was
released in-session — the operator diverted from executing the plan to revising this skill — and
the planning session allocated and appended its own row before the executor ever existed. So link 1
is not theoretical. **If plan mode releases in your session for any reason, land the row then**,
deriving the id with both instruments at that moment exactly as any other lander would; do not
carry an unlanded row out of a session that became capable of landing it.

Compose it on ONE line, in these 10 columns:

```
YYYY-MM-DD | run-id | target | rounds | agents spawned | findings closed/opened | fixer-introduced
defects | rulings N | fresh-pass finds: N | post-impl gaps: (left blank; the evaluator fills it)
```

**Carry that composed line VERBATIM in BOTH the checkpoint deliverable and the executor prompt.**
The deliverable copy is what makes the spend visible to the operator even if no row ever lands.

Rules for composing, all of which the `ledger grammar` gate row or the lander enforces:

- **The run-id column is the literal placeholder `<allocated-at-append>`. A planner never claims
  an id.** Every id collision in this ledger's history came from a planner asserting an id that
  a different run already held. You cannot know at planning time what the file will contain at
  landing time, and the id is the one column that depends on it.
- **No OTHER `<...>` placeholder may survive into the handed-off block.** Fill every value at
  checkpoint time, when the loop's counts exist. A block carrying any other bracket pair is
  malformed and the lander will report it rather than land it.
- **Field separator: `" | "` (space-pipe-space), with NO trailing separator** — the row ends
  with its last field's text. A trailing `|` adds a spurious empty final field, which fails
  either the arity check or the mandatory last-field prefix.
- **No composed value may contain a literal `|`** — substitute `/` or `;`. A pipe inside free
  text silently shifts every positional read after it while the first and last field checks
  still pass.
- **The last field's `post-impl gaps:` prefix is mandatory unconditionally**, left with no value
  at composition time.
- **`UNRECORDED` is a permitted bare value in columns 3 through 9 only** — never in the date or
  the id, and in the last field only as `post-impl gaps: UNRECORDED`, behind the prefix.
- **Column 1 is the PLANNING date**, not the date the row lands. So is the date embedded in the
  id. The landing date is not recorded. (A lander using its own date would silently fork the id
  space against a lander using the planning date.)

**What the ledger counts, and what it does not.** A row records a run that reached a
landing-capable session. **A plan that is shelved at checkpoint never lands a row, so the rounds
it spent are not counted here** — that spend is visible only in the checkpoint deliverable the
operator saw at the time. This is a known undercount in precisely the case the meter was built
for: a loop that spends six rounds and produces nothing shelved is invisible spend, and the
longitudinal record does not see it. Read a monthly total as "cost of runs that landed", never
as "cost of the loop".

**Who may create this file.** Only the operator, or a session under their explicit instruction.
The checker fails hard on absence rather than skipping, so bringing the ledger into existence is
a ruled act, never a session's fallback. Initial content is the `#` header line plus optional
`Note ` lines; a ledger with zero rows is legitimate.

**RUNS.md is a COST METER, not the falsifier.** The falsifier is direct and needs no instrument
(Standing limits). What RUNS.md exists for is that a loop living inside planning **spends invisibly**
— the operator sees a plan, not the rounds and agent calls behind it, which is the apparatus-growth
hazard at its quietest. The columns are chosen so cost stays visible and cappable, and so the two
self-feeding terms (`findings closed/opened`, `fixer-introduced defects`) are legible.

**One grammar lives in THIS file: run rows, each starting with a date.** `run-id` is `<date>-<n>`,
`n` from 1 per day, where `<date>` is the PLANNING date — `date | target` collides when two runs hit
one target in a day, and that pair is how the post-implementation session finds the line it must
annotate.

### Allocating the id — at LANDING, never at planning

**Derive the id set from the file itself, at the moment you append.** Do not trust any id a plan
or a prompt supplies, including one it says it verified.

```
awk -F'|' '/^[0-9][0-9][0-9][0-9]-/ {gsub(/^ +| +$/, "", $2); gsub(/^ +| +$/, "", $3);
           print $2 " -> " $3}' .claude/skills/plan-review/RUNS.md
```

- The row anchor is **year-agnostic**. Never write a literal year: `/^2026-/` returns an empty
  set from the next January, which reads exactly like a ledger with no rows and allocates a
  colliding `-1`. Any date filter is applied to the EXTRACTED ids, not to the row match.
- Use **explicit repetition**, never the interval form `{4}`. Interval support is not portable
  across awk implementations; under one that lacks it, `{4}` matches nothing — the same
  empty-set collision by another route.
- **Never read this file with a file-reading tool to derive ids.** It has exceeded a display cap
  before, and a truncated read drops the TAIL, which is where the highest `n` lives. An
  under-read allocates a colliding id — the exact defect this step exists to prevent.
- **Run a SECOND, independent instrument every time** and require agreement:
  `grep -cF "| post-impl gaps:" .claude/skills/plan-review/RUNS.md`. It is content-anchored, not
  date-anchored, so it does not share the awk's premise that rows begin with a date — a
  leading-date grep would, and both reading empty under a shape change would look like
  concordance. **Both zero ⇒ a legitimately empty ledger: allocate `-1` and say so. Any
  disagreement ⇒ instrument failure: stop and report, never allocate.**

Then filter the extracted ids to the planning date and take **the successor of the highest `n`**.

**If the supplied block carries a concrete id instead of the placeholder, that is a grammar
violation.** Report it, allocate fresh, and record the reassignment — do not silently occupy the
next slot.

### Where an in-row annotation goes

A reassignment note, or a retroactive registration's provenance note, occupies a **provenance
field inserted immediately before the last field**. A row without an annotation omits that field
entirely, which is why legitimate arity is exactly 10 or 11. Multiple notes share the one field,
semicolon-separated. Appending a note to the target column mislabels it; appending it to the last
column breaks that column's mandatory prefix.

### The landing chain

The row is landed by **the first write-capable session that touches the run**, in this order:

1. **the planning session itself**, immediately after plan approval releases plan mode, if that
   ever happens in-session;
2. **the executor, at execution START** — before other work. Persist first: a run that dies at
   step 7 should still be in the ledger.
3. **the evaluator, retroactively**, with unreconstructible columns marked `UNRECORDED` rather
   than estimated (fields 1–2 excepted — see below).

**Every link first checks whether an earlier link already landed the row**: the derivation above
prints `id -> target`, so look for this run's target. A duplicate append is the missing append's
dual and is equally silent.

**The lander makes exactly TWO modifications to the received block**: substitute the allocated id
for the placeholder, and insert the provenance field if a reassignment occurred. **Nothing else.**
A value that looks wrong to you is REPORTED, not corrected — a wrong planner value is a finding
about the handoff, but a corrected one is a fabrication with a helpful motive, sourced from the
party the meter is measuring and indistinguishable from a supplied value at every read site.

**Never compose a row that was not handed to you.** If no block accompanies the prompt, that is a
handoff failure: report it and route the registration to the retroactive path. Invented counts
land green under every grammar check.

**Residual, unruled:** if a retroactive registration cannot reconstruct the PLANNING date, fields
1–2 cannot be written and cannot be `UNRECORDED` either. No instance has occurred; the case is
open (OQ-337 R5), not silently resolved.

**A grammar check runs in the gate.** `python/runs_ledger_check.py --check`, wired as gate row
`ledger grammar`, enforces arity, the id grammar, date agreement, id uniqueness, the last-field
prefix, and the absence of unfilled placeholders. **It cannot see a missing append** — a run that
never landed leaves the file perfectly grammatical. That half is this procedure's job.

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
comply — report the refusal at the volume of a completion"); **the composed run row, verbatim, with
the landing obligation quoted**; **the RUNS.md annotation obligation below, quoted**; and mid-run
question routing:

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

### The landing obligation, to be quoted into the prompt

> **Step 0 — land this run's row FIRST, before any other work.** The composed row is above; the
> only placeholder in it is the id. Do not append it yet — first, read-only:
>
> 1. Check the ledger exists in the working tree AND `git ls-files --error-unmatch
>    .claude/skills/plan-review/RUNS.md` exits 0. **Exit 1 means untracked — stop and ask.** Any
>    other nonzero is instrument failure (git fatal, or no git binary); report it as that, never
>    as an untracked ledger.
> 2. Derive the existing `id -> target` set with the pinned year-agnostic awk above, AND run the
>    content-anchored second instrument alongside it. Paste both commands and their full output.
>    Any disagreement between them is instrument failure — stop, do not allocate.
> 3. **If a landed row already names this run's target, it is already landed** (an earlier link
>    in the chain fired). Do not append. Verify the landed row against your composed block,
>    report any discrepancy rather than correcting it, and continue.
> 4. **If no row block accompanied this prompt**, that is a handoff failure: report it, skip the
>    append, route registration to the evaluator's retroactive path, and CONTINUE the run.
>    Never compose the row yourself.
> 5. **If the block carries any `<...>` placeholder other than the id, or its field count is not
>    exactly 10**, report it, skip the append, route to the retroactive path, and continue. An
>    11- or 12-field block is most likely a literal `|` inside a composed value.
>
> Then append. Ensure the file ends with a newline first —
> `[ -n "$(tail -c1 .claude/skills/plan-review/RUNS.md)" ] && echo >> .claude/skills/plan-review/RUNS.md`
> — because an append onto a file without one concatenates onto the last row, and a count witness
> still reads 1 on that corruption. Substitute the allocated id for the placeholder; insert a
> provenance field only if you reassigned. **Nothing else changes.**
>
> **Witness, both halves pasted in the same turn:** `grep -cF "| <the allocated id> |"` (expect
> exactly 1; `-F` is required — under ERE those pipes read as alternation and match every line,
> and the delimiters are required because a bare prefix grep miscounts once a two-digit `n`
> exists) AND `tail -2` of the file, which is the only one of the two that shows a merged line.
>
> Angle brackets in this prompt are notation for values you write out. **The appended row must
> contain none** — the `ledger grammar` gate row rejects them.

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
§7.8); paste witnesses. A done-claim without a same-turn witness is OPEN (paste-or-untag).

**Then find this run's row and annotate it: `post-impl gaps: N (what)`.** Locate it with the
pinned `id -> target` derivation, never by reading the file.

**This is now GATED, not merely instructed (2026-08-25).** `runs_ledger_check.py` carries a second
arm: a landed row still empty after `post-impl gaps:` **more than 7 days past its planning date is
a FINDING**, and `ledger grammar` goes red. Discharge is always available and cheap — the real
gaps, or `post-impl gaps: UNRECORDED` when they cannot be reconstructed. The prose obligation alone
had been 40% unmet (4 of 10 rows, measured 2026-08-25), which is the produced-but-not-consumed
shape applied to the apparatus's own meter.

**Why this column and not another.** It is the only outcome-bearing field in the ledger and the
only one that has produced findings the review loop could not: the *"a check that cannot fail, or
a check whose name does not match what it measures"* species was identified across rows
`2026-08-21-3` and `2026-08-24-1` and promoted into CLAUDE.md on 2026-08-24. The reviews did not
find that; the annotations did. An empty column is indistinguishable from *no gaps were found*,
so leaving it empty makes the one instrument that works unreadable.

**If the row is not there, that absence is itself a post-impl gap.** You are the last link in the
landing chain: register the run retroactively, marking every column you cannot reconstruct from
the plan artifact as `UNRECORDED` — marked, never estimated. Note the retroactive registration in
the provenance field immediately before the last field. Fields 1 and 2 are excepted: the planning
date and the id are always writable at landing time, so neither is ever `UNRECORDED`. Record the
missing append against OQ-337; every instance so far has been a distinct mechanism with the same
silent shape.

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
  **First negative reading, 2026-08-25 (`2026-08-25-3`):** the operator cut the delivered plan by
  ~60% and every cut item was loop-generated. That is not a failure to improve; it is degradation
  along the axis the operator reads, while the axes the reviewers check improved. Diagnosis and
  remedies: the fixer bullet below, the Phase-1 size budget, and the 2-round cap. **This did NOT
  license retirement** — the same run's evidence is that the reviewers earned their keep (the
  definition-fires-on-absence catch, the liability-not-discharged catch, the framing correction the
  operator adopted against his own sentence, and — decisively — *the rule that later caught the
  operator's own error came from loop round 2*). Recalibrated, not retired.
- **The fixer is a source of rounds, not just the reviewer (witnessed 2026-08-20; confirmed as the
  PRIMARY defect 2026-08-25).** In one measured round on a complex plan, 13 of 22 findings closed
  and **15 new ones opened**, two of them defects the revising instance introduced while fixing —
  including a **fix-label attached to a non-fix**. Those counts came from a contaminated instrument
  (the primed reviewer held the shakedown spec; OQ-334 R5) — a qualitative lesson, not a clean
  measurement. The clean measurement is `2026-08-25-3`: reviewer findings were overwhelmingly real,
  and the plan still bloated 2.5×, because **every finding got a fix and no finding got a decline**.
  The loop can feed itself, and the round cap does not constrain a fixer that over-builds — a
  smaller pile of over-built responses is still over-built. The constraints that bite are at the
  **fix** step (smallest-change + explicit decline, Phase 2 step 5) and on the **artifact**
  (the size budget, Phase 1). Hit the round cap, stop, report.
- **Intended evolution, named:** Phase 1's checklist graduation is *designed* to erode the reviewer's
  marginal value on shape/record/reviver — that erosion is success, not failure. As it happens,
  narrow the reviewer's mandate toward the **two axes the sender structurally cannot run on
  itself**: the **specification test** (it cannot un-know its own context) and the
  **falsifiability pass** (it wrote the criteria, so it reads them as it meant them). Amended
  2026-08-25 — this bullet previously named the specification test *alone*, and the ledger's
  three-instance record says the falsifiability axis is the scarcer of the two.
