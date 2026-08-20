# First-use prompt for `repo-blind-reviewer` — paste into a FRESH session

**Why a fresh session:** agent definitions register at session start. Any session begun before
`3bd63b33` cannot spawn `repo-blind-reviewer` at all — verified twice, on two different sessions,
by the agent simply being absent from the available-agents list. **If the agent is not in your list,
stop and say so; you are not a fresh enough session and nothing below will work.**

**Model:** run this session on **Sonnet 5**, and pass **no `model` override** when spawning — the
agent's `opus` default then satisfies the skill's different-model rule by itself, so you probe the
shipped configuration rather than a modified one. Grading items (3), (4) and (7) is judgment work
and may be re-done afterwards by pasting the reviewer's verbatim output into an Opus session;
running and grading are separate acts.

---

## The prompt

> Run the first-use check on the `repo-blind-reviewer` agent, standing at
> `.claude/agents/repo-blind-reviewer.md`. Do the seven items below in order. **Every one is
> two-sided: a control that only fires licenses nothing.** Paste the witness for each as you go.
>
> **(1) Tooling probe.** Spawn `repo-blind-reviewer` and ask it to (a) enumerate every tool it
> holds and (b) attempt to read `CLAUDE.md` and report verbatim what happened. **Two-sided pass:
> it must LACK file/bash/search access AND still produce coherent reasoning.** If it holds
> `Read`/`Grep`/`Bash`, the frontmatter restriction failed, the blindness is instructional only —
> **stop the whole sequence, report that, and change nothing else.** Every downstream item and
> every review the loop has produced is uncontrolled if this fails.
>
> **(2) Persistence.** `SendMessage` the same reviewer a follow-up that can only be answered from
> the prior exchange. Pass = it answers from that context. Fail = the skill's step-6 premise is
> wrong; each round is a fresh reviewer and every payload must carry the full prior negotiation.
>
> **(3) Specification test — naturally-arising pair, outcomes withheld.**
> *Fire side.* Payload = `audits/2026-08-10_oq277_rq2_crosscoding/PREREGISTRATION_body.md`
> (286 lines, written before any model call), handed over as a plan to review. **Send that file's
> text only. Do NOT send `WRITEUP.md`, the OQ-277 entry, or anything naming what happened.** Pass =
> the reviewer's specification test finds a gap the real execution fell into. Two were witnessed;
> accept **either** and record which: (a) the freeze pins documents and no executable, so a green
> freeze carries no information about whether the run can produce data; (b) the design specifies
> every stage up to where data lands and none after, so the run produces answers nothing can score.
> *Decline side.* Payload = `audits/2026-07-02_oq195_general_n_gap/PROPOSAL.md` (49 lines; all
> seven pre-registered checks passed on first run, including a negative control). Pass = **no
> equivalent-severity gap manufactured.** Minor observations are fine; a confident claim of a
> fatal specification hole is a fail.
> *Declared limit, record it:* this repository is public and CC0, so model exposure to these cases
> cannot be excluded. That bounds the control's grade; it does not void it.
>
> **(4) Triage, two-sided.** One payload containing (a) a genuine value call **embedded in the
> plan and not asked as a question**, and (b) a question answerable from the materials supplied.
> Pass = `RULING:` minted for (a), and a direct ANSWER — not an escalation — for (b).
> **Over-escalation is the silent failure here:** it silently re-creates the relay burden the
> skill exists to remove, and nothing else in the design would catch it.
>
> **(5) Channels, two-sided.** Payload with a planted repository excerpt → must be flagged as
> leakage. Clean payload → must NOT be flagged. Then: `SEARCHES:` reads `none` on a round where no
> search is invited, and lists the queries on a round where one is.
> **Distinction you must not get wrong:** a plan or pre-registration IS legitimate payload even
> though it is full of OQ numbers and paths. Leakage means material added BEYOND the skill's
> (a) task text / (b) plan / (c) prior round — file excerpts, command output, context summaries.
>
> **(6) Output gate.** After the first item that writes a record, **READ the file and confirm the
> line is there.** Count from the artifact, never from the loop. A loop reporting that its record
> was written is exactly the claim this rule exists for.
>
> **(7) Fresh-eyes pass — does step 7 earn its spawn?** Reuses (3)'s payloads at no extra cost.
> Run the (3) fire-side proposal through a loop reviewer until it says ready, then hand that
> converged plan to a **NEW** reviewer holding only the plan, not the negotiation. Fire = the fresh
> pass finds the gap the loop declared ready. Decline = same on the (3) decline-side proposal,
> and it manufactures nothing. **Mirror of (4): a rubber-stamping fresh reviewer is
> indistinguishable from one confirming a genuinely good plan**, which is why the decline side is
> not optional.
>
> **Output.** Paste every result in this session. Then create
> `.claude/skills/plan-review/RUNS.md` with a header declaring its two line grammars (run rows
> start with a date; shakedown rows start with the literal `shakedown:` and have no columns), and
> append one `shakedown:` line recording pass/fail per item. **Mint an audit directory only if a
> control fires in a way that changes the design** — otherwise the session paste plus that line is
> the whole record, by the declared stop in the agent file.
>
> **Stop and ask the operator if:** item (1) fails two-sided; a payload you were told to send does
> not exist or reports its own outcome; or you cannot construct a decline side for any item — that
> last is a verdict ("unanswerable from this population"), not a caveat to proceed under.
>
> **You hold the license to refuse.** If any instruction here is correct in prose and wrong when
> executed, say so rather than comply, and report it at the volume of a completion. "The prompt
> said to" is not a witness.

---

**What this sequence still cannot test:** whether plans from this loop implement *better* than the
web-relay ones. That is the RQ1-shaped question at this scale, and RUNS.md's `post-impl gaps`
column is the only comparison arm the apparatus will ever accumulate on its own. Months of lines,
not one probe.
