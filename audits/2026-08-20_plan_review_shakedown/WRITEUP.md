# The repo-blind reviewer is blind in tools and sighted in context — and a negotiated "Ready" is not gap-free

**Executed:** 2026-08-20
**OQ:** OQ-334 (minted mid-arc, from item (1)'s own result), with OQ-287 as parent
**Verdict:** The instrument works and its charter did not describe it. `tools:` restriction is real
and enforced at spawn-time schema configuration; **context restriction never existed** — six
injection channels deliver project rules, memory, and git state to every subagent regardless. The
charter is corrected by **jurisdiction**: cold-reader simulation survives and is arguably *more*
faithful (the receiver it simulates will hold `CLAUDE.md`), while outside-the-framing verification
(`CWC:P2`) is lost entirely and reverts to the human relay. Against that bound the review function
still demonstrated: the specification test found the withheld ground-truth gap on a blinded payload
and, unprompted, a second gap nobody had flagged. **The load-bearing result is item (7):** a genuine
five-round loop reached `VERDICT: Ready`, and a fresh instance holding only that converged text
immediately found a self-contradictory sentence that had survived all five rounds — plus a gap the
same reviewer had itself flagged at round 4 and waived as "non-blocking" en route to Ready.
**Negotiated convergence accumulates a charity a fresh read does not have.**
**Substrate:** no pipeline run, no corpus. Apparatus and documents only. The scored run executed in
a clean room outside the repository (`/tmp/reviewer-cleanroom`, since deleted-on-reboot); its
artifacts are copied verbatim into `evidence/`. Payloads are blinded historical plans; the harness
enforced `tools: WebSearch`, agent default model `opus`, spawning session Sonnet 5, no override.
**Fired:** live — a control's stated premise was falsified before the control was relied on, and
five separate defects in the operator-side prompts were caught by receivers refusing to execute
them.

---

## Evidence map

| artifact | what it is |
|---|---|
| `evidence/RUNS.md` | the run's own record, written in the room and read back per its output gate; item-by-item results with witnesses. **This is the primary record** and the only copy — the room was outside git. |
| `evidence/PROMPT_as_run.md` | the prompt the scored session executed, verbatim |
| `evidence/agent_stripped_as_run.md` | the agent definition as installed in the room, with the shakedown block removed (see *the exam-paper defect* below) |
| `evidence/payload_fire_blinded.md` | blinded fire-side payload (a historical proposal whose specification gap was witnessed in execution) |
| `evidence/payload_decline_blinded.md` | blinded decline-side payload |
| `audit_log.md` | HEAD stamp pair |

Blinding was mechanical and line-local — identifier substitution only, line counts asserted
unchanged, so no content could be silently dropped. That assertion **fired once** on a first attempt
whose multi-line regex consumed a newline; the substitution was redone line-locally.

## What was found, in the order it was found

**1. The tools restriction is real; the context restriction never existed.** Item (1) passed
two-sided. The reviewer then reported, unbidden, that its context already held both `CLAUDE.md`
files, `MEMORY.md`, and `gitStatus` naming four recent commits — **verified against ground truth,
all four commit subjects byte-accurate**, which a `WebSearch`-only agent cannot reconstruct.

**2. The inversion.** The skill was built on the claim that spawning made blindness *"structurally
enforced rather than depending on discipline about what gets pasted."* On the context axis that is
**backwards**: a web instance receives only what is pasted; the subagent receives the project's full
rule set always. Tools was the channel someone thought to restrict; the harness delivers context
through channels nobody enumerated.

**3. Six channels, not four.** (i) project `CLAUDE.md`; (ii) global `CLAUDE.md`; (iii) `MEMORY.md`
plus relevance-triggered sibling files — payload-dependent, so no static sweep can see it; (iv)
`gitStatus` recent commits — **non-stationary**, so any burn-check is an as-of result and not a
property; (v) **the agent definition is project-scoped**, so a clean room does not blind the
reviewer, it deletes it; (vi) **`gitStatus`'s untracked-file list**, which leaked
`payload_fire.md`/`payload_decline.md` — telegraphing the fire/decline dyad shape before any payload
was read. Channel (vi) was found by the reviewer and independently replicated by a second instance.

**4. The exam-paper defect.** The agent file's body *is* the subagent's system prompt. The first-use
and shakedown spec had been stored there for findability, so **every reviewer spawned before the
clean room read its own test plan** — two worked specification-defect exemplars and the pass
criterion for every item. Runs before the stripped copy are contaminated on items (3), (4), (5) and
(7); the room's copy carries the charter only.

**5. Payload contamination is anti-correlated with payload quality.** A bounded sweep of 199 audit
directories found 13 genuine specification-gap candidates; **7 of the top 8 were burned.**
`CLAUDE.md` is a *promotion channel* — its function is to lift witnessed defects into always-loaded
rules — so the more an incident taught, the likelier its lesson is already injected. **The remedy
was NOT to edit `CLAUDE.md`:** removing a live defense against a witnessed failure to unburn a test
is the falsifier-weakening move, it would have fixed 1 burn of 8 (five are git-log and unremovable),
and the passages belong to the scheduled rule-freshness pass judged on their own merits.

**6. The decline-side criterion was refuted, not the reviewer.** *"A proposal that executed clean"*
is an execution outcome; a decline arm needs a plan with **no gap to find.** Verified true of the
artifact: its per-item verification samples only the detector's positive class and the determinate
off-diagonal cells, never the concordant stratum — structurally unable to detect under-firing. A
sophisticated, clean-executing plan can carry a real specification gap that simply never got
tripped. The selection heuristic was the design error.

**7. The load-bearing result — item (7), fire side.** Five real rounds, each a genuine revision, to
`VERDICT: Ready`. Handed to a brand-new instance with zero negotiation history: *another round
needed*, with two findings that matter. The comparator-set sentence (*"rest-of-corpus = all 119
files"*) is **self-contradictory** and survived all five rounds because the negotiating reviewer had
accumulated context about what was *meant* and stopped parsing it literally. And `RECON.md`,
load-bearing three times, was never added to the substrate list — **flagged by that same reviewer at
round 4 and explicitly waived as "non-blocking, carried"** before it said Ready.

**8. The receivers refused, five times, and every refusal was a real defect.** A room they could not
reach; a registry they could not refresh; a canary the agent's own system prompt named; a pass
criterion that was **architecturally unpassable** (this harness withholds the function at spawn time,
so no runtime tool-call error can ever exist to paste); and an instruction that conflicted with the
authoritative spec. **None was caught by the sender re-reading.** All five were in prompts written by
the instance that had just documented the rule they violate.

## What this licenses, and what it does not

**Licensed.** The instrument detects specification gaps on text it has not seen the outcome of —
demonstrated once cleanly (the unprompted ratchet finding, which no answer key contained), plus one
direct hit on withheld ground truth. Triage, channel-flagging and persistence all passed two-sided.
The fresh-eyes pass is **not** rubber-stamping.

**Not licensed.** That the loop produces *better plans*. That is the RQ1-shaped question at this
scale and only production accrues it. This is a **day-one smoke test**, and its own record says so.

**Declared bounds.** The global `CLAUDE.md` arrived in every round — a *sensitivity-raising* tier
applying uniformly to both arms, which makes this a **rule-holding reviewer, not a naive cold
reader.** The repository is public and CC0, so pretraining exposure is not excluded. Item (7)'s
decline arm was stopped short of convergence by a disclosed resource bound, not completed. Item (1)'s
pass criterion was amended mid-run for architectural reasons and the amendment is on the record.

## The consequence that changed the design

Item (7) is not a test of the loop. It is a **step** in it: negotiated "Ready" is a verdict formed at
a framing the artifact does not carry, and a fresh read at another framing recovers what it hid. The
skill now runs the fresh-eyes pass as mandatory production, not as validation.

**The operator's independent precedent, which outranks everything above.** Essays in this project
are finished on a **no-context instance holding only the essay text**, and that practice — standing,
predating this arc, on a different artifact class — *always* finds something that multi-model review,
conversation, and the analysis suite do not. That is a naturally-arising instance **in the
instrument's own history**, the top of the discrimination ladder, and it replicates item (7)'s
finding from outside this arc entirely.
