# PRE-REGISTRATION — OQ-289 recall-channel canary (STAGING DRAFT)

> **THIS FILE IS THE STAGING COPY AND IS NOT YET FROZEN.**
> On run day: **MOVE** it (`git mv`, not copy) to
> `audits/<execution-date>_oq289_recall_canary/PREREGISTRATION.md`, record its md5 in that
> dir's `audit_log.md` **physically above** the `<!--OQ289-FIRST-RESULT-->` sentinel, and
> only then run `--smoke` / `--live`.
> `oq289_recall_canary.py:assert_spend_go()` **refuses to spend while this staging file
> still exists** — two live copies of a frozen document with no queryable fact of
> canonicity is Build Discipline Pattern 2 performed on the freeze itself.
>
> It is staged here rather than in `audits/` because a post-adoption audit dir with no
> `WRITEUP.md` turns `audit_writeup_gate` **red**, and a check red by construction at
> introduction teaches the institution to route around it. The dir is created on run day,
> when the writeup can land in the same session.

**OQ:** ISSUES OQ-289 (successor to the retracted OQ-286). Feeds OQ-290 (Ω_P disposition).
**Driver:** `python/audits/oq289_recall_canary.py`
**Execution date:** _[stamped at freeze]_
**Approval:** MANUAL. The `--add-dir` prior art de-risked the spawn contract but not the
write path, the slope instrument, or the recall attachment. The first live call of *this*
driver is where those get witnessed, and that is worth one pair of eyes.

## 0. ORDERING — smoke before freeze (operator ruling, 2026-08-12)

**THE FREEZE WAS SCHEDULED BEFORE THE THING IT DEPENDS ON.** §10's smoke item — does
`--tools ""` also suppress the `relevant_memories` attachment, and does recall fire under
`-p` at all — **determines whether Arm A is runnable at all.** Freezing a prereg that names
an unrunnable test forces an amendment, and **an amended freeze is a weaker instrument than
one frozen a day later.**

So the order is: **smoke → resolve §10 → freeze → sweep.** Smoke's scope is declared
separately and in advance at `python/audits/oq289_smoke_scope.md`, whose md5 is persisted
with every smoke artifact; `assert_smoke_go()` refuses without it. Smoke renders **no
verdict** from §6's table and the driver structurally does not compute one in `--smoke`
mode.

**Smoke run 1 is spent and did NOT discharge the item** (evidence:
`python/audits/oq289_smoke_run1/`). It returned 0/3 on both arms with no positive control
distinguishing "the marker did not arrive" from "we never gave the recall system anything to
find." Two amendments to THIS document descend from it, and both are the reason the ordering
ruling was right:

- **§5's `cache_read_input_tokens == 0` assertion is UNSATISFIABLE and is replaced.** All six
  units returned `cache_read` of 3,289 / 4,479 with `input_tokens = 2` — the CLI caches the
  system prompt. Frozen as originally written, that HALT would have voided **every rung**.
- **§10's smoke gains a third arm**, `SMOKE_INDEX`, the always-loaded positive control that
  makes a zero readable at all.

Run 1 also **validated the primary instrument**: `delivered` was perfectly stable across k=3
within each arm (9,002 ×3, 10,262 ×3, zero variance), and the 1,260-token inter-arm gap is
exactly the `Read` tool definitions.

### SMOKE RUN 2 IS SPENT AND THE ANSWER IS NO — **THIS DOCUMENT IS NOT READY TO FREEZE**

9 calls, scope md5 `d0b9f31f`, evidence `python/audits/oq289_smoke_run2/`. Pre-registered row
fired: **`index n/n, sibling 0/n` — the ATTACHMENT path specifically does not deliver under
`-p`.** `--tools ""` is not the cause; sibling content failed on every arm, and on the
`Read`-enabled arm it arrived only because the model **fetched it with a tool call**.

**Arm A as designed in §3 is NOT RUNNABLE.** Per the smoke scope's own pre-committed rule,
that is the finding rather than a probe defect to tune away: **Arm A needs a triggering
mechanism designed before the sweep, not during it.** Freezing this document today would
freeze an unrunnable Arm A — which is precisely the outcome the ordering ruling existed to
prevent, and it worked.

**Three further amendments are owed before any freeze, all found by run 2:**

1. **§5's zero-tool-call verification must actually use `--output-format stream-json`.**
   Under `json` the message stream is absent, so `observed_tool_calls()` returned **0 for
   every unit including the three that demonstrably called a tool** — `TOOL_CALL_ON_SUPPRESSED_ARM`
   was **a check that could not fire**, the exact converse of the `cache_read` gate that could
   not pass, in the same driver on the same day. (Now returns `None` = UNMEASURED, with
   `num_turns` wired as a declared proxy.)
2. **§3's Arm A′ is UNIMPLEMENTABLE as specified.** "Measured as an observed tool call, never
   a self-report" has no referent under `json`. It requires `stream-json`, and that is now a
   precondition of the arm rather than a formatting preference.
3. **§4/§6 must account for a new false-ABSENT mode.** Run 2's index entry used relevance
   wording that reads as an *instruction*; models obeyed it, went to fetch, and never reported
   a marker sitting on line 1 of the file they were reading from. **A prompt can suppress the
   report of canaries the model can plainly see** — an absence-shaped success, and it would
   have scored as `DROPPED` or `ATTACHMENT_NEVER_FIRED` in §6's table.

---

## 1. The question

Recalled memory files under
`~/.claude/projects/-home-scott-bin-structural-dynamics-model/memory/` are delivered as
per-turn `relevant_memories` attachments. **Which constant pair governs them, and does an
over-limit file arrive truncated?**

Three things are underdetermined by the code and are what this run settles:

- **(a) which pair governs at all** — or whether both apply at different stages;
- **(b) per-file vs per-recall-batch** application;
- **(c) pre- vs post-processing** application.

## 1b. Phase 0 — the measurement this all descends from, DATED AND CONFIG-LABELLED

**Measured 2026-08-12, CLI 2.1.229, `~/.claude/settings.json` md5 `bc56274c` (6 keys,
**including** the `env` block), no `--add-dir` in play.** The config label is not decoration:
the `env` block — `CLAUDE_CODE_ADDITIONAL_DIRECTORIES_CLAUDE_MD=1` — **postdates the plan's
original Phase 0 measurement**, and a reader reconciling this table against a fresh
measurement would otherwise find a discrepancy with no recorded cause.

| Source | Bytes | Share |
|---|---|---|
| `CLAUDE.md` (project) | **91,029** (1,107 lines, flat, no `@import`s) | 88.6% |
| `memory/MEMORY.md` | 9,906 (83 lines) | 9.6% |
| `~/.claude/CLAUDE.md` (global) | 718 | 0.7% |
| SessionStart hook `additionalContext` | 1,042 | 1.0% |
| **delivered total** | **102,695 B ≈ 27.0k tokens @ 3.8 B/tok** | |

**This table is already stale in one cell, by our own hand, and that is recorded rather than
silently refreshed.** `CLAUDE.md` is **92,351 B** as of the OQ-289 documentation pass, which
promoted a memory-consolidation tripwire into it. Headroom against `R9o` = 4,194,304 B moved
**46.1× → 45.4×**. Nothing in the retraction turns on the difference; the point of recording
it is that a witnessed fact has a shelf life and this one aged inside a single day.

## 2. Stated priors, recorded here because the freeze must cover them

These are **predictions to be falsified, not findings.** Binary strings witness shipped
code; a canary witnesses the path actually taken. **If the run contradicts them, the run
wins.**

| Constant | Value | Path | Prediction |
|---|---|---|---|
| `NSp` | 4,096 B | `PIe(..., {truncateOnByteLimit:true})` via `QSp` | governs sibling recall |
| `Npa` | 200 lines | same | line-axis limit on the same path |
| `kae` | 25,000 B | `WEr`, incl. the `WEr(s.content,"memory")` call site | may govern instead |
| `iJ` | 200 lines | same | line-axis limit on that path |
| `R9o` | 4,194,304 B | `CLAUDE.md` "Project" read | **not** under test — 46× headroom, skipped-whole |

**Prior, stated:** the author considers `NSp`/`Npa` more likely than `kae`/`iJ` because the
attachment assembly is the code path that appends the truncation notice and the `Read`
pointer. **This prior is weak** — the discovery of `WEr(s.content,"memory")` is what
demoted it from an assumption to a prior — and it is recorded so that a result matching it
is not read as stronger confirmation than it is.

**No expected verdict is written into the deliverable template.**

## 2b. INTERPRETATION COMMITMENT — what each branch does to §8.5, written BEFORE the data

**The bracket is already in the design, so the run settles WHICH pair governs. The EXPOSURE
is what it does not settle, and the two branches do very different things to
`amnesiac_institution_v0_6.md` §8.5.** Committing now, because otherwise the kae branch
arrives looking like a refutation, gets argued about after the data is visible, and the
section's weight is set by whoever argues better — **which is the discretion the freeze
exists to remove.** This is the same defect `orphaned_controls()` caught in the driver, one
level up: an instrument that computes a verdict after seeing the numbers.

**Common to both branches — the structural claim is NOT falsified either way.** A merged file
exceeds a per-file delivery limit, and the 2026-08-10 consolidation is what created that
exposure. What the branches change is the **weight** §8.5 may give it.

### Branch NSp — `NSp`/`Npa` govern (19 of 53 exposed)

§8.5 **stands as written.** The two-caps tension carries the weight the section gives it:
nineteen merged files delivering 16–89%, with a `Read` pointer whose following is itself
unwitnessed (Arm A′). OQ-290's ballot is a real ruling over a triaged population.

*§8.5 edit: none beyond removing the `[UNWITNESSED]` tag and naming the count.*

### Branch kae — `kae`/`iJ` govern (1 of 53 exposed)

**CORRECTED 2026-08-12 against the full `WEr` body, and the correction matters.** The
one-file characterisation was *"over by 373 B, delivering ~98.5% — a hairline case at n=1."*
**It is not a hairline case.** `WEr` applies the **line** cut FIRST and the byte cut to the
result:

```js
let a = i ? r.split("\n").slice(0, iJ).join("\n") : r;   // LINE cut first
if (a.length > kae) { ... }                              // THEN bytes, on the result
```

`feedback_prereg_review_riders.md` is 25,373 B **and 359 lines**. The 200-line cap binds
first, yielding **15,451 B — 60.9% delivered by bytes, 55.7% by lines.** The 373-byte
overage never applies, because the line cut already brought the file under `kae`. **The
single most-cited feedback memory loses ~39% of itself.**

*§8.5 edit on this branch:* keep the structural claim and **re-scope the emphasis from a
population to a mechanism** — the tension is real and demonstrated at n=1 with a 39% loss,
rather than being a property of nineteen files. Delete any phrasing implying breadth; keep
"consolidating for attention created exposure to a delivery cap," which is exactly as true
at n=1. **Do not upgrade it to a general finding, and do not retract it as a near-miss.**

**Provenance of this correction, recorded because the asymmetry cuts both ways.** The
~98.5% figure was **operator-supplied**, and it was corrected by a **code-read** — reasoning
from the byte constant alone without the order of operations. The OQ-286 close justifies
trusting a code-read where the error budget is wide (46× headroom) and refusing one where it
is narrow; **this is the same ruling running in the other direction**, and it is the
direction that does not usually get written down. Recorded so the asymmetry reads as a rule
about consequence rather than a rule about whose reading wins.

### Both branches — a consequence for OQ-290 that only the code-read reveals

**`WEr` appends NO `Read` pointer.** Its notice is `> WARNING: this memory file is <what>.
Only part of it was loaded. Keep each memory file focused on one topic.` — no path, no tool.
Only the `PIe` path emits *"Use the Read tool to view the complete file at: <path>"*.

Therefore, **committed before the run rather than after — because an option retired in light
of results looks like an option retired because of them:**

**OQ-290's ballot is amended.** Options **1 (split)**, **3 (front-load)** and **4 (do
nothing, cost named)** stand on **both** branches. Option **2 (accept truncate-plus-pointer
as the contract)** is now **BRANCH-CONTINGENT and must be labelled so**: it exists only on
the `PIe`/NSp branch. On the kae branch there is no pointer to accept, so option 2 is not a
choice that exists, and front-loading gets stronger — a truncated `WEr` file is simply
*silently short*.

**Arm A′ is gated on the branch determination, and a kae outcome makes it MOOT, NOT FAILED.**
Its question — does an instance follow the `Read` pointer — is well-posed only on the NSp
branch. Keep the arm; run it only after the branch is determined. **A skipped A′ on a kae
outcome is a correctly-scoped run, not an incomplete one**, and the writeup must say so in
those words, because a silently absent arm reads as a gap in coverage.

## 3. Arms

| Arm | k | Tools | What it establishes |
|---|---|---|---|
| **A** | 3 | `--tools ""` | The threshold. Just-under / just-over pairs around each candidate cap. |
| **A′** | 3 | `--tools Read` | The behavioural half: pointer-follow, measured as an **observed tool call**. |
| **INJECT** | 3 | `--tools ""` | Reporting-channel control: END canary via `--append-system-prompt`, none in any file. |
| **LEAK** | 3 | `--tools ""` | Unintended-payload-path control, **and** the source of `BASELINE`. |

**Arm B (a `CLAUDE.md` size ladder) is deliberately ABSENT.** 46× headroom, and the
failure mode there is skip-whole-and-log rather than tail-truncation. That absence is
OQ-286's retraction, not an oversight.

**On Arm A's discrimination record.** The just-over/just-under pair is the record: same
instrument, same path, one fires and one declines, and **the decline is forced by a
documented constant rather than authored by us.** That places it above an authored decoy
on the discrimination ladder — a decoy shows only that authored decoys get rejected.

**On Arm LEAK's purpose, restated.** It does **not** guard the false positive usually
cited for it. A fresh 16-hex token forecloses inference at 2⁻⁶⁴, so this control **cannot
fire on that threat**, and a reviewer who spots the mismatch discounts the whole control
discipline. Its real function is to witness a canary reaching context by a path nobody
intended — environment, prompt assembly, or a tool call `--tools ""` did not actually
suppress. Kept for that, labelled for that.

**On Arm INJECT's necessity.** Without it, an all-ABSENT run is ambiguous between "the
harness dropped the payload" and "the model won't echo a random-looking token," and that
ambiguity voids the expensive verdict.

## 4. The ladder (12 units × k=3 = 36 calls)

Built by `build_ladder()`; **gate 0 asserts every declared rung has a payload on disk at
exactly its declared byte length** before any md5 is taken. An md5 over a prereg naming
more rungs than exist looks exactly like a freeze.

| Rung | Bytes | Lines | Batch | Axis |
|---|---|---|---|---|
| `NSp_bytes_under` | 3,584 | 40 | 1 | bytes |
| `NSp_bytes_over` | 4,608 | 40 | 1 | bytes |
| `NSp_lines_under` | 3,584 | 180 | 1 | lines |
| `NSp_lines_over` | 3,584 | 220 | 1 | lines |
| `kae_bytes_under` | 24,488 | 40 | 1 | bytes |
| `kae_bytes_over` | 25,512 | 40 | 1 | bytes |
| `kae_lines_under` | 24,488 | 180 | 1 | lines |
| `kae_lines_over` | 24,488 | 220 | 1 | lines |
| `batch_x4` | 3,584 | 40 | **4** | batch |
| `pointer_follow` (A′) | 12,288 | 60 | 1 | behaviour |
| `append_system_prompt` (INJECT) | — | — | — | control |
| `no_payload` (LEAK) | — | — | — | control |

**The line axis varies independently of bytes.** Without it `Npa` and `NSp` stay
confounded: a file over both caps cannot say which one fired.

Each payload carries **fresh 16-hex canaries at START / MIDDLE / END**, minted per unit,
never placed in the prompt. **START is named here as the attachment-fired control**, not
merely one leg of a two-sided pair — see §6.

Filler is asserted (not merely constructed) to contain **no `@word`** — the loader follows
`@`-imports and would add an uncontrolled file to the exact quantity being measured — and
**no 16-hex run**, which would be indistinguishable from a delivered canary at the read
site.

## 5. Primary instrument

**Harness-side token slope, not the model's word.**

```
delivered = input_tokens + cache_creation_input_tokens + cache_read_input_tokens
slope     = Δdelivered / (Δbytes / 3.8)
```

All three components, summed. Slope is computed between **adjacent rungs of the same
(cap, axis) family** — a slope across families would compare a byte ladder to a line
ladder.

| Band | Threshold |
|---|---|
| `PASS_THROUGH` | slope ≥ 0.80 |
| `PARTIAL` | 0.10 ≤ slope < 0.80 |
| `PLATEAU` | slope < 0.10 |
| `undefined` | Δbytes = 0 — **never rendered as 0.0**, because a plateau is a verdict |

**REPLACED after smoke run 1.** The original assertion — `cache_read_input_tokens == 0` per
unit — is **unsatisfiable under this transport**: the CLI caches the system prompt, and all
six smoke units returned `cache_read` of 3,289 / 4,479 with `input_tokens = 2`. Frozen as
written it would have voided **every rung**, which is a gate that cannot pass, as
uninformative as one that cannot fail.

The isolation worry it encoded was cross-unit contamination of `delivered`. The replacement
is satisfiable and strictly better at that worry: **with the payload held identical across k,
`delivered` must be IDENTICAL across k** (`DELIVERED_UNSTABLE_ACROSS_K`). Smoke run 1 shows
it passing with **zero variance** (9,002 ×3, 10,262 ×3). `cache_read` is legitimately
delivered context, stays inside `delivered`, and is **reported, not gated**.

## 5b. SECONDARY DISCRIMINATOR — a truncated file says which path cut it

Found on re-reading the full `WEr` body, and it is stronger than inferring the governing pair
from where a threshold lands, because it is **self-identifying**:

| Path | Appended notice | Read pointer? |
|---|---|---|
| `WEr` (kae/iJ) | `> WARNING: this memory file is <what>. Only part of it was loaded. Keep each memory file focused on one topic.` | **NO** |
| `PIe` (NSp/Npa) | `This memory file was truncated (<N> byte limit \| first <N> lines). Use the Read tool to view the complete file at: <path>` | **YES** |

`WEr`'s notice further names **which axis fired** — `<N> lines (limit: <N>)` (line only),
`… (limit: <N>) — its lines are too long` (byte only), or `<N> lines and <N>` (both).

Recorded per unit as `notice_path` / `notice_axis`. **No notice observed returns `None`, which
is NOT "untruncated"** — the model may simply not have echoed it — and is never coerced.

**IT IS UNCONFIRMED, AND IT CANNOT BE CONFIRMED IN SMOKE.** A notice only appears when a file
truncates, and truncation only happens at a threshold — so any probe able to exercise it would
carry exactly the threshold information smoke's scope forbids. There is no version of smoke
that both stays in scope and tests this.

**Owed confirmation, at the first truncating rung of the sweep, reading pre-committed here:**
a differing notice string is a discriminator only if it *arrives in the delivered text* rather
than being stripped during attachment assembly. If a unit's self-report shows truncation
(START present, END absent) but **no notice string arrives**, the discriminator is
**UNAVAILABLE** — record that plainly, and **do not fall back to inferring the governing path
from thresholds alone as though it had worked.**

`BASELINE` = the mean `delivered` of the **LEAK** arm (the unit with no memory payload).
`ε = 50 tokens`. `elevated` means `delivered > BASELINE + ε`. **BASELINE is a
pre-registered method plus a frozen ε, not a judgement made at analysis time** — that is
the only reason rows 3 and 4 of §6 can be disjoint. If the LEAK arm yields no usable
count, the verdict is `UNSEPARABLE_NO_BASELINE`; it is never defaulted.

## 6. Outcome table — the rows PARTITION, they do not merely cover

Total-over-the-space is **not** sufficient. If two rows can both be true, the analyst picks
between two valid readings *after* seeing data, which is precisely the discretion the
freeze exists to remove. **Evaluated in order; the first matching row wins, and every
row's condition is mutually exclusive with those above it.** Assigned by
`classify()` in the driver, from persisted data — not by hand in the writeup.

| # | Verdict | Condition |
|---|---|---|
| 1 | `LOUD_REFUSAL` | rc ≠ 0 with an "input too long"-class error — a *loud* decline, the informative contrast to a silent one |
| 2 | `CONTRADICTION` | slope `PLATEAU` *while* canaries are reported verbatim → the instrument is wrong; **refuse to render a verdict** |
| 3 | `ATTACHMENT_NEVER_FIRED` | 3/3 all-ABSENT **and** delivered at `BASELINE` — the attachment never arrived at all |
| 4 | `DROPPED` | 3/3 all-ABSENT **and** delivered **elevated** — it arrived and its content did not survive |
| 5 | `DELIVERED` | 3/3 report all three canaries verbatim |
| 6 | `TRUNCATED` | 3/3 report START (+MIDDLE) verbatim and END ABSENT |
| 7 | `BOUNDARY` | any remaining split; **its own row, never coerced into a neighbour** |

**Rows 3 and 4 are the pair that used to overlap** — `DROPPED` as first written subsumed
`ATTACHMENT_NEVER_FIRED`. The delivered-token count is the explicit discriminator.

**Two discriminators, doing different jobs.** Under the slope instrument alone,
*truncated* and *attachment never fired* are indistinguishable — **both plateau.** So:

- **START** separates `TRUNCATED` from the all-ABSENT rows (self-report side);
- **delivered vs `BASELINE`** separates `ATTACHMENT_NEVER_FIRED` from `DROPPED`
  (harness side).

`FABRICATED` is deliberately **absent** from this table. It is a HALT, not a verdict.

## 7. Numeric HALTs

| HALT | Threshold | Scope |
|---|---|---|
| Leak arm reports any canary | ≥1 unit | run **VOID** |
| Inject arm fails to echo its canary verbatim | ≥1 of 3 | run **VOID** (reporting channel broken) |
| **`FABRICATED`** — a regex-matching token emitted that was never minted | ≥1 | run **VOID** |
| `DELIVERED_UNSTABLE_ACROSS_K` — `delivered` varies across k at identical payload | ≥1 rung | that rung **VOID** |
| Live `CLAUDE.md` md5 changes | any | **hard abort** |
| `~/.claude/settings.json` md5 changes | any | **hard abort** |
| Live memory-dir manifest changes | any | **hard abort** |
| Stray `CLAUDE.md` / `.claude/CLAUDE.md` / `.claude/rules/` under a scratch or added dir | any | **hard abort** |
| Tool call observed on a `--tools ""` arm | ≥1 | unit **VOID** |
| Spend | `--max-budget-usd 0.50` per call | **run ceiling: 36 calls, ≤ $18.00** |

**`FABRICATED` is a HALT, not merely a verdict row.** A unit emitting a regex-matching
token that was never minted has broken the self-report channel, **and the breakage is not
local to that unit** — it discredits every `ABSENT` and every verbatim hit in the run. It
belongs beside LEAK and INJECT.

## 8. Isolation

**`--add-dir` is an instruction-injection channel, by default, for every unit.**
`CLAUDE_CODE_ADDITIONAL_DIRECTORIES_CLAUDE_MD=1` is in `~/.claude/settings.json` as of
2026-08-12, verified by a three-arm before/after test, and gates `.claude/CLAUDE.md` and
`.claude/rules/` in the added directory as well as the top-level `CLAUDE.md`. Under a
token-slope primary instrument that is an uncontrolled payload landing **in the exact
quantity being measured.**

- Fresh scratch cwd per unit. The harness keys the memory dir off the cwd, so a scratch
  cwd gets its **own, empty** memory dir (verified against the `--add-dir` fixture's
  leftover project dir, whose `memory/` holds 0 files).
- **`CLAUDE_CONFIG_DIR` is NOT relocated.** `~/.claude/.credentials.json` lives under it;
  moving it likely breaks auth and produces a failure unrelated to the hypothesis.
- Per unit: no stray instruction files under the scratch cwd; live `CLAUDE.md` md5, live
  `settings.json` md5, and the live memory-dir manifest all unchanged.
- Scratch project dirs are removed at run end through a **guard that cannot reach the live
  key** — the cleanup is two-sided in the selftest.

**Deviation from the plan, stated rather than silently substituted.** The plan asks that
"every write path is under `outdir/scratch/`". That is not achievable: the harness derives
the memory dir from the cwd under `~/.claude/projects/`, so the payload **must** be written
there for the channel under test to see it. The substituted guard pins the **live** dir
rather than the scratch one, which is stronger where it matters — a guard watching the
scratch dir would pass while the live dir burned.

## 9. Environment pins

| Pin | Value |
|---|---|
| `claude --version` | **2.1.229** — refuse otherwise |
| `~/.claude/settings.json` md5 | `bc56274c252002d23f95b04ff04d11fe` |
| Model | `claude-sonnet-5` |
| k | 3 |

**Five CLI versions shipped in six days.** A result attributed to 2.1.229 but produced by
2.1.230 is unattributable and unnoticeable after the fact. **Context assembly is a function
of two files and only one of them was being watched** — hence the settings md5 sits on the
same footing as the live `CLAUDE.md` guard.

Every verdict is stated at its scoped altitude: *"truncates at N bytes per file, model
`claude-sonnet-5`, CLI 2.1.229."*

## 9b. THE FOUR CONFIRMATIONS OWED BEFORE FREEZE (operator, 2026-08-12)

| # | Confirmation | Status |
|---|---|---|
| 1 | `~/.claude/settings.json` md5 recorded here **and asserted per unit**, on the same footing as the live `CLAUDE.md` guard — context assembly is a function of two files now | **DONE** — md5 `bc56274c` in §9; `assert_isolation()` asserts both per unit, two-sided in the selftest |
| 2 | Phase 0's 102,695 B table **dated and labelled with the config it was measured under** — the `env` block postdates it | **DONE** — §1b, which also records the one cell that has since gone stale by our own hand (91,029 → 92,351) |
| 3 | `FABRICATED` present **in the HALT table** at ≥1 → run VOID, not only in the verdict table — a minted-token mismatch discredits every ABSENT in the run, not just its own unit | **DONE** — §7 row 3; §6 states its deliberate absence from the verdict table; two-sided in the selftest |
| 4 | `claude --version` checked **now and again immediately before the sweep** — the gate pins it, but drift between freeze and execution is the case the pin is for | **PARTIAL** — checked at approval (2.1.229) and asserted by gate 0b on every invocation including smoke. **The pre-sweep re-check is owed on run day and is not dischargeable in advance.** |

## 10. Settle in `--smoke`, do not assume

Cheap now, expensive to discover after the sweep. **A `--dry-run` does not witness the
write path.**

1. **Does `--tools ""` also suppress the `relevant_memories` attachment?** If recall rides
   the same suppression path, Arm A returns a null that means nothing. **If it does,
   the delivery arms move to a narrow allowlist and the zero-tool-call HALT is re-scoped;
   the run does not proceed on `--tools ""` with an unverified attachment.**
2. **Does auto-memory fire at all under `-p`?** A per-turn recall attachment in a
   single-turn non-interactive call is not guaranteed. Same failure profile as (1).
3. Does `--json-schema` survive `--tools ""`? **If forced to choose, keep `--tools ""` and
   parse text.**
4. One live call must land a **parseable response file with a usage block** before the
   sweep is authorized.

## 11. Prior art — cited, because the earlier "no prior art" claim was false

The three-arm `--add-dir` test run 2026-08-12 (fixture preserved at `python/audits/oq289_prior_art_adddir/`) is a
`claude`-spawning harness on this same delivery channel: a decline arm (`--add-dir`, no
var → NONE) and two fire arms (shell var; `settings.json` env block → `ZARQUON-7741`),
file tools disallowed so the token could not be read off disk, verbatim echo required.

**It discharges nothing** — self-report only, non-hex token, no slope instrument, k=1 —
but it **de-risks the smoke item 'a live call lands and parses'** and **partially
pre-witnesses INJECT** (the model does echo a context-only token verbatim under
tools-disallowed).

*A pre-registration claiming no prior art while prior art sits in the same session is
exactly the small false absence this program is about.*

## 12. Deliverable

`audits/<execution-date>_oq289_recall_canary/` containing `PREREGISTRATION.md` (this file,
moved and frozen), `audit_log.md` (md5 above the sentinel), `payloads/`, `responses/`
(raw stdout, one file per call, written **before** parsing), `reports.json`,
`summary.json`, and one `WRITEUP.md` carrying the required header — executed date, OQ,
one-line verdict **at its scoped altitude**, manifest cite, evidence map naming every
artifact, and the **`Fired:`** bit.
