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

`cache_read_input_tokens == 0` is asserted per unit. Nonzero means isolation failed and
the delivered count is corrupt: **that rung is VOID rather than reported.**

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
| `cache_read_input_tokens > 0` | ≥1 unit | that rung **VOID** |
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

The three-arm `--add-dir` test run 2026-08-12 (fixture `scratchpad/adddir_test/`) is a
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
