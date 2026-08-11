# PREREGISTRATION — OQ-277 RQ2 two-directional blind cross-coding

**Audit:** `audits/2026-08-10_oq277_rq2_crosscoding/`
**OQs:** OQ-277 (the experiment), OQ-278 (fork-residue row / index collision), OQ-280 (this run
produces the coded corpus §2.3 never produced).
**Written:** 2026-08-11, before any model call.
**Assembled by:** `python3 python/audits/oq277_build_prereg.py --write`; verified by `--check`.

> ## FREEZE STATUS — READ THIS FIRST
>
> **NO MODEL CALL HAS EVER BEEN MADE IN THIS AUDIT.** `payloads/` and `responses/` are empty by
> design — verified 0 files each at the time this document was assembled — and stay empty until
> the operator's spend-go.
>
> **THIS DOCUMENT IS NOT YET THE FROZEN PREREGISTRATION.** One designed leg is not yet built:
> the **7 (iii′) exemplar units** are the pending hand-back from
> `HANDOFF_IIIPRIME_EXTRACTOR.md`, which the operator launches as a separate instance. Until
> those land, the assembled packets hold **66 of the designed 73 items** and the expected call
> count is **198 of 219**.
>
> A freeze stamp over an incomplete design would be a success-shaped token: it would look
> exactly like a freeze, and the driver's `assert_spend_go()` would pass. So the ordering is
> enforced structurally instead — **`--live` refuses while any leg is unbuilt**, independently
> of the md5 — and the md5 recorded in `audit_log.md` alongside this document is explicitly
> labelled a DRAFT stamp, not the freeze.
>
> **To complete the freeze:** (1) the (iii′) extractor hands back 7 units; (2) re-run
> `oq277_make_coder_packets.py --build-run` and confirm 73 items; (3) re-run the driver stub and
> confirm `captured == expected == 219`; (4) re-assemble this document and record the NEW md5 in
> `audit_log.md` physically above the first result line; (5) request spend-go.

---

## 1. What is pre-registered here, and what is not

Everything below is fixed **before any result exists**. Where a choice could otherwise be made
with numbers in hand, the rule that makes it is stated rather than the choice.

**Pre-registered:** the three legs and their populations; the unit lists; the coder model and
call shape; k and the unanimity rule; the prompts verbatim; the banned lexicons verbatim; the
leak-exemption list; the gate order; the expected call count and how it is computed; every
declared residue in §7; and the whole of `verdict_grammar_amendment.md`, incorporated verbatim
as Appendix D — including §Q and the two entries added 2026-08-11 (§L.4, §L.5).

**NOT pre-registered here, because it is settled elsewhere and must not be restated in a way
that could drift:** the hypotheses H1–H6 and their numeric HALTs, the interpretation table, the
verdict grammar at the effect-size floor, the staged-extension rule, and the escape check's
close — all live in **ISSUES.md → OQ-277** and in the artifacts this document pins by md5 in
Appendix A. A second copy of a frozen rule is a second thing that can drift from it; the
canonical location stays canonical.

**NOT pre-registered at all, and named so no reader supplies it later:** any mapping between
Wu's classes and our patterns. The writeup emits a PROPOSED mapping table and does not rule
(Ω_C, operator's seat at close).

## 2. The three legs

| leg | units | coded against | prompt | unit-sweep direction |
|---|---|---|---|---|
| direction (i) | Wu's 22 redacted incidents | our six: `P1`–`P6` + `other` | `prompts/direction_i.md` | `i` |
| direction (ii) | our 26 sampled incidents (22 matrix + 4 floor-only) | Wu's five: `A`–`E` + `other` | `prompts/direction_ii.md` | `ii` |
| (iii′) | our 7 newly-extracted published exemplars (+3 referenced anchors) | our six: `P1`–`P6` + `other` | `prompts/direction_i.md` | `ii` |

**(iii′) shares direction (i)'s prompt file deliberately.** Same task, same label space, same
definitions; a separate file would be a byte-copy with no queryable fact saying which is
canonical, and the two would drift the first time a definition was reworded — P2 inside the
experiment that measures P2. **Consequence, pre-registered:** the two legs share an instrument,
so a wording defect in `direction_i.md` is common-mode across them, and **agreement between the
two legs is not corroboration.** The (iii′) row may not be used as an independent check on
direction (i)'s prompt quality.

**(iii′) is n=10, not ~15** (`RULING_2026-08-11_freeze_scope.md` §2.1: 11 named, 1 disqualified).
Three of the ten are existing direction-(i) anchors and are **taken as-is, not re-extracted** —
two extractions of one source would be an unlabelled, unquarantined twin sitting inside a
calibration row, and it would report as agreement. The row is reported **with and without** the
three anchor members, since including them measures partly the same calls H3 already consumed.

**(iii′) coverage: P1(3) / P2(2) / P4(1) / P5(2) / P6(2). P3 = 0.** Pre-registered before any
number exists: **a pattern with zero members contributes NOTHING to the row.** Not read as
agreement, not counted in the denominator, not reported as "no disagreement observed." P3's row
entry is `no members — uncalibrated`. An empty cell and a cell where coder and publication
agreed are the same shape at the read site, and collapsing them is the absorption defect this
experiment studies. At n=10 one unit is 10%: whole-row agreement is reportable, **per-pattern
agreement is not**, and any sentence reading a per-pattern (iii′) figure as a finding is a
pre-registered error.

**Escape units: 0 calls.** Not an oversight. The escape row is CLOSED UNRESOLVED with its
calibration arm structurally one-sided; its licensed output is the extraction-based bounded
claim, and there is no pre-registered escape-coding row for such data to land in. Declared here
so the zero is a decision rather than a silence.

## 3. Interleaving location — DECIDED

`HANDOFF_TWINS_AND_DRIVER.md` §1.5 says either choice is defensible and that leaving it implicit
is not. The choice:

> **Full packets are assembled by `oq277_make_coder_packets.py --build-run`, and THE PACKET IS
> THE RECORD. The driver only sends.**

Full packets win because the freeze needs an md5-able artifact that **is** what was sent. Had the
driver interleaved at send time, the payload dump would be the only authoritative record and the
packet a mere unit source — auditable only after the spend.

`packets/coder_direction_i.json` is the frozen step-2 artifact and is **read, never rewritten**;
its 22 items keep their opaque ids `i-01`..`i-22` and their relative order, with anchors, decoys
and twin arms inserted at seeded slots around them. The preserved subsequence is asserted by a
pre-write gate, not trusted.

**Coder-facing surface:** an item is exactly `{id}` + the four fields
`symptom`, `mechanism_as_described`, `detection_path`, `consequence`. Role, true label,
`matrix_unit`, source id and quarantine status live in the sibling `*_map.json` under a
NOT CODER-FACING header. **The opaque id never reaches a coder** — the rendered prompt shows only
the four fields — so no ordinal marks the anchors, decoys or twin arms out.

**Quarantine keys on `matrix_unit`, and on nothing else** (§I.2). Never on `role`; never on
`overlap_source` alone, which yields 18 cells where the ruling says 22. Machine-checked: both
directions report exactly 22 matrix cells.

## 4. Call shape

- **Coder: `claude-sonnet-5` only.** Stateless single **user** turn, no system-prompt taxonomy,
  no context from prior items. Subagents cannot be coders: `CLAUDE.md` carries P1–P6 into every
  harness instance, so an in-harness coder is not blind by construction.
- **k = 3 same-input redraws per item per leg.** Label = **unanimous 3/3**, else **UNSTABLE**,
  which gets its own row and is **excluded from cells**. A pre-write gate asserts the payload
  md5 is constant across k: if the payload varies, the three draws are not replicates and
  unanimity measures nothing about churn.
- **One label from a fixed vocabulary and nothing else** — no confidence, no rationale, no
  hedging field. k=3 unanimity IS the churn instrument; a second uncontrolled signal would
  become an unpreregistered weight that an adjudicator would read.
- **`other` is first-class**, in the same list shape as the lettered/numbered classes and with a
  positive definition ("a substantive answer, not a leftover"), never a trailing "if none
  apply." Its reachability is a property of the WORDING, and both the both-residue row and the
  escape check depend on it. The decoys test whether the coder *can* return it; the prompt
  determines whether it *will*.
- **The provenance pin is in THIS document, not in any payload.** Putting `CLAUDE.md @ <hash>`
  inside a payload identifies our source to the coder — a leak *through the weights* that a
  payload grep cannot catch, since the coder could recall the published taxonomy instead of
  reasoning from the definition. Payloads carry definitions with provenance stripped; Appendix A
  carries the pins so a reader can verify they were the published ones. A pre-write gate asserts
  no payload contains a commit-hash-shaped token.

## 5. Leak control

### 5.1 Two lexicons, two roles, one module — and the role split is a ruling

`python/audits/oq277_lexicon.py` is the single matcher; a second copy would be a P2 fork inside
the experiment that measures P2. It carries **two** pinned versions:

| name | role | status |
|---|---|---|
| `LEXICON_DETECT` | the live leak-grep | widened 2026-08-11; used everywhere |
| `LEXICON_SELECTION_20260811` | reproducing the pre-declared redaction-pair selection | **FROZEN. Widening prohibited.** One caller: `controls/recheck_predeclared_counts.py` |

**Why two.** The lists joined multiword patterns with a literal space, so hyphen-joined forms
escaped — and three are attested in our own prose: `Build-Discipline Pattern-1`
(`AUDIT.md:144`), `Build-discipline spine` (`FINDINGS.md:22`), `Pattern-6
success-shaped-absorption` (`WRITEUP.md:1`). A leak-grep that catches `Pattern 1` and misses
`Pattern-1` is a **false absence in the instrument that certifies H2**.

Widening is a strengthening for **detection**, where a false positive is conservative — you
investigate, you clear it, nothing is lost. It is **inadmissible for selection**, where a false
positive is silently decisive and the pre-declaration's entire value is that it was fixed before
content was seen. Under the widened lists the declared rule's top-3 changes
(`oq97_pattern6_census` 4 → 9 overtakes `oq138` at 5) — **and it moves toward the corrected set,
the direction that flatters the both-residue row.** That the movement is convenient is exactly
why re-declaring under the widened list was **refused** rather than adopted. Operator ruling,
2026-08-11; recorded in full at amendment §L.5.

**What made the split available was a measurement, not an argument:** across all 54 coder-facing
texts, the widening changes **zero** hit sets.

### 5.2 Sweep scope — a payload is not swept whole under its own direction

Every prompt necessarily contains its own direction's class definitions, so **no payload can
ever be clean under its own direction's full lexicon.** The rule:

> **unit portion** → its own direction's FULL lexicon.
> **whole payload**, prompt included → the OTHER direction's `source_identifying` group only.

Sweeping a payload under the other direction's *full* list would fire on that taxonomy's own
class definitions, which the prompt must contain; `source_identifying` is the group that must
hold in both directions. The definitions block is fixed, reviewed and identical across every
payload in a leg, so it is audited once (`controls/verify_prompts.py`, 49/49) rather than
re-cleared 219 times, while the part that varies per call is swept in full. Strictly stronger
than sweeping unit text alone; loosens nothing.

### 5.3 Gate order — the order IS the control

Before Phase 3, in this order:

1. **COUNT FIRST.** Assert `len(captured payloads) == expected_calls`, with `expected_calls`
   computed from the assembled packets, never hardcoded. A capture bug writing zero payloads
   yields a perfectly clean grep and a green H2 — a success-shaped absence, the exact defect
   class this experiment codes for. **A green grep printed above an unverified capture count is
   the failure shape.**
2. Assert the **2 planted-leak fixtures** are in their own subdirectory and counted
   **separately** — they are dumped, not sent, so they must not inflate the equality above.
3. **THEN** grep, three-way: fixtures **MUST fire**; pre-listed exempt twin-arm payloads **MUST
   fire**; everything else **MUST be clean**.

Quarantined and overlap calls still count toward the expected total (§E *Accounting*).

**Leak exemptions, pre-listed (per-payload, and they must FIRE):** direction (i) `i-28`, `i-29`,
`i-30`; direction (ii) `ii-32`, `ii-33`, `ii-34`, `ii-35`, `ii-36`. These are the unredacted twin
arms, which necessarily contain source vocabulary — that IS the control. An exempt payload that
sweeps **clean** un-redacted nothing and would report a floor of zero by construction, so it is
a gate failure, not a pass. Exempt payloads can never enter cells.

**Any real-payload hit → H2, that direction VOID, not patched mid-run.**

## 6. Expected call count

Computed from the assembled packets, not asserted:

| leg | items | calls at k=3 |
|---|---|---|
| direction (i) — 22 units + 3 anchors + 2 decoys + 3 twin arms | 30 | 90 |
| direction (ii) — 26 units + 3 anchors + 2 decoys + 5 twin arms | 36 | 108 |
| (iii') — 7 new units (3 anchor members reuse their direction-(i) calls) | 7 | 21 |
| escape units | **0** | **0** |
| **assembled total** | **73** | **219** |
| **design total** | **73** | **219** |

> Assembled total equals the design total. The call-count precondition for the freeze is satisfied.

**The live path is structurally gated.** `--live` refuses unless PREREGISTRATION.md exists AND
its md5 is recorded in `audit_log.md` above the first result line AND every leg is built.
Building a driver and smoke-testing it with one real call is the most natural thing in the
world, and it would put a result on disk before the freeze; a rule that depends on remembering
that is not a rule. There is also no default transport: `--stub` or `--live` must be named.

## 7. Declared residues — carried, not discovered later

Each is stated in full at the cited section of Appendix D.

| # | residue | where |
|---|---|---|
| 1 | **Four-measured-units limit** on what the redaction floor licenses | §I.1 |
| 2 | **Floor asymmetry** — a sub-4/4 floor is an UPPER BOUND and §E fails CLOSED | §I.4 |
| 3 | **The quarantine mechanism is UNTESTED** — k = 0 confirms the null and leaves the mechanism unexercised (condition C3) | §M |
| 4 | **`incident_location` is NOT pooled** across the primary and escape strata | §N |
| 5 | **Calibration residue** — no P6 anchor in direction (i); no multi-membership anchor in direction (ii). Any P6 result is UNCALIBRATED and the E↔P6 row lacks anchor support on our side | §O, §O.1, §O.2 |
| 6 | **(iii′) at n=10 with P3 uncalibrated** — calibration data, not a verdict; per-pattern figures are a pre-registered error | §2.4 of `RULING_2026-08-11_freeze_scope.md`; restated in §2 above |
| 7 | **The self-comparison family** — three caught in this arc, each in a different instrument | §P, §P.1 |
| 8 | **H3's narrow licence** — anchors are the taxonomy's own published exemplars; recovering them licenses "the coder is not broken," never "the coder classifies unfamiliar incidents reliably" | §O preamble; ISSUES OQ-277 control (a) |
| 9 | **The asymmetric (i)-vs-(ii) row SHIPS AS TYPED OPEN** — the same-family confound is registered but unmeasured, and registering a confound does not license reading through it | ISSUES OQ-277 interpretation table |
| 10 | **The (iii′) and direction-(i) legs share a prompt** — common-mode instrument; agreement between them is not corroboration | §2 above |
| 11 | **The escape row's zero calls** — CLOSED UNRESOLVED, calibration arm structurally one-sided; no row exists for coded escape data to land in | §2 above; ISSUES OQ-277 |
| 12 | **The pre-declared selection reproduces only under the frozen lexicon**; the live detector is wider by ruling | §L.5; §5.1 above |

### 7.1 The redaction-pair sets, and which one feeds the row

Operator ruling (option C, 2026-08-11): both sets ship, reported separately, **never pooled** —
pooling a taxonomy-restoring arm with a source-identifying-only arm would measure the pooling
convention rather than redaction, the same shape as the pooled-H¹ finding.

| set | units | feeds |
|---|---|---|
| **corrected** | `04`, `07`, `05` | **the both-residue row's floor** |
| **declared** | `04`, `10`, `20` | reported alongside, the pre-declared comparison |

`04` is in both and is **coded once**, serving both. A row must be fed by the instrument that
measures its own quantity, and the instrument was assigned **before either number exists**.

**Units `10` and `20` restore ONLY source-identifying vocabulary** — measured at their own
`files_read` denominator, they contain zero taxonomy vocabulary. Their Δ is a
*source-identifying-redaction* floor and **must never be read as a taxonomy floor.** Every pair
carries a machine-checked `restoration_kind`, and `controls/verify_redaction_twins.py` asserts it
two-sided: a `taxonomy` pair must restore unambiguous taxonomy vocabulary (bare `P[1-6]`
excluded — it is the token that caused the original selection defect), and a
`source_identifying_only` pair must restore none.

**Unit `05` omits the bare `(Pattern 4)` index**, declared in place and machine-checked absent.
Under the published six that index names a different pattern than the incident instantiates: the
source was written before the index collision was known and used the orphan branch's numbering.
Restoring it would inject a **wrong label** rather than un-redact vocabulary. This is the third
independent sighting of the collision and the first producing a wrong label rather than an
ambiguous pointer — restated at ISSUES OQ-278.

## 8. Stopping rule

Every verdict names a tier-available falsifier or ships as typed OPEN (routed to a typed Ω
against `docs/omega_variables.md`, not loosely). Expressibility → a different-family re-code
(named, not bought). Churn verdicts → larger k. Frame-scoped coverage → the escape-check row.
**No tier falsifier available ⇒ typed OPEN, never a quiet verdict.**

## 9. What this document does NOT authorize

- **The staged extension to all 73 directories.** Gated on H5 passing and **not authorized in
  advance**. The extension changes n and nothing else — byte-identical prompts, lexicons,
  verdict grammar, k, model. Any other change is a new experiment with a new prereg.
- **Any escape-unit coding call.** Zero, by the row's close.
- **Any mapping ruling.** Ω_C, the operator's seat at close.
- **Any live model call whatsoever** until the freeze completes and spend-go is given.


---

## Appendix A — pinned sources (md5)

Every artifact this preregistration depends on, pinned so a reader can verify the versions in force. **The pins live here and never in a payload**: a commit hash inside a payload identifies our source to the coder through the weights, where a payload grep cannot reach.

| artifact | md5 | what it is |
|---|---|---|
| `CLAUDE.md` | `743349c6e88104744778316d86c10718` | the published six; Build Discipline block, lines 472-540 |
| `docs/technical/build_discipline.md` | `8501d448c03e77f5013aaff7a9d22ebe` | mechanism text behind the six |
| `packets/wu_source/failure_modes_catalog.md` | `f854454ed2be5bf489f2c5ee133ce013` | Wu's A-E, as fetched |
| `packets/wu_source/llm_observer_ground_truth.yaml` | `f26359b2d9f98b6b310aed9b473a1395` | Wu's dataset rows |
| `python/audits/oq277_lexicon.py` | `dbb7440a86b9eaf98256b1d02ebea229` | the single leak matcher, both pinned versions |
| `prompts/direction_i.md` | `c2d8c9bf65fac64460305c9fbacb0a34` | coder prompt: directions (i) and (iii') |
| `prompts/direction_ii.md` | `4fb7ceeb17aa1dab472c048b698a3b7f` | coder prompt: direction (ii) |
| `verdict_grammar_amendment.md` | `96ca8dba5429ad7be116f3f6b70f83d0` | incorporated verbatim as Appendix D |
| `controls/anchors.json` | `470be752cdfeed9c366f66ea909ada00` | anchor set, both directions |
| `controls/decoys.json` | `7bc88644ff18a1df722f68c04a091bb1` | decoys |
| `controls/planted.json` | `68477df87f2f1e53474f2733ab2b0034` | planted leak fixtures + planted broken unit |
| `controls/redaction_pairs_predeclared.json` | `266264af453bde1ed226183543e0cf9d` | the pre-declared pair selection |
| `controls/redaction_twins_direction_i.json` | `f3d6e9b73af48da6f57d3d037d94a4fd` | 3 unredacted arms |
| `controls/redaction_twins_direction_ii.json` | `bb0058bd38138d98b99b27f88983e531` | 5 unredacted arms, 2 sets |
| `RULING_2026-08-11_freeze_scope.md` | `dcb9674178766def5015e4f9492411cd` | (iii') population and the freeze scope |
| `controls/redaction_pair_selection_defect.md` | `531747bd7d92dc531e40bbddb1475091` | option-C ruling |

**Wu's two source files were fetched 2026-08-10** from `bisdom-cell/openclaw-model-bridge` (public, accompanying the arXiv paper); the fetch manifest carries the same md5s (`packets/wu_source/FETCH_MANIFEST.txt`).

---

## Appendix B — the coder prompts, verbatim

These are the exact templates the driver formats with each item's four fields. **They are the only artifact in this design with no witness** — a prompt cannot be validated by running it, because running it is the spend. `controls/verify_prompts.py` checks them against their pre-registered constraints (49/49); the wording judgement is the operator's, at freeze (C4/C5), and is the only control this artifact has.

### B.1 `prompts/direction_i.md` — directions (i) and (iii')

`````
You will read one description of a software failure and assign it exactly one category.

Here are the six categories, plus a seventh for incidents that fit none of them. All seven are
equally available answers.

P1 — Produced-but-not-consumed.
Data is generated and written, and nothing reads it back into the thing that needs it. A producer
is not done until something consumes its output. This also covers consumed-once-but-not-kept-fresh:
a post-process that is never re-run again goes silently stale while everything downstream keeps
reading it as current.

P2 — One-canonical-thing-became-two.
A file gets copied to a scratch or test location and edited; two versions now exist with no
queryable fact saying which one is canonical. The defect is the absence of a checked canonicity —
"which one is real" lives in someone's memory rather than in a path, a check, or a record.

P3 — Destructive-replace without proof.
Something is deleted, retired, or overwritten that another part of the system relies on, without
running old and new side by side and showing the outputs are identical or justifying every
difference. Believing two things are equivalent by reading them is not the same as demonstrating
it; the comparison is the proof, and it was not made.

P4 — Recap-as-witness substitution.
A summary claim — "done", "verified", "working", "complete" — stands in for the evidence that would
discharge it. Only the actual pasted output discharges such a claim. Reporting N pieces of work
completed while showing evidence for fewer than N is the defect; the missing evidence reads as
present because the summary asserts it.

P5 — Absence satisfies the gate.
A gate, threshold, or quantifier passes because its input is *missing*, not because a condition was
*checked*. A count equals zero when no records exist at all; a "for all X, Y" is trivially true over
an empty table; a ceiling comparison passes on a value that was never really supplied. The check
reports success without ever having examined anything.

P6 — Success-shaped absorption.
An aggregation or a channel cannot distinguish *measured-empty* from *never-looked*, and emits
success-shaped output either way. Each component is individually sound, so no check at any single
site catches it — the defect lives where the components compose. A failed computation and a real
measurement of nothing arrive at the read site as the same value.

other — The incident's mechanism is not one of the six above.
This is a substantive answer, not a leftover. Choose it whenever the mechanism described is
genuinely a different kind of thing, including when the description is too thin to identify a
mechanism at all.

---

Assign the single category that best matches the MECHANISM of the incident below — how the failure
worked, not where it happened or how severe it was.

INCIDENT
--------
Symptom:
{symptom}

Mechanism as described:
{mechanism_as_described}

How it was detected:
{detection_path}

Consequence:
{consequence}

---

Reply with exactly one of these tokens and nothing else:

P1  P2  P3  P4  P5  P6  other

No explanation, no confidence, no punctuation, no other words.

`````

### B.2 `prompts/direction_ii.md` — direction (ii)

`````
You will read one description of a software failure and assign it exactly one category.

Here are the five categories, plus a sixth for incidents that fit none of them. All six are equally
available answers. Each category is given in its original wording followed by an English rendering.

A — 环境/平台 Quirk  (environment / platform quirk)
逻辑正确，运行环境隐含行为导致失效（dev 绿灯，target 暴露）。
系统逻辑正确，但运行环境（OS / shell / 客户端 / 文件系统）的隐含行为导致意外。
*The logic is correct, but implicit behaviour of the runtime environment — operating system, shell,
client, filesystem — causes it to fail. Characteristically: the development environment is always
green, and only running on the real target exposes it.*

B — 设计假设错配  (design-assumption mismatch)
代码假设 vs 实际（部署拓扑 / 契约 / 测试形态 / 涌现行为）。
代码基于某个假设，但假设与实际不一致。
*The code rests on an assumption — about deployment topology, a cross-component contract, the shape
of test input, or emergent behaviour — and the assumption does not match reality. Characteristically:
unit tests covering the logic correctly is not the same as covering the real caller's actual form.*

C — 错误吞噬与稀释  (error swallowing and dilution)
错误发生但被某层静默吃掉 / 跨层稀释失 cause / 自动批量放大。
错误真实发生了，但被某一层静默吃掉，或跨层传递时上游 cause 被稀释，最终用户视角看到的是"成功"或
一个失去信息的告警。
*The error really happened, but some layer silently ate it, or the upstream cause was diluted while
being passed across layers, so what the end user finally sees is "success" or an alert that has lost
its information.*

D — 链式幻觉与编造  (chained hallucination and fabrication)
把污染数据当事实，编造合理叙事推送给用户（最危险）。
错误不是消失，而是被加工成看起来正常的内容。链路中每一跳都会放大幻觉，前一跳的幻觉会被下游当作
事实执行。
*Corrupted data or polluted context is taken as fact and worked up into a plausible narrative that
is pushed to the user. The error does not disappear — it is processed into content that looks
normal. Each hop amplifies it, and one hop's fabrication is executed downstream as fact.*

E — 运维遗漏与取证盲区  (operational omission and forensic blind spot)
代码正确但部署/注册步骤遗漏 / 调试工具自身被屏蔽长潜伏。
两个子机制：(1) 运维遗漏 — 代码正确但部署/注册/配置步骤被漏（声明态 ≠ 运行时态）；
(2) 取证盲区 — 调试工具自身被屏蔽返回空内容，被误读为"正常"，导致超长潜伏。
*Two sub-mechanisms: (1) operational omission — the code is correct but a deployment, registration
or configuration step was missed, so declared state is not runtime state; (2) forensic blind spot —
the debugging tool is itself blocked and returns empty content, which is misread as "normal",
producing an extremely long latency before discovery.*

other — 机制不属于以上任何一类。
*The incident's mechanism is not one of the five above. This is a substantive answer, not a
leftover. Choose it whenever the mechanism described is genuinely a different kind of thing,
including when the description is too thin to identify a mechanism at all.*

---

Assign the single category that best matches the MECHANISM of the incident below — how the failure
worked, not where it happened or how severe it was.

INCIDENT
--------
Symptom:
{symptom}

Mechanism as described:
{mechanism_as_described}

How it was detected:
{detection_path}

Consequence:
{consequence}

---

Reply with exactly one of these tokens and nothing else:

A  B  C  D  E  other

No explanation, no confidence, no punctuation, no other words.

`````

### B.3 Prompt design notes

`````
# Coder prompts — the only artifact in this design with NO witness

**Status: AUTHORED, awaiting operator review at freeze (conditions C4 / C5).**

Every other artifact in this audit is checkable against something: units against their source
directories, anchors against a frozen label source, the lexicon against its selftest, the packets
against their gates. **A prompt has no such referent.** It is the one place where a wording choice
silently becomes a measurement property, and it cannot be validated by running it — running it is
the spend. So it ships as *reviewable content in the preregistration*, and the review is the
control.

The specific hazard, stated so the review knows what to look for: **`other`'s reachability is a
property of the wording, not of the taxonomy.** The both-residue row and the escape check both
depend on `other` being genuinely available to the coder. The decoys test whether the coder *can*
return it; the prompt determines whether it *will*. A prompt that lists `other` as a trailing "if
none of the above apply" makes it a residual, and a residual is under-selected in ways no
downstream check can distinguish from real coverage.

## Three legs, TWO prompt files — and the missing third file is deliberate

| leg | prompt file | codes | against | sweep direction |
|---|---|---|---|---|
| direction (i) | `direction_i.md` | Wu's incidents | our six: `P1`–`P6` + `other` | `i` |
| direction (ii) | `direction_ii.md` | our incidents | Wu's five: `A`–`E` + `other` | `ii` |
| (iii′) | **`direction_i.md`** — the same file | our own published exemplars | our six: `P1`–`P6` + `other` | `ii` |

**There is no `iii_prime.md`, and that is the point.** (iii′) puts the identical task to the coder
as direction (i) — read one incident, assign one of our six — with the identical label space and the
identical definitions. A separate `iii_prime.md` would be a byte-copy of `direction_i.md` with no
queryable fact saying which is canonical, and the two would drift the first time a definition was
reworded. That is P2, inside the experiment that measures P2. **The driver reads one file for both
legs and asserts it is the same file**, so the sharing is a checked fact rather than a convention.

What differs between the two legs is not the prompt but the **packet** and the **sweep direction**:
(iii′)'s units are ours, so they sweep under direction `ii`, while its answers are in direction (i)'s
index. That crossing is the easiest thing in the design to get backwards and is asserted in the
driver rather than left to care.

**One consequence to carry into the prereg:** because both legs share a prompt, a wording defect in
`direction_i.md` is common-mode across them. The (iii′) row cannot be used as an independent check
on direction (i)'s prompt quality, and no reading may treat agreement between the two legs as
corroboration — they share the instrument.

### The label tokens and the index collision

The answer tokens `P1`–`P6` are the published indices, and two of them are known to be ambiguous in
our own records (an index collision between two documents, tracked separately). That ambiguity does
**not** reach the coder: the prompt defines each index explicitly by its rule text, and the coder's
`P3` means exactly what the prompt's `P3` paragraph says. Scoring must therefore compare the coder's
answer against the label as the PROMPT defines it, never against a historical reference to "Pattern
3" or "Pattern 4" elsewhere in the repository — those are ambiguous until dated against the
collision.

## Constraints, all from C5 and the frozen design — none inferred

1. **Taxonomy definitions verbatim.** The published six from `CLAUDE.md`'s Build Discipline block;
   Wu's A–E from the md5-pinned `packets/wu_source/failure_modes_catalog.md`. Wu's are carried in
   the source's own Chinese with a faithful English rendering alongside — the same convention the
   redacted units were extracted under, because the coder reads English and a translation-only
   presentation would silently substitute the translator's reading for the source's.

2. **EXEMPLARS ARE STRIPPED from every definition.** This is the one deviation from "verbatim", and
   it is required twice over:
   - `CLAUDE.md`'s pattern definitions name their exemplars inline, and **those exemplars ARE the
     (iii′) units.** Shipping them would hand the coder the answer key for that entire row.
   - The same exemplars are dense in source-identifying vocabulary (predicate names, OQ ids, file
     paths), which is banned in both directions. In `direction_i.md` they would be OUR source
     leaking into a payload about WU's incidents.
   The rule statements are carried whole; only the parenthetical instances are removed. Each
   definition below is traceable to its line in the pinned source, and the pin lives in the
   preregistration — not here.

3. **File + commit hash pin lives in the PREREGISTRATION, not in the payload.** Putting
   `CLAUDE.md @ <hash>` inside a payload identifies our source to the coder, which is a leak
   *through the weights* that a payload grep cannot catch — the coder could recall the published
   taxonomy rather than reason from the definition. The payload carries definitions with provenance
   stripped; the prereg carries the pin so a reader can verify they were the published ones.

4. **Single user turn.** A stateless SDK call. No system-prompt taxonomy, no conversation, no
   context from prior items. k=3 redraws are three independent calls, not three turns.

5. **One label from a fixed vocabulary and NOTHING ELSE** — no confidence score, no rationale, no
   hedging field. k=3 unanimity IS the churn instrument; a second uncontrolled signal would get
   read by an adjudicator, and a confidence number in particular would become an unpreregistered
   weight on a row that has no pre-registered rule for using one.

6. **`other` is first-class, never a residual.** It appears in the same list shape as the
   lettered/numbered classes and carries a positive definition — *the incident's mechanism is not
   one of the above* — rather than a trailing escape clause.

7. **Source-identifying terms banned in both directions.** No `Wu`, `openclaw`, `arxiv`,
   `model-bridge`; no `OQ-nnn`, `ISSUES.md`, `KNOWN_STATE`, `CLAUDE.md`, `build_discipline`,
   `deferential realism`. Neither taxonomy is ever named as belonging to anyone.

## The sweep exemption these prompts require, and why it is not a loophole

A prompt necessarily contains its own direction's class definitions, so **a payload cannot be swept
clean under its own direction's full lexicon** — `direction_ii.md` contains Wu's class names, and
`iii_prime.md` contains ours. The rule the driver enforces instead:

> Sweep the **unit portion** of every payload under its own direction's full lexicon; sweep the
> **whole payload**, prompt included, under the *other* direction's `source_identifying` group only.

That is strictly stronger than sweeping unit text alone and loosens nothing: the definitions block
is fixed, reviewed, and identical across every payload in a direction, so it is auditable once
here rather than re-cleared 219 times — while the part that varies per call is swept in full.

`````

---

## Appendix C — the banned lexicons, verbatim

One matcher, two pinned versions, one module. `LEXICON_DETECT` is the live leak-grep; `LEXICON_SELECTION_20260811` is frozen at pre-declaration and widening it is prohibited. **Editing either list after the md5 below is recorded invalidates the freeze.**

`````python
#!/usr/bin/env python3
"""OQ-277 banned lexicons + leak matcher — CANONICAL, single source.

Both the redaction sweep and the payload leak-grep in the coding driver import from
here. One matcher, one pair of lists: a second copy would be a P2 fork inside the
experiment measuring P2.

Word-boundary matching is mandatory. A substring matcher fired H2 on a phantom during
this audit's own step 1 ("permission *class b*y default" matched banned "Class B"), and
H2 voids a whole direction. That false positive is a permanent selftest control below.

SEPARATOR TOLERANCE (added 2026-08-11, pre-freeze, while authoring the direction-(ii)
twins). Every multiword pattern below joins its words with `[-\\s]+`, not a literal space.
The bare-space form silently missed the hyphen-joined variant, which is the form the
source corpus actually uses at the three points that matter most:

    audits/2026-06-07_stakeholder_layer_migration/AUDIT.md:144  "Build-Discipline Pattern-1"
    audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md:22  "Build-discipline spine"
    audits/2026-06-11_oq97_pattern6_census/WRITEUP.md:1         "Pattern-6 success-shaped-absorption"

A leak-grep that misses `Pattern-1` while catching `Pattern 1` is a false-absence in the
instrument that certifies H2 — the exact defect class this experiment codes for. Measured
before and after the widening: ZERO coder-facing texts (54 unit/anchor/decoy/planted
blobs) change hit status, so this strengthens the sweep and loosens nothing. Hyphen
variants are permanent selftest controls below.

Frozen into PREREGISTRATION.md verbatim at freeze. Editing a list after the prereg md5
is recorded invalidates the freeze.

Usage:
    python3 python/audits/oq277_lexicon.py --check
    python3 python/audits/oq277_lexicon.py --sweep <units.json> --direction {i,ii}
"""
from __future__ import annotations
import argparse, json, os, re, sys, tempfile

# ---------------------------------------------------------------------------
# Direction (i): Wu's incidents, coded against OUR six. Strip WU's vocabulary.
# ---------------------------------------------------------------------------
BANNED_DIRECTION_I = {
    "class_letters": [
        r"\bClass[-\s]+[A-E]\b",
        r"\b[A-E]\s*类\b",
        r"\bclass[-\s]+[A-E]\s*[:\-—]",
    ],
    "class_names": [
        r"\benvironment(?:al)?[/\-\s]+platform[-\s]+quirk\b", r"\bplatform[-\s]+quirk\b",
        r"\bdesign[-\s]+assumption[-\s]+mismatch\b", r"\bassumption[-\s]+mismatch\b",
        r"\berror[-\s]+swallow(?:ing)?\b", r"\bswallow(?:ed|ing)[-\s]+(?:and|&)[-\s]+dilut\w+\b",
        r"\bdilution\b", r"\bdiluted\b",
        r"\bchain(?:ed)?[-\s]+hallucination\b", r"\bfabrication[-\s]+chain\b",
        r"\boperational[-\s]+omission\b", r"\bforensic[-\s]+blind[-\s]+spot\b",
        r"环境.{0,2}平台", r"设计假设错配", r"错误吞噬", r"稀释",
        r"链式幻觉", r"编造", r"运维遗漏", r"取证盲区",
    ],
    "taxonomy_terms": [
        r"\bfail[-\s]+plausible\b", r"\bgr[ae]y[-\s]+failure\b", r"\btaxonomy\b",
        r"\bsilent[-\s]+failure[-\s]+taxonomy\b", r"\bfailure[-\s]+mode(?:s)?[-\s]+catalog\b",
    ],
    "root_cause_structure": [
        r"\btrigger\b", r"\bamplifier\b", r"\bconcealer\b",
        r"触发器", r"放大器", r"掩护者",
    ],
    "rule_ids": [
        r"\bMR-\d+\b", r"\bINV-[A-Z]", r"\b元规则\b", r"\b不变式\b",
        r"\bmeta[-\s]+rule\b", r"\binvariant[-\s]+INV\b",
    ],
    # Source-identifying terms. NOT in the original design's ban list — added here on the
    # reasoning that a coder recognising the source system could recall the published
    # taxonomy from training data, which defeats the blind exactly as a class name would.
    # Flagged as an ADDITION for operator awareness rather than folded in silently.
    "source_identifying": [
        r"\bopenclaw\b", r"\bmodel[- ]bridge\b", r"\bbaileys\b",
        r"\bWu\b", r"\barxiv\b", r"2606\.14589",
    ],
}

# ---------------------------------------------------------------------------
# Direction (ii): OUR incidents, coded against WU's five. Strip the P-lexicon.
# ---------------------------------------------------------------------------
BANNED_DIRECTION_II = {
    "p_tokens": [
        r"\bP[1-6]\b", r"\bPattern[-\s]+[1-6]\b",
        r"\bthe[-\s]+(?:published[-\s]+)?six[-\s]+patterns\b",
    ],
    "pattern_names": [
        r"\bproduced[-\s]+but[-\s]+not[-\s]+consumed\b",
        r"\bone[-\s]+canonical[-\s]+thing(?:[-\s]+became[-\s]+two)?\b",
        r"\bdestructive[-\s]+replace\b",
        r"\brecap[-\s]+as[-\s]+witness\b",
        r"\babsence[-\s]+satisfies[-\s]+the[-\s]+gate\b",
        r"\bsuccess[-\s]+shaped[-\s]+absorption\b",
    ],
    "pattern_nicknames": [
        r"\bdangling[-\s]+wire\b", r"\bsilent[-\s]+fork\b", r"\bfaith[-\s]+merge\b",
    ],
    "taxonomy_phrases": [
        r"\bsuccess[-\s]+shaped\b", r"\bmeasured[-\s]+empty\b", r"\bdidn'?t[-\s]+look\b",
        r"\bauthored[-\s]+zero\b", r"\bpaste[-\s]+or[-\s]+untag\b",
        r"\bwitness[-\s]+calculus\b",
        r"\bbuild[-\s]+discipline\b", r"\bfail[-\s]+closed[-\s]+on[-\s]+absence\b",
    ],
    # R5's directional expectation must not reach a coder. Recorded as `parasitic`,
    # `cross-cutting`, `layer` — but BARE `layer` is scoped to collocations below.
    # See LAYER_SCOPING_NOTE.
    "r5_framing": [
        r"\bparasitic\b", r"\bcross[-\s]+cutting\b",
        r"\bsix[-\s]+layers\b", r"\blayer[-\s]+column\b", r"\blayer[-\s]+cut\b",
        r"\bsorts?[-\s]+by[-\s]+(?:system[-\s]+)?layer\b", r"\bat[-\s]+different[-\s]+layers\b",
        r"\blayer[-\s]+sorted\b", r"\blayer[-\s]+indexed\b",
    ],
    "source_identifying": [
        r"\bdeferential realism\b", r"\bamnesiac institution\b",
        r"\bOQ-\d+\b", r"\bISSUES\.md\b", r"\bKNOWN_STATE\b", r"\bCLAUDE\.md\b",
        r"\bbuild_discipline\b",
    ],
}

# ===========================================================================
# TWO ROLES, TWO PINNED VERSIONS, ONE MODULE (operator ruling, 2026-08-11).
# ===========================================================================
# The lists above are the DETECTOR. The dicts below are a frozen historical
# artifact used by exactly one caller, and the split is a ruling, not a style
# choice:
#
#   detection (leak-grep)  — a false positive is CONSERVATIVE. You investigate
#                            and clear it, and nothing is lost. Widening is a
#                            strict improvement, so the detector must be as wide
#                            as the evidence supports.
#   selection (density)    — a false positive is SILENTLY DECISIVE. It determines
#                            a choice, and the pre-declaration's entire value is
#                            that it was fixed BEFORE any content was seen.
#
# This is the same distinction controls/redaction_pair_selection_defect.md
# established one ruling earlier: an instrument's error profile is a property of
# its ROLE, not of the instrument. Widening is right for the detector and
# INADMISSIBLE for the selection metric — under the widened lists the declared
# rule's top-3 changes (oq97_pattern6_census 4->9 overtakes oq138 at 5), and it
# moves TOWARD the corrected set, i.e. in the direction that flatters the
# both-residue row. That the movement is convenient is exactly why it cannot be
# taken by re-declaring; see the refusal of option 3 in the ruling.
#
# *** LEXICON_SELECTION_20260811 IS FROZEN. WIDENING IT IS PROHIBITED. ***
# It is a snapshot of the lists in force when controls/redaction_pairs_predeclared.json
# was written. It exists so a past selection can be REPRODUCED, not improved.
# If a defect is found in it, that is a finding to report — never an edit to make.
# Its sole caller is controls/recheck_predeclared_counts.py.

_FROZEN_DIRECTION_I_20260811 = {
    "class_letters": [
        r"\bClass\s+[A-E]\b",
        r"\b[A-E]\s*类\b",
        r"\bclass\s+[A-E]\s*[:\-—]",
    ],
    "class_names": [
        r"\benvironment(?:al)?[/ ]platform quirk\b", r"\bplatform quirk\b",
        r"\bdesign[- ]assumption mismatch\b", r"\bassumption mismatch\b",
        r"\berror swallow(?:ing)?\b", r"\bswallow(?:ed|ing) (?:and|&) dilut\w+\b",
        r"\bdilution\b", r"\bdiluted\b",
        r"\bchain(?:ed)? hallucination\b", r"\bfabrication chain\b",
        r"\boperational omission\b", r"\bforensic blind spot\b",
        r"环境.{0,2}平台", r"设计假设错配", r"错误吞噬", r"稀释",
        r"链式幻觉", r"编造", r"运维遗漏", r"取证盲区",
    ],
    "taxonomy_terms": [
        r"\bfail[- ]plausible\b", r"\bgr[ae]y failure\b", r"\btaxonomy\b",
        r"\bsilent[- ]failure taxonomy\b", r"\bfailure mode(?:s)? catalog\b",
    ],
    "root_cause_structure": [
        r"\btrigger\b", r"\bamplifier\b", r"\bconcealer\b",
        r"触发器", r"放大器", r"掩护者",
    ],
    "rule_ids": [
        r"\bMR-\d+\b", r"\bINV-[A-Z]", r"\b元规则\b", r"\b不变式\b",
        r"\bmeta[- ]rule\b", r"\binvariant\s+INV\b",
    ],
    "source_identifying": [
        r"\bopenclaw\b", r"\bmodel[- ]bridge\b", r"\bbaileys\b",
        r"\bWu\b", r"\barxiv\b", r"2606\.14589",
    ],
}

_FROZEN_DIRECTION_II_20260811 = {
    "p_tokens": [
        r"\bP[1-6]\b", r"\bPattern\s+[1-6]\b", r"\bthe (?:published )?six patterns\b",
    ],
    "pattern_names": [
        r"\bproduced[- ]but[- ]not[- ]consumed\b",
        r"\bone[- ]canonical[- ]thing(?:[- ]became[- ]two)?\b",
        r"\bdestructive[- ]replace\b",
        r"\brecap[- ]as[- ]witness\b",
        r"\babsence satisfies the gate\b",
        r"\bsuccess[- ]shaped absorption\b",
    ],
    "pattern_nicknames": [
        r"\bdangling wire\b", r"\bsilent fork\b", r"\bfaith merge\b",
    ],
    "taxonomy_phrases": [
        r"\bsuccess[- ]shaped\b", r"\bmeasured[- ]empty\b", r"\bdidn'?t[- ]look\b",
        r"\bauthored[- ]zero\b", r"\bpaste[- ]or[- ]untag\b", r"\bwitness calculus\b",
        r"\bbuild discipline\b", r"\bfail[- ]closed on absence\b",
    ],
    "r5_framing": [
        r"\bparasitic\b", r"\bcross[- ]cutting\b",
        r"\bsix layers\b", r"\blayer column\b", r"\blayer cut\b",
        r"\bsorts? by (?:system )?layer\b", r"\bat different layers\b",
        r"\blayer[- ]sorted\b", r"\blayer[- ]indexed\b",
    ],
    "source_identifying": [
        r"\bdeferential realism\b", r"\bamnesiac institution\b",
        r"\bOQ-\d+\b", r"\bISSUES\.md\b", r"\bKNOWN_STATE\b", r"\bCLAUDE\.md\b",
        r"\bbuild_discipline\b",
    ],
}

#: The live detector. Every leak-grep, redaction sweep and payload check uses this.
LEXICON_DETECT = {"i": BANNED_DIRECTION_I, "ii": BANNED_DIRECTION_II}

#: FROZEN. Reproduces the pre-declared selection. Do not widen. One caller only.
LEXICON_SELECTION_20260811 = {"i": _FROZEN_DIRECTION_I_20260811,
                             "ii": _FROZEN_DIRECTION_II_20260811}

LAYER_SCOPING_NOTE = """\
R5 records the banned terms as `parasitic`, `cross-cutting`, and `layer`. BARE `layer` is
deliberately NOT banned, and the deviation is measured rather than argued:

  in the 22 sampled direction-(ii) source dirs (728 KB, 89 files)
    bare "layer"/"layers"          48 hits in 27/89 files
    taxonomy-framing collocations   0 hits in  0/89 files

Banning the bare word would fire on a third of the source files while catching zero real
leaks, and it would strip vocabulary that real mechanisms need ("three layers each
discarded part of the cause" describes an incident, it does not hint at our taxonomy).
Over-redaction destroys codeability and biases units toward `other` — the exact bias
control (c) exists to measure — so an over-broad ban would corrupt the control meant to
catch it. The collocations carry R5's actual hint (that our six sort by system layer) at
zero measured cost. This scoping is frozen with the lexicon and stated in the prereg.
"""

# Shared subject matter. Belongs to NEITHER taxonomy; stripping it destroys codeability.
# The selftest asserts each of these is unmatched by BOTH direction lists, which makes
# "we did not strip shared vocabulary" a checked fact rather than an intention.
PRESERVED = [
    "silent", "silently", "never fired", "green", "empty", "absent", "absence",
    "unknown", "gate", "witness", "control", "count", "zero", "aggregate",
    "layer", "layers", "stale", "fallback", "no error", "reported success",
]

#: Back-compatible alias. `LISTS` has always meant the live detector, and it still does —
#: every existing caller keeps the detection role without changing a line.
LISTS = LEXICON_DETECT
CODER_FACING_FIELDS = ("symptom", "mechanism_as_described", "detection_path", "consequence")


def scan(text: str, direction: str, lexicon: dict | None = None):
    """Return [(group, pattern, matched_text, context)] for every banned hit.

    `lexicon` defaults to LEXICON_DETECT. The ONLY legitimate other value is
    LEXICON_SELECTION_20260811, passed by controls/recheck_predeclared_counts.py to
    reproduce a pre-declared selection. Detection must never be run under the frozen
    lists — it is the version with the known hyphen false-negative.
    """
    hits = []
    for group, pats in (lexicon or LEXICON_DETECT)[direction].items():
        for pat in pats:
            for m in re.finditer(pat, text, re.I):
                hits.append((group, pat, m.group(0),
                             text[max(0, m.start() - 40):m.end() + 40].replace("\n", " ")))
    return hits


class UnitsFormatError(Exception):
    """A units file whose shape cannot be resolved. Raised rather than swept as empty."""


def load_units(path: str):
    """Resolve a units file to a list of unit objects. THREE shapes are accepted.

    1. a JSON list of unit objects
    2. a dict wrapper carrying them under "units"
    3. a SINGLE unit object (a bare dict, no "units" key)

    Shape 3 is why this function exists. The original test was
    `data["units"] if isinstance(data, dict) else data`, and a single unit object IS a
    dict, so a one-unit file took the wrapper branch and died on KeyError: 'units'. Two
    extractors were told to sweep single-object files by a brief that specified this
    exact call, and both hit it (OQ-277, 2026-08-11).

    Fail-closed: a dict that is neither a wrapper nor a recognisable unit raises rather
    than resolving to []. Sweeping zero units and reporting "0 hits" is the shape this
    whole experiment is about.
    """
    if isinstance(data := json.load(open(path)), list):
        return data
    if not isinstance(data, dict):
        raise UnitsFormatError(f"{path}: top level is {type(data).__name__}, expected list or object")
    if "units" in data:
        if not isinstance(data["units"], list):
            raise UnitsFormatError(f"{path}: 'units' is {type(data['units']).__name__}, expected list")
        return data["units"]
    if any(f in data for f in CODER_FACING_FIELDS) or "id" in data:
        return [data]                      # shape 3 — a single unit object
    raise UnitsFormatError(
        f"{path}: object has no 'units' key and no coder-facing field "
        f"({', '.join(CODER_FACING_FIELDS)}) or 'id' — cannot tell a wrapper from a unit. "
        f"Refusing to sweep 0 units.")


def scan_units(path: str, direction: str):
    """Sweep the coder-facing fields of a units file. Returns [(unit_id, field, *hit)]."""
    units = load_units(path)
    out = []
    for u in units:
        for f in CODER_FACING_FIELDS:
            for h in scan(u.get(f, ""), direction):
                out.append((u.get("id", "?"), f, *h))
    return out


def selftest() -> bool:
    ok = True

    def check(label, cond):
        nonlocal ok
        print(f"  {'PASS' if cond else 'FAIL'}  {label}")
        ok = ok and cond

    print("positive controls — a planted leak MUST be caught:")
    for direction, probes in {
        "i": ["This was a Class B failure.", "a classic fail-plausible case",
              "see MR-12 and INV-ONTOLOGY-001", "the concealer was the status file",
              "错误吞噬 与 稀释", "logged in the openclaw repo"],
        "ii": ["this is a P6 instance", "textbook success-shaped absorption",
               "the dangling wire again", "Pattern 5 fired here",
               "P6 is parasitic on the others", "our six sort by system layer",
               "tracked as OQ-97"],
    }.items():
        for probe in probes:
            check(f"dir({direction}) catches {probe!r}", bool(scan(probe, direction)))

    # Separator-tolerance controls (2026-08-11). Each string below is the HYPHEN-JOINED
    # form of a pattern the lists previously expressed with a literal space, and each is
    # drawn from prose that really exists in the source corpus. Before the widening every
    # one of these swept CLEAN — a false absence in the instrument that certifies H2.
    print("\nseparator-tolerance controls — hyphen-joined variants MUST be caught:")
    for direction, probes in {
        "i": ["a Class-B failure", "the fail-plausible rows", "a forensic-blind-spot case",
              "an operational-omission incident", "the failure-modes-catalog rows"],
        "ii": ["Build-Discipline Pattern-1 dangling wire", "Build-discipline spine, twice over",
               "Pattern-6 success-shaped-absorption census", "One-canonical-thing (Pattern-2)",
               "a recap-as-witness substitution", "the absence-satisfies-the-gate shape"],
    }.items():
        for probe in probes:
            check(f"dir({direction}) catches hyphenated {probe!r}", bool(scan(probe, direction)))

    # The two pinned versions must be DIFFERENT, and different in the declared
    # direction. Without this, "we pinned the selection metric" is a comment rather
    # than a fact, and a copy-paste that made the frozen lists identical to the
    # widened ones would look exactly like a correct pin.
    print("\nrole-pinning controls — two versions, each in its declared role:")
    check("the two lexicons are not the same object",
          LEXICON_DETECT is not LEXICON_SELECTION_20260811)
    check("frozen selection lexicon is genuinely DIFFERENT from the detector",
          LEXICON_SELECTION_20260811["ii"] != LEXICON_DETECT["ii"])
    # Probes must ISOLATE the hyphenated token. The full source phrases
    # ("Build-Discipline Pattern-1 dangling wire") also contain vocabulary the frozen
    # list catches for other reasons — `dangling wire`, `success-shaped` — so asserting
    # on them would test nothing about separator handling and would pass either way.
    for probe in ("Pattern-1", "Pattern-6", "Build-discipline"):
        check(f"DETECT catches isolated {probe!r}", bool(scan(probe, "ii")))
        check(f"FROZEN does NOT catch isolated {probe!r} (its known false-negative, "
              f"preserved on purpose so a past selection reproduces)",
              not scan(probe, "ii", LEXICON_SELECTION_20260811))
    # ...and the widening must not have touched anything else: on the full source
    # phrases both versions fire, because both carry the non-hyphenated vocabulary.
    for probe in ("Build-Discipline Pattern-1 dangling wire",
                  "Pattern-6 success-shaped-absorption census"):
        check(f"both versions fire on the full phrase {probe!r} (differing only in WHY)",
              bool(scan(probe, "ii")) and bool(scan(probe, "ii", LEXICON_SELECTION_20260811)))
    for d in ("i", "ii"):
        nd = sum(len(v) for v in LEXICON_DETECT[d].values())
        nf = sum(len(v) for v in LEXICON_SELECTION_20260811[d].values())
        check(f"dir({d}) frozen is a SNAPSHOT not a truncation — same pattern count "
              f"({nf} == {nd}), separators only", nd == nf)

    print("\nfalse-positive controls — legitimate text must NOT be flagged:")
    check("dir(i) ignores 'permission class by default' (the H2 phantom)",
          not scan("permission class by default", "i"))
    check("dir(ii) ignores bare 'three layers each discarded part of the cause'",
          not scan("three layers each discarded part of the cause", "ii"))
    check("dir(ii) ignores 'the reporting layer was silent'",
          not scan("the reporting layer was silent", "ii"))
    check("dir(i) ignores 'the backup failed silently for six days'",
          not scan("the backup failed silently for six days", "i"))

    print("\nshared-vocabulary controls — PRESERVED terms unmatched by BOTH lists:")
    for term in PRESERVED:
        check(f"{term!r} survives both", not scan(term, "i") and not scan(term, "ii"))

    print("\nmatcher-integrity control — a matcher that never fires must fail this:")
    check("scan() is capable of returning hits at all",
          len(scan("Class A fail-plausible MR-4", "i")) >= 3)

    # ---- input-shape controls (added 2026-08-11 after the second receiver hit the
    # single-object KeyError). These go through a REAL file and the real json.load, not
    # a dict handed straight to the normaliser: the defect was on the file path, and a
    # control that skips the path it is protecting witnesses nothing.
    print("\ninput-shape controls — all three accepted shapes, on the real file path:")
    unit_clean = {"id": "shape-probe", "symptom": "a value was read as measured.",
                  "mechanism_as_described": "an empty collection acquired a plausible default.",
                  "detection_path": "two metrics disagreed over one input.",
                  "consequence": "the reading stood for its whole life."}
    unit_leaky = dict(unit_clean, id="shape-probe-leak",
                      symptom="this is a P6 instance, textbook success-shaped absorption.")
    with tempfile.TemporaryDirectory() as td:
        def written(name, obj):
            p = os.path.join(td, name)
            with open(p, "w") as fh:
                json.dump(obj, fh)
            return p

        def check_call(label, fn, want):
            """FAIL on a raised exception instead of dying. A selftest that aborts
            partway is the same crash-vs-result confusion this block exists to fix —
            the run ends, the remaining cases never report, and the exit code is
            shared with an ordinary RED."""
            try:
                check(label, want(fn()))
            except Exception as exc:                                      # noqa: BLE001
                check(f"{label}  [raised {type(exc).__name__}]", False)

        single, single_leak = written("single.json", unit_clean), written("leak.json", unit_leaky)
        as_list, wrapped = written("list.json", [unit_clean]), written("wrap.json", {"units": [unit_clean]})
        junk = written("junk.json", {"note": "no units key, no coder-facing field"})

        # (a) it CONSUMES a single object rather than raising — the reported defect
        try:
            n_single, raised = len(load_units(single)), None
        except Exception as exc:                                          # noqa: BLE001
            n_single, raised = -1, exc
        check("single unit OBJECT file is consumed, not KeyError", raised is None)
        check("single unit object resolves to exactly 1 unit", n_single == 1)

        # (b) and the sweep over it actually LOOKS — "consumed" must not mean "swept nothing".
        #     Without this pair, a fix that returned [] would pass (a) and be worse than
        #     the crash it replaced.
        check_call("planted leak in a single-object file IS caught",
                   lambda: scan_units(single_leak, "ii"), lambda h: len(h) > 0)
        check_call("clean single-object file yields no hits",
                   lambda: scan_units(single, "ii"), lambda h: not h)

        # (c) the two pre-existing shapes are unchanged
        check_call("list form still resolves", lambda: load_units(as_list), lambda u: len(u) == 1)
        check_call("{'units': [...]} wrapper still resolves",
                   lambda: load_units(wrapped), lambda u: len(u) == 1)

        # (d) fail-closed: an unrecognisable object must RAISE, never sweep 0 units
        try:
            load_units(junk); junk_ok = False
        except UnitsFormatError:
            junk_ok = True
        except Exception:                                                 # noqa: BLE001
            junk_ok = False
        check("unrecognisable object RAISES rather than sweeping 0 units", junk_ok)
    return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true", help="run the selftest")
    ap.add_argument("--sweep", metavar="UNITS_JSON")
    ap.add_argument("--direction", choices=["i", "ii"])
    ap.add_argument("--note", action="store_true", help="print the layer-scoping note")
    a = ap.parse_args()
    if a.note:
        print(LAYER_SCOPING_NOTE)
        return 0
    if a.check:
        print("OQ-277 lexicon selftest\n")
        good = selftest()
        print(f"\n{'GREEN — every control fired as pre-registered' if good else 'RED'}")
        return 0 if good else 1
    if a.sweep:
        if not a.direction:
            print("--sweep requires --direction", file=sys.stderr)
            return 2
        units = load_units(a.sweep)
        hits = scan_units(a.sweep, a.direction)
        for uid, field, group, pat, txt, ctx in hits:
            print(f"  LEAK {uid}.{field}  [{group}] {pat} -> {txt!r}\n       ...{ctx}...")
        print(f"\nswept {len(units)} units x {len(CODER_FACING_FIELDS)} fields, "
              f"direction ({a.direction}): {len(hits)} hits")
        return 1 if hits else 0
    ap.print_help()
    return 2


# Exit codes are part of this tool's interface and a caller MAY branch on them.
#   0  swept, no hits          2  usage error
#   1  swept, HITS FOUND       3  did not sweep — aborted before producing a verdict
#
# 3 exists because 1 used to double as "leaks found" and "crashed on load", and the
# crash printed no LEAK lines — so a wrapper reading stdout for leaks saw a clean sweep
# with a failure exit. A crash and a leak were indistinguishable at the interface, and
# the crash produced the QUIETER of the two outputs. The stdout marker below is
# deliberate: a caller that greps stdout and never reads stderr must still see it.
if __name__ == "__main__":
    try:
        sys.exit(main())
    except SystemExit:
        raise
    except BaseException as exc:                      # noqa: BLE001 — deliberate catch-all
        print("SWEEP-ABORTED — no verdict was produced. This is NOT a clean sweep.")
        print(f"  {type(exc).__name__}: {exc}", file=sys.stderr)
        import traceback; traceback.print_exc(file=sys.stderr)
        sys.exit(3)

`````

---

## Appendix D — `verdict_grammar_amendment.md`, incorporated VERBATIM

Binding pre-registration content in its entirety, including §Q and the two entries added 2026-08-11 (§L.4 — a ruling made on evidence that had not been gathered; §L.5 — the second matcher defect and the role split). Inlined by `oq277_build_prereg.py` from the canonical file and asserted byte-identical to it by `--check`, so this copy cannot drift from its source.

`````
# Verdict grammar amendment + extension conditional (operator, 2026-08-10)

**Status: BINDING pre-registration content.** `PREREGISTRATION.md` incorporates this file
**verbatim** when it is frozen; this file is the canonical location and the prereg does not restate
its thresholds. Written before any model call, before direction-(ii) extraction, and before any
matrix exists.

**Why it exists.** The secondary-class observation (`packets/wu_source/observation_secondary_class_predicts_disagreement.md`)
threatens the pre-registered verdict grammar. If Wu's 10 disagreements trace **genuine multi-class
membership** rather than coding instability, then the frozen rule — *expressible iff ≥2/3 of a
class's unanimous members land in ONE pattern; a two-pattern split is "partial"* — misreads its
most interesting outcome. A class splitting across two of our patterns could mean the mapping is
correctly **one-to-two**, not that expressibility is partial. Under multi-membership, "partial"
conflates a true structural mapping with coder instability, and those need opposite readings.

---

## A. Stratum re-declaration

**The selection rule does NOT change and remains frozen:** the stratum is the units whose catalog
class equals their dataset class, computed mechanically from the two md5-pinned files before any
coding (12 units; the complement is 10). Nothing about *which units are in it* moves. What changes
is the **interpretation** of the stratum and the **grammar applied to each side of it**.

Two declared readings of what the stratum is. Both are recorded; neither is asserted:

| reading | claim | status |
|---|---|---|
| **(i) codeability artifact** | agreement selects incidents both sources found *easy to classify* | the original declared cost, unchanged |
| **(ii) single-class** | agreement selects incidents that are *structurally single-class* | supported by an OBSERVATION with three live defeaters — **not a finding** |

**Status inheritance, stated so it cannot be laundered.** Reading (ii) is why the primary read
moves to the stratum, and reading (ii) rests on a non-blind, post-hoc, n=22 observation. **This
amendment does not upgrade that observation to a finding, and no writeup sentence may cite the
amendment as evidence for it.** The dependency runs one way only.

**Pre-registered consequence if reading (ii) is later disconfirmed** (by the blind test specified
in the observation file, or otherwise): the stratum reverts to "merely easier," the primary
expressibility read on it becomes a codeability-biased number, and **the headline must move back
to the full 22**. Recorded now so that reversal is a pre-committed move rather than a judgement
call made by whoever holds the pen at the time.

**The declared cost, sharpened rather than retired.** The earlier freeze said the stratum's higher
expressibility is partly a codeability artifact and is "never the cleaner number." That still
holds, and gains a second edge: on reading (ii) the stratum is easier **because those incidents are
structurally simpler**, not only because they are better written. So the pre-registered error is
now stated in both directions:

- the stratum's expressibility figure may **not** be presented as *the* expressibility of the
  taxonomy, and
- a full-22 headline may **not** be derived from it.

**This supersedes one earlier ruling and the supersession is named.** `RECON.md` §R2 point 3 froze
the stratum's use as "narrow: a check that a full-22 verdict is not being *driven* by the ambiguous
10 — direction of robustness, never a headline." The operator has now promoted the stratum to
carry the **primary** expressibility read. A cold reader meeting both texts should read this one as
current. The narrow-use clause is not deleted, it is *replaced*: robustness now runs the other way,
with the full 22 reported as the robustness read.

---

## B. Verdict grammar, per stratum

### Single-class stratum (the 12) — PRIMARY expressibility read

| verdict | rule |
|---|---|
| **expressible** | ≥2/3 of the class's unanimous members land in ONE pattern |
| **partial** | unanimous members land in exactly two patterns — **see §C, this is the ambiguous row** |
| **inexpressible** | ≥1/3 of unanimous members land in `other` |

### Multi-membership stratum (the 10) — a two-pattern split is EXPECTED, not degraded

| verdict | rule | reading |
|---|---|---|
| **one-to-two (expected)** | unanimous members land in exactly two patterns | the image of Wu's own dual membership; **not** a degraded result |
| **collapse** | unanimous members land in ONE pattern | **informative in its own right**: our six do not resolve a distinction Wu's two records disagree about |
| **diffuse** | three or more patterns, or ≥1/3 to `other` | no coherent mapping |

`collapse` is pre-registered as a first-class outcome precisely because it is the one that would
otherwise be reported as a *success* ("the class is expressible!") while meaning something closer
to the opposite.

---

## C. The two readings of "partial", named

| name | claim | signature |
|---|---|---|
| **R-map** | Wu's class genuinely instantiates **two** of our patterns; the mapping is one-to-two | split is *between* units, each unit individually stable |
| **R-churn** | the coder could not settle; the split reflects measurement, not structure | split is accompanied by units failing to reach unanimity |

Note that the k=3 unanimity rule already removes *within-unit* instability — a non-unanimous unit
goes to the UNSTABLE row and never enters a cell. So a two-pattern split **among unanimous
members** is already partial evidence for R-map. The tiebreaker below makes that explicit and
measurable rather than leaving it as an inference.

## D. Tiebreaker, fixed now

**Primary tiebreaker — class-level UNSTABLE rate against the direction's overall UNSTABLE rate:**

- **R-map favoured** if the class's UNSTABLE rate is **≤** the direction's overall UNSTABLE rate.
  The units are individually as stable as anything else in the run; the split is between units.
- **R-churn favoured** if the class's UNSTABLE rate is **>** the direction's overall rate. The
  class is where the coder wobbles, and the split is a symptom of that.
- **UNINFORMATIVE — no tiebreak, row ships typed OPEN** — if the class has **fewer than 4 unanimous
  members**. Declared in advance, same shape as R5's uninformative branch, so a thin split is never
  read as a mapping.

**Secondary, corroborating only, explicitly NOT decisive:** which stratum the split appears in — a
split inside the single-class stratum leans R-churn, inside the multi-membership stratum leans
R-map. It is non-decisive **because the stratum's meaning rests on the observation**, and letting
it decide would close the loop from observation to verdict without the blind test.

**Any R-map verdict ships as PROPOSED Ω_C**, per this OQ's own Ω-type declaration (mapping
semantics are Ω_C). It must name **which two patterns**, and it awaits an operator ruling. R-map is
never emitted as a finding by the assembler.

---

## E. Extension conditional — the H5-gate decision is a measurement, not a scope argument

The pinned rule ("the extension changes n and NOTHING else") makes the volume problem look
definitional: at 5,176 KB the full 73 needs a different extraction protocol, and a different
protocol is not "n and nothing else," so the extension would be a new experiment by fiat.

**The overlap units make it testable instead.** If two independent extractors, working the same
source directory, produce units that code the same, then a multi-extractor protocol is
*demonstrably equivalent* to a single-extractor one — the extension then changes n **in effect**,
which is what the rule protects. If they do not, extraction variance is a live confound and the
extension needs its own pre-registration regardless of volume.

**Measurement.** 4 overlap units (2 per extractor), direction (ii) only, each extracted
independently twice and each extraction coded at k=3. Per unit:

- both extractions unanimous and **equal** → **agree**
- both unanimous and **different** → **FLIP**
- either extraction UNSTABLE → **uninformative** (recorded, not counted as agreement)

**Thresholds, fixed now:**

| outcome | condition | consequence at the H5 gate |
|---|---|---|
| **INSIDE FLOOR** | 4/4 agree | extension may be priced as changes-n-only |
| **LIVE** | ≥2 FLIPs | extraction variance is a confound; extension needs its own prereg regardless of volume |
| **INDETERMINATE** | 1 FLIP, or ≥2 uninformative | **fail-closed to "needs its own prereg"** |

The middle band fails closed deliberately: absence of resolution must not license the cheaper path
(Build Discipline Pattern 5 — a gate that passes because its input is missing).

**MDE, declared.** 4 units gives **25% resolution**. This can distinguish *no* extraction churn
from *substantial* extraction churn and nothing finer; a single flip is already the indeterminate
band. Raising overlap to 3 per extractor (6 units, 17%) would buy one more discrimination step —
**not adopted this run**, flagged so the resolution is a known limit rather than a discovered one.

**Scope limit.** This floor is measured on **direction (ii) only**. Direction (i) was extracted
whole by a single extractor, so the floor does **not** license any claim about direction-(i)
extraction variance. Any writeup sentence generalising it across directions is a pre-registered
error.

**Accounting.** Overlap units are quarantined from all matrices but their calls **do** count toward
the driver's expected payload-capture count (the Phase-3 standing check compares captured payloads
against expected calls, and a quarantined call is still a call).

---

## F. Training-exposure leak — the source-identifying ban belongs to the registered confound

The source-identifying terms banned in both directions (`openclaw` / `Wu` / `arxiv` one way,
`OQ-\d+` / `CLAUDE.md` / `deferential realism` the other) are **not a new design element and need
no separate ruling.** They are the direct consequence of the confound already registered.

The same-family confound says: our writeups are Claude-authored and the coder is Claude, so
direction (ii) agreement may be inflated by prose-convention familiarity. The identical mechanism
runs one level deeper — **a Claude coder that recognises Wu's paper can recall his five classes
from training rather than reasoning from the definitions in its prompt.** That is not a leak
*through the payload*, which the grep catches; it is a leak *through the weights*, which nothing
else in this design catches. Filed under the existing confound accordingly.

**The ban reduces the channel and does not close it, stated plainly so nobody reads it as
discharged.** If the model recognises the *incidents themselves* — a production agent runtime
with a distinctive fingerprint, a 60-day sandbox denial, a specific reserved-file self-silencing —
no amount of vocabulary stripping helps. Redaction operates on names; recognition operates on
situations. A clean leak-grep is therefore evidence about the payload channel **only**, and no
writeup sentence may promote it to evidence about training exposure.

**The falsifier is unchanged and now does double duty.** The named tier falsifier for the
same-family confound is a **different-family model re-code**. It falsifies *both* channels at once
— prose familiarity and training exposure — because a different family has neither our conventions
nor, plausibly, the same exposure to Wu's artifacts. That raises its value against its cost, and
the writeup should say so when it records the falsifier as named-but-not-bought this run.

## G. Writeup obligations added by this amendment

1. **`movespeed_tcc_sandbox` is a worked P2 instance inside Wu's own artifact.** One canonical
   labeling became two; no queryable fact says which governs; and the row's own `paper_class_ref`
   contradicts its own `taxonomy_class`. It is the paper's §5.1 headline incident. **Both
   taxonomies are forked** — ours between `CLAUDE.md` and `build_discipline.md` (OQ-278), his
   between catalog and dataset — **both forks were found by an outsider, and neither author
   noticed his own.** The corpus in which our pattern is instantiated has no stake in either
   taxonomy, which is what makes it evidence rather than self-application.

2. **Three dated instrument defects from one arc, written up ONCE as a set, not three times.**
   §6.4's recursion — controls need controls — is currently argued from a *hypothetical* no-op
   harness. Three instances from a single pre-spend arc is a stronger section than the argument it
   replaces, and the set has one shared property that no individual instance shows:

   | # | instrument | what it returned | the wrong object it measured |
   |---|---|---|---|
   | 1 | frame-census control, v2 | assertion passed | asserted every extracted name ends in `.md` — **encoding the very assumption the control existed to test**; false for a nested hit, which yields a subdirectory name |
   | 2 | secondary-class predictor, first pass | **6** rows (truth: 8) | regex over structured YAML under-read a multi-line field — **and 6 agreed with the conclusion 8 supports** |
   | 3 | de-blocking witness | baseline max-run **2** (truth: 8) | `sum(1 for _ in g)` over `groupby` counted the `(key, grouper)` **tuple** — reported a fully blocked baseline as already unblocked |

   **The shared property: each returned a well-formed, plausible number about the wrong object,
   from inside the witness for the claim it supported.** Not one produced an error, an exception,
   an implausible value, or a result pointing the wrong way. #2 is the sharpest — it is §6.3's
   `identical: True because both empty` **inverted**: there, two failed measurements agreed with
   each other; here a **failed measurement agreed with a sound one**, so the agreement was actively
   *reassuring*. The error had no signal anywhere in the loop: not in the output, not in the
   direction, not in the plausibility.

   **The honest limit on detection, recorded beside them.** *All three were caught by hand-checking
   a number that looked fine — none by any control.* The control architecture this project runs
   caught zero of the three defects that occurred inside its own instruments. That is the most
   uncomfortable available datum about the apparatus and it belongs in §6.4 with the instances,
   not softened. It also sharpens what the `Fired:` bit can and cannot measure (OQ-276): a catch
   rate computed over controls does not see catches that arrive by suspicion.

---

## H. NO-UNIT row — the census proxy's PRECISION, and the boundary rule that makes it countable

**The hole, named as the operator's own (2026-08-11).** The escape check audits the §4.5 keyword
proxy's **recall** — directories it missed. **Nothing audits its precision** — directories it
admitted whose keywords matched something other than a reported incident. A proxy has two error
rates; one had an instrument and one did not.

**Asymmetric cost, which is why this is not a footnote.** A confirmed escape-check hit relabels
42% as a *lower bound* — a bounded correction in a known direction. NO-UNIT directories attack the
**numerator** directly and can move the point estimate either way depending on how the two rates
compare. **So 42% is currently a figure with one measured error direction and one unmeasured one,
and the writeup must say exactly that rather than reporting the escape check as though it closed
the question.**

### H.1 Boundary rule — fixed NOW, before the remaining units are extracted

A category that will be counted needs its boundary fixed before its members arrive; deciding
per-directory as they show up is how a category silently acquires an extractor's preference — the
same failure the multi-defect directories are already documenting.

> **A directory yields a UNIT if its prose REPORTS a silent-defect incident, anywhere in the
> document, regardless of whether that incident is the directory's subject. A directory is
> NO-UNIT only if its prose DISCUSSES the concept without reporting an instance.**

**Why this line and not "is the incident the subject?"** Because it is the line the census itself
draws. §4.5's numerator counts *incident-bearing directories* by searching **prose**; the claim
being audited is that the prose describes an incident, not that the directory's topic is one.
Adopting a stricter subject-level rule would measure a different quantity than the figure under
audit and the correction would not apply to it.

**Consequence for `2026-06-15_oq131_six_observer`: it is a UNIT**, and its unit stands. Its
incident (a measurement arm silently running against a 57-item substrate where the working set
held 64) is *reported*, not merely discussed; it simply sits in a self-audit subsection rather
than in the headline.

**New mandatory field, applied retroactively to all units: `incident_location`** — `subject` |
`self_audit_subsection` | `incidental`. This costs nothing and buys a second measurement free: if
the 73 are heavily weighted toward `self_audit_subsection`, the numerator counts directories that
noticed their own incidental defects, which supports a *weaker* claim than "73 audits were about
incidents." Report the distribution; do not collapse it.

### H.2 Pre-registered row and thresholds, fixed before the remaining six are extracted

Let **k** = operator-confirmed NO-UNIT directories in the primary sample (n=22).

| outcome | condition | licensed reading |
|---|---|---|
| **null** | k = 0 | **ONLY** "keyword-proxy false-positive rate ≤ **12.7%** at 95% confidence" (1 − 0.05^(1/22)). **NEVER** "the proxy is precise." Deliberately phrased to mirror the escape check's one-sided licence word for word. |
| **fired** | k ≥ 1 | the 73 numerator is an **OVERCOUNT**. Report the corrected figure as a RANGE combining this precision bound with the escape check's recall bound — never as a new point estimate — and mint a re-census OQ, mirroring the escape check's confirmed-hit consequence. |

**Operator confirmation is required per NO-UNIT, exactly as escape-check hits are quarantined.**
The extractor proposing NO-UNIT is the same party whose *thin extraction* is the competing
explanation for finding no incident. Self-certification here would let an extractor convert a hard
directory into a data point. The coder proposes, the operator rules — same rule, opposite
direction.

### H.3 Provenance belongs in the row

State in the row itself: **recall and precision were audited at different times, by different
mechanisms, and the precision audit arrived by accident** — an extractor noticing that one
directory nearly yielded nothing. It was not designed, pre-registered, or budgeted.

That provenance is not a confession, it is the argument: **the census needed a positive control it
never had.** A census whose output is a count of directories cannot distinguish "matched an
incident" from "matched the word" without one — the identical structure the frame control found
from the other direction, where "174 = 73 + 101, partition exact" is an arithmetic identity that
witnesses nothing about whether the census *classifies* correctly. Two independent routes to the
same missing control, one from the denominator and one from the numerator.

---

# Addendum (operator, 2026-08-11) — overlap accounting, and the channel nobody modelled

Written after extractor A's half closed at 13/13, before extractor B started. Incorporated into
`PREREGISTRATION.md` verbatim with the rest of this file.

## I. Overlap accounting — `role` stops encoding two facts

**Ruling: A's 01 and 06 STAY matrix units.** All thirteen of A's units carry `role: "primary"`;
the overlap relationship moves to its own boolean, `overlap_source`, true on the four
floor-participating directories (`2025-05-15_recon_2`, `2026-06-11_oq44_policy_close`,
`2026-06-27_oq124_oq149_committer_convention_control`, `2026-07-11_oq186_oq188_readsite`).
**The driver quarantines on `matrix_unit`, never on `role`** — see §I.2, which corrects this
clause as originally written.

**Why the field split, stated as the defect it removes.** `role` was being asked to encode two
independent facts: whether a unit enters the matrices, and whether it participates in a floor
comparison. Matrix membership is a **sampling** fact, fixed by the seeded draw; floor
participation is a **control** fact, fixed by the overlap design. They vary independently, and one
string cannot say both without ambiguity — which is exactly what produced the inconsistency
(`01` labelled `primary+overlap`, `06` labelled `primary`, both the same kind of thing).

**Why not exclude them.** Excluding the four would drop seeded-sample units from a stratum already
thin at n=22, to protect against a contamination that does not exist. A's extraction of `01` is
not contaminated by B's later independent extraction of the same directory. **The comparison is
quarantined, not the units** — and it is B's versions of the four overlap directories that never
enter cells.

**Cell accounting, so it cannot drift:** each of the 22 sampled directories contributes exactly
ONE unit to the matrices. A contributes 13, B contributes 9, and B's four
overlap-directory extractions are floor-only. 13 + 9 = 22.

### I.1 Pre-registered limit on what the floor licenses

**Four units now have an independently measured extraction error bar. The other nine do not.**
If the floor comes out non-trivial, those four are the only units whose extraction quality is
known.

> **Pre-registered NOW, before the floor is measured:** the four overlap units may NOT be treated
> as representative of extraction quality generally. Any statement about the reliability of the
> unit set is scoped to the four measured directories unless a separate argument for
> representativeness is made and stated. **Four measured units do not license a claim about
> thirteen, or about twenty-two.**

Stated here rather than at close because a limit written after seeing the floor is a limit chosen
against a known result. If the floor is trivial (4/4 INSIDE FLOOR), this clause costs nothing; if
it is not, this clause is the difference between a scoped finding and an implicit claim.

## J. Writeup obligation — the memory system is a leak channel

**The catch.** A commit message landing the two overlap units summarized both incidents in one
line each. Those two directories are extractor B's PRIMARY assignments. A `git log` read by B
would have anchored B's independent extraction on A's selection, converting the extraction-churn
floor into a self-comparison that reports INSIDE FLOOR by construction — and §E rides the
extension decision on that number. Caught by the author before anything was pushed; the message
was amended to state, in place of the content, why the content is absent.

**Why it generalizes, and why it is the sharpest catch of the arc.** Every leak control in this
design targets what reaches the **coder**: payload greps, banned lexicons, redaction, the planted
leak. This one reached the **extractor**, through **version control** — infrastructure the design
treats as neutral substrate rather than as a channel. In an institution whose memory IS a git
repository, the memory system is itself a leak channel, and a blinding protocol has to name it
explicitly, because it is the one surface everyone reads and nobody registers as an input.

**Carry all three surfaces to the writeup, not just the commit message:** `git log`, `git show`
on the landing commits (which prints the unit bodies verbatim), and any sweep that globs the unit
directory and prints what it loads.

**Second-order note for the same section.** This is the second time in this arc that a leak
response was **fixed in advance rather than negotiated at discovery** — the first being the
quarantine rule for escape-check hits. The pre-committed response here is *declare it and VOID
that pair's floor comparison*, never patch and continue: a voided comparison is recoverable, a
silently contaminated one licenses a scope decision on a fabricated basis.

## K. Second directional guess declined by the data

`incident_location` over A's 13: **subject 10 / incidental 2 / self_audit_subsection 1.**

The operator's reading at the interim boundary (A's first 7, which stood at 6 subject / 1
self-audit) raised the possibility that the numerator is weighted toward directories that merely
noticed their own incidental defects — which would support a weaker claim than "73 audits were
about incidents" (§H.1). **The completed half does not show that skew**, and the reading is
recorded as **not held**.

Recorded the same way the pre-registered C/D disagreement guess was recorded as wrong (`RECON.md`
§R2a). **This is the second directional guess of this arc that the data declined.** Both are kept
in the record at the same volume as the confirmations; a design that only remembers its correct
priors is measuring its own memory.


### I.2 Correction to §I, found when B's half landed — `overlap_source` cannot answer the driver's question

§I as written said the driver quarantines on `overlap_source`. **Measured, that yields 18 cells,
not 22:**

```
quarantine on overlap_source alone -> 18 cells  (RULING SAYS 22)
quarantine on matrix_unit         -> 22 cells   (A 13 + B 9)
```

`overlap_source` is a property of the **directory** (is it floor-participating?), so BOTH
extractions of each overlap directory carry it — 8 units, not 4. Quarantining on it drops the
A-side extraction too, and the four overlap directories vanish from the matrices entirely: exactly
the outcome §I ruled against, arrived at by following §I.

**This is the same defect §I diagnosed, one level down.** A boolean was again being asked a
question it cannot answer, because there is a THIRD independent fact: not "does this directory
participate in a floor comparison" (control) and not "was this directory sampled" (sampling), but
**"of the two extractions of this directory, is this the one that enters cells."**

**Fix, implementing the ruling rather than extending it:** a third explicit field, `matrix_unit`,
on all 26 units. `true` for all 13 of A's and for B's 9 non-overlap extractions; `false` for B's
four overlap-directory extractions — which is precisely §I's "B's versions are the ones that never
enter cells," made machine-checkable instead of left as a rule a driver author has to remember.
Checked property, not an intention: **every one of the 22 sampled directories contributes exactly
one cell unit** (verified `set(counts)=={1}` over the 22).

**Operator: overrule if you would rather redefine `overlap_source` to mean the extraction-level
fact instead of adding a field.** Both work; a third field was chosen because collapsing the
directory-level and extraction-level facts back into one boolean is what produced this.

### I.3 Correction to two stated counts

- **A's units carrying `alternatives_not_extracted` is 8 of 13, not 6.** The 6 was written into
  `HANDOFF_EXTRACTOR_B.md` and this OQ's ISSUES entry by the A2 instance, which counted only the
  six it extracted itself and omitted the two recorded by the first A instance before the handoff.
  Corrected wherever stated. Per-unit entry totals: **A 21 entries over 8 units; B 38 over 13.**
- **B's recording-threshold flag stands, at the corrected magnitude.** B recorded every candidate
  it considered and rejected — including ones it judged not to be defects at all (a control working
  as designed, a declared scope limit, a stale comment); A recorded only competing defects. **The
  two halves' `alternatives_not_extracted` counts are therefore NOT comparable as a defect-density
  measure and no outcome row may be built on them.** The divergence is 8/13 vs 13/13, not 6/13 vs
  13/13 — smaller than reported, and real. Flagged by B rather than smoothed, which is the correct
  handling: the convention differed, and a pooled count would have concealed that a convention
  differed at all.

### I.4 Floor asymmetry — pre-registered BEFORE the number exists

B's selection residual is accepted as recorded (three multi-defect directories where the stated
rule left the call close, each loser recorded with its reason). **Two of the three are
floor-participating directories.** So the floor conflates two things: extraction churn (what it is
built to measure) and selection difference (which of several genuine defects an extractor chose).

**The conflation has a SIGN, and that is what makes it usable.** A selection difference inflates
apparent disagreement; it can never deflate it — two extractors who chose different defects from
the same directory cannot thereby agree. Therefore:

| floor result | reading, fixed now |
|---|---|
| **4/4 INSIDE FLOOR** | **clean regardless.** Selection difference could only have pushed away from agreement, so agreement despite it is agreement. |
| **anything below 4/4** | **ambiguous between churn and selection, and must be read as an UPPER BOUND on extraction churn** — never as a churn point estimate. |

This bites directly on §E: the H5 extension conditional uses the floor to decide whether a
multi-extractor protocol is "changes n and nothing else." Under a sub-4/4 floor, §E may not treat
the measured disagreement as extraction variance without first separating the two — so the
conditional fails CLOSED (extension not licensed) rather than resolving against an inflated number.

Pre-registered now, before the floor is computed, because an asymmetry stated after the number
exists is an asymmetry chosen to suit it.

### J.1 The ruling is an artifact under the same discipline — and it needed a positive control

§I was **correct in prose and wrong in machine-checkable form**: following it faithfully produced
the outcome it prohibited (18 cells where it ruled 22). That is spec-vs-implementation drift where
**the spec is the operator's ruling** — a category this project's discipline had not previously
named, because rulings were treated as the thing implementations are checked *against*, not as
artifacts that are themselves checkable.

**The positive control turned out to be the cell count.** Reading the rule does not catch it; the
rule reads correctly. Counting what the rule claims to produce does. Record alongside the git
channel (§J) for the same reason: both are surfaces the design treated as neutral substrate — one
the memory system, one the operator's own instruction — and neither was registered as something
that could carry a defect.

**A rule that yields its own negation when executed is a defect in the rule, not in the execution.**
The check is cheap and general: after any accounting ruling, compute the quantity the ruling names
and compare it to the number the ruling states.

## L. The recurring shape, stated once for the writeup

**A boolean acquires a third question and answers it by silently privileging one reading.** Twice
in two turns: `role` carrying matrix-membership and floor-participation (§I), then
`overlap_source` carrying directory-level and extraction-level participation (§I.2). Both times
the collapse was invisible in the field's own terms — each boolean returned a well-formed answer to
the question it *could* answer — and both times **the tell was an arithmetic check disagreeing
with a stated intent**:

| witness | stated | counted |
|---|---|---|
| cell accounting (§I.2) | 22 matrix units | **18** |
| §4.5 denominator (step 1) | 74 audit directories | **73** after an empty placeholder was excluded |
| de-blocking harness (§G.3) | baseline max-run 8 | **2** |
| frame-census rider (§L.2, 2026-08-11) | census "across all 22 drawn directories" | **n_escape = 8**; there is no 22-directory escape sample |

**Counting the thing the rule claims to produce catches rule defects that reading the rule does
not.** Each of these took seconds and each caught something a careful re-read had already passed.
This belongs in §6.4 next to the controls-need-controls argument: it is the cheap general form of
that recursion, and unlike the recursion it terminates.

### L.1 The propagation instance, recorded because the flag survived by luck

The corrected count (A's units recording alternatives: 8, not 6) **reached two downstream
artifacts — `HANDOFF_EXTRACTOR_B.md` and this OQ's ISSUES entry — before anyone re-derived it**,
and B's recording-threshold flag was built partly on the wrong figure. The flag survived correction
because the divergence was large enough to hold at either magnitude (8/13 vs 13/13 as against
6/13 vs 13/13). **That is luck, not design.** Had the true figure been 12, the flag would have been
a finding about nothing and would have shipped.

Provenance of the error: an instance counted the units *it* had extracted and reported the number
as a property of the whole half, omitting two written by the prior instance before the handoff. It
is the arc's own recurring shape once more — **a plausible number, sourced from a partial view,
reaching consumers with no way to check it** — this time inside the apparatus rather than inside
the corpus it audits.

### L.2 A check that could not return the failure it looked for — and where the vacuity came from

Found while executing the threshold probe's frame census (2026-08-11).

**The rider, and its correction.** The operator's rider directed the census "across all 22 drawn
directories." The escape stratum is **n = 8**; the 22 is the *primary* draw. The number was accepted
uncounted, and **it was wrong in the direction that made the bound look tighter**: with the 2
unseeable directories removed, the escape side's effective n is 6 and its one-sided recall licence
loosens from ~31% to **~39.3%** (1 − 0.05^(1/6)). Moot while six candidates stand — the bound prices
the null and the null is not the live hypothesis — but recorded here with the other stated-vs-counted
witnesses rather than left in the probe's own file. **Self-reported by the party whose number it was.**

**The vacuity, which is the sharper item.** Run as directed, the census over the 22 primary
directories returns **0 unseeable** — and it could not have returned anything else. The primary
stratum is *defined* by a grep over `--include='*.md'`, so every member has an `.md` **by
construction**. The check looks exactly like a passing check and measures nothing: the same shape as
a clean read from a probe that never looked.

**What makes this instance different from the earlier four — the origin.** In this arc's previous
catches the vacuity came from a **coding or harness defect**: something compared an extraction
against its own echo, or a control's own stated numbers made a defect in them unfalsifiable. Those
are things a careful implementer could have written differently. Here **the vacuity is inherited from
the frame's definition**. Nothing was coded wrong; the population was *specified* such that the
question has one reachable answer. That origin is why re-reading the script would never have caught
it — the script is correct — and why the diagnostic has to be applied to the *population*, not the
code: **ask what value of the input would make this line fail, and if the sampling rule already
excludes it, the check is a definition restated.** (Enumeration as the fifth in the arc is the
operator's count, recorded as theirs; the two instances verified in-file here are the §E floor
self-comparison and the `73 + 101 = 174` partition that concealed a positional parse.)

### L.3 The anti-misreading entries, counted because they are invisible by design

**Fourth instance this arc (operator's count, 2026-08-11):** something written down *specifically to
stop a future reader promoting it*. The latest is the threshold probe's item 3 — the blind judge
returned the same verdict §H.1's redacted paragraph asserts, and the coincidence was recorded together
with the reason it is worth almost nothing (the judge returned `extract` on all four, so agreement
there is what a judge extracting everything produces anyway).

**Why the count is worth keeping.** These entries produce nothing citable. They add no finding, move
no number, and a reader encountering one learns only that a tempting inference was already considered
and declined. That is exactly why they are the entries most likely to be dropped for brevity — and
why their presence is the reason the rest of the record can be trusted. Same discipline as recording
predictions that failed: the value is in the asymmetry between what was kept and what could have been
quietly omitted.

## M. §H.2 residue — k = 0 confirms the null AND leaves the mechanism untested

**Operator, 2026-08-11: the null is confirmed and the row stays OPEN.** k = 0 across the full n=22
primary sample (A 0 + B 0). The row licenses **"keyword-proxy false-positive rate ≤ 12.7% at 95%
confidence"** and **nothing more** — never "the proxy is precise."

**Declared residue, recorded explicitly rather than left as an absence:** because zero NO-UNIT
proposals were made, **the operator-confirmation gate never fired**. §H.2's quarantine mechanism —
extractor proposes, operator rules — is therefore **UNTESTED. It has no positive control and we do
not know that it would work.** It has never been exercised end to end even once.

This is the same structure as the escape check's own quarantine, and the same warning applies: an
unfired gate is not a passed gate. If the extension to n=73 runs, the first NO-UNIT proposal will
be the mechanism's first execution as well as its first use — and it will be carrying a decision
at that moment. **Not a closed row; a declared absence with a named graduation step** (exercise the
gate once on a constructed proposal, or accept that its first real firing is also its test).

## N. Pooled `incident_location` — third declined guess, and the observation that survives it

Pooled over all 26 units: **subject 17 / self_audit_subsection 5 / incidental 4.**

**The skew reading is dead at the pooled level too, and is recorded as NOT HELD — the third
directional guess this arc that the data declined** (after the C/D disagreement guess and the
interim-boundary skew reading). Kept at the same volume as the confirmations.

**But the framing question does not vanish with it.** Non-subject counts, both denominators stated
because they differ and the difference is easy to slide over:

| set | n | subject | self_audit_subsection | incidental | **non-subject** |
|---|---|---|---|---|---|
| all extraction units | 26 | 17 | 5 | 4 | **9 of 26** |
| **matrix units** (the ones that enter cells) | 22 | 14 | 5 | 3 | **8 of 22** |

*(Recorded as its own instance of §L: this row was first written as "9 of 22" — the numerator from
the 26-unit set against the 22-unit denominator — and was caught by counting it before the
pre-registration froze, not by re-reading it. The correct figure for the matrix set is 8 of 22.
Same shape as everything else in §L, inside the sentence that was pre-registering the shape.)*

Either way the incident is real and reported in roughly a third of directories without being what
the directory is about. That is enough that §4.5's phrasing still matters: "73 incident-bearing directories" is defensible,
"73 audits *about* incidents" is not, and the two are easy to conflate in a sentence.

**Status: a declared OBSERVATION with its own row, not a finding.** It has no pre-registered
threshold, was not designed for, and the distinction it turns on (subject vs reported-anywhere) is
the §H.1 boundary rule's own — so it can be reported and cannot be scored.

## O. Declared calibration residue — what the anchors do NOT cover, both directions

Fixed before any coding. Both gaps are consequences of anchor-selection rules that were correct
and mechanical; neither is a defect in the rules. They are stated because H3 is a narrow licence
and the two places it does not reach are the two places the experiment most wants to read.

### O.1 Direction (i) has no P6 anchor

The anchor set is **{P1, P2, P5}**. The published P6 exemplar was disqualified because its incident
is the incident an extracted unit already describes (§ anchors, `controls/anchors.json`).

**P6 is the pattern carrying the most conceptual load** — it is the composition-layer pattern, it
is what R5's pre-registered directional expectation concerns, and it is our side of the E↔P6
correspondence the writeup emits as a PROPOSED Ω_C mapping. So:

> **H3 at ≥2/3 over {P1, P2, P5} licenses "the coder is not broken" across three patterns and says
> NOTHING about the coder's handling of P6. Any P6 result in direction (i) is UNCALIBRATED and must
> be reported as such. The E↔P6 row additionally lacks anchor support on OUR side of the mapping,
> on top of already being PROPOSED rather than ruled.**

Fixing it is not available without cost: the only published P6 exemplar strong enough to anchor on
is the one that collides, and constructing a new P6 anchor would mean the extractor assigning a
pattern — which is the boundary the whole design rests on. **Declared, not repaired.**

### O.2 Direction (ii) has no multi-membership anchor — same shape, opposite direction

The direction-(ii) anchors are drawn **exclusively from the agreeing 12**, because "unambiguously
stated by Wu" requires both frozen records to concur; **none comes from the complement 10.**
Verified mechanically, not assumed.

Under the amendment's own naming the agreeing 12 is the single-class stratum — **with §A's status
caveat carried, not laundered**: reading (ii) is an observation with three live defeaters and is
not asserted here. The gap holds under either reading, because it rests only on the mechanical
fact that every anchor sits in the 12 and none in the 10.

> **H3 for direction (ii) is calibrated on single-class incidents only. The multi-membership
> stratum — the one whose verdict grammar §B had to be amended for, where a two-pattern split is
> the EXPECTED result rather than a degraded one — has no anchor. Recovery on unambiguous
> single-class incidents does not license reading the coder's behaviour where a unit legitimately
> carries two labels.**

**The two gaps are the same shape in opposite directions:** in each, the anchor rule selected for
*unambiguity* and unambiguity is exactly what the hard stratum lacks. An anchor set is a sample of
easy cases by construction, and both gaps are that fact showing up where it costs something.

### O.3 The P6 gap is ONE incident showing up in three instruments, not three residues

**Recorded 2026-08-11 at the operator's reading, before the (iii′) extraction ran.**

`system_gradient`'s `[] → 0.0` fallback — the incident where a system-level gradient metric read
exactly `0.0` on every input it had ever been given, because every computation had silently failed
and the fallback was indistinguishable from measured-flat — appears at three separate places in
this run's apparatus:

| # | instrument | how the incident appears | consequence |
|---|---|---|---|
| 1 | direction-(i) anchor set | **disqualified** — the published P6 exemplar's incident is already an extracted unit (`controls/anchors.json` `_disqualified`) | §O.1: no P6 anchor; any direction-(i) P6 result is UNCALIBRATED |
| 2 | (iii′) population, row 9 | **disqualified** on the anchors.json precedent (`RULING_2026-08-11_freeze_scope.md` §2.1) | the strongest (iii′) P6 exemplar is out; n=10 eligible, 7 written |
| 3 | direction-(ii) twins, unit `05` | **used** — as the redacted arm of the `oq93_grid_viability_probe` pair, where collision with the unit is the definition of the control | the corrected set's third pair; role-appropriate, checked before writing (`controls/redaction_pair_selection_defect.md` → *The oq93 collision check*) |

**Why this is worth a section rather than three separate declarations.** Read as three entries, the
P6 situation looks like ordinary attrition — an anchor lost here, an exemplar lost there. Read as
one incident, it is a **single point of failure sitting under the pattern that carries the most
conceptual load**: P6 is the composition-layer pattern, it is what R5's pre-registered directional
expectation concerns, and it is our side of the E↔P6 correspondence the writeup emits as a PROPOSED
Ω_C mapping. Every instrument that could have calibrated P6 independently is drawing on the same
underlying incident, so the residues are **correlated, not additive**, and no amount of accumulating
them produces coverage.

**What it does NOT change.** Nothing here is a new defect and nothing is repaired by recording it.
Instrument 3 remains role-appropriate (an anchor must be an incident the coder has not otherwise
seen; a twin's redacted arm *is* the unit). Instrument 2's disqualification remains *conservative
rather than required* — different label spaces, different runs — and stands as declared. The
correction is to the **reading**: §O.1's "no P6 anchor," the (iii′) row-9 disqualification, and the
twin-pair collision check must be reported as three faces of one incident, and any writeup sentence
that totals them as independent residues is over-counting the evidence against P6 coverage while
under-stating its concentration.

**The corpus fact underneath, stated so the gap is not read as an oversight.** There are three
published P6 exemplars. The one strong enough to anchor on is the one that collides; constructing a
new one would require the extractor to assign a pattern, which is the boundary the entire design
rests on. **Declared, not repaired** — as in §O.1, and for the same reason.

## P. The self-comparison family — one section, not three catches

Three times in this arc a **self-comparison** was caught before it landed, each in a different
instrument, each arriving through a different door. Write them as a family; individually each reads
as a lucky catch, together they show a mechanism with a stable shape.

| # | instrument | the self-comparison it would have produced | door it came through |
|---|---|---|---|
| 1 | extraction-churn floor | two "independent" extractions that are one extraction and its echo — 4/4 INSIDE FLOOR whatever the truth | reading the other extractor's unit "just for the schema" |
| 2 | the same floor, again | same outcome, reached without opening a file | a commit message summarising the unit, read via `git log` |
| 3 | direction-(i) anchors | a unit and its own anchor drawn from ONE incident — recovery measuring whether the coder can match a text against a near-copy of itself in the same run | the published exemplar happening to be an extracted unit's incident |

**The shared shape: an apparatus measuring agreement between two things that are not independent,
and reporting the agreement at full confidence** — because agreement is exactly what a working
version of each instrument produces. None of the three would have errored, and all three would have
produced the reassuring number.

**They differ in the door, and that is the transferable part.** One was a rule everyone knew and
could break by accident; one was infrastructure nobody had registered as a channel (§J); one was a
selection rule that was correct and still selected a duplicate. **A blinding protocol that names
only the first is two-thirds blind.**

### P.1 The anchor redaction rule was a compositional gap

Both redaction rules were written per-direction, and each was locally complete. **An anchor is by
construction the one artifact belonging to both directions at once** — a unit from taxonomy X
interleaved into a run coding taxonomy Y — so it fell between two rules neither of which was wrong.
The fix (redact anchors against BOTH lexicons) is one line; the gap was invisible until an artifact
existed that instantiated both scopes simultaneously.

**That is a composition-layer defect in the control architecture — found while building controls
for a taxonomy that names composition-layer defects.** Worth the line it costs in the writeup: the
architecture reproduced the failure class it was built to detect, at the level where the classes
compose, which is where its own taxonomy says to look.

---

# Addendum (operator, 2026-08-11) — the twins pass: an instrument's error profile belongs to its role

## Q. Control (c)'s selection metric measured a different taxonomy

Full evidence, options and ruling: `controls/redaction_pair_selection_defect.md`. Ruled **option
C** — the declared three pairs are kept and the two the corrected metric picks are added, reported
as two separate sets, with the **corrected set carrying the both-residue row** and the declared set
reported alongside as the pre-declared comparison. Fixed before either number exists, so the row
cannot acquire a choice at writeup time.

### Q.1 The mechanism, which is new and is not a coding bug

The direction-(ii) banned lexicon was built and validated as a **detector**. In that role a false
positive is *conservative*: it fires H2, you investigate, you clear it, nothing is lost — and this
arc already banked exactly that outcome as catch #3 ("permission *class b*y default").

The same matcher was then reused as a **density metric to select on**. In that role the identical
false positive is **silently decisive**, and it ranked first the one sampled directory with nothing
in it to measure (`five_leg_twin_comparison`: 21 of 21 hits are `P1`/`P2`/`P3` used as that
directory's own probe names).

**An instrument's error profile is a property of its ROLE, not of the instrument.** Validating it
in one role licenses nothing about the other. This belongs in §6.4 beside the three worked
examples, and it is the one that is *not* a coding bug — **the matcher was correct; the reuse was
the defect.** The three existing examples all show an instrument being wrong; this shows a right
instrument in a wrong job, which no amount of testing the instrument would have caught.

**The shape it would have produced, stated because it is the reason this is the sharpest defect of
the arc:** a floor of zero, reported as "redaction costs nothing," clearing the both-residue row to
be read as a finding. **The control designed to protect that row would have flattered it.**

### Q.2 The §1 instruction was correct in prose and wrong in force — corrected form

`HANDOFF_TWINS_AND_DRIVER.md` §1 said "do not re-derive them 'to check'." It was written to prevent
**reselection** and it landed as a prohibition on **verification** — two different things collapsed
into one sentence. **Third time in this arc a rule has been correct in prose and wrong in force**
(§I.2 the cell-accounting field; §J.1 the ruling-as-artifact; this).

**Corrected form, adopted:** checking is *permitted and expected*; reselecting is *mechanically
prevented*. `controls/recheck_predeclared_counts.py` is the shape — it recomputes the stated numbers,
reports whether the pre-declared selection is invariant, and **exits non-zero if the selection ever
moves**, which routes the move to the operator instead of letting a script make it.

**General principle, for §J beside the git channel:** *an instruction that forbids checking a
control's stated numbers makes a defect in them unfalsifiable by construction — a strictly worse
failure than the reselection it prevents.* §J already holds two surfaces the design treated as
neutral substrate (the memory system; the operator's own instruction). This is the third: **a
blinding rule that blinds the verifier as well as the coder.**

### Q.3 §L gets a summary line, not a fourth row

The stated-versus-counted table now has four entries (22/18 cells, 74/73 directories, 8/2 max-run,
~15/10 exemplars), plus 3 of 9 direction-(ii) density counts that did not reproduce. Four is enough
that the list is no longer the point. **The summary line: every one of them was found by counting
what a rule claims to produce, and not one by reading the rule.** Re-reading had already passed all
of them.

### Q.4 A direction-(ii) source cites the orphaned fork branch — an OQ-278 datum from outside the fork pass

Pair 1's source names the pattern for its own incident in its own voice:

> "**Verdict: the existing mandatrophy authoring surface is a Build-Discipline Pattern-1 dangling
> wire.**" — `audits/2026-06-07_stakeholder_layer_migration/AUDIT.md:144`

That is a source citing the **orphaned fork branch**, and it arrives from *outside* the
fork-residue pass that was built to measure the fork. Flag it separately in the writeup: the
fork-residue row is fed by a pre-registered instrument, and this is unsolicited evidence of the
same kind arriving by another route, which is worth more than a row it did not come from.

### Q.5 Two constructions worth naming, because both close a door this arc found open

**A check must not go green on an empty input.** `controls/verify_redaction_twins.py` reports
`redaction_twins_direction_ii.json exists — NOT WRITTEN YET` as a **FAILURE** rather than passing
over a file that is not there, and asserts **both** directions per pair (unredacted arm must fail
the sweep, redacted arm must pass). One-sided checking would pass a pair whose redacted arm was
never redacted, or one whose unredacted arm restored nothing — and **a floor of zero is exactly
what both one-sided checks report as healthy.** The arc has now caught green-on-empty in three
instruments (`system_gradient`'s `[] → 0.0`; the OQ-66 MaxEnt soft-failure comparing `[no_top,…]`
against itself; the payload-capture count that would clear a leak-grep by writing zero payloads).
Naming the construction, not just the catches: **a gate whose input is absent must be RED, and a
check that cannot distinguish measured-empty from didn't-look is not a check.**

**A row's instrument is assigned before its number exists.** The both-residue row is fed by the
corrected redaction-pair set, fixed in `controls/redaction_pair_selection_defect.md` **before
either floor is measured**. This closes the last place in the design where a choice could be made
with results in hand — the failure mode is not dishonesty but a writer holding two defensible
numbers and no pre-committed rule for which one feeds the row.

### L.4 A ruling made on evidence that had not been gathered — a different entry from a count that did not reproduce

**Recorded 2026-08-11, by the instance that then gathered the evidence.**

Every entry in §L's table so far has the same shape: *a number was stated, the number was counted,
they disagreed.* This one does not. Here the number was **right**, and the entry is about **when it
was available**.

`controls/redaction_pair_selection_defect.md`'s corrected set — `04` (9), `07` (4), `05` (2) — was
selected on **directory-level** taxonomy-vocabulary counts, and the operator's option-C ruling put
that set under the both-residue row on the strength of them. But a twin restores vocabulary from the
unit's **own sources**, so the load-bearing denominator is `files_read`, not the directory. The
defect document measured `files_read` for the *declared* set only (3 / 0 / 0, which is what exposed
the defect); **for the corrected set it was never measured before the ruling.**

It was measured this session, before any arm was written:

| unit | directory-level (ruled on) | `files_read`-level (measured after) |
|---|---|---|
| `04_stakeholder_layer_migration` | 9 | **3** |
| `07_oq97_census` | 4 | **3** (4 hits, 3 distinct) |
| `05_oq93_grid_viability` | 2 | **2** |

**It came out clean.** All three are non-empty at the denominator that matters, so the corrected set
is exactly what the ruling intended it to be, and nothing is retracted. The ordering compresses
(9 > 4 > 2 becomes 3 ≈ 3 > 2) but membership does not move, and membership is all the ruling used.

**Why it is logged anyway, and logged separately.** A correct decision made on evidence that had not
been gathered is not the same event as a stated count that failed to reproduce, and the arc has now
seen both. The failure mode it belongs to is invisible from the outcome: had `07` or `05` come back
at 0 — as `10` and `20` did at this same denominator — the row would have been fed by an instrument
with nothing to measure, and the ruling would have read as sound right up until the floor was
reported. **The check that would have falsified it cost one script and ran in seconds; what made it
skippable was that the decision already looked right.** That is the same asymmetry §L.3 keeps: the
entries worth most are the ones that produce nothing citable.

Sharpened form for §6.4, alongside *count what the rule claims to produce*: **ask at what
denominator the decision is load-bearing, and check whether the number in hand was measured at that
denominator or at a convenient one.** Directory-level and `files_read`-level are both real
quantities; only one of them is the one the twin's floor depends on.

### L.5 The second matcher defect — and both were found by re-deriving a control's own numbers

**Recorded 2026-08-11.** §Q.3 ruled that the stated-versus-counted list gets a summary line rather
than more rows. This is that line, extended by one observation the arc has now earned.

The direction-(ii) leak lexicon joined its multiword patterns with a **literal space**, so every
hyphen-joined variant escaped it. Three are attested in the repository's own prose, and they are the
three that matter most:

| escaping form | where |
|---|---|
| `Build-Discipline Pattern-1` | `audits/2026-06-07_stakeholder_layer_migration/AUDIT.md:144` |
| `Build-discipline spine` | `audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md:22` |
| `Pattern-6 success-shaped-absorption` | `audits/2026-06-11_oq97_pattern6_census/WRITEUP.md:1` |

A leak-grep that catches `Pattern 1` and misses `Pattern-1` is a **false absence in the instrument
that certifies H2** — the sweep returns clean and the cleanliness is a fact about the matcher, not
about the payload.

**This is the second defect in this arc whose locus was a MATCHER rather than a count** (the first
being §Q's density metric, where `\bP[1-6]\b` fired on local probe names). Both were found the same
way: **by someone re-deriving a control's own numbers** — which is precisely the practice the
original §1 prohibited and the corrected §1 permits (§Q.2). The prohibition would have left both
defects unfalsifiable by construction, and the record now shows the permission paying for itself
twice.

**The resolution is the role split, not a wider list** (operator ruling, 2026-08-11; the module
carries it as `LEXICON_DETECT` and the frozen `LEXICON_SELECTION_20260811`). Detection false
positives are conservative — you investigate and clear them — so the detector must be as wide as the
evidence supports. Selection false positives are silently decisive, and the pre-declaration's entire
value is that it was fixed before content was seen. Widening moves the declared rule's top-3
(`oq97_pattern6_census` 4 → 9 overtakes `oq138` at 5), **and it moves TOWARD the corrected set — the
direction that flatters the row.** That the movement is convenient is exactly why re-declaring under
the widened list was refused rather than adopted. Same principle as §Q's: *an instrument's error
profile is a property of its ROLE, not of the instrument.*

**What made the split available was a measurement, not an argument:** across all 54 coder-facing
texts (units, anchors, decoys, planted fixtures), the widening changes **zero** hit sets. Without
that number, widening a live instrument mid-arc would have been an unmeasured change to the thing
every leak claim depends on. It is logged here because "the change is safe" is the sentence this
arc has learned to distrust unless a count sits behind it.

### L.6 The seventh vacuous check — and the first one inside the instrument built to prevent vacuous checks

**Found 2026-08-11, by the instance that had written the gate, while stamping the preregistration.
Operator ruling: this belongs in §6.4 beside the other worked examples, because it is the one that
closes the argument.**

`oq277_crosscoding_driver.py` exists to enforce one ordering: **assert the payload-capture count
BEFORE running the leak-grep**, so that a clean grep over zero captured payloads cannot read as a
green H2. That is its purpose; §Q.5 already names it as one of the arc's three green-on-empty
catches.

Its freeze-ordering check searched `audit_log.md` for a prose line saying *"first result"*, in
order to assert the preregistration md5 sat above it. **The file contained no such line.** The
regex matched nothing, the guard fell through, and the check reported success — **satisfied by the
absence of its own input.**

| | |
|---|---|
| what the check claimed | "the prereg md5 is above the first result line" |
| what it measured | nothing; no boundary existed to locate |
| what it returned | pass |
| what a real failure would have looked like | pass |

**Why this instance is the one for §6.4.** The previous six are defects in instruments that measure
*the corpus* or *the apparatus*. This one is a defect in the instrument built **specifically to stop
this defect class**, written by someone holding the rule in mind, in the same file whose docstring
explains why counting must precede grepping. Every available layer of prevention was present and
none of them fired.

**So the recursion is not resolved by another gate, and this is the closing move of the
controls-need-controls argument.** Adding a control to check the control reproduces the problem one
level up — the checking control can be vacuous in exactly the same way, and nothing in the
construction stops it. What actually terminated the regress here was not a better gate but **a
person asking, of a specific line, what value would make it fail** — and finding there was none.
The recursion terminates in someone counting, not in a deeper instrument. §6.4 should say that
plainly rather than leaving the reader to infer that enough layers eventually suffice.

**The repair, and its shape.** The prose match was replaced by an explicit sentinel
(`<!--OQ277-FIRST-CODING-RESULT-->`) that **fails closed**: no marker, no live call. The
distinction that matters is not "regex versus sentinel" — it is that the old form's failure mode
was *silence* and the new form's is *refusal*. **A boundary that cannot be located is not a
boundary**, and a check whose input may be absent must treat absence as red.

### L.7 A control that declined on four real cases and fired on its author — the arc's best-evidenced control

**Recorded 2026-08-11 at the operator's grading.**

`controls/verify_redaction_twins.py` gained a check that every OQ id claimed in a pair's
`restored_from_source` block is really present in that arm's coder-facing text. The rationale is
that a provenance block listing vocabulary the arm never restored is a **recap-as-witness
substitution inside the control**: the reader audits the list instead of the text.

On its first run it **failed** — on unit `05`, an arm written minutes earlier by the instance that
had just written the check, which claimed `OQ-93` and `OQ-96` the arm did not carry.

**Why this is graded above a planted-fixture validation.** Under *a positive control demonstrates
DISCRIMINATION, not detection*, planting the target shows only that the instrument CAN fire; the
witness that its firing carries information is a case it **DECLINED**. This check's first run
produced both, on real material, in one pass:

| grade available | what this run produced |
|---|---|
| authored decoy (weakest) | not used |
| naturally-arising negative | **four** — pairs `ii_1`, `ii_2`, `ii_4`, `ii_5` each declined |
| decline in the instrument's own history | **this IS its own history** — first run, four declines and one fire |

Four real declines and one real catch, with no planted case anywhere in the run. **It also caught
its author**, which removes the usual worry about a control tuned to pass the material it was
written against.

**Scope, stated so the grade is not over-read.** The check discriminates on the class of claim it
covers — **OQ ids**, the machine-checkable subset of `restored_from_source`. Prose claims in the
same block ("the section's own title", "the source's own positive-control sentence") are NOT
covered and remain unwitnessed. The record entitles the check to "discriminates on OQ-id claims,
evidenced by 4 declines and 1 catch on real material," and to nothing wider. Its discrimination
record lapses if the block's format changes or if it is reused in a different role.

**The disposition on the catch.** Both ids were restored into the arm rather than struck from the
list, because both are genuinely attached to that incident in the source (`FINDINGS.md:1` titles
the probe `OQ-93`; `PREREGISTRATION.md:6` names the `OQ-96` shim interim, which the arm's mechanism
already referenced). The check did not force a retraction — it forced the arm to become what its
own provenance block already claimed.

### L.8 The second unplanted fire in the arc — and the mirror image of L.7's grade profile

**Recorded 2026-08-11 at the operator's reading. Logged beside §L.7 deliberately: these are the
arc's only two controls that fired on real, unplanted material, and their evidence profiles are
opposite.**

`python/audits/oq277_build_prereg.py --check` asserts that `PREREGISTRATION.md` is byte-identical to
a fresh assembly of its canonical sources. On 2026-08-11 it went **RED** — not on a planted fixture,
but because `verdict_grammar_amendment.md` had gained §L.6 and §L.7 at operator grading and the
assembled document still carried the pre-amendment text. The drift was real, the cause was ordinary
work, and nothing about the run was constructed to test the check. Recorded at the time in
`audit_log.md`; graded here.

**Had that RED occurred one step later it would have been the freeze invalidation notice** — which
is the whole reason the check exists, observed operating rather than asserted.

**The grade, and why it is not L.7's grade.** Under *a positive control demonstrates DISCRIMINATION,
not detection*, the fire is the cheap half and the declines are what license the reading. This check
has the reverse profile from §L.7, and the difference should not be smoothed over:

| | §L.7 (`verify_redaction_twins.py`, the no-overstatement check) | §L.8 (`oq277_build_prereg.py --check`) |
|---|---|---|
| fire | 1, on real material, caught its own author | 1, on real material, unplanted |
| declines | **4, on four distinct pairs** — genuinely different inputs | **3, all the same comparison re-run** — low variety |
| decline quality | each pair a real near-miss the check had to let pass | one was taken immediately after `--write`, comparing a file to a fresh assembly of the sources that had just produced it — **near-tautological, close to a check that cannot fail** |
| net | discrimination evidenced on both sides | **fire side strong, decline side weak** |

So the honest statement is: **one unplanted fire on genuine drift, with a decline set that carries
little information.** That is the strongest *fire*-side evidence available short of the instrument's
own history containing a hard negative, and it is materially weaker than §L.7 overall. Recording it
as "the second naturally-fired control" is correct; recording it as evidence of the same strength as
§L.7 would not be.

**Scope, stated so the grade is not over-read.** The check discriminates on **byte-identity between
the assembled document and its sources**, and on nothing else. It cannot detect an assembly that is
internally consistent and wrong, a source that is itself mistaken, or a missing source that was
never registered for incorporation — an unregistered appendix drifts freely and the check stays
GREEN. Its record entitles it to "detects incorporated-source drift, evidenced by one unplanted fire
on real drift," and to nothing wider. The record lapses if the source manifest changes.

**Liveness re-witnessed at this recording, and labelled as what it is.** Before this section was
written, `--check` was GREEN at md5 `c1040cd04815c206791b5ab3192697be`, matching the DRAFT stamp in
`audit_log.md`. Writing §O.3 and this section turned it RED. That pair is a **planted** two-sided
liveness witness — it confirms the check is wired and still bidirectional at HEAD; it **adds nothing
to the discrimination grade above**, which rests entirely on the unplanted fire. Output pasted in
`audit_log.md` under the 2026-08-11 amendment entry.

### L.9 The eighth vacuous check — the first one at an INTERFACE rather than in a check's logic

**Recorded 2026-08-11 at the operator's ruling; repaired the same day at commit `3e16a1d8`,
before the freeze.**

`oq277_lexicon.py --sweep` — the leak gate, the instrument the blindness of the entire experiment
rests on — could not consume the single-object unit file that the (iii′) brief's own prescribed
command passes it. It died on `KeyError: 'units'` because `isinstance(data, dict)` is true of a
single unit object as well as of a wrapper.

**That is the visible half and it is the boring one.** The sharp half is what the failure looked
like from outside:

| | crash | leak found |
|---|---|---|
| exit code | `1` | `1` |
| `LEAK` lines on stdout | none | one per hit |
| what a stdout-reading wrapper concludes | **clean sweep** | leaks, reported |

**A crash and a leak were indistinguishable at the interface, and the crash produced the *quieter*
of the two outputs.** A caller could reasonably read exit 1 as "leak found and reported" and find
nothing reported; or grep stdout, find no `LEAK`, and record a clean sweep. Both readings are
wrong in the direction that lets a contaminated packet through.

**Why this is the eighth instance and not a repeat of the previous seven.** The first seven were
checks whose *logic* could not return the failure they looked for — a `forall` over an empty table,
a prose match for a marker that did not exist, a comparison of a placeholder against itself. **This
one's logic is sound. The instrument works.** `scan()` matches correctly, the selftest's controls
all fire, and on a well-formed input the sweep is exactly right. The vacuity is entirely in the
**boundary where a caller reads the result** — the same information (`exit 1`, no `LEAK` lines)
carries two incompatible meanings and nothing in the channel distinguishes them.

> **The distinguishing feature, stated for §6.4: a check can be correct and still unreadable.**
> Every prior instance was repaired by fixing the check. This one is repaired by fixing what the
> check *says* — a distinct exit code (`3` = did not sweep), and an abort marker printed on
> **stdout**, because a caller that greps stdout and never reads stderr must still see it. Auditing
> a verification stack for vacuity therefore has to include its interfaces, not only its
> predicates, and *"the instrument is correct"* does not answer the question.

**Two receivers, and the refusal is what surfaced it.** The escape extractor reported the crash;
the brief still carried the broken command verbatim; the (iii′) extractor hit it again and reported
it a second time. Neither was caught by a reader. Both were caught by an instance made to *execute*
the instruction — the receiver's-license mechanism (`build_discipline.md` → *The receiver's license
to refuse*), firing on the arc that minted it. **A third receiver might not have read the
traceback**, which is the operator's stated reason for repairing a frozen instrument rather than
carrying the defect into the run.

**The repair's own control, since an introduced instrument is itself a claim.** Seven `input-shape`
cases run through a real file and the real `json.load`, not a dict handed to the normaliser — the
defect lived on the file path, and a control that skips the path it protects witnesses nothing.
Two-sided by construction: *"consumed without raising"* is paired with *"a planted leak in a
single-object file IS caught,"* because a repair that returned `[]` would satisfy the first and be
**worse than the crash it replaced**. Negative control: reverting only the normaliser turns 5 of
the 7 cases FAIL and leaves the list and wrapper cases PASS — it discriminates rather than failing
everything.

**Declared, because it is the same shape one level in:** the first attempt at that negative control
went red on an `IndentationError` in the scratch copy and therefore tested nothing; and the
selftest as first written *aborted* partway through the reverted run instead of reporting FAIL,
reproducing the crash-vs-result confusion inside the fix for the crash-vs-result confusion. Both
were caught and repaired (`check_call` marks FAIL on a raised exception); they are recorded rather
than smoothed because §6.4's claim is that the recursion terminates in someone counting, and this
is what that looks like when it happens twice in one repair.

`````

---

## Appendix E — frozen unit lists

Opaque ids as assembled, in emitted order. Non-coder-facing detail (role, true label, `matrix_unit`, source id) lives in the sibling `*_map.json` files.


### direction (i) — 22 units + 3 anchors + 2 decoys + 3 twin arms

- items: **30** · matrix cells: **22** · quarantined: **8** · unit-sweep direction: **(i)**
- leak-exempt (MUST fire): `i-28`, `i-29`, `i-30`
- emitted order: `i-26 i-28 i-01 i-24 i-02 i-03 i-04 i-05 i-06 i-07 i-08 i-09 i-10 i-25 i-23 i-11 i-27 i-12 i-29 i-30 i-13 i-14 i-15 i-16 i-17 i-18 i-19 i-20 i-21 i-22`
- packet md5: `5ea6a09baca85a7016705e9d4d447772` · map md5: `45eaa0c4a45c3639d80968a44a95a290`

### direction (ii) — 26 units + 3 anchors + 2 decoys + 5 twin arms

- items: **36** · matrix cells: **22** · quarantined: **14** · unit-sweep direction: **(ii)**
- leak-exempt (MUST fire): `ii-32`, `ii-33`, `ii-34`, `ii-35`, `ii-36`
- emitted order: `ii-01 ii-27 ii-02 ii-28 ii-03 ii-34 ii-30 ii-04 ii-05 ii-06 ii-07 ii-08 ii-29 ii-09 ii-10 ii-11 ii-12 ii-13 ii-33 ii-14 ii-15 ii-32 ii-36 ii-16 ii-31 ii-17 ii-18 ii-19 ii-20 ii-35 ii-21 ii-22 ii-23 ii-24 ii-25 ii-26`
- packet md5: `f8070ddab3d5f3a047d1d18662fc23cb` · map md5: `5c988ea54b2d65b73af5cb87ce5c1e24`

### (iii') — 7 new units (3 anchor members reuse their direction-(i) calls)

- items: **7** · matrix cells: **0** · quarantined: **7** · unit-sweep direction: **(ii)**
- leak-exempt: none
- emitted order: `iii-01 iii-02 iii-03 iii-04 iii-05 iii-06 iii-07`
- packet md5: `75ff29fcc793065b639747297a77ae9a` · map md5: `d2b2556b28a8634c931ffdbcff3c65a9`

---

*End of preregistration.*
