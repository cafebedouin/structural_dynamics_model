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

<!--CALLTABLE-->

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
