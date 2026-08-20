# OQ-287 Limb 2 — the redirect reduces to supersession hygiene, because the external exposure it was sized for was never executed

**Executed:** 2026-08-20
**OQ:** OQ-287 (second limb)
**Verdict:** Limb 2's stated justification — an already-sent letter citing `§2.9(b)` that "cannot be
edited" — **generates no repository obligation**, and the surface that could not be repaired after
the fact (a reader following `§2.9(b)` out of a *published* appendix) was **never created**: v0.6's
appendices are A/B/C/D and the letter appears once, as a repo-path pointer. Limb 2 therefore reduces
to a forward pointer, discharges now, and stops riding Limb 1's schedule; its 2026-09-14 review date
is retired with its reason. **Scope: this is a ruling about the obligation, not a claim that the
letter's `§2.9(b)` cite is accurate** — it is, and it stays accurate, because §2.9 keeps its number.
**Caveat carried:** the standing check installed here has an *enumerated* publication set with no
owner and no gate (declared residual, below).
**Substrate:** no pipeline run — this pass is entirely on the paper/tracker substrate. Code state:
`git HEAD 1df6b118`, clean at start.
**Fired:** live — the standing check was run **before** it was written down, and it flipped a claim:
`EXTRACTION_PROMPT.md` §3's heading *"what moves, from where"* had produced a wrong-but-reasonable
reading in an actual receiver (that v0.6 §9 relocates), which propagated into this plan's own V4 as a
`§9.2` exposure. Five independent lines settle it the other way; **V4's `§9.2` half is withdrawn**.
Two further live corrections: the sub-item promise stands at **four** v0.6 sites, not the two the
plan enumerated; and OQ-280, which the plan treats as in flight and gating Step 3, **had already
resolved** (`d8bb9522`).
**Evidence map:**
- `WRITEUP.md` — this file; the ruling, the withdrawal, and the residuals.
- `standing_check.sh` — the narrowed standing check, executable so the recipe cannot rot. Witnesses
  the two-arm design: run it with no arguments for the three sections OQ-287 ever put at risk;
  `--selftest` for its discrimination controls. Consumed by the ruling in §2 and by ISSUES OQ-287.

---

## 1. What was checked, at HEAD, rather than taken from the OQ text

Every row below was executed 2026-08-20. **Line pins in the plan were already stale** — §2.8 had
moved `:590 → :628`, §2.9 `:632 → :670`, §4.3 `:988 → :1026`, and KNOWN_STATE's uneditability
paragraph `:1731 → :1766`. That is the OQ-280 write landing underneath a plan that named the risk
and then carried the pins anyway. Everything here was located by content.

| # | check | result |
|---|---|---|
| V1 | OQ-278 index ruling landed (gates Limb 1 §III) | GREEN — `doc_pattern_check`: 8 indices, 0 collisions, selftest 7/7 |
| V2 | citation apparatus live | GREEN — `claim_cite_check`: 63 live citations, selftest 3/3 |
| V3 | v0.6 structure matches the salvage map | GREEN — `checks.sh`: rows 1–4 PASS, selftest 6/6 (row 1's containment arm reports **VACUOUS** by its own label — an empty post-A2 population, correctly declared rather than counted as evidence) |
| V4 | Wu letter's v0.6 cites | `§2.9(b)` **and** `§9.2`, at `LETTER_2026-08-11_wu.md:15`. **The `§9.2` half is withdrawn — see §3.** |
| V5 | is the letter a published appendix? | **No.** v0.6's appendices are A/B/C/D (`:2950,:2998,:3072,:3101`); the letter occurs once, at `:2733`, as a repo path. Never executed as a publication designation. |
| V6 | is the letter's own `§2.9(b)` cite made false by this pass? | **No.** §2.9 keeps its number and its text; only the promise attached to it changes. |
| V7 | standing check, run retrospectively | executed; clean on content exposure; **one defect surfaced, one arm's population still untested** — §2 and §3 |

## 2. V7 — the standing check, run before it was written down

The check greps the artifacts **designated for external publication** for references to a section
about to move. The set is enumerated, not inferred: today
`amnesiac_institution_v0_6.md` and `concealment_without_a_concealer_v0_4.md`. Scoped instead to "the
appendix," it would have run over an empty set and passed vacuously — Build Discipline Pattern 5,
on the check built to prevent a Pattern-5-shaped exposure.

`concealment_without_a_concealer_v0_4.md` is the only cross-document member, and it returns:

- `:5`, `:27`, `:1158` — provenance, acyclicity, references. **Arrangement, not content.** No exposure.
- `:34` — cites **v0.6 §2.8/§2.9 by number**, describing them as declared-temporary *pending the
  practice paper*. Not the letter's exposure (it names the destination, not the content), but it goes
  **stale the day Limb 1 lands**, in a document the extraction prompt forbids editing. Routed as a
  ruling, not repaired here; its trigger is planted in `EXTRACTION_PROMPT.md` §10, where the receiver
  who trips it is actually reading.
- `:20–25` — the canonicity marker.

### 2.1 The prose arm is owed by construction, and now has a witnessed mechanism

The canonicity marker at `:23–24` reads: *"That paper is canonical for **the institution** — the case
study, differential amnesia, the memory economy, **the organizational form**, self-instrumentation."*
**No section numbers appear in that sentence.** A number-grep is therefore structurally blind to it
for *any* section it covers, whatever moves. That is a property of the artifact, not an incident.

**v0.6 §9 IS "The Organizational Form"** (`:2196`). So this is not a hypothetical:

| arm | result on §9 | reading |
|---|---|---|
| arm 1 — number (`§9`) | **11 cross-document hits, every one a false positive** | concealment owns its own §9, *"The repair: boundary carriage and external re-derivation"* (`:816`) — a different section entirely |
| arm 2 — prose (`the organizational form`) | **1 hit, `:24` — the true reference** | invisible to arm 1 |

**Both arms fail on the same section, in opposite directions**, and only the pair reads correctly.
The script now prints the number-collision warning rather than reporting 11 unlabelled
"cross-document" hits, because an unlabelled count there reads as exposure and is not.

**Discrimination record, and its honest limit.** The *mechanism* — an absence licensed by keyword
hits alone, against a claim made in other words — has a **naturally-arising witnessed catch** four
days old: KNOWN_STATE 2026-08-20's paraphrase false-absence entry, where the literal string
`cross-type` returned 0 while §3.3 made the claim in other words, and the crosswalk published *"Git
is not identified as the cross-type instrument"* as a finding. The rule drawn there governs here
verbatim: *an absence verdict licensed by keyword hits alone is scoped to the keywords, and the
keywords are the author's, not the document's.* **What remains untested is this population**: no
section that actually moves has a live prose-only exposure, so nothing below witnesses arm 2 firing
on a real relocation. That gap was not closed by planting one — an authored catch in a repo that
tracks `[UNWITNESSED]` rows would be a false witness, and the ladder rates a plant below a natural
negative anyway.

> **Declared residual — written here rather than found later.** The enumeration has no owner and no
> gate. If a third artifact is designated for publication and nobody adds it to `PUBSET`, the check
> goes green over a stale set — silently, and with the same shape as R2's aptness blindness: the
> green reads as verification of a set it never saw. Acceptable at two members maintained by one
> operator; not acceptable undeclared.

## 3. The defect the check surfaced: an ambiguous heading that already moved a receiver

`EXTRACTION_PROMPT.md` §3 is headed **"Salvage map — what moves, from where"** and lists
§7/§7.4/§8/§9/§10 in the same table as §2.8/§2.9. Read as written, §9 moves. It does not. Five
independent lines settle it:

1. Only §2.8 and §2.9 carry `[DECLARED TEMPORARY]` markers — `checks.sh` asserts exactly 2.
2. §4's redirect table covers only §2.8/§2.9.
3. §3's own trailing note names only §2.8/§2.9 as *"the SUPERSEDED side once this lands."*
4. OQ-287's recommended split says v0.6 **keeps** the memory economy, organizational form and
   self-instrumentation.
5. Concealment's canonicity marker says the same, in the prose sentence at `:23–24`.

The "moves" reading rests on **one word in a heading**. §7–§10 are material for the practice paper to
**draw on**, not material that relocates.

**Consequence: `§9.2` was never at risk, and V4's `§9.2` half is withdrawn.** This matters beyond the
bookkeeping — it is a live instance of the receiver test failing in the direction the discipline
predicts. An instruction correct in prose ("salvage map") produced a wrong-but-reasonable action in a
receiver who had read only the prompt, and the sender re-reading the design would not have caught it,
because re-reading exercises recognition. Repaired at the source by splitting the table in two.

## 4. The ruling recorded (operator, this session)

Three things fused in OQ-287's text, separated:

- **R-A — the obligation to Wu is external and discharged on send.** Nothing in the repo can compel
  an edit to a letter in someone else's inbox. The letter's annotation header already records reply
  status and its one over-statement. **Uneditability generates no repository obligation.** As written,
  OQ-287's sentence read as though it *generated* one, and would have regenerated this exact question
  on 2026-09-14 with less context in the room.
- **R-B — the real exposure was the appendix reader, not Wu.** A reader following `§2.9(b)` out of a
  *published* artifact into a vacated section is the surface that cannot be repaired after the fact.
  Per V5 that designation was never executed, and the no-action default keeps it unexecuted. **If the
  letter stays filed evidence, the external exposure is zero.**
- **R-C — Limb 2 therefore reduces.** No sub-item granularity is owed; it was owed *solely* because of
  the letter's `§2.9(b)`. What remains is ordinary supersession hygiene — a forward pointer when the
  practice paper lands — and it closes now.

**The one branch point**, recorded where the decision is actually made (the letter's own header, not
OQ-287's body): **promoting the letter out of filed evidence re-instates Limb 2's sub-item table at
§2.8/§2.9**, and the 2026-09-14 date with it. `§9.2` is not in the reversion.

## 5. The plan's own count went wrong twice, in the step whose purpose is fixing a wrong count

The plan enumerated **two** v0.6 sites promising the sub-item table (the §2.8 and §2.9 markers).
There are **four**: the two markers, the header block at `:68` (*"the redirect is owed at sub-item
granularity"*), and the §13 paragraph at `:2642` (*"the redirect table is what keeps this sentence
true"*). All four are amended in this pass.

Not carelessness — **a hand-maintained summary of a list that a later edit lengthened, with no gate
between them.** It is the same shape as the `§7.4` nine-versus-eleven drift the plan itself was
correcting, one level up. The repair applied in both places is the same: replace the standing count
with something that cannot go stale, or re-derive it at use.

## 6. Residue

- **`concealment_without_a_concealer_v0_4.md:34`** — becomes false when Limb 1 lands. A ruling for
  the operator (amend the line, or accept a knowingly-stale sentence), **not** a silent repair —
  silently repairing it leaves the defect in the design. Trigger planted in `EXTRACTION_PROMPT.md`
  §10, tied to the landing rather than to a date.
- **`EXTRACTION_PROMPT.md` §3, new rows §7.8 and §9.4** (both landed 2026-08-19, after the map was
  frozen) are placed under **"draws on"** by **inheritance from their parents** — §7 and §9 are both
  already routed there — and are marked as inherited, not ruled. If the operator wants either routed
  differently, the row says so rather than hiding the call.
- **R5 had no recorded baseline.** Pass A's WRITEUP names the class and the reproduce command but
  never a number, so the plan's *"compare against Pass A's recorded count"* had nothing to compare
  against. Baseline established here: **23 section-only citations (6 in `docs/`, 17 in apparatus)**,
  as of 2026-08-20. Growth without a decision means the scheme is eroding at its edge while every
  gate stays green.
- **The heading split has no test but a receiver reading it cold** — the check that already failed
  once here. Downstream evidence is free: if the next receiver reads the split table and still tries
  to relocate §9, the repair did not take. A thing to watch, deliberately **not** a new gate row.
- **Limb 1 remains open.** OQ-287 stays `mitigated` until the extraction lands; its status flips to
  `open` when that work starts, so the `[NEXT]` frontier is honest while it is live.
