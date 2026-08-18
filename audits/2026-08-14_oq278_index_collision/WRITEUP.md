# OQ-278 — the pattern-index collision: archaeology and the disambiguated label set

**Executed:** 2026-08-14
**OQ:** OQ-278
**Verdict:** The dating rule at `ISSUES.md:11425` is **VOID and unconditionally so** — a date
cannot tell you which document an author had open, because both lists were live simultaneously
for every citation that exists. Recovery is by **mechanism**, not by date. Two findings extend
the entry: the blast radius of the 2026-08-11 vacating is **9 stale citations, not 3**, and
`Pattern N`/`PN` is a **seven-way** overloaded namespace, not four-way.
**Substrate:** no pipeline run; documents, git history, and `git ls-files` at HEAD `62922f29`.
**Fired:** live — the dating-rule graduation step is retired, the wrong-label count goes 1 → 4,
the stale-pointer count goes 3 → 9, and two namespaces were discovered by the sweep's own false
positives. **Fired a second time on 2026-08-14** when the gate mode built from §4.6 disagreed
with §4.6 on 7 of 9 sites: the hand list was right and the sweep's own regexes were wrong (see
§3(c)), so `LABEL_SET.tsv` had been shipping under-recovered rows. **Fired a third time on
2026-08-17** (§2.1, §7.6): the published census was stale at its own commit rather than by later
drift, and a positive control pinned to a site on its own repair list went red the day the repair
landed.

**Evidence map**

| artifact | what it is |
|---|---|
| `PREREGISTRATION.md` | R1a/R1b/R2/R4 branch conditions, registered **before** this sweep ran. Also carries two corrections to the plan's evidence. |
| `python/pattern_citation_check.py` | the re-runnable sweep, **moved to `python/` 2026-08-14** when the gate mode was added (a scanner here plus one in `python/` would be Pattern 2 on this audit's own subject). `--sweep` writes the label set; `--check` is the gate row `vacated cites`; `--selftest` runs two known positives + one naturally-arising negative. |
| `LABEL_SET.tsv` | **743** machine rows at the 2026-08-17 close, one per candidate, `mechanism_slug`-keyed; **48** are taxonomy citations. **The artifact OQ-294 consumes.** Regenerate with `--sweep`; byte-identical across runs. The census is a *point-in-time* count of a line-keyed sweep over tracked files — see §2.1 before citing any figure here as stable. |
| §4 below | the hand-adjudicated table — every taxonomy-sense row read in context, not accepted on report. It is the **reference** the machine sweep is checked against, and it won that check (§3(c)). |

---

## 1. The dating rule is void, and the lead argument needs no archaeology

**A date cannot tell you which document the author had open.** Both lists were live
simultaneously from `220739b8` (2026-05-30) to now, so no date partitions the citations. The
graduation step at `ISSUES.md:11425` directs a future instance to *"bucket each by whether it
predates or postdates the divergence"* — that instruction cannot work, whatever the divergence
date turns out to be.

**Corroboration: the two lists were born divergent.** At `7af6b945` (2026-05-29)
`build_discipline.md` was *created* carrying Patterns 1 and 2 only, while `CLAUDE.md` already
carried a third item — appended unbolded and hard-wrapped at ~55 chars where items 1–2 wrap at
~90:

```
$ git show 7af6b945:docs/technical/build_discipline.md | /usr/bin/grep -nE '^## Pattern'
17:## Pattern 1 — Produced-but-not-consumed (the dangling wire)
57:## Pattern 2 — One-canonical-thing-became-two (the silent fork)

$ git show 7af6b945:CLAUDE.md | sed -n '/^## Build Discipline/,/^## /p' \
    | /usr/bin/grep -nE '^\*\*[0-9]+\.|^[0-9]+\. '
8:**1. Produced-but-not-consumed (the dangling wire).** Information is correctly generated,
18:**2. One-canonical-thing-became-two (the silent fork).** A file or record gets copied to
30:3. Destructive-replace without proof (the faith merge). Before
```

**And the counts converged at the exact commit where the contents diverged**, which is why
nothing detected it:

```
7af6b945  2026-05-29   CLAUDE=3  BD=2   <- unequal, for one day
220739b8  2026-05-30   CLAUDE=4  BD=4   <- counts CONVERGE; contents DIVERGE at 3 and 4
aaba00e0  2026-05-31   5  5    f8f9eb6b  2026-06-10   6  6    HEAD  6  6
```

That is Pattern 2's *"both copies parse"* with a mechanism: any check comparing member counts
reads green from 2026-05-30 forever. It stayed undetected across 151 commits touching one or
both files. `python/doc_pattern_check.py` (landed this session) compares **names per index**
for exactly this reason.

**The wedge is visible at HEAD without archaeology.** `build_discipline.md:660`
`## The shared root` opens *"**Both** patterns are special cases…"* — meaning P1 and P2 — yet it
now sits **between** Pattern 3 (`:601`) and Pattern 4 (`:686`). It was directly after Pattern 2
when only two patterns existed; Pattern 3 was inserted above a summary sentence that does not
count it.

## 2. `Pattern N` / `PN` is a SEVEN-way overloaded namespace, not four-way

OQ-278 and the plan both name four senses. The sweep found seven, and **two of them were
discovered by the sweep's own false positives** — which is the honest way to report them.
Counts at the final run:

| namespace | rows (2026-08-14) | rows (2026-08-17 close) | how it was found |
|---|---|---|---|
| `oq277-frozen-prereg` | 339 | 339 | **not ambiguous**: a local defined namespace pinned verbatim by the md5-frozen prereg, precisely so the out-of-harness coder could not read it by reference |
| `other-unclassified` | 112 | 138 | bare `P3`/`P4` with no taxonomy vocabulary anywhere near |
| `oq278-subject` | 99 | 68 | OQ-278's own body and this audit — subject, not citation |
| `paper-publication` | 51 | 49 | the six paper versions PUBLISH the list; definitional restatements, not consumers |
| **`taxonomy-candidate`** | **49** | **48** | the actual citation population — §4 |
| `analysis-enumeration` | 49 | 49 | essays, uke transform outputs, recon reports, protocols and analysis scripts numbering **their own** findings |
| `decompose-manifest-candidate` | 18 | 18 | `"candidate_pattern": "Interpretive Capture (Pattern 3)"` — the DR engine's own vocabulary |
| `prolog-variable` | 15 | 15 | a Prolog variable literally named `P3` |
| `prolog-conflict-catalog` | 9 | 9 | `diagnostic_summary.pl:374`'s independent `P1`–`P10` EXPECTED CONFLICT CATALOG |
| **`cwc-claim-row`** | **9** | **10** | **found as a false positive:** `CWC:P3` is a *concealment paper claim row*, guarded by `python/claim_cite_check.py` |
| | **750** | **743** | |

### 2.1 The published census was stale at its own commit — attributed by ROW IDENTITY, not by count

**A census that grew against its published figure has two available explanations, and one of
them is already witnessed on this exact instrument** (§3(d): the sweep read its own output and
reported 671 → 1421 as discovery). So the delta was reconciled by row key `(file, line)` before
the new figures were published, not after — the reverse order would launder a possible
self-consumption artifact into the record, in the pass whose job is repairing the record.

The benign explanation held, and it is worse than the plan assumed. **The 750 figure was already
stale when it was committed:** the `LABEL_SET.tsv` landing in the *same commit* (`fd73ec9e`) held
**755** rows, and `c06bcb26` took it to **761** without touching the census. The 750 belongs to
an earlier intra-session run, and the files that grew between the run and the commit are the
ones the sweep scans and this session was editing — `ISSUES.md`, `KNOWN_STATE.md`, and this audit's
own `WRITEUP.md`/`PREREGISTRATION.md`. **A line-keyed census of tracked files invalidates itself as
it is written**, which is the same lesson §3(d)'s corollary drew for the md5 pin.

Attribution of the two subsequent deltas, per file, by row key:

```
fd73ec9e -> c06bcb26   (755 -> 761, net +6)
  ISSUES.md +1  KNOWN_STATE.md +2  PREREGISTRATION.md +3  WRITEUP.md +0
  — all four are files c06bcb26 itself edited.

c06bcb26 -> 2026-08-17 pre-repair   (761 -> 805, net +44)
  ISSUES.md +16  KNOWN_STATE.md +6  audits/README.md +2  scripts/gate.sh +2
  audits/2026-08-17_oq251_.../{PREREGISTRATION,WRITEUP,audit_log,probe_p4_conjuncts} +15
  python/bound_selector_check.py +3
  — every delta file was either edited by one of the five commits since, or created by them.
  No unattributed growth; the self-consumption shape is not present (and the fixpoint holds:
  two consecutive `--sweep` runs are byte-identical).

2026-08-17 pre-repair -> close   (805 -> 743)
  the §4.6 repairs remove the nine `destructive-replace` index citations and the live
  `bound-probe` ones (the residue is prose that names the old index on purpose); and the
  close COMPRESSED OQ-278's own 442-line entry, which alone took `oq278-subject` 115 -> 68.
  That last step is the standing reason this figure is not a stable quantity: the sweep scans
  the trackers, so compressing an entry or adding a KNOWN_STATE one moves it by tens of rows.
```

**How the figures above were made honest, given that this file is itself scanned.** Publishing a
census inside the corpus it counts has no fixed point *unless the last edit adds no countable
token* — so: all prose landed first, then `--sweep`, then **only the numerals** were patched (a
digit carries no `Pattern N`/`PN`), then `--sweep` again to confirm the row count had not moved.
743 rows before the patch and 743 after, and two consecutive runs against a settled tree are
byte-identical. **No md5 is stamped here on purpose** — the label set is line-keyed, so editing
this file moves its own rows and any content hash would invalidate itself as it was written,
which is §3(d)'s corollary (*pin the PRODUCER, never the artifact's content*) applied to the very
file that recorded it. **The generalisable rule: a self-counting document is
correct only at a fixed point, and reaching one requires the last write to be token-free.**

**Consequence for OQ-294:** its stated precondition — *"`--sweep` must leave the file
byte-identical"* — did not hold at the time it was written, because the committed TSV was not the
product of a run against the committed tree. It holds now, and the durable form of the
precondition is *regenerate, then compare* — never *assume unchanged*.

**The `CWC:P3` false positive is direct empirical support for Step 0's namespacing.** This
sweep — written by someone who had just read `claim_cite_check.py`'s warning that *"Their `A2`s
are DIFFERENT CLAIMS. An unnamespaced scanner would read v0.6's own table rows as citations"* —
proceeded to do exactly that, and was corrected only because the row was hand-read. A
prohibition gate on bare `Pattern N` is not buildable at this false-positive rate; namespacing
at write time is.

## 3. FOUR probe defects — two caught by reading, two caught by an instrument built from the reading

Recorded because each has the shape this taxonomy is about.

**(a) A binary file was decoded and classified as a citation.** `git ls-files` includes
binaries; reading them with `errors="replace"` produced, from
`agent/analysis/originals/spacex_s-1_files/riskfactorscover1b.jpg` offset 43696, a byte sequence
containing `P4` — classified `bound-probe`, confidence `recovered`. **Decoded noise reads
exactly like a citation at the read site.** Fixed by excluding files with a NUL in the first
8 KiB and any that fail strict UTF-8 decode.

**(b) Window membership cannot disambiguate, and the known positive proved it.** The first
recovery rule asked which mechanism vocabulary appeared in a ±3-line window. It returned
`bound-probe|fabricated-default` for `audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md:23`
— because that paragraph says *"Build-discipline spine, **twice over**"* and names both
mechanisms in adjacent lines. Any window wide enough to recover the sense contains both. Fixed
by **nearest-mechanism by character distance**, with a 40-char margin below which the row is
reported `unrecoverable` with both candidates rather than resolved by rule precedence.

This is why the sweep's controls run before it will write the label set at all: an uncontrolled
census is a positional parse waiting to happen.

**(c) THE HAND LIST BEAT THE MACHINE, and that is how two more bugs were found.** When §4.6's
nine stale citations were turned into the `--check` manifest, the checker disagreed on **7 of
9** — it could only recover 2. The hand adjudication was right and the sweep's own regexes were
wrong, in the dullest possible way:

| regex | never matched | sites lost |
|---|---|---|
| `faith merge` | `faith-merge` | 3 |
| `old-vs-new diff` | `Old-vs-new **output** diff` | 1 |
| *(absent)* | `before/after byte-identical`, `pipeline identity`, `faithful … diff` | 3 |

**Under-recovery is silent: a missed match presents as `unrecoverable`, which reads like a
result rather than a miss.** `LABEL_SET.tsv` had been shipping under-recovered rows to OQ-294 —
the artifact whose entire purpose is to be *cleaned* ground truth. This is the audit's own
`Fired:` bit firing a second time, and it is the argument for the instrument the operator asked
for: the hand pass and the machine pass disagreeing is what surfaced it, and neither alone would
have.

**(d) The sweep was reading its own output.** `LABEL_SET.tsv` is committed, so `git grep` finds
it and every row it already held became a candidate on the next run: the census compounded to
**1421 rows against 671 real candidates**, and would have grown at every future run. A producer
that consumes its own artifact reports **growth as discovery**. Fixed by an explicit
`SELF_OUTPUT` skip; the witness is that two consecutive `--sweep` runs with no intervening edit
are byte-identical (a fixpoint), which is the property that failed before.

**Corollary, learned by getting it wrong once here: pin the PRODUCER, never the artifact's
content.** A first attempt pinned the label set's md5 inside `ISSUES.md` for OQ-294 — but the
label set is a line-keyed census of tracked files, so it changes whenever any scanned file is
edited, `ISSUES.md` included. **The hash invalidated itself as it was written**, and chasing it
would have read as churn rather than as the corruption the pin exists to catch. OQ-294 now pins
the producing commit (`fd73ec9e` or later) and instructs regeneration.

## 4. The adjudicated label set — every taxonomy-sense row read in context

`LABEL_SET.tsv` is the machine artifact. The rows below are the hand-ruled classification.
**Keyed on mechanism; the index survives only as `raw_text_as_found`** — so this table is valid
under every branch of R1a/R1b/R2 and the eventual ruling never has to touch it.

### 4.1 Definitional restatements (28 rows) — not citations

`docs/amnesiac_institution/*.md` × 6 versions publish the CLAUDE.md list. Point-in-time; only
the current version is amended, per R4.

### 4.2 Meta — references to the collision itself (7 rows)

`KNOWN_STATE.md:462, 472, 492, 493, 599, 868`; `audits/2026-08-13_oq287_defork/EXTRACTION_PROMPT.md:17`.
Correct as written; they name both senses.

### 4.3 `build_discipline.md`-sense, correctly attributed (13 rows)

Cite `build_discipline.md` explicitly and carry its mechanism. **No repair needed** — Step 0's
`BD-P3`/`BD-P4` form makes them robust to a renumbering.

`ISSUES.md:859, 945`; `KNOWN_STATE.md:5684`; `docs/engine_handoff_5.md:148, 215, 217, 228, 328`;
`docs/engine_handoff_6.md:383`; `docs/engine_handoff_7.md:144`;
`docs/technical/swipl_load_path_and_probe_gotchas.md:226`; `ISSUES.md:852`
(*"51 triples, UNBOUND query, Pattern 3 verified"* — the corrective names the mechanism).

### 4.4 WRONG LABEL under the published set — bare index naming `build_discipline.md`'s member (4 rows)

**The plan's count was 4 and it holds, but the membership differs — one it listed is not a
wrong label, and two it did not list are.**

| site | raw | mechanism | note |
|---|---|---|---|
| `audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md:23` | `(Pattern 4)` | `fabricated-default` | OQ-278's third sighting. Point-in-time audit — **not** retro-edited |
| `prolog/coercion_projection.pl:86` | `(Pattern 4)` | `fabricated-default` | **NEW — and it is in CODE**, a comment at the fixed site |
| `docs/design/the_perturbation_move.md:116` | `Pattern 4` | `fabricated-default` | **NEW** — the `Supp=0.5` fallback |
| `python/omega_resolver.py:820, 830` | `Build Discipline Pattern 4/5` | `fabricated-default` + absence-gate | **NEW, and cross-wired in the OPPOSITE direction**: names `CLAUDE.md`'s *section title* while carrying `build_discipline.md`'s member |

Not counted, per the plan: `python/audits/oq290_frontload_check/riders_BEFORE.md:313` — index 4
*is* recap-as-witness under `CLAUDE.md`, so it is correct, and it is a frozen `_BEFORE` snapshot.

### 4.5 CROSS-WIRED — attributes to `build_discipline.md`, carries `CLAUDE.md`'s member (3 rows)

| site | raw |
|---|---|
| `docs/design/the_perturbation_move.md:137` | ``see `build_discipline.md` Pattern 3 / faith-merge`` |
| `docs/the_perturbation_principle.md:276` | ``in-progress consolidation, `build_discipline.md` Pattern 3 / faith-merge`` |
| `docs/design/design_gaps.md:1053` | `old-vs-new diff-witness (Build Discipline Pattern 3)` |

**Authors treat the DETAIL DOC as canonical.** All three attribute to `build_discipline.md`
while carrying `CLAUDE.md`'s mechanism — the predictable consequence of `CLAUDE.md:158` naming
it *"the full patterns."* That argues for the detail doc as canonical roster and `CLAUDE.md` as
derived summary, and it is evidence R1/R2 should weigh.

### 4.6 STALE — cite the mechanism DEMOTED AND VACATED on 2026-08-11 (9 rows)

**This is the finding that most exceeds the entry's estimate. The plan said three; there are
nine.**

| site | raw | also |
|---|---|---|
| `ISSUES.md:350` | `faithful Pattern-3 diff` | |
| `KNOWN_STATE.md:4508` | `Witness (Pattern 3): full suite before/after byte-identical` | |
| `audits/2026-06-07_stakeholder_layer_migration/MIGRATION_PLAN.md:158` | `Old-vs-new output diff per Build Discipline Pattern 3` | |
| `audits/2026-06-11_oq109_phase_b/b3_open1_discharge.md:25` | `Pattern-3 pipeline identity` | point-in-time |
| `audits/2026-06-12_oq106_retire/README.md:45` | `Pattern 3: destructive-replace owes the diff` | point-in-time |
| `audits/2026-06-18_oq104_citation_checker/FINDINGS.md:55` | `Pattern-3 faith-merge` | point-in-time |
| `docs/design/the_perturbation_move.md:137` | | also §4.5 |
| `docs/the_perturbation_principle.md:276` | | also §4.5 |
| `docs/design/design_gaps.md:1053` | | also §4.5 |

Each now points at an index that is **empty in one document and occupied by an unrelated
mechanism in the other**. **The 2026-08-11 vacating created nine broken pointers and nobody
swept.** That is `build_discipline.md:1392` (*a correction landed in PROSE is not landed until
every instrument encoding the same assumption is checked*) and `:2558` (*a correction is not
done until the old value's consumers are swept*) firing on the taxonomy's own repair — the
third time in this entry's history that resolving the collision has committed the defect the
collision instantiates.

Repair is **Step 4, after R2**, not now: the surviving *witness rule* ("prove before you
replace") is cited by name with no index, which is ruling-independent — but doing it before R2
would mean touching three files the Step 5 renumbering may touch again.

### 4.7 UNRECOVERABLE (1 row in files)

`ISSUES.md:4243` — *"copy-into-audit rejected as Pattern-3, allowlist forbidden"*. Neither
claimant's mechanism fits (copying a file into an audit dir is closer to Pattern 2), so the
index is the only information present. Gets `[AMBIGUOUS — OQ-278]` at Step 4, never a guess.

**Declared scope limit:** this sweep covers **tracked files only, not commit bodies.** The
plan reports a second unrecoverable in commit `aaba00e0`'s body. Commit messages are
immutable and cannot be repaired, so they are outside the label set by design — but the count
"2 unrecoverable" belongs to the union, and **1** is this artifact's number.

## 5. Recovery rate

Of the 48 taxonomy-sense rows the machine classifies at the close, **23 recover to a mechanism,
12 are `inferred`, 13 `unrecoverable`** (at the 2026-08-14 run: 49 rows, 29/8/12 — the recovered
count falls because the repairs removed index citations, which is the ruling working)
— and the hand pass in §4 recovers more than the machine does,
which is why §4 rather than the TSV is the reference for the ruling. **Content recovery works;
date-based recovery does not exist.** The machine/hand gap is itself the finding of §3(c) and
is now bounded rather than assumed: the `--check` row fails if the two disagree on the vacated
slug.

## 6. What this changes for OQ-278

1. **Retire the dating-rule graduation step** at `ISSUES.md:11425`. Replace with: recovery is by
   mechanism; this label set is the artifact.
2. **The blast-radius paragraph's wrong-label count goes 1 → 4**, and gains a **stale-pointer
   count of 9** it did not have.
3. **The namespace count goes 4 → 6**, and two of the six are *pinned* rather than ambiguous —
   which shrinks the genuinely-ambiguous population well below what the entry assumes.
4. **R1/R2 gain one piece of evidence**: authors treat the detail doc as canonical (§4.5).
5. **Step 4's repair list is now enumerated** (§4.4–4.7) and is ruling-independent in form but
   deliberately held until R2.

**Not ruled here.** R1a, R1b, R2 and R4 are the operator's, and their branches were registered
in `PREREGISTRATION.md` before this sweep ran precisely so the ruling is not made against an
account written with a direction.

---

## 7. The repair pass (2026-08-17, after R2 = C2 and R1b′ = B1′ landed)

**Re-adjudicated against the new numbering before anything was touched**, per the plan's
instruction not to blind-repair.

### 7.1 The four §4.4 "wrong labels" are all RETROACTIVELY CORRECT — zero edits

C2 put `fabricated-default` at index 4 in **both** documents, so every one of them now reads
right, each verified in context rather than assumed:

| site | text | verdict |
|---|---|---|
| `audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md:23` | *"the 0.0 fallback is a fabricated default (Pattern 4)"* | names the mechanism explicitly — correct |
| `prolog/coercion_projection.pl:86` | *"a fabricated default (Pattern 4)"* | correct |
| `docs/design/the_perturbation_move.md:116` | *"the `Supp=0.5` fallback, Pattern 4"* | correct |
| `python/omega_resolver.py:820, 830` | *"aborts loudly + classified on any mismatch (Build Discipline Pattern 4/5)"* | 4 = fabricated-default, 5 = absence-satisfies-the-gate — correct, and the §4.5 cross-wiring dissolves with the collision |

**The wrong-label class emptied itself.** This is the corroboration the ruling filed as
corroboration and explicitly not as ground — recorded here so the record shows it was checked,
not assumed.

### 7.2 The principle that decided what gets edited: CLAIMS are point-in-time, POINTERS are navigation

§4.4's rows are *claims* — a dated finding about what a defect was — and a dated audit's claims
are not retro-edited even when wrong. §4.6's rows are *pointers*: navigation to a mechanism's
home. A pointer that no longer resolves is broken navigation, not preserved history, and
repairing it preserves the record rather than rewriting it. Every repair below **names the
mechanism** where the index used to be, so the original sense survives the repair — which is also
why the repaired rows drop out of the sweep: the stale index token is the thing removed.

### 7.3 All nine §4.6 stale pointers repaired; the manifest block retired

`ISSUES.md:350`, `KNOWN_STATE.md`, `MIGRATION_PLAN.md:158`, `b3_open1_discharge.md:25`,
`oq106_retire/README.md:45`, `oq104_citation_checker/FINDINGS.md:55`, `design_gaps.md`,
`the_perturbation_move.md:137`, `the_perturbation_principle.md:276` — each now names the
surviving witness rule (*prove before you replace*) or the plain mechanism (*old-vs-new diff*,
*faith-merge*) instead of a vacated index. `pattern_citation_check.DISPLACED`'s
`destructive-replace` block is now `{}` — **swept clean**, re-derived from the corpus each run
rather than asserted, so a tenth appearing anywhere reds with no allowlist to hide in.

### 7.4 The DISPLACED member's live citations moved to index 7

The second stale class, created by this ruling rather than the earlier one and declared in
commit 0 before the move: `ISSUES.md`'s two self-check pointers, `engine_handoff_5.md`'s section
and cross-refs, `engine_handoff_6.md:383`, `swipl_load_path_and_probe_gotchas.md:226`,
`bound_selector_check.py`'s docstring and its **runtime message** (which printed `BD-P3` at users),
and `scripts/gate.sh`'s comment. **16 rows across 7 files remain declared as residue, not
backlog** — prose that names the old index on purpose: the explanations of the renumbering, the
dated `KNOWN_STATE` entries that recorded it, the oq251 audit log, a SHA-pinned fixture quote in
OQ-300, and this instrument's own prose.

### 7.5 `ISSUES.md:4433` marked `[AMBIGUOUS — OQ-278]`, never guessed

*"copy-into-audit rejected as Pattern-3, allowlist forbidden"* — neither claimant's mechanism
fits (copying a file into an audit dir is closer to Pattern 2), so the index is the only
information present. Labelled, per the three-valued confidence rule.

### 7.6 A control retired itself by working — and it was re-anchored, not deleted

The sweep's second positive control was pinned to `docs/design/design_gaps.md`'s cross-wired
citation, which is **on this instrument's own repair list**. Repairing it turned the control red
for the best possible reason and blocked the sweep from writing a label set at all. **A control
pinned to a site its own instrument is meant to repair retires itself the day the instrument
works.** All three positives are now anchored on artifacts nothing is licensed to edit — a dated
audit finding (`fabricated-default`), a frozen `_BEFORE` snapshot (`recap-as-witness`), and a
completed audit log (`bound-probe`) — one per recoverable mechanism, which is strictly more
coverage than the two it replaced.
