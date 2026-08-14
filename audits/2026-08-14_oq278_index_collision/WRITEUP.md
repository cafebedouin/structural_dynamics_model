# OQ-278 — the pattern-index collision: archaeology and the disambiguated label set

**Executed:** 2026-08-14
**OQ:** OQ-278
**Verdict:** The dating rule at `ISSUES.md:11425` is **VOID and unconditionally so** — a date
cannot tell you which document an author had open, because both lists were live simultaneously
for every citation that exists. Recovery is by **mechanism**, not by date, and it works for 55
of 66 taxonomy-sense references. Two findings extend the entry: the blast radius of the
2026-08-11 vacating is **9 stale citations, not 3**, and `Pattern N`/`PN` is a **six-way**
overloaded namespace, not four-way.
**Substrate:** no pipeline run; documents, git history, and `git ls-files` at HEAD `62922f29`.
**Fired:** live — the dating-rule graduation step is retired, the wrong-label count goes 1 → 4,
the stale-pointer count goes 3 → 9, and two namespaces were discovered by the sweep's own false
positives.

**Evidence map**

| artifact | what it is |
|---|---|
| `PREREGISTRATION.md` | R1a/R1b/R2/R4 branch conditions, registered **before** this sweep ran. Also carries two corrections to the plan's evidence. |
| `sweep_pattern_citations.py` | the re-runnable sweep. `--sweep` writes the label set; `--selftest` runs two known positives + one naturally-arising negative. |
| `LABEL_SET.tsv` | 666 machine rows, one per candidate, `mechanism_slug`-keyed. **The artifact OQ-294 consumes.** |
| §4 below | the hand-adjudicated table for the 66 taxonomy-sense rows — every one read in context, not accepted on report. |

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

## 2. `Pattern N` / `PN` is a SIX-way overloaded namespace, not four-way

OQ-278 and the plan both name four senses. The sweep found six, and **two of them were
discovered by the sweep's own false positives** — which is the honest way to report them:

| namespace | rows | how it was found |
|---|---|---|
| `oq277-frozen-prereg` | 339 | **not ambiguous**: a local defined namespace pinned verbatim by the md5-frozen prereg, precisely so the out-of-harness coder could not read it by reference |
| `other-unclassified` | 126 | bare `P3`/`P4` with no taxonomy vocabulary anywhere near |
| **`taxonomy-candidate`** | **66** | the actual citation population — §4 |
| `analysis-enumeration` | 48 | essays, uke transform outputs, recon reports, protocols and analysis scripts numbering **their own** findings |
| `oq278-subject` | 39 | OQ-278's own body and this audit — subject, not citation |
| `decompose-manifest-candidate` | 17 | `"candidate_pattern": "Interpretive Capture (Pattern 3)"` — the DR engine's own vocabulary |
| `prolog-variable` | 15 | a Prolog variable literally named `P3` |
| `prolog-conflict-catalog` | 9 | `diagnostic_summary.pl:374`'s independent `P1`–`P10` EXPECTED CONFLICT CATALOG |
| **`cwc-claim-row`** | **7** | **found as a false positive:** `CWC:P3` is a *concealment paper claim row*, guarded by `python/claim_cite_check.py` |

**The `CWC:P3` false positive is direct empirical support for Step 0's namespacing.** This
sweep — written by someone who had just read `claim_cite_check.py`'s warning that *"Their `A2`s
are DIFFERENT CLAIMS. An unnamespaced scanner would read v0.6's own table rows as citations"* —
proceeded to do exactly that, and was corrected only because the row was hand-read. A
prohibition gate on bare `Pattern N` is not buildable at this false-positive rate; namespacing
at write time is.

## 3. Two probe defects, both caught by reading rather than by a control

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

## 4. The adjudicated label set — 66 taxonomy-sense rows, each read in context

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

**55 of 66** taxonomy-sense rows recovered to a mechanism by hand (§4.3–4.6 plus the
definitional and meta rows); 1 unrecoverable in-file; 10 are `analysis-enumeration`-adjacent
rows retained in the candidate set because their path rule is coarse. **Content recovery works.
Date-based recovery does not exist.**

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
