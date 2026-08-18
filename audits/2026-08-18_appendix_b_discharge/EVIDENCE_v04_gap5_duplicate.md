# Evidence for the V04 §7 duplicate gap-"5" — assembled BEFORE any edit

**Executed:** 2026-08-18 · **OQ:** OQ-309 · **Substrate:** `docs/amnesiac_institution/V04_CONSOLIDATION_MANIFEST.md`

**Status: NO EDIT HAS BEEN MADE.** This file exists so the adjudication rests on evidence the
operator can see rather than on an instance's read. The disposition question is at the end.

## 1. The defect, as it stands on disk

```
192:1. **Appendix B's manifest is still mostly `[UNWITNESSED]`**, and several of its numbers moved
196:2. **The cross-coding result does not exist.** No matrix, no H5 floor. Always a subsection, never
198:3. **Items 14 and 15 cite entries by name rather than line**, because `audit_log.md` is
200:4. ~~This manifest has had no second reader.~~ **Discharged 2026-08-11**: the operator read it and
205:5. **A COMPLETENESS CLAIM IS SCOPED TO ITS SEARCH FRAME, AND THE FRAME IS A SELECTION RULE NOBODY
216:6. **Two self-inflicted counting defects, both in this document, both in a summary about the set
239:5. **Its own totals row was wrong on first write** and was caught by a script that recounted the
```

§7 ("Known gaps in this manifest") is an ordered markdown list whose literal numbering runs
**1, 2, 3, 4, 5, 6, 5** — two blocks numbered "5", at `:205` and `:239`.

## 2. Block A — `:205`, the FIRST "5"

```
5. **A COMPLETENESS CLAIM IS SCOPED TO ITS SEARCH FRAME, AND THE FRAME IS A SELECTION RULE NOBODY
   STATES.** This manifest's frame was *"repository files carrying forward-flags."* A producer that
   wrote into the **operator's context** rather than into a file is invisible to it — not missing,
   *unreachable* — and the inventory reads as complete because **every item in it belongs**.

   **This is the same species as the genre-based pin rule** (`SPEC_next_preregistration.md` §1):
   a defensible criterion, applied by everyone, never articulated, producing a manifest whose
   omission has no shape. Three instances now — the pin manifest (texts, not executables), the
   experiment design (before data lands, not after), and this manifest (files, not conversations).
   **State the frame; the omission is only visible from outside it.**

```

## 3. Block B — `:216`, item "6"

```
6. **Two self-inflicted counting defects, both in this document, both in a summary about the set
   below it** — instances eight and nine of the arc's signature:

   | # | where | claimed | actual | caught by |
   |---|---|---|---|---|
   | 8 | totals row | `18 / 6 / 4` | `19 / 5 / 4` | a script recounting the grade column |
   | 9 | prose header | `~20 producers … 12 files` | 8 files, 1 non-file | **the second reader** |

   **The script that caught the eighth could not catch the ninth**: it checks the grade column and
   the item count against the table, and **nothing checked the prose header against the table
   below it.** A check is scoped to what it reads, and a document's summary sentence is a claim
   like any other.

   **Re-run after ANY edit** — the second command is the one added in response to the ninth:

   ```bash
   # item count and grades, from numbered rows ONLY.
   # Trap: the legend's own W/R/U rows match a naive grade regex and inflate the total by 3.
   grep -cE '^\| [0-9]+ \|' V04_CONSOLIDATION_MANIFEST.md

   # header-vs-table: every figure in the header must be derived from §6.
   # There is no automatic check for this. Read the header and §6 side by side.
   ```
```

## 4. Block C — `:239`, the SECOND "5" (the candidate for removal)

```
5. **Its own totals row was wrong on first write** and was caught by a script that recounted the
   grades from the numbered rows: the header claimed `18 / 6 / 4`, the enumerated set is
   `19 / 5 / 4`. A summary line disagreeing with the set it summarises — in the totals row of a
   document whose stated purpose is to prevent exactly that, written by the instance that had just
   documented the pattern. Corrected, and recorded rather than silently fixed: **the check that
   caught it compared a claimed number against the artifact**, which is the only mechanism that has
   caught anything in this arc. Re-run it after any edit:

   ```
   grep -cE "^\| [0-9]+ \|" V04_CONSOLIDATION_MANIFEST.md      # item count
   # and recount the grade column from numbered rows only — the legend's W/R/U rows
   # match a naive grade regex and inflate the total by exactly 3.
   ```
```

## 5. Git evidence — Block C predates Blocks A and B, and was never revisited

The file has exactly two commits:

```
ede866c7 2026-08-11 v0.4 manifest: second-reader pass — 35 items; the frame excluded a producer
eeab8a33 2026-08-11 v0.4 consolidation manifest — 28 items, 8 producers, de-duplicated
```

**In `eeab8a33` (the original), §7 had five items and Block C was item 5:**

```
153:1. **Appendix B's manifest is still mostly `[UNWITNESSED]`**, and several of its numbers moved
157:2. **The cross-coding result does not exist.** No matrix, no H5 floor. Always a subsection, never
159:3. **Items 14 and 15 cite entries by name rather than line**, because `audit_log.md` is
161:4. This manifest was assembled by one instance in one pass. **It has had no second reader**, which
163:5. **Its own totals row was wrong on first write** and was caught by a script that recounted the
```

**The second-reader commit `ede866c7` INSERTED Blocks A and B above Block C and did not touch it.**
The diff hunk that adds them ends immediately before Block C's unchanged context lines:

```
+5. **A COMPLETENESS CLAIM IS SCOPED TO ITS SEARCH FRAME, AND THE FRAME IS A SELECTION RULE NOBODY
+   STATES.** This manifest's frame was *"repository files carrying forward-flags."* A producer that
+   wrote into the **operator's context** rather than into a file is invisible to it — not missing,
+   *unreachable* — and the inventory reads as complete because **every item in it belongs**.
+
+   **This is the same species as the genre-based pin rule** (`SPEC_next_preregistration.md` §1):
+   a defensible criterion, applied by everyone, never articulated, producing a manifest whose
+   omission has no shape. Three instances now — the pin manifest (texts, not executables), the
+   experiment design (before data lands, not after), and this manifest (files, not conversations).
+   **State the frame; the omission is only visible from outside it.**
+
+6. **Two self-inflicted counting defects, both in this document, both in a summary about the set
+   below it** — instances eight and nine of the arc's signature:
+
+   | # | where | claimed | actual | caught by |
+   |---|---|---|---|---|
+   | 8 | totals row | `18 / 6 / 4` | `19 / 5 / 4` | a script recounting the grade column |
+   | 9 | prose header | `~20 producers … 12 files` | 8 files, 1 non-file | **the second reader** |
+
+   **The script that caught the eighth could not catch the ninth**: it checks the grade column and
+   the item count against the table, and **nothing checked the prose header against the table
+   below it.** A check is scoped to what it reads, and a document's summary sentence is a claim
+   like any other.
+
+   **Re-run after ANY edit** — the second command is the one added in response to the ninth:
+
+   ```bash
+   # item count and grades, from numbered rows ONLY.
+   # Trap: the legend's own W/R/U rows match a naive grade regex and inflate the total by 3.
+   grep -cE '^\| [0-9]+ \|' V04_CONSOLIDATION_MANIFEST.md
+
+   # header-vs-table: every figure in the header must be derived from §6.
+   # There is no automatic check for this. Read the header and §6 side by side.
+   ```
 5. **Its own totals row was wrong on first write** and was caught by a script that recounted the
    grades from the numbered rows: the header claimed `18 / 6 / 4`, the enumerated set is
    `19 / 5 / 4`. A summary line disagreeing with the set it summarises — in the totals row of a
```

The three lines with a leading space at the end are **context, not additions** — Block C is
byte-identical across both commits. It is the pre-correction text; nothing in the second-reader
pass renumbered or removed it.

## 6. Content evidence — Block B subsumes Block C

| | Block C (`:239`, the stale "5") | Block B (`:216`, item 6) |
|---|---|---|
| the defect | totals row claimed `18 / 6 / 4`, set is `19 / 5 / 4` | **row 8 of its table**: `totals row \| 18 / 6 / 4 \| 19 / 5 / 4 \| a script recounting the grade column` |
| what caught it | "a script that recounted the grades from the numbered rows" | same, in the `caught by` column |
| the lesson | "the check that caught it compared a claimed number against the artifact" | same, plus the *second* defect (the prose header) the script could not catch |
| the re-run block | 2 commands (item count; recount grades) | **the same 2 commands plus a third instruction**, with the note *"the second command is the one added in response to the ninth"* |

Block B is a strict superset: same defect, same catcher, same lesson, same commands, **plus** the
ninth defect and the scoping finding ("a check is scoped to what it reads"). Block C carries exactly
one sentence not restated in Block B — *"A summary line disagreeing with the set it summarises — in
the totals row of a document whose stated purpose is to prevent exactly that, written by the instance
that had just documented the pattern."*

## 7. Corroboration from outside the file

`amnesiac_institution_v0_6.md:1444-1445` (§7.4's nine-instance table) records both defects as
**separate numbered instances of one arc**, which is Block B's framing and not Block C's:

```
| 8 | a consolidation manifest built to prevent double-counting | its own totals row disagreed with the table beneath it | a script that recounted the table |
| 9 | the same manifest | its summary sentence preserved both a wrong figure *and* its correction, in different grammatical roles | a second reader |
```

## 8. What the evidence supports, and what it does not

**Supported:** Block C is the pre-second-reader text; Block B was written to supersede it and
restates every load-bearing element of it; leaving both produces a list numbered `1,2,3,4,5,6,5`,
which is a *silent renumber* hazard of exactly the species §5.2 of the paper forbids for the
taxonomy's own indices.

**Not supported by this evidence:** that Block C should be *deleted* rather than *renumbered*. The
second-reader commit did not state an intent to remove it, and one sentence of its prose is unique.
Deleting it destroys that sentence; renumbering it to `7` preserves it at the cost of leaving a
now-redundant entry in a document whose subject is redundant counting.

## 9. The disposition question — the operator's call

Three defensible dispositions, none of which an instance should pick unilaterally, because the
document is a point-in-time record and the choice trades a *correctness* value against a *record
integrity* value:

- **(a) Renumber Block C to `7`.** Minimal, reversible, preserves the unique sentence and the
  document's history. Leaves a redundant gap entry.
- **(b) Delete Block C, first folding its one unique sentence into Block B.** Removes the
  redundancy; edits a dated record; the folded sentence acquires a new context.
- **(c) Delete Block C outright.** Cleanest list; loses the sentence.

**Recommendation: (a).** It is the only option that is purely additive to the record, and the
manifest is explicitly a point-in-time artifact (`audits/README.md`: existing dirs are point-in-time,
never renamed). It also leaves the duplicate-numbering *incident* legible as the tenth instance of the
arc's signature — a summary structure disagreeing with itself — which (b) and (c) both erase.

**Whichever is chosen, the self-checks run after the edit:** `/usr/bin/grep -cE '^\| [0-9]+ \|'`
(item count, must stay 35) and the manual header-vs-§6 read.
