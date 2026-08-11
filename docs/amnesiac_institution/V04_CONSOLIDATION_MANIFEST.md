# v0.4 consolidation manifest

**Assembled:** 2026-08-11 · **Scope:** every item flagged forward to *The Amnesiac Institution*
across the OQ-277 arc and its predecessors.

**This is an inventory. No prose, no argument, no restructuring.** It exists because the paper is a
consumer that has not run: items were written into **8 repository files** (13 matched the search
patterns; 5 excluded with reasons, §5) plus **one non-file producer** (§4b), under a one-writer
boundary that correctly prevented anyone from consolidating them. Its purpose is to make v0.4
writable in one pass rather than an archaeology expedition.

> **This sentence was wrong on first write and is the manifest's second self-inflicted counting
> defect** (§7.5–7.6). It read *"~20 producers wrote flagged items into 12 files."* Neither figure
> survived §6: there are **8** producer files, not 12, and *"~20"* was an uncorrected estimate.
> The instructive part is the mechanism — **12 was the correction to ~20**, the corrected quantity
> was *files*, and it landed in the header **alongside** the number it replaced rather than in
> place of it. The sentence preserved both the error and its correction in different grammatical
> roles, and read as complete. **Every figure in this header is now derived from §6, and §7.6's
> check enforces that.**

---

## How to read this, and the two traps it is built to avoid

**Counting rule.** Items are counted **once, at their canonical location**. Where a source is
inlined verbatim elsewhere, the inlined copy is recorded as a *location*, never as an item.
`PREREGISTRATION.md` inlines `verdict_grammar_amendment.md` in full: a grep-and-tally over both
yields **22 where there are 11**, which is this arc's exact defect profile — a plausible number
produced by counting one thing at two addresses. **Totals below are asserted against the
enumerated set, not summed from per-file counts.**

**Second-order duplication, also handled:** one item often carries several flag lines. The
amendment has **12 flag lines** resolving to **8 distinct items**. Flag-line counts and item
counts are reported separately and never interchanged.

**Grades.**

| grade | meaning | licence at v0.4 |
|---|---|---|
| **W** | witnessed finding — has a pasted run, diff, or count | assertable |
| **R** | declared residue — real, scoped, deliberately not closed | assertable *as a residue*, with its scope |
| **U** | unresolved flag — noted, not adjudicated | **not assertable**; needs a ruling or a run first |

**⧉ FROZEN STRATUM — the flag that matters most.** Items marked **⧉** live inside
`verdict_grammar_amendment.md`, which is incorporated verbatim into the **frozen**
`PREREGISTRATION.md` (md5 `4118f64e`, a run was made under it). That document **cannot be edited**
— `--write` refuses on a recorded freeze stamp, and amending an incorporated source after results
exist would invalidate the freeze retrospectively. Confirmed in practice this arc: the §6.4
material written on 2026-08-11 had to be routed below the results sentinel because §L could not
take it.

> **Consequence: for ⧉ items this manifest is the only channel to v0.4.** Every other item can be
> re-read at its source if this file is lost. These cannot be edited in place, so an ⧉ item exists
> in exactly two locations — a frozen document nobody may touch, and here. **They are transcribed
> rather than pointed at, deliberately.** A later reader who finds this stratum unusually verbose
> should read that as the single-point-of-failure mitigation it is, not as redundancy to trim.

---

## 1. Frozen stratum — `verdict_grammar_amendment.md` ⧉

*Canonical location. Also inlined at `PREREGISTRATION.md` (verbatim, whole file) — that is a
location, not a second set of items.* **12 flag lines → 8 items.**

| # | item | source | target | grade |
|---|---|---|---|---|
| 1 | Cross-family falsifier is higher-value against its cost because another family shares neither our conventions nor plausibly the same exposure to Wu's artifacts; the writeup must record it as **named-but-not-bought this run** | `:191` §F | §4.4 / §9.3 | **R** |
| 2 | Three dated instrument defects from one arc, written up **ONCE as a set**, not three times — a stronger section than the hypothetical no-op harness §6.4 currently argues from | `:204` §G.2 | §6.4 | **W** |
| 3 | **The control architecture caught ZERO of the three defects that occurred inside its own instruments** — all three found by suspicion, none by any control. Sharpens what the `Fired:` bit can measure (OQ-276): a catch rate computed over controls does not see catches arriving by suspicion | `:225` §G.2 | §6.4 + §8.2 | **W** |
| 4 | **Count what the rule claims to produce** — catches rule defects that reading the rule does not; the cheap general form of the recursion, and unlike the recursion **it terminates** | `:479`, `:497` §L/§L.1 | §6.4 | **W** |
| 5 | **An instrument's error profile is a property of its ROLE, not of the instrument** — validating in one role licenses nothing about another. The one worked example that is *not* a coding bug: the matcher was correct, the reuse was the defect | `:748` §Q | §6.4 | **W** |
| 6 | **Ask at what denominator the decision is load-bearing**, and whether the number in hand was measured at that denominator or at a convenient one | `:850` §N | §6.4 | **W** |
| 7 | The seventh vacuous check — the first **inside the instrument built to prevent vacuous checks**; operator-ruled as the one that *closes* the argument. **The recursion terminates in someone counting, not in a deeper instrument, and §6.4 should say that plainly** | `:900`, `:920`, `:931` §L.6 | §6.4 | **W** |
| 8 | **A check can be correct and still unreadable** — vacuity at an *interface* rather than in a check's logic; plus two nested recurrences recorded rather than smoothed | `:1059`, `:1088` §L.9 | §6.4 | **W** |

### 1b. Frozen findings NOT explicitly flagged to a section ⧉

*Same single-point-of-failure exposure; no target was named by their authors.*

| # | item | source | suggested target | grade |
|---|---|---|---|---|
| 9 | **§L.7** — the arc's best-evidenced control by *fixture* grade: 4 real declines + 1 catch on real material, and it caught its own author | `verdict_grammar_amendment.md` §L.7 | §4.2 | **W** |
| 10 | **§L.8** — the second unplanted fire, graded explicitly *below* §L.7 (strong fire side, weak decline side) | §L.8 | §4.2 | **W** |
| 11 | **§O.3** — the P6 calibration gap is ONE incident in three instruments, not three residues; correlated, not additive | §O.3 | §4.3 / §5 | **W** |

---

## 2. Results stratum — `audit_log.md`, below the sentinel

*Written after the freeze; **routing note**: §6.4's assembler must read below the sentinel as well
as above it, because the amendment could not take these.*

| # | item | source | target | grade |
|---|---|---|---|---|
| 12 | **The recursion is not hypothetical — it is the observed behaviour of every repair in this arc.** Five instances, and **not one was caught by a gate**; every one by comparing a claimed number against the artifact | `audit_log.md:403` | §6.4 | **W** |
| 13 | **The detector reported ITSELF.** The instrument for finding unwired controls named itself on its first run — the recursion in one line, by its own criterion. The transferable half is the handling: **the only thing separating a legitimate exemption from an invisible one is whether someone wrote it down** | `:441`, `:461` | **§6.4 closing; probably replaces §9.5** | **W** |
| 14 | Stamp evidence: the driver was never pinned; the analysis half does not exist; two pinned sources have drifted | `:` (stamp-ruling entry) | §5 / §8.3 | **W** |
| 15 | The **live-run loss**: 219 calls, gates all green, nothing persisted | `:` (live-run entry) | §6.4 / §8.3 | **W** |

---

## 3. Other OQ-277 producers

| # | item | source | target | grade |
|---|---|---|---|---|
| 16 | **P3 correction owed BEFORE v0.4 restates §4.3** — a defect nobody named will not carry the words | `RULING_2026-08-11_freeze_scope.md:236` | §4.3 | **U** |
| 17 | If P3's records are **prevention records, not failures**, the honest v0.4 statement is that P3 is a *discipline*, not a defect pattern — taxonomy becomes five plus a rule (**structural, not editorial**) | `RULING…:270` | §4.3 | **U — BLOCKING** |
| 18 | The §6.4 **tally** (running count of instrument defects) — flagged twice as *needing to land*, currently in the other instance's amendment | `EXTRACTION_NOTES.md:326`, `:329`, `:349` | §6.4 | **U** |
| 19 | The unit-06 leak **was caught by the sweep**, which bears on §6.4's honest limit — a limit §6.4 declares and has not closed | `EXTRACTION_NOTES.md:320`, `:324` | §6.4 | **W** |
| 20 | §6.4's recursion with a **concrete instance instead of a hypothetical no-op harness**; quote the assertion and its failure | `HANDOFF.md:197` | §6.4 | **W** |
| 21 | Queue for v0.4 **alongside the OQ-280 §2.3 correction** | `RECON.md:123` | §2.3 | **U** |
| 22 | The E↔P6 correspondence must be carried as a **PROPOSED** mapping row, not a ruled one | `RECON.md:166` | §4.3 / §9.2 | **R** |
| 23 | The `permission class` pair — instructive false-positive/true-negative pair | `RECON.md:251` | §4.2 | **R** |

---

## 4. Outside the arc

| # | item | source | target | grade |
|---|---|---|---|---|
| 24 | **Literature memo — written explicitly "for v0.4"**; the whole document is a producer | `literature_verification_memo.md:1` | §9.1 | **W** |
| 25 | Review-by-reading is weak → §3.2 and §6.4 | `…memo:75` | §3.2, §6.4 | **W** |
| 26 | An external corpus whose **denominators are trajectories, not audit directories** — bears on the §6.4 recursion problem | `…memo:147` | §6.4 | **R** |
| 27 | An external result that is **better evidence than anything in the repository** for §6.4 | `…memo:221` | §6.4 | **W** |
| 28 | A writeup obligation recorded by the executing instance | `audits/2026-08-09_oq262_coexists_severance/A5_leak_check.md:7` | §4.2 | **R** |

---

## 4b. The non-file producer — the operator's working conversation

**Added 2026-08-11 by the second reader.** §1–4 inventory items flagged forward **in repository
files**. The working conversation is also a producer, and items generated there went into
`build_discipline.md` or `CLAUDE.md` — or nowhere — **without ever acquiring a needs-to-land note
that the search would find**. They are therefore invisible to the frame, not absent from the arc.

**All seven are grade U: none has been adjudicated as paper content.** Three (29, 30, 33) would
change **what a section claims**, not merely what it cites — flagged **‡**.

| # | item | current home | target | grade |
|---|---|---|---|---|
| 29 ‡ | **The instrument stratum.** §4.3's patterns are illustrated by *substrate* incidents, but nearly every defect this arc caught was in an **instrument**, with a different failure signature — a plausible number about the *wrong object*. §4.5's 42% is a rate over **one stratum**, and the other has never been measured | conversation only | §4.3, §4.5 | **U** |
| 30 ‡ | **Restraint efficacy.** §9.3's four efficacies are all about controls *firing*. Nothing represents **a plausible escalation declined on evidence**, and this arc declined at least eight. The `Fired:` bit has **no encoding** for a decline | conversation only | §9.3, §8.2 | **U** |
| 31 | **Differential amnesia.** The operator forgets detail and retains shape; instances forget everything and retain nothing. Reorganizes §5's mechanism inventory **by which kind of forgetting each tool addresses**, and identifies **git as the cross-type one** | conversation only | §5 | **U** |
| 32 | **Imposed forgetting.** §7.6 frames consolidation as damage control. The monthly pass is the operation that **produces** the general instruction set, the recent history and the frontier — not cleanup after them. Unbounded retention is not memory, it is a pile | conversation only | §7.6 | **U** |
| 33 ‡ | **The second jurisdiction.** §8.2 confines the human to *value* decisions. Noticing the same check red three times across sessions is **evidential, not value-laden**, and structurally unavailable to any instance | conversation only | §8.2 | **U** |
| 34 | **A positive control demonstrates DISCRIMINATION, not detection**, with the role-dependence corollary (an instrument validated in one role is a new instrument in another) | `build_discipline.md` — landed, **unflagged to the paper** | §6.3 | **U** |
| 35 | **Gate the output, not only the input**, and *a gate row never seen red is a row, not a check* | `build_discipline.md`, `CLAUDE.md` — landed, **unflagged** | §6.3 / §6.4 | **U** |

**Items 34–35 are findable; 29–33 are not.** The first pair reached repository files and can be
re-read at source. The other five exist only in a conversation, which makes their exposure the same
as the ⧉ frozen stratum's for the opposite reason: not *unamendable*, but *unlocated*.

---

## 5. Excluded, with reasons — checked, not skipped

The hypothesis that 2026-02 → 2026-07 audits carry lost forward-flags **was tested and did not
hold.** Those files matched only on broad patterns, for these reasons:

| source | why excluded |
|---|---|
| `epsilon_substrate_dependency_report.md:340` | **Its "§6.4" is a DIFFERENT paper's section** (the DR paper's declaration discipline). No reference to the amnesiac paper anywhere in the file. **An index collision of the OQ-278 species** — same number, different document — and folding it in would have imported a foreign claim under a matching address |
| `amnesiac_institution_v0.2.md:309`, `v0.3.md:331` | internal cross-references (`failure modes (§6.4)`), not forward-flags |
| `HANDOFF_EXTRACTOR_B.md:44`, `HANDOFF_TWINS_AND_DRIVER.md:49` | pointers to `HANDOFF.md`'s obligations — locations, not items |
| `PREREGISTRATION_threshold_calibration.md:313` | concerns judge behaviour on a redacted paragraph, not a paper section |
| `kritik_ingest/SCORING.md:81`, `false_ci_rope`, `oq232`, `oq259` | "for the writeup" refers to **their own** audit writeups |

---

## 6. Totals — asserted against the enumerated set

| quantity | value |
|---|---|
| producer **files** with genuine forward-flags | **8** (5 OQ-277 arc + literature memo + A5_leak_check + audit_log) |
| **non-file** producers | **1** — the working conversation (§4b) |
| files matching the flag patterns | 13 (5 excluded above, with reasons) |
| **distinct items** | **35** (28 from files, 7 from §4b) |
| raw flag lines across all sources | ~48 — **never used as an item count** |
| ⧉ frozen-stratum items (unamendable; manifest is their only channel) | **11** |
| §4b items **unlocated** outside this manifest (29–33) | **5** |
| grade **W** / **R** / **U** | **19 / 5 / 11** (= 35, recomputed from the numbered rows) |
| **U items that BLOCK v0.4** | **1** — item 17, the P3 ruling (Ω_C, operator's seat) |
| **‡ items that change what a section CLAIMS** (not what it cites) | **3** — 29, 30, 33 |

**Total asserted by enumeration**, not by summing per-file greps. The per-file sum would read 39+
because the amendment is counted at two addresses and multi-line items counted per line.

---

## 7. Known gaps in this manifest

1. **Appendix B's manifest is still mostly `[UNWITNESSED]`**, and several of its numbers moved
   during this arc (175→174, the census defect, the (iii′) enumeration, 198→219, the incidence
   figure's three blind spots). Not a blocker for drafting; **a blocker before circulation**. This
   manifest is the input to that pass, not a substitute for it.
2. **The cross-coding result does not exist.** No matrix, no H5 floor. Always a subsection, never
   the spine — but §4.5 cannot cite it.
3. **Items 14 and 15 cite entries by name rather than line**, because `audit_log.md` is
   append-only and its line numbers move.
4. ~~This manifest has had no second reader.~~ **Discharged 2026-08-11**: the operator read it and
   returned two findings — the header/totals contradiction (§7.6) and the excluded producer (§4b).
   Independently recomputed and holding: 28 file-derived numbered rows, grades 19/5/4 over them,
   frozen stratum 11, and the eight source files matching §6's parenthetical breakdown exactly.

5. **A COMPLETENESS CLAIM IS SCOPED TO ITS SEARCH FRAME, AND THE FRAME IS A SELECTION RULE NOBODY
   STATES.** This manifest's frame was *"repository files carrying forward-flags."* A producer that
   wrote into the **operator's context** rather than into a file is invisible to it — not missing,
   *unreachable* — and the inventory reads as complete because **every item in it belongs**.

   **This is the same species as the genre-based pin rule** (`SPEC_next_preregistration.md` §1):
   a defensible criterion, applied by everyone, never articulated, producing a manifest whose
   omission has no shape. Three instances now — the pin manifest (texts, not executables), the
   experiment design (before data lands, not after), and this manifest (files, not conversations).
   **State the frame; the omission is only visible from outside it.**

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
