# v0.4 consolidation manifest

**Assembled:** 2026-08-11 · **Scope:** every item flagged forward to *The Amnesiac Institution*
across the OQ-277 arc and its predecessors.

**This is an inventory. No prose, no argument, no restructuring.** It exists because the paper is
a consumer that has not run: ~20 producers wrote flagged items into 12 files under a one-writer
boundary that correctly prevented anyone from consolidating them. Its purpose is to make v0.4
writable in one pass rather than an archaeology expedition.

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
| producer files with genuine forward-flags | **8** (5 OQ-277 arc + literature memo + A5_leak_check + audit_log) |
| files matching the flag patterns | 13 (5 excluded above, with reasons) |
| **distinct items** | **28** |
| raw flag lines across all sources | ~48 — **never used as an item count** |
| ⧉ frozen-stratum items (single point of failure) | **11** |
| grade **W** / **R** / **U** | **19 / 5 / 4** (= 28, recomputed from the numbered rows) |
| **U items that BLOCK v0.4** | **1** — item 17, the P3 ruling (Ω_C, operator's seat) |

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
4. This manifest was assembled by one instance in one pass. **It has had no second reader**, which
   under §4.2's own standard makes it a producer whose consumer has not yet run.
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
