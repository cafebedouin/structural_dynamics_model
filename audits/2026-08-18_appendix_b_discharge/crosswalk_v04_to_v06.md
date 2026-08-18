# Crosswalk: `V04_CONSOLIDATION_MANIFEST.md` (35 items) → `amnesiac_institution_v0_6.md`

**Executed:** 2026-08-18 · **OQ:** OQ-309 · **Substrate:** `docs/amnesiac_institution/V04_CONSOLIDATION_MANIFEST.md`
(35 numbered rows, assembled 2026-08-11) against `docs/amnesiac_institution/amnesiac_institution_v0_6.md`
(2,582 lines; v0.6 dated 2026-08-12, §5 amended 2026-08-17).

## How to read this table, and why it is addressed by anchor text

**Targets in the manifest are v0.3/v0.4 section numbers**, and v0.6 renumbered almost everything:
v0.4 §2.1–2.5 (Methods) → v0.6 §4.1–4.5; v0.4 §4.3 (patterns) → v0.6 §5.1; v0.4 §4.3.1 → v0.6 §5.2;
v0.4 §4.5 (incidence) → v0.6 §5.4; v0.4 §5.1/§5.2/§5.3 → v0.6 §6.1/§6.4/§6.5; v0.4 §6.3/§6.4/§6.4.1/§6.7
→ v0.6 §7.3/§7.4/§7.4.1/§7.7; v0.4 §7.3/§7.6 → v0.6 §8.2/§8.5; v0.4 §8.2(+§8.2.1) → v0.6 §9.2;
v0.4 §9.3/§9.4/§9.5 → v0.6 §10.3/§10.4/§10.5. **v0.6's own §6.4 is a DIFFERENT section** (*The
convergence question*) from the §6.4 that 13 manifest rows point at (the recursion, now §7.4) — a live
index collision of the OQ-278 species, which is why every row below carries an **anchor-text quote**
rather than a section number alone. A v0.7 renumber invalidates the § column and not the quote.

Two targets in the manifest do not resolve to a plausible section under any version's numbering and
are recorded as **target-as-written unresolved**: item 24's `§9.1` (v0.3/v0.4 §9.1 is *The hazard*,
self-instrumentation — the literature memo has no business there) and the `§4.2` shared by items 9, 10,
23 and 28 (v0.3/v0.4 §4.2 is *The design principle*, while all four rows are control-quality items that
belong at v0.4 §6.3 / v0.6 §7.3). Those rows are adjudicated **by content**, and the mismatch is
recorded rather than silently repaired.

**Scope of the NOT-LANDED verdicts, stated because it bounds them.** Each verdict rests on a
*reading* of the v0.6 sections the item could plausibly have landed in (§§3.3, 4.2–4.5, 5.1–5.4,
6.2–6.5, 7.3, 7.4, 7.4.1, 7.7, 8.2–8.5, 9.2, 10.3–10.5, 14, Appendix B) — not on a line-by-line read
of all 2,582 lines — supplemented by keyword probes with two-sided controls, saved at
`evidence/crosswalk_absence_probes.txt`. **The probe run caught a defect in itself and the record is
kept:** the file is hard-wrapped, so a line-oriented `grep` silently misses any phrase straddling a
newline; the first run scored a control phrase that IS present as 0 hits. Multi-word probes are
re-run against a whitespace-flattened copy. A NOT-LANDED verdict here means *"absent from the
sections where it belongs, and absent from a controlled keyword sweep of the whole file"* — not
*"absent from the document"* at a strength no sweep of this kind can license.

**Verdicts.** `LANDED` — the content is in v0.6, quote given. `PARTIAL` — part landed, the missing half
named. `NOT-LANDED` — checked, absent. `SUPERSEDED` — a later ruling replaced the item's own premise.
`RESIDUE-U` — carried forward as open under OQ-309. `OPERATOR` — routed to the operator's seat.

---

## §1 — Frozen stratum ⧉ (`verdict_grammar_amendment.md`), items 1–8

| # | grade | target (as written) | v0.6 landing | anchor text in v0.6 | verdict |
|---|---|---|---|---|---|
| 1 | R | §4.4 / §9.3 | §14 RQ2 `[COST CORRECTED]` box | *"Blind-code Wu's five mechanism classes against the seven patterns **in both directions**… It was attempted (ISSUES OQ-277)… The item is still worth doing… it is not a weekend."* | **LANDED** — the cross-family comparison is named, priced, and explicitly not bought. The *residue* form the item asked for is the form it took. |
| 2 | W | §6.4 | §7.4 (nine-row table) | *"A single pre-spend arc in August 2026 produced nine instances of the apparatus committing, inside its own repairs, the defect the repair was addressing."* | **LANDED** — and superseded upward: written up once as a set, at nine rather than three. |
| 3 | W | §6.4 + §8.2 | §7.4 property paragraph + §10.5 | *"**The property of the set: not one of the nine was caught by a gate.**"* / *"§7.3 requires that a control's licence come from a case it **declined**; this instrument has never declined"* | **LANDED** — both halves: the zero-catch finding and its consequence for what the `Fired:` bit can measure. |
| 4 | W | §6.4 | §7.4 property paragraph | *"Every one was caught by a person or a script **comparing a claimed number against the artifact it described** — a diff, a directory listing, a file count, a re-read."* | **LANDED** in generalized form. The item's own phrasing (*count what the rule claims to produce*) does not appear as a named rule; the mechanism it names is the section's thesis. |
| 5 | W | §6.4 | §7.3 | *"a cross-role reuse is a *new* instrument owing its own decline. A matcher whose false positives were conservative as a *detector* becomes silently decisive as a *selection metric*: the error profile belongs to the role."* | **LANDED** verbatim in substance, including the worked example. |
| 6 | W | §6.4 | §5.4 + §6.2 | *"accurate at the framing it was formed at and silent about the instrument stratum until someone asked what the denominator ranged over"* / *"**Do not compare 45–75% to this paper's 42%.** Advani's denominator is *failures*; this paper's is *audit directories*."* | **LANDED** as two worked instances. Not stated as a standalone rule ("ask at what denominator the decision is load-bearing"); the instances carry it. |
| 7 | W | §6.4 | §7.4 | *"The recursion does not terminate in a deeper instrument, because a deeper instrument is another claim. It terminates in someone counting."* | **LANDED**, and this is the sentence the item asked for "plainly". |
| 8 | W | §6.4 | — (nearest: §5.2.1) | nearest anchor: *"A member present at an index but taught nowhere the reader will meet it is, at the read site, indistinguishable from absent."* | **NOT-LANDED.** The *vacuity-at-an-interface* framing (a check correct in its logic and unreadable at its interface) is absent; §5.2.1 carries an adjacent shape for a taxonomy entry, not for a check. The two nested recurrences are not carried. → **RESIDUE-U**. |

## §1b — Frozen findings with no author-named target ⧉, items 9–11

| # | grade | target (as written) | v0.6 landing | anchor text in v0.6 | verdict |
|---|---|---|---|---|---|
| 9 | W | §4.2 *(unresolved — see header)* | — | §7.3 carries a *different* best-graded control: *"a detector that names exactly the two affected functions at the commit that orphaned them, and returns empty at the commit before."* | **NOT-LANDED.** §L.7's fixture-graded control (4 real declines + 1 catch on real material; it caught its own author) does not appear. → **RESIDUE-U**. |
| 10 | W | §4.2 *(unresolved)* | — | — | **NOT-LANDED.** §L.8, the second unplanted fire graded explicitly below §L.7, does not appear. Its value is precisely the *graded pair*, which is the shape §7.3's discrimination ladder wants and lacks a worked instance of. → **RESIDUE-U**. |
| 11 | W | §4.3 / §5 | — | — | **NOT-LANDED.** No occurrence of the correlated-not-additive reading of the P6 calibration gap (controlled sweep: 0 hits). The paper counts the three census instrument defects as *independent* (§5.4: *"the losses are independent"*), which is a claim about a different triple — recording this row matters because a reader could mistake one for the other. → **RESIDUE-U**. |

## §2 — Results stratum (`audit_log.md` below the sentinel), items 12–15

| # | grade | target (as written) | v0.6 landing | anchor text in v0.6 | verdict |
|---|---|---|---|---|---|
| 12 | W | §6.4 | §7.4 | *"Through v0.3 this section argued the recursion from a *constructed* example. It no longer needs to."* + the nine-row table | **LANDED**, at nine instances rather than five. |
| 13 | W | §6.4 closing; *probably replaces §9.5* | §7.4.1 | *"A detector was written to find controls that nothing calls. **On its first run it named itself**… the exemption was written into the source with its reason."* | **LANDED.** The "replaces §9.5" half is **not** what happened: v0.6 §10.5 survives and was *extended* (the honest limit is now an act of documentation, with the exemption table). Recorded because the item proposed a replacement the paper declined. |
| 14 | W | §5 / §8.3 | §14 RQ2 box + §7.4.1 | *"the freeze pinned sixteen texts and **no executable**, and… the **analysis half — scorer, overlap-pair identification, matrix construction — exists in neither code nor design**."* | **PARTIAL.** Two of three stamp findings landed. *"Two pinned sources have drifted"* is absent. → **RESIDUE-U** (small). |
| 15 | W | §6.4 / §8.3 | §7.4 row 4 + §14 RQ2 box | *"it printed \"persisted 219\" from an in-memory counter while zero files existed"* / *"the live run **made all 219 calls and persisted nothing**, because every gate in the driver was an input gate"* | **LANDED** twice, at both altitudes. |

## §3 — Other OQ-277 producers, items 16–23

| # | grade | target (as written) | v0.6 landing | anchor text in v0.6 | verdict |
|---|---|---|---|---|---|
| 16 | U | §4.3 | §5.2 | *"The third — *destructive-replace without proof* — was **demoted to the witness calculus (§7.7) by operator ruling on 2026-08-11**."* | **LANDED.** The correction landed *before* the restatement, which is what the item demanded. |
| 17 | **U — BLOCKING** | §4.3 | §5.2 + §7.7 + §5.2.1 | *"the evidence says it is a **discipline** — a thing one does — rather than a **defect shape** — a way systems fail silently. It moves to §7.7 intact."* / §7.7: *"It is a witness rule, not a failure shape."* | **SUPERSEDED — and the block is DISCHARGED.** See the adjudication below. |
| 18 | U | §6.4 | §7.4 | the nine-row table itself (`# / the repair / the same defect, inside it / what caught it`) | **LANDED.** The tally exists, is numbered, and carries its denominator (*"Eight of the nine come from a **single arc**"*). |
| 19 | W | §6.4 | — | — | **NOT-LANDED.** The unit-06 leak, and the fact that the *sweep* caught it, appear nowhere (controlled sweep: 0 hits for `leak` and `unit-06`). This bears on §7.4's honest limit, which the section declares and does not close. → **RESIDUE-U**. |
| 20 | W | §6.4 | §7.4 | *"### 7.4 The recursion — observed, not hypothetical"* | **LANDED** — the hypothetical no-op harness is gone from the section title down. |
| 21 | U | §2.3 | §4.3 | *"Classification into the patterns was **retrospective and single-coder**… There was no blind coding, no second coder, and no inter-rater measurement."* | **PARTIAL.** The disclosure landed. The OQ-280 fork it was queued alongside — **perform** the coding or **amend** the section to state what actually happened — is still open and is the operator's choice (OQ-280 `Status: open`). Explicitly out of scope for this pass. → **OPERATOR** (already seated at OQ-280; not re-raised). |
| 22 | R | §4.3 / §9.2 | §6.2 mapping table | *"\| **E** — operational omission and forensic blind spots \| P1, P2, and §7.3's positive-control rule \| Strong."* | **SUPERSEDED, with residue.** The E↔P6 correspondence the item was about no longer exists — v0.6 maps **E→P1/P2** and **C→P6**. The item's *methodological* demand survives unmet: the table's Assessment column reads as ruled ("Strong", "**The strongest convergence**") and carries no PROPOSED/claim-tier marking, though §0 types every other claim in the paper. → **RESIDUE-U**. |
| 23 | R | §4.2 *(unresolved)* | — | — | **NOT-LANDED.** No `permission class` anywhere in v0.6 (controlled sweep: 0 hits). The pair's value was as a false-positive/true-negative *pair* — the shape §7.3 explicitly wants ("a naturally-arising negative drawn from the population"). → **RESIDUE-U**. |

## §4 — Outside the arc, items 24–28

| # | grade | target (as written) | v0.6 landing | anchor text in v0.6 | verdict |
|---|---|---|---|---|---|
| 24 | W | §9.1 *(unresolved — see header)* | §6.2, §6.3, §6.5, References | *"All four works below were verified against live sources on 2026-08-10 (§6.5)."* / *"### External — concurrent work on the failure class (verified 2026-08-10)"* | **LANDED.** The memo is consumed as a whole document across three sections plus the reference apparatus, including its verified/unverified split (§6.5's *"recorded as an open search, not as citations"*). |
| 25 | W | §3.2, §6.4 | §6.2 | *"This is independent, better-controlled support for §3.2's rejection of review-by-reading and for §7.4's recursion problem."* | **LANDED**, and the sentence even carries the renumber (v0.4 §6.4 → v0.6 §7.4) correctly. |
| 26 | R | §6.4 | §6.2 | *"**Do not compare 45–75% to this paper's 42%.** Advani's denominator is *failures*; this paper's is *audit directories*… The numbers are not commensurable."* | **LANDED** as a boxed non-comparability warning — a stronger form than the item asked for. |
| 27 | W | §6.4 | §6.2 | *"The same work establishes that LLM judges cannot detect it: no configuration exceeded 0.65 AUROC, and judges anchored on confident closing language rather than verified state change."* | **LANDED.** |
| 28 | R | §4.2 *(unresolved)* | — | — | **NOT-LANDED.** The writeup obligation recorded at `audits/2026-08-09_oq262_coexists_severance/A5_leak_check.md:7` is not discharged in v0.6. → **RESIDUE-U**. |

## §4b — The non-file producer (the operator's working conversation), items 29–35

| # | grade | target (as written) | v0.6 landing | anchor text in v0.6 | verdict |
|---|---|---|---|---|---|
| 29 ‡ | U | §4.3, §4.5 | §5.4 stratum-limit box | *"**42% is a rate over one stratum, and the other has never been measured.** The instrument stratum is not a subset of the audited directories."* | **LANDED**, and it is now the section's load-bearing qualification — carried again at §2.A and §7.4's denominator box. |
| 30 ‡ | U | §9.3, §8.2 | §10.3 | *"**A fifth efficacy the instrument cannot see at all — restraint.**… **The `Fired:` bit has no encoding for any of them**"* | **LANDED**, including the decline count ("at least eight") and the refusal to instrument it. |
| 31 | U | §5 | §3.3 (+ §9.2) | *"**Every party in this institution forgets. They forget different things, on different timescales, with different signatures, and each signature needs a different instrument.**"* | **PARTIAL.** The reorganization landed as a four-party table keyed by failure signature and instrument. **Git is not identified as the cross-type instrument** — §9.2 mentions version control among three mechanisms without the cross-type claim. → **RESIDUE-U** (small). |
| 32 | U | §7.6 | §8.5 | *"**Unbounded retention is not memory; it is a pile**"* | **LANDED**, and §8.5 is reframed as *"forgetting as the operation"* in its own heading — exactly the reframe the item asked for. |
| 33 ‡ | U | §8.2 | §9.2 | *"**The second is evidential, and it is created by amnesia rather than by the limits of verification.**… it is **structurally unavailable to any instance**, because each instance sees one session."* | **LANDED**, promoted into the section title (*two* jurisdictions). |
| 34 | U | §6.3 | §7.3 | *"**A positive control demonstrates discrimination, not detection.**… The witness that its firing carries information is a case it **declined**"* | **LANDED**, with the full grade ladder and the role-dependence corollary. |
| 35 | U | §6.3 / §6.4 | §14 RQ2 box + §10.5 | *"every gate in the driver was an input gate and no code path wrote responses"* / *"a falsifier that has never fired in the falsifying direction is precisely the shape it exists to catch"* | **LANDED.** Both halves present: gate-the-output as the worked instance, and the never-red-row rule applied to the apparatus's own falsifier. |

---

## Adjudication of item 17 (the one U-BLOCKING row)

**The item, verbatim** (`V04_CONSOLIDATION_MANIFEST.md:107`): *"If P3's records are **prevention
records, not failures**, the honest v0.4 statement is that P3 is a *discipline*, not a defect pattern
— taxonomy becomes five plus a rule (**structural, not editorial**)."*

**Its antecedent was tested and confirmed.** v0.6 §5.2 point 4: *"a failure-shape sweep of the full
repository history… returned, for the period in which the rule existed: five destructive commits,
four prevention records, one non-deletion, and **zero** cases of a deletion that had to be undone."*
That is the item's conditional, measured — and the sweep's ability to find was established
independently (three real delete-then-restore episodes from before the rule; it fires when witness
language is stripped).

**Its consequent was executed, by operator ruling, on the same day the item was written.** §5.2:
*"demoted to the witness calculus (§7.7) by operator ruling on 2026-08-11"*; §7.7's own header note:
*"Relocated here from the failure taxonomy on 2026-08-11 (§5.2). It is a witness rule, not a failure
shape."* The move was structural, not editorial: the rule changed *layer*, the index was vacated and
never reused, and the reason for not renumbering is stated (*"a visible gap is a checked fact; a
silent renumber is a fork"*).

**Its arithmetic was then superseded.** "Five plus a rule" was correct on 2026-08-11 and is no longer
the count: OQ-278's 2026-08-17 ruling settled index 4 and moved the peer member off the grave, giving
**seven members at eight indices** with the same member at every index in both documents,
machine-checked per index (`python3 python/doc_pattern_check.py --check`). §5.2.1 records this.

**Verdict: SUPERSEDED / LANDED. No residue survives, and nothing is routed to the operator.**
Both halves of the conditional are discharged by dated, witnessed history that postdates the item,
and the ruling was the operator's own — so no instance is self-ruling anything here (§9.2). The
manifest's `U items that BLOCK v0.4` count of **1** is therefore **0** as of 2026-08-18.

**One thing the discharge does NOT cover, recorded so it is not read as closed:** item 17 asked what
P3's *records* are. The four prevention records it names are, by §7.7's own account, a rule whose
enforcement is *"unusually cheap and unusually often skipped"* — and §7.7 declares this very document
its most visible current violation. That is a declared exemption in the paper, not a residue of item 17.

---

## Roll-up

| verdict | items | count |
|---|---|---|
| **LANDED** | 1, 2, 3, 4, 5, 6, 7, 12, 13, 15, 16, 18, 20, 24, 25, 26, 27, 29, 30, 32, 33, 34, 35 | 23 |
| **PARTIAL** | 14, 21, 31 | 3 |
| **SUPERSEDED** | 17, 22 | 2 |
| **NOT-LANDED** | 8, 9, 10, 11, 19, 23, 28 | 7 |

23 + 3 + 2 + 7 = **35**, checked against the row count (`/usr/bin/grep -cE '^\| [0-9]+ \|'` over the
manifest = 35). Item 21 is counted once, as PARTIAL, and additionally flagged **OPERATOR** because its
open half is OQ-280's perform-vs-amend fork — a seat, not a second verdict. Item 17 is counted once,
as SUPERSEDED, and its U-BLOCKING flag is discharged.

**Residue carried to OQ-309:** items 8, 9, 10, 11, 14 (partial half), 19, 22, 23, 28, 31 (partial
half) — ten rows, all grade W or R in the original manifest, none of them blocking. Item 21's fork
stays at OQ-280 and is not re-raised here.

**What the walk changes in the manifest's own totals.** `U items that BLOCK v0.4` goes **1 → 0**
(item 17 discharged). The `19 W / 5 R / 11 U` grade distribution is unchanged — grades record what the
manifest knew on 2026-08-11 and are not re-graded by a later landing; the landing status is this
crosswalk's column, deliberately kept in a separate document so the manifest stays a point-in-time
record (`audits/README.md`: existing dirs are point-in-time, never renamed).
