# OQ-277 — RQ2 two-directional blind cross-coding (Wu A–E × published P1–P6): RETIRED PRE-SPEND, no matrix exists and none will

**Executed:** 2026-08-10
**OQ:** OQ-277 (experiment, CLOSED 2026-08-12), OQ-278 (fork-residue row), OQ-280 (the corpus §2.3 never produced)
**Verdict:** **The experiment was RETIRED before any coding spend, by operator ruling 2026-08-12,
because its comparison set self-disagrees under its own author's hand** — Wu's failure-modes
catalog and his labelled dataset carry the same 22 incident IDs and **agree on class for 12 of the
22 (55% agreement); the remaining 10 disagree.** A confusion matrix indexed against a reference
that reproduces against itself at that rate cannot separate *our taxonomy disagrees with Wu's* from
*Wu's disagrees with itself*, which is the one ambiguity RQ2 existed to resolve. **Nothing was
coded, no model call produced a persisted response, and no matrix exists** — this directory is
input material and method record, never results. Scope caveat: the retirement is a judgement about
*this comparison set*, not about either taxonomy; §4.1 of `../../docs/when_apparatus_sharpens_taxonomy.md`
holds open that a fork like Wu's may be purpose-relative labelling rather than error, and this audit
did not rule which.
**Substrate:** no pipeline run — this audit's substrate is the audit record itself
(`audits/`, 174 directories as of 2026-08-10) plus Wu, **arXiv:2606.14589v1**. The paper is
cited by its permanent arXiv identifier, not by a repository path: a local convenience copy
sits under `docs/amnesiac_institution/` but `*.pdf` is gitignored, so a fresh clone would not
have it and a path citation would be unreproducible. The material this audit actually codes —
Wu's failure-modes catalog and his labeled incident dataset — is frozen inside this directory
under `packets/wu_source/` with md5s, and IS tracked.
**Fired:** live — two plan-stage findings were independently verified before any spend and
are already minted: the pattern taxonomy is FORKED between `CLAUDE.md` and
`docs/technical/build_discipline.md` (OQ-278), and v0.3 §2.3 describes a P1–P6 classification
that produced no artifact (OQ-280). A third, smaller catch landed during step 1: Appendix B's
§4.5 denominator included an empty placeholder directory (73/175 → 73/174; headline 42%
unchanged). None of these required the experiment to run — the experiment's *design* surfaced
them.

## Close (2026-08-12) — the instrument cannot answer the question it was built for

The close is authoritative in `../../ISSUES.md` → **OQ-277 → `### CLOSE (2026-08-12)`**, with its
*Disposals* and *Revival conditions* blocks; that entry governs, and this section only points at it
so a cold reader arriving here first is not misled. In summary, as recorded there:

- **Not a sunk-cost retreat and not a resource deferral.** The 219 spent calls are spent on either
  branch and argue for neither. The reason is the reference's instability, and no sample size
  repairs it.
- **Disposals, stated so they are not rediscovered:** the Ω_C mapping rulings ("is Wu-D our
  P4+P6?", deficiency vs different-valid-cuts) are **deliberately UNMADE**; the OQ-278 bundle is
  **dissolved**; OQ-280 **unblocks** as the sharper and cheaper successor;
  `SPEC_next_preregistration.md` is **retained and re-scoped** as a reusable artifact — do not
  delete it with this close; the frozen stamp `4118f64e` **stands**.
- **Revival conditions:** Wu resolves which artifact is canonical or publishes a reconciliation; or
  a scorer and matrix builder exist for another reason; or a third comparison set appears with
  measured, adequate self-agreement. Absent one of those, **do not re-propose the spend** — the
  blocker is not resources.
- **The successor question needs nobody's cooperation:** does OUR six-pattern taxonomy reproduce
  against itself? Deliberately not minted here; it belongs with OQ-278.

**On the `Fired:` line above, for the cold reader.** The `live` bit records that the cross-coding
discovery flipped the experiment's *premise* — it does **not** record that results were produced,
and the two are consistent rather than contradictory. The bit's criterion in this case is open as
**RULING R6** and is the operator's seat; it is deliberately left as authored rather than
"corrected", because silently re-scoring a landed bit corrupts the rolling catch rate the
instrument exists to measure. One fact bearing on that ruling postdates this line and is recorded
here rather than acted on: the 2026-08-23 operator refinement in `../README.md` → *The `Fired:`
bit* states that the bit tracks **the OQ's own question**, not apparatus self-test or record
hygiene. OQ-277's own question was never measured. Whether the plan-stage findings (OQ-278, OQ-280)
count as that question's catch or as record hygiene is exactly what R6 must decide.

## Residue

- **`matrices/` is permanently absent by retirement**, not pending. It was Phase-4 output; Phase 4
  will not run. A reader who finds no `matrices/` directory has found the correct state.
- **`responses/` is empty by defect, and that emptiness is itself the artifact** — see
  `responses/EMPTY_BY_DEFECT.md`. The live run made 219 calls and persisted nothing; the marker
  file exists because git does not track empty directories, and without it "spent 219 calls,
  persisted nothing" would be indistinguishable from "never reached this phase" on a fresh clone.
- **The Ω_C mapping rulings remain unmade** and no later reader should treat any prose in this
  directory as having made them.
- **The methods writeup that survives this audit is `../../docs/comparing_incident_taxonomies.md`**
  — Rule 1 (:33) is the generalised, publishable form of the finding: *before comparing two
  taxonomies, measure whether each reproduces against itself, and report the rate as a precondition
  rather than as a robustness check.*
- **The build-side generalisation is `../../docs/technical/build_discipline.md` →
  *Cross-artifact reconciliation*** (2026-08-25): two artifacts naming the same units owe an
  ID-keyed join whose output is "fork found, ruling owed". This audit's R2 is its case law.

## Evidence map

One line per artifact in this directory (README rule: *every artifact appears*). Completed
2026-08-25 — the pre-close map omitted the recon, ruling, log, handoff and sweep files.

- `RECON.md` — the read-only pass that established what was answerable. **Carries Finding R2**, the
  finding that retired the experiment: the same 22 incident IDs, 12 agreeing on class (55%
  agreement) and 10 disagreeing, the 10 named with both class assignments, and the per-class
  marginals (catalog A1/B4/C5/D4/E8 vs dataset A4/B3/C4/D5/E6, both summing to 22). Also R2a (the
  disagreement is systematic; E is the hub) and R2b (R2 is a finding about the comparison set,
  independent of the cross-coding).
- `PREREGISTRATION.md` — the frozen design, stamp `4118f64e`. Never retro-edited; quoted, never
  amended. `PREREGISTRATION_body.md` is its assembled body source.
- `audit_log.md` — the chronological log; carries the prereg md5 **above** the first result line,
  which is what witnesses the freeze-before-spend ordering.
- `RULING_2026-08-11_freeze_scope.md` — the operator ruling fixing the (iii′) population and the
  scope of the freeze. Pinned by `../../python/audits/oq277_build_prereg.py`.
- `verdict_grammar_amendment.md` — incorporated verbatim as the preregistration's Appendix D.
- `SPEC_next_preregistration.md` — the specification governing the next stamp. **Retained and
  re-scoped at close as a reusable artifact** (Disposal 4): it is the repository's best statement
  of what a preregistration must pin, and applies to the next one whatever its subject. No spend is
  requested by it.
- `P3_FAILURE_SHAPE_SWEEP.md` — the OQ-278 P3 consumer sweep executed during this arc.
- `LETTER_2026-08-11_wu.md` — outbound correspondence to the source author, reporting the
  direction-(i) reconstruction's incidental finding: his failure-modes catalog and his labeled
  dataset assign **different classes to 10 of the same 22 incidents** (they agree on the other 12
  — 55% agreement; single author, own incidents, own taxonomy, the most favorable conditions a
  taxonomy will ever face). Witnesses that the comparison set is **unstable under its own author's
  hand**. Carries a repository annotation header (not part of the letter) recording reply status.
  **Not a dependency** — OQ-277 does not block on a reply, and none is required for the close.
- `HANDOFF.md`, `HANDOFF_EXTRACTOR_A2.md`, `HANDOFF_EXTRACTOR_B.md`,
  `HANDOFF_ESCAPE_EXTRACTOR.md`, `HANDOFF_IIIPRIME_EXTRACTOR.md`, `HANDOFF_TWINS_AND_DRIVER.md` —
  the receiver prompts written for each build limb (*Write the receiver's prompt before calling a
  design done*). Retained as method record: they are the specification tests the design was put
  through, and several of the arc's findings surfaced in writing them rather than in running them.
- `prompts/` — the coder-facing prompts (`direction_i.md`, `direction_ii.md`), both pinned.
- `frame/` — the frozen frame: full directory listing, the census split (incident-bearing vs
  non-census), the pinned regeneration command, and the seeded samples. Witnesses that the
  sample was drawn before any coding, from a stated population.
- `packets/` — redacted incident units (both directions), the coder-facing material.
  `packets/wu_source/` holds the two md5-frozen Wu sources; `packets/wu_unit_id_map.json` holds the
  **hand-derived** catalog-vs-dataset unit map (header: NOT CODER-FACING) behind R2's figures.
- `payloads/` — every exact assembled API payload, written BEFORE send. Witnesses H2 (leak).
- `responses/` — raw model responses, one file per call. **Empty by defect**; see
  `responses/EMPTY_BY_DEFECT.md` and Residue above.
- `controls/` — anchors, decoys, planted-leak payload, planted-broken unit, redaction-bias
  pairs, and `redaction_pair_selection_defect.md` (the option-C ruling). Each carries its
  pre-registered want=fire / want=pass.
- `payloads_stub/`, `responses_stub/`, `STUB_RUN_README.md` — the **synthetic** full-scale stub run
  (`mode: stub`, 219 units, stamped 34 minutes AFTER the failed live run). Witnesses §2.1 row 6 of
  the SPEC — that gate 4, the output-side gate, is built and closes write-then-verify at production
  scale — which is otherwise a claim a cold reader cannot check. **Not results**; the README states
  the fail-closed `mode == "live"` requirement now carried as SPEC §3 item 6.
- `matrices/` — **absent, permanently.** Phase-4 output of a phase that will not run.
