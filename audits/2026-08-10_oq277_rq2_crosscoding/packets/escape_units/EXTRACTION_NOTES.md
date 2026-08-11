# EXTRACTION NOTES — escape check (n=8)

**Extractor:** escape-check instance, 2026-08-11. **Role:** extract only; never code, never assign
a failure class. **Condition:** blinded per `HANDOFF_ESCAPE_EXTRACTOR.md`.

Status: COMPLETE — 8 unit files written, leak sweep clean with a live-matcher control, no model
call made. The ordered file list below was written AS I GO and is not reconstructed; the summary
sections at the bottom were written at the end.

## THE HEADLINE, STATED FIRST BECAUSE IT IS UNCOMFORTABLE

**6 of 8 directories yield a candidate hit. All 6 are QUARANTINED and PROPOSED, none is confirmed.**
The handoff says most should be NO-UNIT and that a single false positive would move a published
figure; six is not one. Read the calibration risk in *Split and clauses* below before treating this
as a result — the one thing I cannot check, by construction, is whether my reading of the boundary
rule matches the primary sample's, because I have not seen the primary sample. This is the first
live exercise of the quarantine mechanism (previously k=0), and it is arriving at six times the
volume anyone designed it for.

**Operator ruling, 2026-08-11: §H.2 confirmation WITHHELD — six of eight is not usable as an escape-check
result, and the extractor is right about why.** Two explanations survive and blindness is what makes
them indistinguishable; that is the design working, not failing. The row is not scored until the
threshold-comparability probe runs: **`PREREGISTRATION_threshold_calibration.md`**, in this
directory, pre-registered before either party has seen a result. Its escape-side draw is executed
and recorded there. Until it runs, nothing in this file licenses a numerator, a miss rate, or a
sentence about the proxy's completeness.

## Conventions applied (declared before extraction, overridable by the operator)

- **C-1 (prose in directories that have none).** Two frame directories carry no prose document
  (`2026-02-25_spectral_laplacian`: `.py` + `outputs*/` only; `2026-07-24_oq153_update_authority_step2`:
  a single `.pl` file). I read docstrings, comments, and saved output text as that directory's prose.
  Rationale: a thin read biases toward NO-UNIT in exactly the direction that would flatter the
  keyword proxy. Flagged to the operator at start; not a per-directory verdict.
- **C-2 (file-list granularity).** Each opened path is appended immediately after its read returns.
  Where several files were read in one turn they are appended together, before any further read.
  Directory listings (`ls`) are recorded as listings, not as opened files — no file body was read.
- **C-3 (notes file path).** The handoff names this file bare in "Done means" item 3 and as
  `packets/escape_units/EXTRACTION_NOTES.md` in item 4; treated as one file at the latter path.

## Limit of this list (stated, not hedged)

This list is authored by the party it constrains. It converts an assurance into a *checkable*
assurance — auditable against the do-not-open table — and nothing more. The independent instrument
is the four floor comparisons, which collapse toward zero if blindness broke.

## Ordered list of every file opened

1. `audits/2026-08-10_oq277_rq2_crosscoding/HANDOFF_ESCAPE_EXTRACTOR.md`
   (the handoff itself; read in full before anything else, per its own instruction)

### Directory 1 — `audits/2026-02-25_spectral_laplacian`

Listings only (no body read): `audits/2026-02-25_spectral_laplacian/`, and its `outputs/`,
`original_outputs/`, `outputs_haiku/` subdirectories.
Convention C-1 turned out NOT to apply here: the directory does carry prose, inside `outputs*/`.

2. `audits/2026-02-25_spectral_laplacian/original_outputs/spectral_audit_report.md` — full read
   (28K; the fullest of the three report variants — the only one carrying a §8)
3. `audits/2026-02-25_spectral_laplacian/outputs/spectral_audit_report.md` — headers only
   (`grep -n '^#'`), to check whether the re-run variant carries a section the fullest one lacks
4. `audits/2026-02-25_spectral_laplacian/outputs_haiku/spectral_audit_report.md` — headers only,
   same purpose. Neither variant adds a section; both stop at §7.

### Tooling (read once, applies to all 8)

5. `python/audits/oq277_lexicon.py` — lines 120–229 only (`scan_units`, `main`, the selftest
   probes). Read because `--sweep` rejected a single escape unit and I needed the input shape and
   the scanned-field list before trusting any sweep result. See the tooling defect below.

**Tooling defect, for the operator / the other instance — I did not fix it, because the extractor
does not code.** The handoff's self-check command is
`oq277_lexicon.py --sweep packets/escape_units/NN_x.json --direction ii`, but `scan_units` does
`data["units"] if isinstance(data, dict) else data` and then iterates — so a single unit object
(the format the handoff itself specifies for escape units) raises `KeyError: 'units'`. The command
as written cannot run against the file format it is written for. Workaround used here: a scratchpad
bundle that loads every `packets/escape_units/*.json` into a list, adds an `id` from the filename,
and sweeps that. No repo file was modified.

**The sweep's own positive control (run through the bundle path, not the tool's path).** A clean
sweep is only evidence if the matcher and my wrapper both fire. Two controls, both witnessed:
(a) `oq277_lexicon.py --check` → GREEN, every pre-registered control fired; (b) a planted unit
carrying one known-banned phrase in each of the four coder-facing fields, appended to my own bundle
and swept by the same command I use for real units → 5 hits, at least one in each of the four
fields, while the real unit in the same run contributed 0. So "0 hits" from this route is a
measured clean, not a wrapper that dropped the fields.

**Blindness note recorded at the point it arose, not at the end:** reading the checker exposed
direction-(ii) vocabulary (its selftest probes name banned terms). This is not a leak of the
primary sample — it is the taxonomy I am required to strip, and most of it is in the always-loaded
project instructions anyway. No primary unit body, response, matrix, or coding result was involved.

### Directory 2 — `audits/2026-05-30_authoring_closure_fabricated_defaults`

6. `audits/2026-05-30_authoring_closure_fabricated_defaults/audit_authoring_closure_fabricated_defaults.md` — full read
7. `audits/2026-05-30_authoring_closure_fabricated_defaults/tripwire_fabricated_defaults_results.json` — full read

### Directory 3 — `audits/2026-06-04_oq71_depth_lineage`

Listings: the directory and its `lineage_probe_01/` and `tree_spec/` subdirectories.
Convention C-1 DOES apply here — the directory carries no prose document at all.

8. `a2_richness_alldims_results.json` — full
9. `lineage_probe_01/quarantine.json` — full
10. `pilot_machinery_results.json` — full
11. `powered_readout_results.json` — full
12. `gate2_capture.py` — full (read for its docstring; it is a capture harness)
13. `cur_sixdim.txt`, `depth_sixdim.txt`, `live_sixdim.txt`, `pilot_sixdim.txt`, `v5_repro.txt`,
    `v5_sixdim.txt` — first 300 bytes each; `depth_sixdim_tagged.tsv`, `live_sixdim_tagged.tsv`,
    `pilot_sixdim_tagged.tsv`, `v5_repro_tagged.tsv` — first 2 lines each. Purpose: establish
    these are machine-emitted term data, not prose. They are.
14. Structural survey (script, not eyeball) over every `.json` in the directory and both
    subdirectories — key sets to depth 2 plus every free-text string longer than 120 chars. This
    is how I looked for prose instead of assuming its absence from filenames. Files touched by
    that survey: `control_membership.json`, `gate2_prechange.json`, `gate2_postchange_off.json`,
    `gate2_postchange_on.json`, `gate2_toy_seeds.json`, `lineage_seeds.json`,
    `lineage_probe_01/lineage.json`, and all 15 `tree_spec/*.json`, plus the four already listed.
15. `control_membership.json` — the metadata fields (`description`, `provenance`, `n`) printed
    directly; the 300-element id list not read item by item.

Incidental, recorded because the do-not-open table names commit messages as a leak channel: the
`provenance` block of `control_membership.json` embeds a commit subject. It belongs to the
2026-06-03/04 corpus generation, not to the OQ-277 arc, and I reached it by reading a data file,
not by running `git`. No `git log`, `git show`, or commit body was read at any point.

### Directory 4 — `audits/2026-06-10_signature_liveness_crosscorpus`

16. `MATRIX.md` — full
17. `desirepath_cell_control.out` — full (its provenance header carries the incident)
18. `dmv_cell_control.out` — full

### Directory 5 — `audits/2026-06-11_oq46_close`

19. `writeup.md` — full
20. `evidence/` — listing
21. `evidence/probe1_output.txt` — opened; first ~2KB shown directly, then the non-warning tail
22. `evidence/probe2_output.txt` — non-warning tail
23. `evidence/probe3_output.txt` — non-warning tail
    (purpose of 21–23: check whether any declared control failed. All three named controls report
    FIRED, matching the writeup. The probe `.pl` sources were not read — the writeup is the prose.)

### Directory 6 — `audits/2026-06-11_oq98_verdict_join`

24. `writeup.md` — full. Nothing else in the directory was opened: the writeup states the incident,
    its mechanism, its detection, and its consequence directly, and names every other artifact as
    its witness. Declared so the thinness is visible rather than implied — see the note below.

### Directory 7 — `audits/2026-07-01_oq197_source_h1_crosstab`

25. `README.md` — full
26. `crosstab_output.txt` — full
27. `twin_crosstab_output.txt` — full
    (the two scripts and three TSVs are the README's raw inputs; not read)

### Directory 8 — `audits/2026-07-24_oq153_update_authority_step2`

28. `enum_controls.pl` — full. It is the only file in the directory (listing includes dotfiles).

### Files written by me (not reads, listed for completeness)

`packets/escape_units/01…08_*.json`, this file, and three scratchpad files outside the repo
(`mkbundle.py`, `sweep_bundle.json`, `planted.json`).

---

## Split and clauses

| # | directory | verdict | clause applied |
|---|---|---|---|
| 1 | 2026-02-25_spectral_laplacian | **UNIT** | REPORTS — reconciliation section states its own earlier phase's result was an artifact of three compounding errors |
| 2 | 2026-05-30_authoring_closure_fabricated_defaults | **UNIT** | REPORTS — the incident is the document's subject, with a measured effect |
| 3 | 2026-06-04_oq71_depth_lineage | NO-UNIT | neither clause — the directory contains essentially no prose (escalated, see below) |
| 4 | 2026-06-10_signature_liveness_crosscorpus | **UNIT** | REPORTS — a control artifact's validity header states its own first run was invalid, and pastes it |
| 5 | 2026-06-11_oq46_close | **UNIT** | REPORTS — a tracked premise contradicted by the live authoring instruction, plus a second load path never recorded |
| 6 | 2026-06-11_oq98_verdict_join | **UNIT** | REPORTS — section headed as the defect, quoting the offending output |
| 7 | 2026-07-01_oq197_source_h1_crosstab | **UNIT** | REPORTS — provenance section states the first run's clause matched zero seats for every item |
| 8 | 2026-07-24_oq153_update_authority_step2 | NO-UNIT | DISCUSSES — failure modes named only as things the controls exclude; no run recorded |

All six candidates are silent-failure-shaped rather than merely defect-shaped: in each, a
well-formed output stood in for a measurement that had not happened (a reimplemented criterion,
a fabricated constant, an input write that was refused, a fallback firing where the tracked model
said it could not, a headline computed from one field, a query matching nothing). I checked this
deliberately, because "any defect at all" would be a looser rule than the one I was given.

**The calibration risk, stated as the thing most likely to be wrong.** The boundary rule I applied
is broad by construction — an incident anywhere in the document counts, subject or not. This repo's
working discipline is to write down what went wrong, so under a broad rule a high hit rate is what
you would expect *if the rule is being read the same way on both sides*. Whether it is, I cannot
check: the primary sample's applications are exactly what I am blind to. Two readings of the six
are live, and the operator's confirmation pass is what separates them:
(a) the keyword proxy genuinely misses incidents that are reported without its keywords — in which
case the 73/174 numerator is a substantial undercount and 42% is a floor well below the true value;
(b) I am applying REPORTS more liberally than the primary extractor did, in which case the two
strata are not comparable and the escape check measures my threshold rather than the proxy's recall.
**Nothing in my instructions lets me distinguish these, and I did not try to.** The cheapest
discriminator I can name without breaking blindness: hand one or two of my six to whoever holds the
primary conventions and ask whether they would have extracted it — before any of them is scored.

**What may not be written on the back of this file.** Not "the proxy is incomplete", not a
corrected numerator, not a miss-rate estimate. Zero *confirmed* hits would have licensed only
"miss rate ≤ 31.2% at 95%"; six *proposed* hits license nothing at all until they are ruled.

## Genuinely ambiguous — escalated, and now RULED (operator, 2026-08-11)

All three escalations below were ruled the same day. The rulings are recorded in the unit files
themselves (`operator_ruling_2026_08_11` keys) so a cold reader hits them at the unit, not only here.

1. **Directory 3 — no prose, defect present only as a machine-written artifact.** RULED: NO-UNIT
   stands, and it is recorded as a **third category — outside the frame**, not an in-frame NO-UNIT.
   A prose-searching census can neither hit nor miss a directory with no prose, so counting it as an
   ordinary NO-UNIT would inflate the precision denominator with a directory the proxy was never
   able to see. Not a defect in the figure — a defect in the frame. Carried in the unit file as
   `metadata.frame_status = outside_frame_no_prose`, with an explicit do-not-pool note. **The open
   consequence is now MEASURED, not open** — see `FRAME_AUDIT_prose_visibility.md` in this
   directory: 4 of 101 directories are unseeable (effective population 97), 12 more carry the
   proxy's own keywords in files its filter never opens, and the two blind spots are independent.
   **Both of my NO-UNITs are among the 4**, so in-frame this stratum reads 6 candidates from 6
   directories, not 6 from 8 — removing the out-of-frame directories removes the only two that were
   making the result look softer. The remaining 14 escape directories need the same census run
   against them, which I cannot do and should not.
2. **Reference to an incident recorded elsewhere.** RULED: does NOT count as reporting one. The
   boundary rule was fixed against the census's own line, and a pointer to another directory's
   incident would double-count it — the frame partitions directories, not incidents. My
   non-extraction in directory 4 stands, and this now governs both strata.
3. **Directory 5, one incident or two.** RULED: one unit, as extracted. The standing rule is
   fullest-described-mechanism with alternatives recorded; making an exception now would be a
   convention change mid-stratum.

## Superseded — the original escalation text, kept for provenance

1. **Directory 3 is a third case the boundary rule does not name.** It carries a defect only as a
   machine-written artifact (a quarantine list of unresolvable edge targets, one of them a
   misspelling) with no prose anywhere. Both clauses presuppose prose. I applied the rule as
   written and returned NO-UNIT rather than extending it, because extending it would be the
   extractor deciding what counts as a report. Note the consequence for the escape check itself: a
   directory with no prose cannot be a keyword-proxy hit in either direction, so it tests recall
   only vacuously. If several of the 101 are prose-free, the frame is weaker than its n suggests.
2. **Does a reference to an incident recorded elsewhere count as REPORTING one?** Directory 4's
   sweep document cites two previously recorded incidents in other work as corroboration for its
   own numbers. I did not extract them, and directory 4 is a UNIT on other grounds, so nothing
   turns on it here — but the question bears on both strata and should be ruled once rather than
   per directory.
3. **Directory 5: one incident or two?** The false premise and the unrecorded second load path are
   two grounds for the same conclusion; I extracted them as one incident because the document
   frames them that way, and splitting would have produced two half-mechanisms. If the primary
   convention splits such cases, this unit should be re-cut.

## Redaction

Every unit was swept with `--direction ii`; the final run over all 8 is **0 hits**, with a planted
control unit in the same session producing 5 hits across all four coder-facing fields, so the clean
is measured rather than assumed. One real leak was caught and fixed en route: unit 06 quoted a
taxonomy phrase verbatim from its source document. A bonus `--direction i` sweep is also 0.

One judgment call the matcher does not cover: unit 04 keeps its source's own phrasing for the
mechanism ("a clean read identical to a read that never looked"). It is not on either banned list,
and it is the sentence that makes the unit codeable, but it is distinctive repo phrasing and a
coder who has seen this codebase might recognize its provenance. Flagged rather than silently kept.

I did not strip shared subject-matter vocabulary (silent, fallback, empty, stale, green, no error);
per the handoff that vocabulary belongs to neither taxonomy and stripping it would corrupt the
control that direction (c) exists to measure.

## Did the blindness rule hold, and by what route do I know

**It held.** The route, in order of how much it is worth:

1. **The strongest thing available to me is negative and structural: I never had occasion to open
   anything on the list.** The eight source directories are all outside the OQ-277 directory. From
   inside it I read exactly one file — the handoff — and wrote only into `packets/escape_units/`.
   I never opened `packets/our_units/`, `packets/wu_units.json`, `packets/coder_direction_i.json`,
   any file in `controls/`, `responses/`, or `matrices/`, the freeze-scope ruling, or the
   redaction-pair defect note. I ran no `git log`, no `git show`, and read no commit body; the one
   commit subject I encountered was a string inside a data file in directory 3, reached by reading
   that file, and it belongs to a 2026-06 corpus generation rather than to this arc.
2. **The ordered file list above is the checkable artifact**, and it is auditable line by line
   against the do-not-open table. Its limit is the one the handoff already states and I will not
   soften: it is authored by the party it constrains. It makes the assurance checkable; it does not
   make it proof.
3. **Declared exposure, recorded at the moment it happened rather than confessed here:** reading
   `oq277_lexicon.py` lines 120–229 put direction-(ii) taxonomy vocabulary in front of me via its
   selftest probes. That vocabulary is what I am required to strip, and most of it is in the
   always-loaded project instructions regardless. It is not primary-sample content.
4. **The instrument that actually tests this is not mine.** The four floor comparisons collapse
   toward zero if blindness broke; the file list is the cheap corroborating one. Nothing in this
   section should be read as substituting for that.

No slip to declare. Had there been one I would have written it here rather than continued.

## For the record (operator, 2026-08-11)

Two entries the operator dictated on reading this extraction. Both are apparatus findings, not
findings about the eight directories.

1. **The `scan_units` KeyError is a handoff specifying a format its own check cannot consume.** It
   is the **fifth stated-versus-executed defect** in this arc, and the **first where the rule and
   the checker disagree** rather than the rule and the count. The earlier four were quantity
   mismatches; this one is a category mismatch between two authored artifacts that were supposed to
   describe the same object.
2. **The leak in unit 06 was caught by the sweep**, which matters against §6.4's honest limit. The
   running tally: the control architecture has now caught **two untracked paths and one real leak,
   and zero of five instrument defects.** The split still isn't random — the controls catch what
   flows *through* the instruments and miss what is *wrong with* them, which is the same asymmetry
   §6.4 declares and has not yet closed.

**Where entry 2's tally belongs:** the running §6.4 count lives in the other instance's amendment
ledger, not in this file. I have not edited that document — it is another writer's, and this repo's
one-writer rule is exactly what prevents me from appending to it mid-arc. Recorded here so the
number is not lost; it still needs to land in §6.4.

## Handoff back

Packet assembly, the driver, and the arc prereg are the other instance's. **The full queue for the
assembler is in `PREREGISTRATION_threshold_calibration.md` → *Queue for the assembler*: the
primary-side draw, the unseen fifth item for the recognition check, the assembly, and the frame
census across all 22.** Two further items belong to whoever holds those documents, not to me: the
§4.5 reading (in `FRAME_AUDIT_prose_visibility.md`, destined for the paper) and the 2-of-4 draw for
the arc writeup's §9.3 efficacy discussion.

Of those, the two I am structurally barred from: the primary-side draw (I must not see those
units), and the frame census over the other 14 escape directories
(`frame_audit_prose_census.py`, in this directory, run against the full escape sample rather than my
slice — the count must not be extrapolated from mine, which drew 2 of the 4 unseeable at p ≈ 0.031).

Four things travel with these units: the six quarantined candidates and their asymmetric cost; the threshold-comparability
probe, pre-registered in `PREREGISTRATION_threshold_calibration.md` with its escape-side draw
executed and its outcome semantics fixed — **it needs a primary-side draw I must not make, and an
operator spend-go if the judge is a live instance**; the tooling defect in the sweep command
(unfixed by me — the extractor does not code); and the §6.4 tally above, which needs to land in the
amendment ledger.

**No model call was made.** `payloads/` and `responses/` were not touched.
