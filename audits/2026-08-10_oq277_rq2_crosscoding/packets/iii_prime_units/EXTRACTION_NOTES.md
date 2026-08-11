# (iii′) exemplar extraction — notes

**Extractor:** the (iii′) extractor instance, 2026-08-11.
**Status:** COMPLETE — 7 unit files written, leak sweep clean with a live-matcher control, no
model call. Ordered file list written as I went (operator ruling, 2026-08-11).
**No model call was made. `payloads/` and `responses/` untouched.**

Limit of this list, stated so nobody treats it as closing the question: it is authored by the
party it constrains. It converts an assurance into a *checkable* assurance, not into proof.

---

## 1. Ordered list of every file opened

Each row is in the order it was first opened. `read` = file contents rendered into my context.
`probe` = the file was interrogated by a command whose output was contents (grep hits, a line
slice, a key list) but the file was never rendered whole. Both are listed, because a grep hit is
still text from the file reaching me.

| # | file | how | why |
|---|---|---|---|
| 0 | `CLAUDE.md` | **read (involuntary)** | The harness auto-loads it into the system context at session start, including the six-pattern block that is this row's source. Declared first because it was not a choice I made and it is the densest answer-key prose in the population. |
| 1 | `audits/2026-08-10_oq277_rq2_crosscoding/HANDOFF_IIIPRIME_EXTRACTOR.md` | read | the brief |
| 2 | `audits/2026-08-10_oq277_rq2_crosscoding/RULING_2026-08-11_freeze_scope.md` | read | §2.1 population, §2.4 row design |
| 3 | `audits/2026-08-10_oq277_rq2_crosscoding/` (dir listing) | probe | locate `packets/iii_prime_units/`, confirm it did not already exist |
| 4 | `audits/2026-08-10_oq277_rq2_crosscoding/controls/anchors.json` | read | licensed by the brief: the three referenced anchors, for format and register |
| 5 | `CLAUDE.md` lines 465–544 | read | the six-pattern block; the naming source for all 11 exemplars |
| 6 | `9d9e62c1:CLAUDE.md` (git object) | probe | line count + md5 only; contents never rendered. Pre-check 3 (drift). No commit message was read. |
| 7 | `CLAUDE.md` lines 473/474/479/486/501/508/515/524/526/527/528 | probe | per-line content verification of every cited exemplar |
| 8 | repo-wide grep `Supp=0.5` | probe | pre-check 2 (row 8 vs the fork-residue anchor) |
| 9 | `ISSUES.md` lines 11030–11074 | read | pre-check 2: what the fork-residue pass's anchor actually is |
| 10 | `docs/technical/build_discipline.md` — greps for `OQ-178`, `classify_at_time`, `recap`, section headers | probe | locate mechanism text; establish which exemplars have worked sections |
| 11 | `docs/technical/build_discipline.md` lines 571–685 | read | its Pattern 4 (fabricated default) + Pattern 5 + the OQ-178 dual — mechanism for unit 08, and pre-check 2 |
| 12 | `prolog/drl_composition.pl` — grep `Supp = 0.5 / BaseX / base_extractiveness` | probe | pre-check 2: are the two fallbacks the same code branch? |
| 13 | `python/audits/oq277_lexicon.py` | read | the sweep instrument; needed its input shape and scanned-field list before trusting any sweep result |
| 14 | `audits/.../packets/escape_units/*.json` | probe | **top-level key list only, via `json.load(...).keys()`** — no prose rendered. Establishes the on-disk unit shape the sweep must consume. |
| 15 | `audits/.../packets/escape_units/EXTRACTION_NOTES.md` | probe | grep for `sweep|lexicon|units` only. Checking whether the sweep-command defect was already known. It was. |
| 16 | `docs/technical/build_discipline.md` lines 44–121 | read | mechanism for units 01, 02 |
| 17 | `docs/technical/build_discipline.md` lines 416–485 | read | mechanism for unit 05 |
| 18 | `docs/technical/build_discipline.md` lines 738–795 | read | mechanism for units 10, 11 |
| 19 | `audits/.../audit_log.md` — grep for `198`, then lines 100–149 | read | the call-count arithmetic and the recompute instruction attached to it (§6) |
| 20 | `audits/.../packets/run/*.json` | probe | **list length only**; then `n_items`/`n_matrix_cells`/`n_quarantined` from the two `_map.json` files. No item text was rendered. |

**Not opened, deliberately:** `prompts/`, `responses/`, `matrices/`, `packets/our_units/*.json`,
`packets/wu_units.json`, `packets/coder_direction_i.json`, `PREREGISTRATION.md`, and any commit
message (`git log` / `git show <commit>` without a `:path` suffix). The one git call I made was
`git show 9d9e62c1:CLAUDE.md`, which emits file bytes and no message, and its output went to
`wc -l` and `md5sum` rather than to the screen.

**Written to:** `packets/iii_prime_units/` only. Nothing else in the repo was modified.

---

## 2. What was written, what was referenced, what has no members

**Written — 7 files, numbered by the population's own rows so the gaps are the record:**

| file | row | true label |
|---|---|---|
| `01_unread_sensitivity_results.json` | 1 | P1 |
| `02_grouping_not_stamped.json` | 2 | P1 |
| `05_tracker_sprawl.json` | 5 | P2 |
| `06_three_edits_one_pasted.json` | 6 | P4 |
| `08_offgrid_probe_time_zero.json` | 8 | P5 |
| `10_filtered_warning_channel.json` | 10 | P6 |
| `11_partial_grid_system_verdict.json` | 11 | P6 |

**Referenced, NOT re-extracted — 3 existing direction-(i) anchors.** Recorded here by id so the
row can be reported with and without them (§2.1 point 3) without anyone re-deriving membership:

| row | exemplar | anchor id | published label |
|---|---|---|---|
| 3 | the post-process artifact frozen at the older snapshot | `anchor_i_1` | P1 |
| 4 | the generator script existing at two paths | `anchor_i_2` | P2 |
| 7 | the certifying check passing over an empty table | `anchor_i_3` | P5 |

**No file written for row 9.** Disqualified in the frozen population as colliding with an
already-extracted unit. The absent `09` is the record of that.

**P3 — `no members — uncalibrated`.**

The phrase is written as given. P3 contributes nothing to the row: it is not read as agreement,
not counted in the denominator, and not reported as "no disagreement observed", "n/a",
"consistent", or a blank. There is no P3 exemplar named inside the published block, so there is
nothing to extract — the population is empty, not thin, and no amount of n buys it. Recorded
also as required: I did not at any point want to write a P3 unit to make the row look complete,
and the reason is worth stating because it is not virtue — by the time I reached P3 I had already
read the ruling's §2.5–2.7, which pre-registers two competing readings of what that empty cell
means. Filling it would have destroyed a pre-registered discrimination, which is a more concrete
deterrent than the instruction was.

**Mechanism depth per unit — full section, bullet, or single clause:**

| unit | what the source gave | thin? |
|---|---|---|
| 01 | one bullet, 2 sentences; detection taken from the section's class-level diagnostic | **thin** |
| 02 | one bullet with producer, omitted step, affected count, lost capability; own diagnostic | full |
| 05 | one bullet, 3 lines, prefixed "Historically" | **thin** |
| 06 | **one parenthetical clause in the published block, and NOTHING in the mechanism file** | **thinnest** |
| 08 | its own titled sub-section: probe, count, data shape, both failed corrections, measured harm | full |
| 10 | numbered instance, 3 lines: filter, suppressed content, duration, terminal failure | full |
| 11 | numbered instance, 5 lines: grid fill, the aggregate's shape, the unconsulted ratio | full |

No unit was padded from my own knowledge of the codebase. Where the source was thin the unit is
thin, and each such unit says so in its own `extraction_notes`.

---

## 3. The four pre-checks the operator ordered, run before any unit was written

### Pre-check 1 — rows 4 and 5 both cite `CLAUDE.md:486`. Verified by content: **correct, not a transcription error.**

```
486: duplicated `generate_kernel_corpus.py`; the old ISSUES/AGENDA/PRIORITIES/TODO tracker sprawl).
```

Both exemplars are named in one parenthetical on one physical line. Nothing to correct. (The
mechanism file separates them into two bullets, at :424-426 and :427-429.)

### Pre-check 3 — line drift against commit `9d9e62c1`. **No drift. The file is byte-identical.**

```
== HEAD CLAUDE.md lines: 998
== 9d9e62c1 CLAUDE.md lines: 998
== diff --stat 9d9e62c1 HEAD -- CLAUDE.md:   (empty)
01084bd81a969da9c40a464ce29cfbb4  -            <- git show 9d9e62c1:CLAUDE.md
01084bd81a969da9c40a464ce29cfbb4  CLAUDE.md    <- working tree
```

Every one of the 11 cited lines was then verified to hold its exemplar, individually:

```
473: reads it back into the thing that needs it (unread `*_sensitivity_results.json`;
474: `kernel_grouping.json` not stamped into the `.pl` files). **Rule: a producer is not done until
479: post-process the orchestrator never re-runs goes silently stale (the `w1_sheaf_join` artifact
486: duplicated `generate_kernel_corpus.py`; the old ISSUES/AGENDA/PRIORITIES/TODO tracker sprawl).
501: "three edits witnessed" with only the third pasted). If a witness cannot be produced this turn,
508: on a fabricated default. Instance: `natural_law_signature`'s `BeneficiaryCount == 0` reads
515: `classify_at_time` at `Time=0`) against data authored on a *different* grid (a story whose ε series
524: witnessed in one day (2026-06-10): `system_gradient`'s `[] → 0.0` fallback (every gradient ever
526: the construct's whole life); `grep -v Warning` (a dead-module warning printed at every load for
527: four months into a universally filtered channel, then crashed the suite — OQ-96); findall-over-
528: partial-levels (an 8/32 one-level grid read as a full-system `increasing_coercion` verdict).
```

All 11 hold. **One correction of record, not of substance:** row 11's exemplar begins at 527 and
completes at 528 — the cited line carries the name, so the citation stands, and unit 11's `source`
field states the 527-528 span rather than the bare 527.

Note on how this check was run: `git show 9d9e62c1:CLAUDE.md` emits file bytes and no commit
message, and its output was piped to `wc -l` / `md5sum` rather than displayed. No commit body was
read.

### Pre-check 2 — is row 8 the same incident as the fork-residue pass's anchor? **NO. Not the same incident.** Row 8 written.

The fork-residue pass's anchor is *"the `classify_at_time` `Supp=0.5` fabricated-default
exemplar"* (`ISSUES.md`:11055). Row 8 is *"the OQ-178 dual — `classify_at_time` at `Time=0` off
the authored grid"*. They share a predicate and the constant `0.5`, which is why the check was
worth ordering. They are two different incidents:

| | fork-residue anchor | row 8 |
|---|---|---|
| datum | `suppression_requirement` (`Supp`) | `base_extractiveness` (`BaseX`) |
| code branch | `drl_composition.pl:179` — **removed** by OQ-41 row 23 | `drl_composition.pl:219` `BaseX = 0.5` — **still present, now flagged** |
| OQ / date | OQ-33, 2026-05-30 | OQ-178, 2026-06-24 |
| why it fires | the datum is absent corpus-wide; fallback fires on 100% of the path | the datum EXISTS, on a different grid; the probe missed it |
| direction of the harm | fail-open: an invented value flows on | the fail-CLOSED repair discarded real data |
| measured consequence | 279/443 type flips; ~99% input exposure | `robust_context_count` 0→156; a real snare-vs-scaffold divergence erased |

Witness for the "different branch" row, since that was the load-bearing one:

```
prolog/drl_composition.pl
175:    %   → fail-closed `unknown` (never fabricate; OQ-41 row 23 killed the Supp=0.5
219:    ;   BaseX = 0.5, EpsBacked = false   % :201 fabrication — now FLAGGED, not silent
```

Two distinct clauses, one killed and one live. So no stop, and no fourth sighting on that
comparison.

**But a weaker relative of the shape IS here, and it is reported rather than buried.** Row 8's
incident *begins* with a fabricated-default event — the mechanism text says in so many words that
the lookup "hit the `BaseX=0.5` fabricated-default branch". That branch is the mechanism the
orphaned `build_discipline.md` Pattern 4 names. So this single incident is published under P5 (as
its dual) while its first step instantiates the orphaned BD pattern that shares an index with the
published P4. That is **one incident spanning both sides of the collision**, not two exemplars
colliding — a different and softer shape than the three prior sightings, and I am not calling it a
fourth. It is a datum for OQ-278 for one specific reason: it is evidence the orphaned Pattern 4
is **load-bearing inside an incident the published set already owns**, which bears on §2.6's point
that resolving toward the published branch would retire a real pattern for the wrong reason.

**A second datum for OQ-278, found while writing unit 06 and not looked for.** The published P4
(recap-as-witness) is the other member existing only on the published side of the collision. Like
the published P3, **it has no worked section in the mechanism file at all** — checked, not
assumed: `grep -i "recap\|three edits"` over `docs/technical/build_discipline.md` returns hits only
in unrelated later sections and none in any pattern section, and the file's own section at index 4
is the different rule (fabricated default). It differs from P3 in that it *does* name an
identifiable incident in the published block. So of the two published-only members, one has an
incident and no worked mechanism, and the other (§2.3) has neither. That is a sharper version of
the ruling's §2.5 observation and it points the same way: the published-only branch is the one
with less behind it.

### Pre-check 4 — sweep direction. Swept `--direction ii`, as instructed.

Confirmed correct from the instrument itself rather than from the instruction:
`BANNED_DIRECTION_II` is the list holding `P[1-6]`, `Pattern-N`, the six pattern names, the
nicknames, and `CLAUDE.md` / `ISSUES.md` / `build_discipline` — i.e. **our** vocabulary, which is
what must not reach a coder reading **our** incidents. Direction (i) strips Wu's vocabulary and is
irrelevant to these units. Both were run anyway; both are 0 (§5).

---

## 4. Declared residues and escalations — things I did not decide

### 4.1 ESCALATED: unit 11 may collide with an already-extracted unit, and I cannot check it

Row 9 was disqualified from the population for describing the same incident as extracted unit
`05_oq93_grid_viability`. Rows 9 and 11 are **two instances named in the same section, in the same
construct, found by the same probe on the same day**, presented by the source at two different
altitudes:

- row 9 (disqualified): a value-level fall-through emitting `0.0` after every computation failed;
- row 11 (written): an aggregate assembled over a grid that was one level of four.

These are distinct defects, and the source treats them as distinct. **But whether unit
`05_oq93_grid_viability` describes only the first, or the whole probe including the second, is not
knowable from where I sit** — `packets/our_units/*.json` is on my do-not-open list, and the
disqualification probe that cleared this question for row 9 was run by a party who could read it.

I wrote unit 11 because the frame is frozen and names it a WRITE row; redrawing the frame is not
mine. **The check is owed by the assembling instance before unit 11 enters a packet:** open
`packets/our_units/05_oq93_grid_viability.json` and ask whether its incident is the fall-through,
the partial-grid aggregate, or both. If it covers the aggregate, unit 11 is an unlabelled twin
inside a calibration row and must be dropped — which changes the population to 6 written / 9
eligible and the totals in §6. The evidence that points the *other* way, stated because it is the
weaker of the two and I would rather it be discounted than assumed: `controls/anchors.json`
`_disqualified` names row 9 specifically and describes it as "the richest published exemplar",
which reads as a match on the fall-through rather than on the whole battery.

### 4.2 The tooling defect: the sweep command the brief specifies cannot consume the format the brief specifies

This is the second sighting. The escape extractor reported it (their `EXTRACTION_NOTES.md` §"the
tooling defect"), and the brief I received still prescribes the failing command verbatim.

```
$ python3 python/audits/oq277_lexicon.py --sweep <one-unit>.json --direction ii
Traceback (most recent call last):
  File ".../oq277_lexicon.py", line 275, in scan_units
    units = data["units"] if isinstance(data, dict) else data
KeyError: 'units'
exit=1
```

Two ways it is worse than a crash. First, `exit=1` is *also* the code for "leaks found", so a
wrapper checking only the exit status reads the crash as a leak. Second and worse in the other
direction: the crash path prints **no** `LEAK` lines, so a wrapper checking only stdout for
`LEAK` reads the crash as a **clean sweep** — a check that goes green because it never ran.

**No repo file was modified.** I bundled the 7 unit objects into a JSON list in the scratchpad,
added an `id` from each filename, and swept that with the unmodified tool (§5). Reported rather
than repaired, because `oq277_lexicon.py` is frozen prereg content and not mine to edit.

### 4.3 A register decision I made, declared because it is arguable

The published block quotes some exemplars as literal identifiers (`grep -v Warning`, predicate
names, filenames). I rendered all of them as descriptions rather than reproducing them, following
the three finished anchors, which carry no identifiers at all. The reasoning: these incidents are
named *inside the document that assigns their labels*, so a verbatim identifier is a lookup key
straight to the answer key, in a way it would not be for a direction-(ii) unit. This is not a
lexicon requirement — the sweep would pass either way — so it is a judgement, and if the operator
wants the identifiers restored for codeability the units can be re-rendered without re-extracting.

### 4.4 One count discrepancy inside a source, recorded not smoothed

Unit 05's exemplar and its mechanism bullet both name **four** tracking documents. The resolution
note elsewhere in the same published file names **five** surfaces consolidated. The unit follows
the exemplar as named (four) and the discrepancy is recorded in the unit's own
`extraction_notes`. It does not affect the label.

---

## 5. Leak sweep — 7 units, all four coder-facing fields

**Matcher selftest:** `python3 python/audits/oq277_lexicon.py --check` → `GREEN — every control
fired as pre-registered`.

**Bundle non-emptiness assertion** (a sweep over empty strings returns 0 hits and looks clean):

```
bundled 7 units x 4 fields, 11349 chars of coder-facing text
    01_unread_sensitivity_results   1432 chars
    02_grouping_not_stamped         1430 chars
    05_tracker_sprawl               1513 chars
    06_three_edits_one_pasted       1560 chars
    08_offgrid_probe_time_zero      1979 chars
    10_filtered_warning_channel     1638 chars
    11_partial_grid_system_verdict  1797 chars
```

**Positive control, through the same bundle path** — a planted unit carrying taxonomy vocabulary
in each of the four fields:

```
swept 1 units x 4 fields, direction (ii): 35 hits    exit=1
  (fired in all four fields: p_tokens P1 / Pattern-1, pattern_nicknames 'dangling wire',
   pattern_names 'produced-but-not-consumed' + 'success-shaped absorption',
   taxonomy_phrases 'success-shaped' + 'measured-empty',
   source_identifying 'build_discipline' + 'OQ-97' + 'CLAUDE.md' + 'ISSUES.md')
```

**The real sweep:**

```
swept 7 units x 4 fields, direction (ii): 0 hits     exit=0
swept 7 units x 4 fields, direction (i):  0 hits     exit=0
```

Clean in the required direction and clean in the bonus one. What this licenses and what it does
not: it says none of the banned strings survived in the four fields. It does not say a coder
cannot recognise the source — the sources are the densest taxonomy prose in the repository and
the de-identification in §4.3 is a judgement, not a checked fact.

---

## 6. Call count, recomputed from units on disk

Not carried from `198 + 21`. Counted:

```
coder_direction_i_map.json    n_items = 30
coder_direction_ii_map.json   n_items = 36
                              assembled items counted = 66
packets/iii_prime_units/*.json                        =  7
                              total items             = 73
                              73 x k=3                = 219
```

**73 items / 219 calls.** It agrees with the carried figure; the agreement is the finding, not the
method — the numbers above come from `n_items` in the two assembled maps plus a glob of the
directory I just wrote, and `k=3` is the pre-registered unanimity draw.

**Two ways this recomputation is still soft, stated rather than left implicit.** (a) It assumes
each of my 7 unit files becomes exactly one packet item; the builder is the authority on that and
it has not been run — step 2 of the freeze sequence, which is not mine. (b) If the §4.1 collision
check drops unit 11, the totals become **72 items / 216 calls**. The freeze sequence's step 2 and
step 3 recomputations remain owed and are not discharged by this section.

---

## 7. Boundaries observed

- **No model call was made.** `payloads/` and `responses/` untouched; I ran no driver, stubbed or
  otherwise.
- **Nothing outside `packets/iii_prime_units/` was written or modified.** Commits name that path
  explicitly (another instance is committing to this repo concurrently).
- **Not opened:** `prompts/`, `responses/`, `matrices/`, `packets/our_units/*.json`,
  `packets/wu_units.json`, `packets/coder_direction_i.json`, `PREREGISTRATION.md`, any commit
  message. The two packet files I did touch (`packets/run/*_map.json`) were read for the scalar
  `n_items` only, via a script that printed five named metadata keys and never the item list.
- **The frame was not redrawn.** 11 named, 10 eligible, 7 written, 3 referenced, 1 disqualified,
  P3 empty. I widened nothing and substituted nothing.
