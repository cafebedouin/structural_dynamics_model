# HANDOFF — (iii′) exemplar extraction (7 new units, 3 referenced). Read ONLY this file and the sources it names.

**Written:** 2026-08-11, by the twins-packets-prompts-driver instance.
**You are the EXTRACTOR. You never code.** If you find yourself deciding which pattern a unit
*should* be labelled, you have broken the experiment. You already know every true label — they are
printed in the table below — and that is exactly why the discipline here is about what reaches the
TEXT, not about what you know.

## Why this is a separate instance, and what the separation actually buys

The instance that wrote this brief is authoring the **coder prompts**, which means it is fixing the
exact wording in which P1–P6 are put to the coder. An extractor who has just spent a session
phrasing the six patterns will, without intending to, phrase incidents in the vocabulary of the
definition it just wrote — and the (iii′) row measures precisely whether a blind coder recovers the
published label from the incident alone. A unit written in the definition's own words would make
that row report the prompt's fluency and call it inter-rater agreement.

So the split is: **it holds the prompts, you hold the units, and neither of us does both.** This is
narrower than the escape check's blindness condition (C1) — you are NOT required to be ignorant of
the primary sample's results, and you may know the true labels. Your obligation is a WRITING
obligation.

## Do not open

| do not open | why |
|---|---|
| `responses/`, `matrices/` | coding results (currently empty — keep it that way) |
| `prompts/` | the coder prompts. Reading them is the one thing that would undo the split above. |
| `packets/our_units/*.json` | the 26 primary-sample units. Not a leak risk to you, but see "do not re-extract" — you must not converge your 7 onto their phrasing either. |

`git log` / `git show` is a **live leak channel** in this arc (`verdict_grammar_amendment.md` §J):
a commit message once summarized two unit bodies. Do not read commit bodies for this audit.

Reading `RECON.md`, `HANDOFF.md`, `RULING_2026-08-11_freeze_scope.md` and
`verdict_grammar_amendment.md` is fine and expected — they carry rulings.

## What (iii′) is

Direction (i) codes **Wu's** incidents against our six. Direction (ii) codes **our** incidents
against Wu's five. **(iii′) codes our own PUBLISHED EXEMPLARS against our own six** — the
incidents `CLAUDE.md`'s Build Discipline block names as instances of P1–P6, put to a blind coder
to see whether the published label is recoverable from the incident alone.

It is an **inter-rater calibration row, not a verdict** (`RULING_2026-08-11_freeze_scope.md` §2.4).
The true label is not your judgement — it is read off the section the exemplar is named inside,
the same `label_source` boundary `controls/anchors.json` already uses.

**This makes your redaction obligation heavier than direction (ii)'s, not lighter.** In direction
(ii) a leak lets a coder guess the source. Here a leak hands the coder the answer key: these
incidents live *inside the document that assigns their labels*, so their natural prose is the
taxonomy's own prose.

## Your population — 10 eligible, of which you write 7

Boundary: **an exemplar incident named inside `CLAUDE.md`'s six-pattern Build Discipline block**
(lines 472–540), with mechanism text drawn from `docs/technical/build_discipline.md`. Enumerated in
`RULING_2026-08-11_freeze_scope.md` §2.1; every line citation below was re-verified against
`CLAUDE.md` on 2026-08-11 before this brief was written.

> **Line numbers are pinned to commit `9d9e62c1`** (`CLAUDE.md` 998 lines, last touched
> `f95fc857`, 2026-08-11). `CLAUDE.md` is a high-churn file. **If a cited line does not contain
> the exemplar named beside it, the line number has drifted — locate the exemplar by CONTENT
> (grep the quoted phrase) and record the corrected line in your notes.** Do not extract whatever
> happens to be at the stale line number: that silently substitutes a different incident into a
> row keyed by published label, and it would read as a coder disagreement rather than as an
> extraction error.

| # | pattern | exemplar | CLAUDE.md | you do |
|---|---|---|---|---|
| 1 | P1 | unread `*_sensitivity_results.json` | 473 | **WRITE** |
| 2 | P1 | `kernel_grouping.json` not stamped into the `.pl` files | 474 | **WRITE** |
| 3 | P1 | `w1_sheaf_join` froze at n=563 while the corpus grew to 772 | 479 | **REFERENCE** `anchor_i_1` |
| 4 | P2 | duplicated `generate_kernel_corpus.py` | 486 | **REFERENCE** `anchor_i_2` |
| 5 | P2 | the ISSUES/AGENDA/PRIORITIES/TODO tracker sprawl | 486 | **WRITE** |
| — | **P3** | **— none named —** | — | **nothing exists to write** |
| 6 | P4 | "three edits witnessed," with only the third pasted | 501 | **WRITE** |
| 7 | P5 | `natural_law_signature`'s `BeneficiaryCount == 0` / the 404 NL certifications | 508 | **REFERENCE** `anchor_i_3` |
| 8 | P5 | the OQ-178 dual — `classify_at_time` at `Time=0` off the authored grid | 515 | **WRITE** |
| 9 | P6 | `system_gradient`'s `[] → 0.0` fallback | 524 | **DISQUALIFIED — do not write** |
| 10 | P6 | `grep -v Warning` suppressing a fatal warning for four months | 526 | **WRITE** |
| 11 | P6 | findall-over-partial-levels — an 8/32 one-level grid read as a system verdict | 527 | **WRITE** |

**11 named, 10 eligible, 7 for you.** The frame is frozen. Do not redraw, substitute, widen, or
extend it — in particular, do not recover a larger n by admitting exemplars attached to the *other*
Build Discipline rules (false-absence, the relocating confound, and the rest). Those are named as
instances of different rules, so their P1–P6 label would have to be **assigned rather than read
off**, and assigning is coding.

### Two constraints carried VERBATIM from the operator

> **P3 has zero exemplars.** Its row entry is `no members — uncalibrated`. Never read as
> agreement, never counted in the denominator, never reported as "no disagreement observed."
> Coverage is P1(3)/P2(2)/P4(1)/P5(2)/P6(2).

An empty cell and a cell where coder and publication agreed are the same shape at the read site,
and collapsing them is the absorption defect this experiment studies. The population is **empty,
not thin** — no amount of n buys it. If you find yourself wanting to write a P3 unit so the row
looks complete, that impulse is the finding, not a gap to fill: record it in your notes and write
nothing.

> **⚠ DO NOT SMOOTH THIS LINE.** Of everything in this brief, `no members — uncalibrated` is the
> phrase a well-intentioned instance is most likely to improve into something that reads better —
> "no disagreement observed," "P3: n/a," "P3: consistent," or simply a blank cell. **Every one of
> those is wrong, and wrong in the same direction:** each is the shape a cell takes when coder and
> publication AGREED, so each converts *we could not measure this* into *this came out fine*. That
> substitution is the exact defect the (iii′) row is being run to detect, performed on the row
> itself. Write the phrase as given, in the row and in your notes. If a downstream reader finds it
> ugly, that is the phrase doing its job.

> **`anchor_i_1` (`w1_sheaf_join`) is already extracted and is taken AS-IS, not re-extracted.**
> Two extractions of one source would silently become an unplanned floor pair. Same for
> `anchor_i_2` and `anchor_i_3` — 3 of the 10 are existing direction-(i) anchors, so the extractor
> writes **7 new units** and references the 3.

**Do not write a file for #3, #4 or #7.** Not a re-extraction, not a "cleaned-up version", not a
second unit "in the (iii′) house style." The redaction twins in this same audit are pairs precisely
*because* one incident was deliberately rendered twice; an accidental second rendering of an anchor
would be an unlabelled, unquarantined twin sitting inside a calibration row, and it would report as
agreement.

You MAY read `controls/anchors.json` to see the three finished units — for format and register, so
your 7 sit alongside them coherently. That is the only reason to open it, and reading it does not
license rewriting them.

> **Row reports with and without the 3 anchor members** (§2.1 point 3), since including them
> measures partly the same calls H3 already consumed.

You do not compute that row. Record in your notes which 3 are references so the accounting can be
run both ways without a reader having to re-derive membership.

## Unit format — identical to the primary sample

One JSON per unit you WRITE, to `packets/iii_prime_units/NN_<slug>.json`. **`NN` is the population
row number from the table above (`01`, `02`, `05`, `06`, `08`, `10`, `11`) — NOT a fresh 1..7
sequence.** Keeping the population's own numbering means the gaps ARE the record: a reader seeing no
`03`/`04`/`07` file finds the three referenced anchors, and no `09` finds the disqualified P6
exemplar, without consulting a second document. A renumbered 1..7 would erase exactly the four
absences this row must declare.

```json
{
  "source": "CLAUDE.md:473 (Build Discipline pattern block); mechanism text docs/technical/build_discipline.md:NN-NN",
  "extractor": "iii_prime",
  "role": "iii_prime",
  "matrix_unit": false,
  "true_label": "P1",
  "label_source": "named inside CLAUDE.md's Build Discipline section 1 at line 473. Read off, not assigned.",
  "files_read": ["CLAUDE.md", "docs/technical/build_discipline.md"],
  "symptom": "...", "mechanism_as_described": "...",
  "detection_path": "...", "consequence": "...",
  "extraction_notes": {"selection_rule_applied": "...", "alternatives_not_extracted": ["..."]},
  "metadata": {"incident_location": "subject|incidental|self_audit_subsection", "incident_location_note": "..."}
}
```

- **`matrix_unit` is ALWAYS `false`.** (iii′) units can never enter a direction-(i) or (ii) cell.
  Quarantine keys on this field and nothing else (§I.2) — never on `role`, never on `true_label`.
- **`true_label` and `label_source` are NOT coder-facing.** They live in the unit file so the row
  can be scored; the packet builder strips them. Only the four fields below ever reach a coder:
  `symptom`, `mechanism_as_described`, `detection_path`, `consequence`.
- **Unit = INCIDENT, never manifestation.** One incident per exemplar.
- **A unit can only carry the mechanism the prose states.** Where `build_discipline.md` describes
  an exemplar in one clause, say so in `extraction_notes` and write the thin unit honestly rather
  than inflating it from your own knowledge of the codebase. A unit padded with mechanism the
  source does not state measures your reconstruction, not the publication's.
- **`metadata.incident_location` will not fit these cleanly, and that is expected.** The primary
  sample's three values classify where in a *report* an incident sits; a (iii′) exemplar sits in a
  *definition block*. Use `subject` when `build_discipline.md` gives the incident its own worked
  section, `incidental` when it appears only as a parenthetical inside the rule's statement, and
  record the strain in `incident_location_note`. Do NOT invent a fourth value — §N already forbids
  pooling `incident_location` across strata, so a new token here would create a category that
  exists in one stratum and silently reads as missing in the others.

## Redaction — the part that is easy to get backwards

**Sweep (iii′) units under direction `ii`.** This looks wrong and is right: `--direction ii` is the
list that strips **our** P-lexicon, and these are **our** incidents. The label space the coder
answers in (P1–P6) is direction (i)'s, but the vocabulary that must not reach them is ours.

```
python3 python/audits/oq277_lexicon.py --sweep audits/2026-08-10_oq277_rq2_crosscoding/packets/iii_prime_units/NN_x.json --direction ii
```

> **REVISION 2026-08-11 (operator ruling) — this command now works as written; it did not
> before.** Until commit `3e16a1d8` the sweep could not consume a single-object unit file: it
> died on `KeyError: 'units'` and **exited 1, which is also the leaks-found code**, while
> printing no `LEAK` lines. A caller reading stdout for leaks saw a clean sweep. **Two
> extractors were sent this command and both hit it**, working around it by bundling units into
> a list; the second reported it as an instruction that is correct in prose and wrong when
> executed, which is the refusal that got it fixed.
>
> The corrected tool accepts a bare single unit object, a `{"units": [...]}` wrapper, and a
> list; **exit 3 now means "did not sweep"**, distinct from `1` = swept and found hits, and an
> abort prints `SWEEP-ABORTED — no verdict was produced` **on stdout**. A units file it cannot
> recognise raises rather than sweeping zero.
>
> **If you are reading this brief as a receiver: the command above is verified, but the
> instruction preceding it was wrong for two full extractions. Run the tool's `--check` first
> — the seven `input-shape` cases are the proof this specific breakage is gone.** Marked in
> place rather than silently corrected, because the brief was frozen when both receivers hit
> it and the record of a specification that read correctly and executed wrong is worth more
> than a clean page.

Strip from the four coder-facing fields:

- every pattern name, nickname and index — `Pattern 1`, `P1`, `dangling wire`, `silent fork`,
  `faith merge`, `produced-but-not-consumed`, `success-shaped`, `measured-empty`, `didn't-look`;
- **the hyphen-joined forms too** — `Pattern-1`, `Build-Discipline`, `Build-discipline`,
  `success-shaped-absorption`. The lexicon was widened on 2026-08-11 to catch these because three
  of them were escaping it in exactly the source files you are about to read
  (`AUDIT.md:144`, `FINDINGS.md:22`, `WRITEUP.md:1`). Your sources are the densest taxonomy prose
  in the repository; expect the sweep to fire and expect to rewrite;
- source-identifying vocabulary: `OQ-nnn`, `ISSUES.md`, `KNOWN_STATE`, `CLAUDE.md`,
  `build_discipline`, `deferential realism`.

**Do NOT strip shared subject matter** — `silent`, `never fired`, `green`, `empty`, `absent`,
`stale`, `fallback`, `count`, `zero`, `aggregate`, `layer`. It belongs to neither taxonomy, and
removing it destroys codeability, which biases units toward `other` — the exact bias control (c)
exists to measure. Over-redaction corrupts the control meant to catch it.

**The specific trap for this row.** These exemplars are named *inside their own definitions*, so
the source sentence often IS the definition. "A producer is not done until something consumes its
output" is the rule, not the incident. Write what happened — an artifact was generated, written to
disk, and nothing ever read it back into the thing that needed it — and let the coder do the
matching. If your `mechanism_as_described` could be pasted into the taxonomy as a definition, you
have written the label, not the incident.

## Done means

1. **7 files** in `packets/iii_prime_units/`, one per WRITE row above. No file for #3, #4, #7 or
   #9. No P3 file.
2. **Leak sweep clean** over every coder-facing field of all 7, under `--direction ii`, pasted.
3. **An ORDERED LIST of every file you opened, written AS YOU GO, not reconstructed at the end**
   (operator ruling, 2026-08-11), in `packets/iii_prime_units/EXTRACTION_NOTES.md`. Its limit,
   stated so nobody treats it as closing the question: the list is authored by the party it
   constrains, so it converts an assurance into a *checkable* assurance, not into proof.
4. A short `packets/iii_prime_units/EXTRACTION_NOTES.md` carrying: the 7 written and the 3
   referenced by id; P3 recorded as `no members — uncalibrated` with the reason; for each unit
   whether `build_discipline.md` gave you a full mechanism or a clause; and any exemplar you found
   genuinely ambiguous — **escalate rather than decide**.
5. **No model call.** `payloads/` and `responses/` stay empty until the operator's spend-go at
   prereg freeze. If you think a live call is required, **stop and ask** — do not decide it.

Then hand back. Packet assembly, the driver, the prompts and the prereg are the other instance's.

## One thing to flag if you hit it

If an exemplar's incident turns out, on reading `build_discipline.md`, to be **the same incident as
another exemplar under a different pattern index**, stop and report it rather than writing both.
That is the OQ-278 index-collision shape and it has now been sighted three times in this arc
(`controls/redaction_pair_selection_defect.md` → "Third instance of the same shape"). A fourth
sighting inside the calibration row itself would be worth more than the unit.
