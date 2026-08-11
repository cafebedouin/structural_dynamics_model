# HANDOFF — extractor A's second half (direction (ii), units 8–13 of A's 13)

> ## ✅ CLOSED 2026-08-11 — A's half is 13 of 13. Entry point is now `HANDOFF_EXTRACTOR_B.md`.
>
> All six remaining units landed (`packets/our_units/04, 09, 10, 11, 12, 13`), commits
> `959a1596`, `6c625274`, `3c976d12`. Final sweep: **13 units × 4 fields, direction (ii),
> 0 leak hits**. Schema check: 0 gaps; `incident_location` = subject 10 / incidental 2 /
> self_audit_subsection 1. **Zero NO-UNIT proposals** — every directory in A's half reported an
> incident, so §H.2's `k` is not closed until B's half lands.
>
> **Do not work from this file's task list.** It is retained for the conventions in
> *"Conventions established"* (which bind B) and for the record of why A split at 7.
>
> **⚠ The blind-overlap rule in §1 below now has teeth.** When it was written, no overlap unit
> existed anywhere, so it could not be violated. Four unit files now exist that extractor B must
> not open (`01`, `06`, `12`, `13`), and `git show` on the commits above prints their bodies.
> The prohibition list and the two leak channels are in `HANDOFF_EXTRACTOR_B.md`.

**Written:** 2026-08-11 by extractor A after landing 7 of its 13 units.
**Read first:** `HANDOFF.md` (original), then `frame/extraction_split.json`, then this file.
**You are the EXTRACTOR. You never code.** Assigning a pattern to a unit breaks the experiment.

## Why this handoff exists, stated as the rule it follows

The operator's split ruling: *attention degrades before compaction, continuously and invisibly,
which is the failure with no error signal.* I stopped at 7 because the remaining six directories
include the two largest in my half, and extracting those in a depleted context produces thin units
— which bias toward `other` and confound control (c), the control built to measure exactly that.

**Applying this arc's own rider: these volumes are MEASURED, not estimated.** The previous handoff
estimated 184 KB where the truth was 737 KB, wrong by 4x in the direction that made the handoff
look smaller. Measure before planning against any number here.

## State

| | |
|---|---|
| A's units DONE | **7 of 13** — `packets/our_units/0{1,2,3,5,6,7,8}_*.json` |
| A's units REMAINING | **6**, measured at **263.1 KB** total |
| Leak sweep on all 7 | 0 hits, direction (ii) |
| B's half | untouched — B's 11 primary + 2 overlap are B's |

**Remaining, measured:**

| directory | KB | note |
|---|---|---|
| `2026-06-07_stakeholder_layer_migration` | 75.6 | largest in A's half |
| `2026-06-21_maxent_seat_aware` | 70.6 | |
| `2026-06-21_oq138_fsm_route_conversion` | 40.1 | |
| `2026-06-25_oq182_trajectory_revive` | 37.7 | |
| `2026-06-27_oq124_oq149_committer_convention_control` | 21.5 | **OVERLAP** — B extracts this independently |
| `2026-07-11_oq186_oq188_readsite` | 17.6 | **OVERLAP** — B extracts this independently |

**The two overlap units are the extraction-churn floor and are load-bearing.** Extract them
without reading B's version, ever. If you read B's unit first the floor measures agreement between
a unit and its own copy, and the H5-gate extension decision (`verdict_grammar_amendment.md` §E)
rides on that number.

## Conventions established — follow them, they are not preferences

1. **One unit per directory**, written as its own file in `packets/our_units/NN_slug.json`. Per-unit
   files so a compaction boundary costs at most the unit in flight.
2. **Fields:** the four coder-facing ones (`symptom`, `mechanism_as_described`, `detection_path`,
   `consequence`) plus `source_dir`, `extractor`, `role`, `files_read`, `extraction_notes`.
3. **`files_read` is mandatory** — it is what makes the adjudicator's fidelity spot-check
   re-derivable instead of a trust exercise.
4. **The unit is the INCIDENT the directory reports, never the directory's own subject.** An audit
   about clustering whose incident is a mis-parsed field yields a unit about the mis-parsed field.
5. **When a directory carries more than one defect, record the one not extracted** in
   `extraction_notes.alternatives_not_extracted`, with why. Three units so far needed this. It
   keeps the choice auditable rather than invisible.
6. **Write units free of the P-lexicon at composition time**, then sweep. Do not write freely and
   redact after — redaction-after produces mangled mechanisms, and mangled mechanisms bias toward
   `other`.
7. **Sweep after every unit:**
   ```
   cd audits/2026-08-10_oq277_rq2_crosscoding/packets
   python3 - <<'PY'
   import json,glob,sys; sys.path.insert(0,'../../../python/audits')
   import oq277_lexicon as L
   for f in sorted(glob.glob('our_units/*.json')):
       u=json.load(open(f))
       for fl in L.CODER_FACING_FIELDS:
           for h in L.scan(u.get(fl,''),'ii'): print('LEAK',f,fl,h)
   PY
   ```
8. **Commit every 2–3 units.** In-flight work is what compaction destroys.

## A live issue you will hit, and must NOT resolve by manufacturing

`2026-06-15_oq131_six_observer` nearly yielded **no unit**: it is a positive-result measurement
study, every gate passed, and its only incident sits in a self-audit subsection rather than in its
subject. I extracted it, and flagged it.

**If a sampled directory carries no silent-defect incident at all, record a NO-UNIT file with the
reason. Do not manufacture one.** A fabricated unit is worse than a missing one: it enters cells
and cannot be distinguished from a real unit afterwards.

**Why this matters beyond bookkeeping — it is a possible finding.** The escape check measures the
keyword proxy's **recall** (dirs it missed). Nothing measures its **precision** (dirs it admitted
whose incident is incidental to their subject). If NO-UNIT directories turn up in the primary
sample, that is evidence the 73 numerator behind the published 42% is an **overcount**, in the
opposite direction from the escape check's undercount. Count them; do not average them away. The
figure has now survived two denominator corrections — assume a third defect exists.

## After extraction completes (both halves)

Per `HANDOFF.md` §4 and the amendment:

1. Controls — anchors (3/direction), decoys (2), redaction-bias pairs (3/direction, quarantined
   and leak-exempt), planted leak, planted broken unit. **For the redaction pairs, prefer the 9 of
   22 directories carrying heavy pattern vocabulary** (`2026-07-20_five_leg_twin_comparison` at 21
   hits, `2025-05-15_recon_2` at 7, `2026-06-11_oq97_pattern6_census` at 4) — that is where the
   redacted-vs-unredacted delta is largest and the floor is actually informative.
2. Driver `python/audits/oq277_crosscoding_driver.py` wrapping `call_with_retry`, dumping every
   assembled payload BEFORE send, importing the leak-grep from `python/audits/oq277_lexicon.py`
   — **do not write a second matcher.**
3. `PREREGISTRATION.md`, incorporating `verdict_grammar_amendment.md` **verbatim**, md5 into
   `audit_log.md` **above** the first result line.
4. Request operator spend-go.

**Before Phase 3: assert the driver's payload-capture count EQUALS the expected call count, then
grep.** A capture bug writing zero payloads yields a clean leak-grep and a green H2. Count first.
Overlap units are quarantined from matrices but their calls still count in that total.

---

## Added 2026-08-11 (operator) — read these two before you start

### 1. The blind-overlap rule: NOT EVEN ONCE, and the likely violation is accidental

You will be tempted to open A's version of an overlap unit "just to check the format." **Do not.
Not once, not partially, not for the schema.** The format is fully specified above and in any
non-overlap unit — `packets/our_units/02_blocking_gate.json` is a fine template and is *not* an
overlap unit.

Reading A's version silently converts the extraction-churn floor into a self-comparison: the two
"independent" extractions are then one extraction and its echo, they agree by construction, and the
measurement reports 4/4 INSIDE FLOOR no matter what the truth is. **The H5-gate extension decision
rides on that number** (§E of `verdict_grammar_amendment.md`), so the failure would not stop at the
control — it would license a scope decision on a fabricated basis.

This is the instruction most likely to be broken by accident rather than by choice, which is
exactly why it is stated this bluntly. The overlap dirs are
`2026-06-27_oq124_oq149_committer_convention_control` and `2026-07-11_oq186_oq188_readsite`.

### 2. Redaction-bias pairs are ALREADY PRE-DECLARED — do not choose them

`controls/redaction_pairs_predeclared.json`, fixed **before** the remaining units exist, by a
mechanical rule. Choosing pairs after seeing which units came out thin would select the control on
the outcome it measures.

Two of the three direction-(ii) pairs are **in A's unextracted half** —
`2026-06-21_oq138_fsm_route_conversion` and `2026-06-07_stakeholder_layer_migration`. Extract them
exactly as you extract any other unit. **Do not write them "more carefully" because you know they
are control units** — that would inflate the unredacted arm and shrink the measured floor toward
zero, which reads as "redaction costs nothing" and would retire a control by flattering it.

### 3. NO-UNIT has a boundary rule now — apply it, do not re-decide it

`verdict_grammar_amendment.md` §H.1: **a directory yields a UNIT if its prose REPORTS an incident
anywhere in the document**, whether or not it is the subject; **NO-UNIT only if the prose DISCUSSES
the concept without reporting an instance.** Every unit carries a new mandatory
`metadata.incident_location` field: `subject` | `self_audit_subsection` | `incidental`.

A NO-UNIT call is **operator-confirmed, never self-certified** — you proposing NO-UNIT and you
having extracted thinly are the two competing explanations for the same observation, and you cannot
adjudicate between them. Write the NO-UNIT file with your reason and the files you read; the
operator rules.
