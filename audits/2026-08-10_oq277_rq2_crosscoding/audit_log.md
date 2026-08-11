# Audit log — OQ-277 RQ2 cross-coding

Append-only. One entry per frozen artifact or irreversible step, with the witness that pins it.

## 2026-08-11 — threshold-comparability probe: judging artifacts FROZEN

Pre-registration: `packets/escape_units/PREREGISTRATION_threshold_calibration.md`.
Assembler: this instance. Judge: a fresh instance, not yet run, **spend-go NOT granted** — no model
call has been made.

### Artifacts and their md5, recorded at freeze

| file | md5 | bytes |
|---|---|---|
| `packets/judging/PACKET.md` | `d72b28b4a99c4d755338784981d74e64` | 12768 |
| `packets/judging/KEY.md` | `e63c66ea8832cb0f8853c9398d72e21e` | 1705 |
| `packets/judging/VERDICT_TEMPLATE.md` | `ae3ad399d7cab87daf60ad1a7e8cbc4e` | 96 |

Reproducers, committed alongside: `packets/judging/draw_primary.py` (primary draw),
`packets/judging/assemble_packet.py` (composition, order, §H.1 redaction). Held, never shown:
`packets/judging/_held_primary_draw.json`.

### Ordering constraints — the reason this log exists

Pre-committed by the operator, and the whole point is that each step is witnessed by a commit that
precedes the disclosure it gates:

1. **KEY.md is committed BEFORE PACKET.md is shown to any judge.** Discharged: both are frozen in
   this commit, and the packet has been shown to no one at the time of writing.
2. **The judge's completed verdict file is committed BEFORE KEY.md is opened.** Not yet due — the
   pass has not run. The verdict file is `packets/judging/VERDICTS.md`, from the blank
   `VERDICT_TEMPLATE.md`; commit it, record its md5 here, and only then open the key.
3. **Neither artifact may be edited after the other is seen.** The md5s above are what makes that
   checkable rather than promised. Re-verify with
   `/usr/bin/md5sum packets/judging/{PACKET,KEY,VERDICT_TEMPLATE}.md` before opening the key; any
   drift from the table voids the pass.

### Seeds

| draw | seed | recorded |
|---|---|---|
| escape-side (2 candidates) | `20260811` | in the prereg, before assembly, not re-rolled |
| primary-side (2 known-positives) | `20260811` | `draw_primary.py`, before assembly |
| presentation order | `20260811` | `assemble_packet.py` |

### Declared at freeze — read with any outcome

- **Location match EXACT**; no substitution, nothing to declare on that axis.
- **§H.1 ships redacted**: 2 paragraphs omitted, marked in place with reasons and count, full text at
  `verdict_grammar_amendment.md` @ `4360fcdc`. Verbatim shipping would have disclosed one item's
  verdict and the stratification key.
- **Amendments 1 and 3 RETIRED, not satisfied.** No recognition flags exist; the outcome table's
  recognition row cannot fire. The same-family confound applies to the calibration arm and is
  declared, not repaired.
- **Field length separates the two strata perfectly** in this draw (gap 1,802 chars, no lexical
  tells). Unrepairable without breaking the verbatim pin; biases toward AGREE, the expensive outcome.
- **The probe is n=4 and detects only a gross threshold mismatch.** An AGREE must be re-fired at a
  different presentation order before it licenses scoring; a VOID stands at k=1.

### Leak sweep on PACKET.md, run after freeze

255 forbidden strings checked (every unit's `source_dir`, filename, `extractor`,
`incident_location`, `files_read`, and every `extraction_notes` value, across both strata,
length > 6). **1 hit, localized and benign:** the token `subject` occurs 3× in the retained §H.1
rationale ("is the incident the *subject*?") and **0× inside the four items** — including 0× in any
shown field of any unit. It is the rule's own wording, which Amendment 4 mandates shipping; the
paragraph that *defines* `subject` as an `incident_location` value is one of the two redacted ones,
so the stratification key is not disclosed by it.

Two controls, both fired: a string known to be in the packet matched, and a planted forbidden string
changed the hit set — so an empty-ish result here is a measurement, not a matcher that never ran.
