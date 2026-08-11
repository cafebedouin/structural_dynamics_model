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

## 2026-08-11 — pass 1 run; ordering constraints discharged in full

| step | witness |
|---|---|
| KEY.md committed before the packet was shown | `b49fd273`, packet shown to no one at that commit |
| judge's verdicts committed before KEY.md opened | `32228a4a`, md5 `bcce03a51669c5e6375e4258881beee9` |
| neither artifact edited after the other was seen | md5s re-verified against the freeze table at each step, no drift |

`packets/judging/VERDICTS.md` — pass 1, four `extract`, one tool call total (the isolated Read), no
self-reported breach. Judge blindness rested on instruction plus single-file isolation, backed by the
harness tool-call count; **not mechanically enforced — declared, not repaired.**

**Outcome: AGREE, braked twice and not scorable.** Both brakes were pre-registered before the pass:
the re-fire owed by any model judge, and the length-tell ruling under which AGREE does not license the
six candidates proceeding to §H.2. Reordering cures the first and cannot touch the second.

**The sharper result: the calibration arm did not discriminate.** All-`extract` passes the arm by
construction, so this pass cannot separate "shares the threshold" from "extracts everything." It
cannot be repaired from existing material — §M records k = 0, so no known-negative exists to draw.
The arm is **one-sided by construction**. Full reasoning in the prereg's Result section.

## 2026-08-11 — cross-coding PREREGISTRATION assembled; DRAFT stamp, freeze NOT yet complete

**No model call has been made in this audit.** `payloads/` and `responses/` verified empty
(0 files each) at the time of writing.

### The stamp

| artifact | md5 | status |
|---|---|---|
| `PREREGISTRATION.md` (1991 lines, assembled) | `c1040cd04815c206791b5ab3192697be` | **DRAFT — not the freeze** |

*Supersedes draft `95e1fc00368a6b7bf4d2886cf02e4c65` (1908 lines), invalidated the same day when
`verdict_grammar_amendment.md` gained §L.6 and §L.7. Recorded rather than overwritten: the
document's md5 changed because an incorporated source changed, and
`oq277_build_prereg.py --check` went RED on exactly that — a naturally-arising fire of the
byte-identity check, not a planted one. Had this been the real freeze, that RED would have been
the invalidation notice.*

**Why this is a draft and not the freeze.** One designed leg is not built: the **7 (iii′)
exemplar units** are the pending hand-back from `HANDOFF_IIIPRIME_EXTRACTOR.md`. The assembled
packets hold **66 of the designed 73 items**; expected calls stand at **198 of 219**.

A freeze stamp over an incomplete design would be a success-shaped token — it would look
exactly like a freeze, and every check below it would pass. So the ordering is enforced
structurally rather than by this label: `oq277_crosscoding_driver.py --live` **refuses while any
leg is unbuilt**, before it even looks at the md5, because an md5 over a subset would silently
re-pre-register a smaller experiment as though it were the designed one.

### Completing the freeze — the exact sequence

1. (iii′) extractor hands back 7 units to `packets/iii_prime_units/`.
2. `python3 python/audits/oq277_make_coder_packets.py --build-run` → confirm **73 items**, no
   `INCOMPLETE` banner.
3. `python3 python/audits/oq277_crosscoding_driver.py --stub --dry-run` → confirm
   `captured == expected == 219`, printed **above** the grep output.
4. `python3 python/audits/oq277_build_prereg.py --write` → new md5; `--check` must be GREEN.
5. Record that md5 in this file, **above the sentinel below**, as the FREEZE stamp.
6. Request spend-go. Do not code before it is given.

> **RECOMPUTE 219, DO NOT CARRY IT.** `198 + 21 = 219` is stated in this file and in the handoff
> messages, and it is exactly the kind of premise this arc keeps catching: an arithmetic that is
> almost certainly right, sourced from a partial view, reaching a consumer with no way to check
> it (§L.1). At step 2 the total must come from **units on disk** — the builder counts the
> packets it just wrote — and at step 3 from the driver's own `expected`, computed from those
> packets. **Neither may be satisfied by matching the number written here.**
>
> Two ways the carried figure could be wrong, both cheap to miss: the extractor hands back a
> number other than 7 (an exemplar found ambiguous and escalated, or two exemplars discovered to
> be one incident — the brief's closing flag), or one of the 7 fails the leak sweep and does not
> enter the packet. In either case `73` and `219` are the wrong targets, and a step-2 check that
> confirms "73, as expected" would pass by agreeing with a stale message rather than by counting.
> **If the recomputed total is not 73/219, that is a finding to report, not a discrepancy to
> reconcile.**

### The first-result boundary, marked explicitly

The rule is that the preregistration md5 must sit physically **above the first coding result**.
That boundary is now a machine-checkable sentinel rather than a matter of reading. It is marked
this way because the driver's first version searched the log for a line saying "first result",
this file contained no such line, and the check therefore **passed vacuously** — a gate
satisfied by the absence of its own input, which is the defect class this experiment exists to
study. The sentinel now **fails closed**: no marker, no live call.

## 2026-08-11 — amendment gains §O.3 and §L.8; DRAFT stamp INVALIDATED, re-stamp owed at freeze step 4

**No model call has been made in this audit.** The (iii′) extraction is in flight; the driver still
refuses while that leg is unbuilt.

Two operator-flagged records were written into `verdict_grammar_amendment.md`, which is incorporated
verbatim into `PREREGISTRATION.md`:

- **§O.3** — the P6 calibration gap is ONE incident (`system_gradient` `[] → 0.0`) appearing in three
  instruments (direction-(i) anchor: disqualified; (iii′) row 9: disqualified; direction-(ii) twin
  unit `05`: used, role-appropriate), not three independent residues. The residues are correlated,
  not additive. No new defect; the correction is to the reading.
- **§L.8** — `oq277_build_prereg.py --check`'s RED on the §L.6/§L.7 drift, graded as the arc's second
  unplanted fire and explicitly graded BELOW §L.7: one real fire, but a decline set of three re-runs
  of one comparison, one of them taken immediately after `--write` and therefore near-tautological.

### The DRAFT md5 is now stale — recorded, not overwritten

| | md5 |
|---|---|
| shipped `PREREGISTRATION.md` (the DRAFT stamp above) | `c1040cd04815c206791b5ab3192697be` |
| fresh assembly after the amendment edits | `029a063cd0be390d17edd651e2836920` |

`--check` was **GREEN** immediately before the edits and **RED** immediately after:

```
GREEN — PREREGISTRATION.md is byte-identical to a fresh assembly
        md5 c1040cd04815c206791b5ab3192697be
        every verbatim appendix matches its canonical source
EXIT=0

RED — shipped PREREGISTRATION.md differs from a fresh assembly.
      A source changed after the document was built. Re-run --write, and if an md5
      is already recorded in audit_log.md, the freeze is INVALIDATED and must be re-stamped.
      shipped md5 c1040cd04815c206791b5ab3192697be
      fresh   md5 029a063cd0be390d17edd651e2836920
EXIT=1
```

**This pair is a PLANTED liveness witness, not a discrimination witness.** It shows the check is
wired and bidirectional at HEAD. It adds nothing to §L.8's grade, which rests entirely on the
earlier unplanted fire.

**No re-`--write` was run here, deliberately.** Rebuilding now would produce a third md5 that the
(iii′) hand-back invalidates again. The rebuild belongs at **step 4 of the freeze sequence above**,
after the units land and the item/call totals are recomputed from disk. Until then `--check` is
expected RED and that RED is correct — the document genuinely does not match its sources.

## 2026-08-11 — (iii′) hand-back received; unit 11 collision check DISCHARGED; totals hold at 73/219

**No model call has been made in this audit.** Units landed at `5ce2c37e`, pathspec-scoped to
`packets/iii_prime_units/` (8 files, 575 insertions, nothing else touched).

### The escalation the extractor could not resolve, resolved here

The extractor wrote unit `11_partial_grid_system_verdict` under a **declared, unresolved collision
risk** against extracted unit `our_units/05_oq93_grid_viability` — `our_units/` was on its
do-not-open list. Its stated premise was that rows 9 and 11 are "two instances in the same construct
from the same probe on the same day." **Checked against both unit texts and the source directory:
no collision, and the premise is also wrong.**

| | `our_units/05_oq93_grid_viability` (row 9's incident) | `iii_prime_units/11` (row 11) |
|---|---|---|
| symptom | a gradient metric read exactly `0.0` on every input, including ones built to be non-zero | an aggregate over 8 of 32 cells, all on one level, published as a whole-system directional verdict |
| mechanism | enumerator silently semidet (cut) → collection always **empty** → **fabricated default** `0.0` | collection **non-empty but unrepresentative**; coverage 0.25 was computed and printed, and **no consumer required it** |
| failure | absence rendered as a plausible measured value | partiality not carried to the read site |
| source | `audits/2026-06-10_oq93_grid_viability_probe/` (FINDINGS.md, PREREGISTRATION.md) | `CLAUDE.md:527-528` + `build_discipline.md:761-765` |

**The directory check, run because "same probe" was the load-bearing claim.** `2026-06-10_oq93_grid_viability_probe/`
contains **no** `8/32`, no `8 of 32`, and no `findall`. Its three `increasing_coercion` hits are
verdict labels in the gradient probe's own results table, not the partial-grid incident. Positive
control on the same grep path: `gradient` occurs 7× in `FINDINGS.md`, so the empty results are
measurements, not a grep that failed to read the file. **Same day (2026-06-10, the three Pattern-6
instances CLAUDE.md groups together), different probe.**

**Verdict: unit 11 stands.** Row 9's disqualification was incident-level and remains correct on its
own facts; it does not reach row 11. **Totals are unchanged — 73 items, 219 calls.** The §4.1
contingency (72/216) does not fire. *The operator may still overturn this on conservatism grounds —
it is an evidence call, not a ruling, and it is recorded here so the reversal has something to
attach to.*

### Standing hazard for the assembler, second sighting, NOT repaired here

`oq277_lexicon.py --sweep <one-unit>.json --direction ii` — the command the (iii′) brief prescribes
— **cannot consume the single-object format the same brief specifies** (`KeyError: 'units'`,
exit 1). The escape extractor reported this; the brief still carries it verbatim; this extractor hit
it again and worked around it by bundling into a list, tool unmodified.

**Why it is worse than a broken command.** Exit 1 is *also* the leak-found code, and the crash
prints no `LEAK` lines — so **a wrapper that reads stdout for leaks sees a clean sweep**. That is a
success-shaped absorption inside the leak gate itself. Both extractors caught it only because they
read the traceback. Not repaired here: the lexicon is a frozen pre-registered instrument and the
brief is a frozen handoff, both above the fix-on-sight threshold. **Escalated to the operator.**

### Verified independently at hand-back (not taken on the extractor's word)

- 7 units on disk, gaps intact at `03`/`04`/`07`/`09`, `matrix_unit: false` on all 7.
- `CLAUDE.md` at `9d9e62c1`: 998 lines, md5 `01084bd81a969da9c40a464ce29cfbb4` — matches what the
  extractor reported. **Now stale at HEAD** (1017 lines, md5 `a8597d83…`): this instance committed an
  unrelated CLAUDE.md insertion at line 598 (`03fda56e`) *while the extractor was running*. All 11
  cited lines are 473–527, so **no pin moved** — verified, not assumed. Recorded because the substrate
  was mutated under a running receiver, which is the frozen-substrate rule and it was luck that the
  insertion landed below every pin.
- The extractor's second OQ-278 datum, re-checked with a control: `build_discipline.md`'s numbered
  patterns are 1 produced-but-not-consumed, 2 silent-fork, **3 bound-probe-bypasses-clause-order**,
  **4 fabricated-default**, 5 absence-satisfies-gate, 6 success-shaped-absorption — so **the indices
  3 and 4 name different patterns in the two documents**, and CLAUDE.md's P3 (destructive-replace)
  and P4 (recap-as-witness) have **no worked section** in `build_discipline.md`. Control: the string
  `recap-as-witness` occurs 4× in that file as prose, so the heading probe would have found a section
  had one existed. This is OQ-278 seen from a third angle and it explains the (iii′) P3 row's
  emptiness mechanically: the published P3 has no worked mechanism to extract from.

## 2026-08-11 — PREREGISTRATION **FROZEN**. This is the freeze stamp.

**No model call has been made in this audit.** `payloads/` and `responses/` verified **0 files
each** at the moment of stamping. The only payloads that exist are the 219 stub dumps in
`payloads_stub/`, which were never sent.

| artifact | md5 | status |
|---|---|---|
| `PREREGISTRATION.md` (2256 lines, assembled) | `4118f64ecaab06260c2b30841121e7b2` | **FROZEN — this is the stamp** |

<!--OQ277-FREEZE-STAMP: 4118f64ecaab06260c2b30841121e7b2 -->

*The line above is the machine-readable form of this stamp, added 2026-08-11. `--check`
previously had to infer "frozen" from prose in the table, which it could not do, so it treated
post-freeze source drift as a fatal error and prescribed a re-write that would have destroyed
this record. A DRAFT stamp deliberately carries no such marker.*

*Supersedes DRAFT `c1040cd04815c206791b5ab3192697be` (1991 lines) and the earlier
`95e1fc00368a6b7bf4d2886cf02e4c65` (1908 lines). Both are recorded rather than overwritten; each
was invalidated by an incorporated source changing, and `--check` went RED on exactly that both
times.*

### The freeze sequence, discharged step by step

| step | witness |
|---|---|
| 1. (iii′) units handed back | 7 files at `packets/iii_prime_units/`, commit `5ce2c37e`; gaps at `03`/`04`/`07`/`09` intact |
| 2. `oq277_make_coder_packets.py --build-run` | **73 items · 219 calls at k=3**, no `INCOMPLETE` banner; all build gates passed |
| 3. `oq277_crosscoding_driver.py --stub --dry-run` | **`captured = 219   expected = 219`**, printed **above** the grep output; fixtures 2/2 fired, 8 exempt twin arms fired, everything else clean |
| 4. `oq277_build_prereg.py --write` then `--check` | **GREEN**, md5 `4118f64e…`, every verbatim appendix matches its canonical source |
| 5. stamp recorded here, above the sentinel | this entry |
| 6. spend-go | **NOT GIVEN — the operator's seat. No live call may be made.** |

### The total was RECOMPUTED, not carried — and it agrees

`198 + 21 = 219` was stated in this log and in three handoff messages. It was **not** used to satisfy
any step. Step 2's total comes from the builder counting the packets it just wrote; step 3's from the
driver's own `expected`, computed from those packets; the extractor independently recomputed 73 from
units globbed off disk. **Three independent derivations, all 219, none consulting the carried
figure** — and the (iii′) leg genuinely could have come back at 6 or 8 (one exemplar was written under
a declared collision risk that would have made it 72/216 had it been upheld; it was checked and
discharged, see the hand-back entry above). **The agreement is the finding; had any derivation
disagreed, that would have been a finding to report rather than a discrepancy to reconcile.**

### Changed between the DRAFT and this freeze — the audit trail for the md5 delta

1. **The 7 (iii′) units landed** (`5ce2c37e`) — the leg that made the DRAFT a draft. Packets went
   66 → 73 items, 198 → 219 calls.
2. **`verdict_grammar_amendment.md` gained §O.3, §L.8 and §L.9** (`51f74622`, `142a3c33`) — the P6
   concentration, the second unplanted fire, and the eighth vacuous check.
3. **`oq277_lexicon.py` was repaired** (`3e16a1d8`) — the leak gate's crash and its leak verdict were
   indistinguishable at the interface. The tool is incorporated verbatim into the prereg, so the
   repair is inside the frozen document. **This is the one change to a frozen instrument in this
   sequence, made on an explicit operator ruling after two receivers hit it**, and it is recorded
   here so the freeze is not read as covering an unmodified toolchain.

### What the stamp does and does not bind

It binds the design: what is coded, by whom, in what order, what each outcome means, and every
declared residue. **It does not license a call.** `oq277_crosscoding_driver.py --live` still refuses
until spend-go, and the first coding result may not be appended above the sentinel below.

Everything above this line is design. Everything below it is result.

<!--OQ277-FIRST-CODING-RESULT-->

## 2026-08-11 — LIVE RUN EXECUTED. 219 calls spent. **THE ANSWERS WERE NOT PERSISTED.**

Spend-go granted by the operator (scope: 219 calls, Sonnet 5, k=3, directions (i), (ii), (iii′);
no extension, no escape coding, no re-runs). `--live` ran once, after the pre-run control repair
at `bdd93fe2`. The gate order held and every gate passed.

**The calls were made. The results are gone.**

`oq277_crosscoding_driver.py` has **no code path that writes `responses/`** — verified by direct
grep over every write in the file: the only writes are the payload dump and the selftest's own
fixtures. `run()` builds `results`, computes `resolved`, prints aggregate counts, and returns. The
per-item answers are local variables and were discarded when the process exited.

**`--dry-run` asserts a distinction that does not exist.** Its help text reads *"do not write
responses/"* and its console line reads *"--dry-run: responses NOT written. responses/ left
empty."* Both imply the non-dry-run path writes them. Neither path does. The flag controls a print
statement.

### What the spend bought, in full

```
k=3 bookkeeping: 73 items resolved, 69 unanimous, 4 UNSTABLE
  direction_i    22 cells (matrix_unit & unanimous) ·  8 quarantined ·  0 UNSTABLE
  direction_ii   19 cells (matrix_unit & unanimous) · 14 quarantined ·  4 UNSTABLE
  iii_prime       0 cells (matrix_unit & unanimous) ·  7 quarantined ·  0 UNSTABLE
```

That is the entire retained product of 219 calls. **The console output was complete, not
truncated** — the run's first line was visible inside the captured tail, so nothing was lost to the
pipe. There is no per-item label anywhere: not on disk, not in the transcript.

### What is therefore NOT recoverable

- **Every matrix cell.** 41 unanimous matrix items resolved to *some* label; which label each got
  is gone. Neither matrix can be filled.
- **The H5 churn floor.** §E defines it on the 4 overlap **pairs** in direction (ii)
  (`2026-06-11_oq44_policy_close` = ii-01/ii-02; `2026-07-11_oq186_oq188_readsite` = ii-11/ii-20;
  `2026-06-27_oq124_oq149_committer_convention_control` = ii-19/ii-24; `2025-05-15_recon_2` =
  ii-21/ii-23), and it needs each member's resolved label. **The floor cannot be computed.** The
  global `4 UNSTABLE` is not the floor and must not be substituted for it — §E's floor is
  agree/FLIP/uninformative over pairs, and the four unstable items have not even been identified.
- **The (iii′) row**, the fork-residue row, and every downstream verdict.

### The gate architecture had a hole exactly where it was not looking

Gate 1 is *"COUNT FIRST — captured payloads vs expected calls."* It counts **payloads** — inputs.
Nothing anywhere counts **responses**. The driver verifies that every call has an input on disk and
never that any call left an output on disk. Every gate in the run is oriented toward not leaking
and not miscounting inputs; **there is no gate on retaining the product**, and the run reported
GREEN on all of them while producing nothing.

**This is the experiment's own subject matter, executed on the experiment.** A success-shaped
run: every gate green, the expected totals printed, the failure invisible at every check because
no check looked at the output side.

### The pre-flight asymmetry, which is mine

Before spending, this instance exhaustively controlled the **refusal** path — five constructed bad
states plus the converse, because a red selftest was not going to be spent through. It never
checked the **capture** path. The gate that stops a spend was verified to the strongest available
standard; the gate that keeps the spend's product was never asked to exist. *Both* were untested by
construction before today; only one of them got tested, and the choice of which was driven by which
one had a red light on it.

**Not retried.** A second run is a second spend and is the operator's ruling, not this instance's —
and the tool must be repaired and its capture path controlled *before* any such run is proposed.
Recorded here, below the sentinel, because this is a result: the run happened.

## 2026-08-11 — FOR §6.4: the recursion is not a worry, it is the observed behaviour of every repair in this arc

**Operator ruling, 2026-08-11.** Recorded here rather than in `verdict_grammar_amendment.md`
because that file is incorporated verbatim into the **frozen** `PREREGISTRATION.md`
(`4118f64e`), and a run has already been made under that stamp. Editing an incorporated source
now would invalidate the freeze retrospectively — post-hoc editing of a preregistration after
results exist, which is the precise thing the freeze ordering exists to make impossible.
**§6.4 must take this from the results section, not from the amendment.** Anyone assembling
§6.4 should read below the sentinel as well as above it.

**The claim §6.4 should make.** *Controls need controls* is currently argued as a structural
worry — that adding a gate to check a gate reproduces the problem one level up. This arc supplies
something stronger and less comfortable: **it is not a hypothetical. It is what every repair in
this arc actually did**, and the pattern is now dense enough to be a signature rather than a
coincidence.

| # | the repair | the same defect, committed inside it | how it was caught |
|---|---|---|---|
| 1 | measuring the lexicon's crash-vs-leak confusion | the baseline table logged four `KeyError` crashes as "leaks found" | a **number moved** — those rows changed exit 1 → exit 3 in the post-fix diff |
| 2 | the negative control on that repair | went red on an `IndentationError`, testing nothing | reading the output instead of the exit code |
| 3 | the same selftest | **aborted** partway instead of reporting FAIL — crash-vs-result confusion inside the fix for crash-vs-result confusion | running it against the reverted copy |
| 4 | the capture repair | printed `persisted 219` from `len(results)` while **zero** files existed | **comparing the printed number against disk** |
| 5 | the same repair | failure banner promised responses were "ON DISK and recoverable" when none were | the same comparison |

**The load-bearing observation: not one of these was caught by a gate.** Every one was caught by
someone comparing a claimed number against the artifact it claimed to describe — a diff, a
directory listing, a file count. The recursion does not terminate in a deeper instrument, and the
arc now has five instances showing what it terminates in instead.

**And the asymmetry that decides where to look.** In the same session the driver's *refusal* path
received the strongest control in the arc — five constructed bad states plus a converse — while
its *capture* path received nothing. Both were untested by construction. The refusal path had a
**red light**; the capture path had **no writer at all**, and a writer that does not exist emits no
error, no warning, and nothing to inspect. Attention followed the signal, and the signal was
uncorrelated with the cost. **A verification stack audited by following its red lights will
systematically miss every defect whose signature is silence** — which is the class that takes whole
runs.

### The most compact statement §6.4 has — the detector reported ITSELF

**Operator ruling, 2026-08-11.** `orphaned_controls()` was written to find controls that nothing
calls. **On its first run it named itself**, because nothing called it yet.

That is the whole recursion in one line, and it is better than the five-instance table above
because it needs no narrative: **the detector for a defect class is a member of that defect
class.** Not analogously, not as a cautionary framing — literally, by its own criterion, on its
first execution.

**And the handling is the transferable part, not the anecdote.** The exemption is real:
`orphaned_controls` genuinely is a selftest instrument, its wiring witness genuinely is that
`--selftest` fails without it, and guarding it against its own rule would be incoherent. So the
exemption is correct. **The only thing separating a legitimate exemption from an invisible one is
whether someone wrote it down** — and the arc has a worked example of the alternative: the
genre-based pin rule (`SPEC_next_preregistration.md` §1) was also a real, defensible selection
rule, applied by everyone including the operator, and it cost 219 calls **because nobody stated
it**. Same species. One was written into the source as an exemption with its reason; the other
was never articulated and therefore never examined.

> **§6.4 should end here rather than on the five-instance table.** The recursion is not resolved
> by a deeper instrument and not by a better rule. What terminates it is someone writing down the
> exemption they just took — which is an act of documentation, not of verification, and is why the
> apparatus's honest limit is a person and not a gate.

## 2026-08-11 — EVIDENCE for the stamp ruling (the ruling itself is the operator's, PENDING)

The operator asked whether a second run goes under the existing stamp or needs a new one, noting
the circularity: the answer determines what a path enumeration is *for*, but the enumeration is
what one would want in hand to rule. **Most of the circularity dissolves — the question turns on
three facts that are available now.**

### Fact 1 — the driver was NEVER pinned by the preregistration

`oq277_crosscoding_driver.py` appears **0 times** in the builder's `PINNED` manifest. The freeze
pins `CLAUDE.md`, `build_discipline.md`, Wu's two sources, **the lexicon**, both prompts, the
amendment, six control files and two rulings — and **not the instrument that executes the run**.

So "the prereg was frozen against a driver that could not retain its output" *understates* it. The
freeze made **no claim about the driver at all**. A green freeze check was therefore fully
compatible with an instrument that could not produce data — which is exactly what happened, and it
is not a coincidence that no check caught it.

### Fact 2 — the ANALYSIS half does not exist, in code or in specification

| artifact | status | witness |
|---|---|---|
| H5 scorer (agree / FLIP / uninformative over the 4 overlap pairs) | **does not exist** | no OQ-277 file contains `FLIP`/`INSIDE FLOOR`/`uninformative`; the 8 repo hits are unrelated (`g_channel_flip_audit`, the sweeps, `tensions_ledger`) |
| matrix construction | **does not exist** | only four `oq277_*` scripts exist: `build_prereg`, `crosscoding_driver`, `lexicon`, `make_coder_packets` |
| `matrices/` | **empty** since it was created 2026-08-10 | directory listing |
| a specified mechanism/phase for producing either | **not in the frozen design** | the document fixes the verdict *grammar* and §E's thresholds, and names no phase, script or procedure that computes them; "Phase 3" occurs once and refers to the calls |

*Controls: `FLIP` occurs 3× in the frozen document, so the term is real and the probe reads; the
same greps return the files they should for `matrix_unit`.*

**Consequence, which is larger than the stamp question:** even a *perfect* capture run would have
produced 219 scoreable answers that **nothing in the repository can score**. Pattern 1 — a producer
with no consumer — was sitting one stage downstream of the stage that just failed, and would have
fired next. The capture failure hid it.

**Stated precisely, because the design is silent rather than wrong:** scoring was either to be done
by hand at writeup, or it was an unnoticed gap. **The frozen text does not distinguish these**, and
this entry does not guess. What is certain is that no mechanism exists and none is named.

### Fact 3 — two pinned sources have already drifted, with material derived from the failed run

`CLAUDE.md` and `docs/technical/build_discipline.md` have moved since the freeze, both carrying the
*Gate the output* discipline the failure produced. Not fatal for a re-run — the coder prompts are
pinned separately and are unchanged — but the stamp already describes a repository state that no
longer exists.

### The recommendation, and the strongest case against it

**Recommendation: NEW STAMP** — agreeing with the operator's stated prior, on a stronger basis.
The prior was "frozen against a defective driver"; the evidence is that the freeze **did not reach
the driver**, and that the analysis half is absent from both the code and the design. A new
preregistration should pin the driver and specify the scorer, which is precisely what the existing
one cannot be made to do without being rewritten.

**The strongest case for keeping the stamp, stated fairly:** the *design* is intact and unchanged —
what is coded, by whom, in what order, every threshold, every declared residue. A preregistration's
job is to pin the design, not the plumbing; the plumbing failed, so repair it and re-run under the
same design. That is coherent, and it is the position to beat.

**Why it loses:** "the plumbing is not part of the preregistration" is the assumption that allowed a
defective instrument to run under a valid freeze. If the stamp does not cover the instrument, then a
GREEN freeze check carries no information about whether the run can produce data. Widening what the
stamp pins is the repair, and widening it *is* a new stamp.

**What this does to the enumeration.** Under a new stamp it stops being an audit of a frozen design
and becomes a **specification**: what the next preregistration must pin (driver and scorer included),
and what must be built and two-sidedly witnessed before it is frozen. That is the more useful
artifact, and it is the one that would have prevented this run.

**PENDING: the ruling is the operator's.** Recorded as evidence, not as a decision.

### CORRECTION, same day — commit `f0e91cc0`'s message is WRONG, and it is instance six

That commit's message states *"Freeze md5 4118f64e unaffected — audit_log.md is not an incorporated
source, verified GREEN."* **The check printed RED and the message was written anyway.** The
reasoning was sound and the check was run; the *result of the check* was not read before the claim
was committed. **A claim not backed by the check that was actually executed — committed in the very
commit that records five prior instances of that shape.** Sixth instance. Caught the same way as
the other five: by comparing a claimed state against the artifact.

**The corrected state, both halves, because the first sentence of the message is true and the last
is false:**

| question | answer | witness |
|---|---|---|
| Is the frozen document intact? | **YES** | `md5(PREREGISTRATION.md) = 4118f64ecaab06260c2b30841121e7b2`, identical to the stamp of record. What was preregistered has not changed. |
| Is `audit_log.md` an incorporated source? | **NO** — correct as stated | not in the builder's manifest |
| Is `--check` GREEN? | **NO — RED** | two incorporated sources moved *after* the freeze: `CLAUDE.md` (+20) and `docs/technical/build_discipline.md` (+58), both from `cb1b33e5`, the post-run discipline writeup |

**So the RED is real and it is not a tampering signal.** `--check` asks *"is the shipped document
byte-identical to a fresh assembly?"* and after a freeze that question has two very different
answers wearing one colour:

- **the shipped document was altered** — fatal, the freeze is broken;
- **the sources moved on** — expected, and in fact required, since a frozen document is a snapshot
  and the repository keeps working.

**The checker cannot distinguish them, which is the lexicon's exit-1 defect again**: one signal,
two incompatible meanings, and the quieter reading is the dangerous one. Here it is inverted —
the alarming colour is attached to the benign case, which trains a reader to discount it. **A
post-freeze mode is owed**: after a stamp exists, `--check` should verify the shipped document
against its *recorded md5* (fatal on mismatch) and report source drift separately as
informational-with-a-list. Not built; declared, and it is a prerequisite for the next freeze rather
than for the next spend.

**Standing consequence for this arc:** the frozen prereg can no longer be rebuilt from HEAD sources
and must not be. `PREREGISTRATION.md` at `4118f64e` is the record of what was preregistered; any
future `--write` produces a *different* document and would need its own stamp, its own freeze entry,
and its own spend-go.
