# Specification for OQ-277's NEXT preregistration

**Written:** 2026-08-11 · **Status:** specification, nothing frozen, no spend requested
**Operator ruling this implements:** NEW STAMP (2026-08-11)
**Supersedes nothing.** The existing stamp is retained — see §4.

This is the artifact whose absence cost 219 calls. It is not an audit of the frozen design; it
is the list of what the next preregistration must pin, what must exist and be witnessed
**before** it is frozen, and what is currently untested — with the untested items **named**,
not left to be inferred from which checks happen to be red.

---

## 1. The pin criterion, stated causally

**Pin everything the result causally depends on. Not everything that reads like a
specification.**

The existing freeze pins sixteen artifacts: two spec documents, two source artifacts, the
lexicon, both prompts, the amendment, six controls, two rulings. **Every one of them is a
text.** Nothing executable is in the manifest — not the driver, not the packet builder, not
the scorer that does not yet exist.

The pinning instinct followed **genre**: things that read like specifications got pinned,
things that run did not. Nobody wrote that rule down and everybody applied it, the operator
included when specifying the freeze contents. It is invisible precisely because it produces a
manifest that looks complete — every item in it genuinely belongs, and the omission has no
shape.

**Consequence, demonstrated rather than argued:** the freeze's GREEN carried no information
about whether the run could produce data. A check that does not reach what it is checking is
not a weak check; it is a different check wearing the name.

> **The criterion for the next manifest:** an artifact is pinned if changing it could change
> the result. That includes every executable in the path — driver, packet builder, lexicon,
> scorer, matrix builder — and the fixtures their controls run against. Genre is not a
> criterion. If an artifact's substitution would alter a number in the writeup, it is pinned.

**This section exists because the next preregistration will reproduce the genre rule by
default.** It is the kind of omission that is only visible once, from the far side of a
failure.

### 1.1 The genre error has a SECOND face, and §2.3 is it

**Operator observation, 2026-08-11.** The five artifacts that do not exist (§2.3) are **all
analysis stages** — scorer, pair identification, matrix construction, redaction-floor scoring,
the (iii′) row. That is not five independent omissions. It is **the same selection rule as the
pin manifest, on a different axis**:

| axis | what got specified | what did not |
|---|---|---|
| pin manifest | artifacts that read like **specifications** (texts) | artifacts that **run** (executables) |
| experiment design | everything **up to the point where data lands** | everything **after** it |

**The design specified the production of data and not its consumption.** Extraction, redaction,
blinding, packet assembly, ordering, capture, unanimity, leak sweeps — all pinned, all
controlled. Scoring, the stage the findings are actually made at — absent, in code and in text.

**Consequence for this criterion:** stating "pin every executable" is not sufficient, because
when the scorer is built it will *become* an executable and the same instinct will treat it as
downstream plumbing rather than as part of the design. **The pin criterion must reach the
analysis stages explicitly, or the next freeze pins a scorer that a later instance can silently
replace** — which would put the numbers in the writeup one substitution away from unpinned,
with a GREEN freeze check throughout. That is this arc's failure with a different noun.

---

## 2. The path enumeration

Every path the spend depends on, with its status **measured this session**, not recalled. The
witness column names what was actually run; "none" means no control exists, and is the point of
the table.

### 2.1 Built and two-sidedly witnessed

| # | path | witness |
|---|---|---|
| 1 | leak lexicon: three input shapes, fail-closed on unrecognisable | 7 `input-shape` controls, real file path; negative control reverting only the normaliser fails 5 of 7 and leaves list/wrapper passing |
| 2 | lexicon: crash vs leak distinguishable at the interface | exit 3 = did not sweep, distinct from 1 = hits; `SWEEP-ABORTED` on **stdout**; two-sided |
| 3 | driver gate 0 — k=3 redraws are same-input | fires on a varying payload; **converse** — identical payloads do not fire |
| 4 | driver gate 1 — captured payloads vs expected | fires on a short capture; fires on missing fixtures |
| 5 | driver gate 3 — three-way leak sweep | fixtures 2/2 fire, 8 exempt arms fire, rest clean, on real packets |
| 6 | driver gate 4 — **output side** | 5 refusals (missing, zero-byte, out-of-vocabulary, unparseable, count mismatch) + converse; end-to-end broken variants: writer neutered → gates 1–3 green, gate 4 red; all-blank → count passes, content catches |
| 7 | capture: raw text persisted per call, verified on landing | `write_response` control; resolution reads back from disk |
| 8 | capture-dir provenance (run-id keyed) | 6 controls: refuses this-run contamination, proceeds on another run's data, proceeds on empty; foreign-run refusal + 2 converses; post-write re-check added |
| 9 | `--live` spend-go refusal | 5 constructed bad states + converse; the refusal branch was **first exercised 2026-08-11**, having existed unexercised for the whole arc |
| 10 | k=3 resolution: unanimity and UNSTABLE routing | unanimous resolves to the label; split resolves to `UNSTABLE` |
| 11 | prereg freeze integrity | frozen+matching → GREEN, frozen+**altered** → RED, draft-only log not read as a stamp, drift list names only what moved; gated, and the gate row itself witnessed red |

*Driver selftest total: **27 controls, 0 failures**.*

### 2.2 Exists, NOT two-sidedly witnessed

| # | path | what is missing |
|---|---|---|
| 12 | packet assembly (`--build-run`) | its build gates ran and passed on real data; **no negative control** — nothing demonstrates the frozen-order / content-md5 / coder-surface gates can fail |
| 14 | anchor recovery scoring (H3, ≥2/3 over {P1,P2,P5}) | thresholds are pre-registered; **no code, and no control** |

**Row 13 — live transport — was moved OUT of this table** to §3.2, on the operator's weighting
(2026-08-11): it is the heaviest row here and does not ship as a declared residue.

### 2.3 DOES NOT EXIST — neither code nor specified mechanism

| # | path | status |
|---|---|---|
| 15 | **H5 scorer** — agree / FLIP / uninformative over the 4 direction-(ii) overlap pairs, then INSIDE FLOOR / LIVE / INDETERMINATE | no code anywhere; the frozen design fixes the verdict grammar and thresholds and names **no** phase, script or procedure that computes them |
| 16 | **overlap-pair identification** | done by hand this session from `coder_direction_ii_map.json`; no code |
| 17 | **matrix construction** (both directions) | no code; `matrices/` empty since it was created 2026-08-10 |
| 18 | **redaction-floor scoring** (twin arms, corrected vs declared sets) | no code |
| 19 | **(iii′) row construction** | no code |

**These five are the finding, not a gap in this document.** Even a perfect capture run would
have produced 219 scoreable answers that nothing in the repository can score.

---

## 3. What must be true before the next freeze

1. **§2.3 exists as named, built, witnessed artifacts — before the stamp, not after.** The
   scorer and matrix builder are **instruments**. They get pinned and they get two-sided
   controls like everything else. Minimum, pre-registered here:
   - a fixture where all four overlap pairs agree **must** score `INSIDE FLOOR`;
   - a fixture where one pair flips **must** score `INDETERMINATE` (the middle band, which
     fails closed);
   - a fixture where two pairs flip **must** score `LIVE`;
   - a fixture where a pair is UNSTABLE **must** score `uninformative`, and must not be
     silently counted as agreement.
2. **§2.2 gets its negative controls**, or ships with its status declared in the prereg text.
2b. **Every detector built for §2.3 is graded against its historical commit pair where one
   exists**, not only against its fixture. Available pairs, verified 2026-08-11: control
   orphaning (`cb1b33e5` / `4e0d8725`), capture-path absence (`cb1b33e5~1`), lexicon
   single-object `KeyError` (`3e16a1d8~1`). Unavailable: drift-list false positives and the
   crashes-logged-as-leaks baseline, both caught before commit. **Where no pair exists, the
   detector ships at fixture grade and says so.** Rule:
   `build_discipline.md` → *When a defect is found, its before-commit is a free negative control*.
3. **The manifest is rebuilt under §1's criterion** — every executable in the path, plus its
   fixtures.
4. **The freeze-integrity check covers the new stamp** (the gate entry generalises or is
   re-pointed; it carries a retirement note tied to OQ-277's close).
5. **The scorer's control density is stated, not assumed.** See §3.1.
6. **The scorer refuses stub data, fail-closed (added 2026-08-12).** Before scoring any response
   set it **reads the sibling `_run.json` and asserts `mode == "live"`** — refusing on
   `mode == "stub"`, and refusing on a missing or unparseable `_run.json` rather than proceeding.
   Rationale: the repository now holds a full 219-unit **stub** response set
   (`responses_stub/`, `mode: stub`, run-id `stub-4118f64e-…`; see `STUB_RUN_README.md`), retained
   because it is the evidence for §2.1 row 6. A scorer pointed at it would emit a clean 219-unit
   result computed entirely from fabricated tokens — success-shaped output from a run that
   measured nothing, i.e. Pattern 6 carrying this audit's own name. The guard is one line because
   `mode` was made machine-readable for exactly this; **it is a requirement rather than a warning
   label because a label is not a check.** Its two-sided control: point the scorer at
   `responses_stub/` (must refuse) and at a fixture with `mode: live` (must proceed).

### 3.2 Live transport — negative controls BEFORE the spend request, not as a residue

**Operator weighting, 2026-08-11: the heaviest row in the enumeration.**

Live transport has been exercised **exactly once**, by the run that failed. It returned 219
answers, so it works in the nominal case — and that is an **existence proof at n=1**, not a
control. Nothing witnesses what happens when it does not go nominally, and **the next run will hit
at least one non-nominal case across 219 calls.**

Required before the next spend request, each two-sided:

| condition | required behaviour | why it is not hypothetical |
|---|---|---|
| **retry** | a transient failure retries and the eventual answer is captured **once**, not twice | `call_with_retry` is the wrapper; nothing has observed a retry, and a retry that double-writes corrupts the k=3 bookkeeping |
| **truncation** | a reply cut short by `max_tokens` is detected, not silently coerced | `max_tokens=16`; a truncated `"P"` or `""` must fail gate 4's vocabulary assertion rather than resolve |
| **refusal / non-answer** | a refusal is captured raw and reported out-of-vocabulary | already covered by gate 4 *if it reaches capture*; unwitnessed on the live path |
| **malformed reply** | whitespace, casing, punctuation, extra prose | `extract_text(...).strip()` is the only normalisation; nothing has tested what a chatty answer does |
| **hard failure mid-run** | the run halts with the answers so far **on disk** | the per-call write-then-verify is built and stub-witnessed; **not witnessed against a real mid-run transport failure** |

**These are testable without spending**, by substituting a transport that raises, truncates,
refuses and rambles on schedule — the driver already parameterises `transport`, so the fixture
costs nothing. **A transport-fault fixture is the specific artifact owed.**

### 3.1 The silence at the stage where the findings get made

If scoring was intended as a manual writeup step, then the design controls **every** upstream
stage — extraction, redaction, blinding, packet assembly, capture, unanimity, leak sweeps —
and then hands the actual numbers to an unwitnessed instance reading response files with no
procedure, no controls and no pre-registered method.

**Control density goes to zero exactly at the stage where the findings are made.**

The frozen text does not say which of *manual step* or *unnoticed gap* was intended, and this
document does not guess. Either way the next preregistration must state it as a **requirement**
rather than an observation: the scorer is an instrument, it is pinned, and it is controlled.

---

## 4. What the old stamp IS

`4118f64ecaab06260c2b30841121e7b2` is **not invalidated and not superseded.** It is **retained,
with the run that proved it**, as:

> **the record of a design that could not produce its own result.**

It pinned sixteen texts, made no claim about any executable, specified an analysis half that
did not exist, and returned GREEN throughout a run that persisted nothing. That is a more
valuable artifact than a clean freeze would have been: a clean freeze demonstrates that a
process was followed, while this one demonstrates what following the process failed to catch.

**The next stamp must cite it** — by md5, with this document, in the new preregistration's own
provenance section. A preregistration that cites the one before it as a known failure mode is
carrying its own falsification history, which is the thing the arc keeps discovering it needs.

**Operational consequences, already enforced:**
- `oq277_build_prereg.py --write` **refuses** while the shipped document matches a recorded
  freeze stamp. Producing the next preregistration requires moving the frozen one aside
  deliberately, under its own commit.
- The frozen document must never be rebuilt from current sources; two pinned sources have
  already drifted (`CLAUDE.md`, `build_discipline.md`), both carrying discipline the failure
  itself produced.

---

## 5. Not requested, not authorized

No spend is requested by this document. No run is proposed. The next request comes when §3 is
discharged, and it will carry this table with every row's status re-measured at that time —
because a status measured today is a claim with a shelf life, and this whole arc is about
premises that were true when written.
