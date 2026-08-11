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
| 13 | live transport | exercised exactly once, by the run that produced 219 answers. That is an existence proof, not a control: no witness for retry behaviour, truncation, refusal, or a malformed reply |
| 14 | anchor recovery scoring (H3, ≥2/3 over {P1,P2,P5}) | thresholds are pre-registered; **no code, and no control** |

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
3. **The manifest is rebuilt under §1's criterion** — every executable in the path, plus its
   fixtures.
4. **The freeze-integrity check covers the new stamp** (the gate entry generalises or is
   re-pointed; it carries a retirement note tied to OQ-277's close).
5. **The scorer's control density is stated, not assumed.** See §3.1.

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
