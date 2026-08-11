# DEFECT — control (c)'s direction-(ii) selection metric counts a different taxonomy

**Found:** 2026-08-11, by the twins instance, before writing any twin and before any spend.
**Status:** **OPERATOR RULING REQUIRED.** Nothing has been reselected. The pre-declared pairs
stand until ruled on.
**Reproduce:** `python3 controls/recheck_predeclared_counts.py` (exit 0; both tables).

## What the pre-declaration claims

`controls/redaction_pairs_predeclared.json` selects the three direction-(ii) redaction pairs by
"pattern-vocabulary hit count in their source prose," and states the value the pre-declaration
bought:

> "The three direction-(ii) directories were selected on vocabulary density (21, 5 and 4 hits), so
> the arm is non-empty **by construction rather than by luck** — that is what the pre-declaration
> bought."

## What is actually true

`\bP[1-6]\b` is in the direction-(ii) banned lexicon. Audit directories in this repository use
`P1`, `P2`, `P3` for **their own local numbering**, unrelated to the six patterns. Splitting each
directory's hits into that ambiguous token versus vocabulary that can only mean our taxonomy
(`Pattern N`, the pattern names, the nicknames, `success-shaped` / `measured-empty` / `didn't-look`):

| directory | declared | bare `P#` | taxonomy | selected |
|---|---|---|---|---|
| `2026-06-07_stakeholder_layer_migration` | 4 | 1 | **9** | ✔ |
| `2025-05-15_recon_2` | 7 | 7 | 5 | (overlap, excluded) |
| `2026-06-11_oq97_pattern6_census` | 4 | 0 | 4 | |
| `2026-06-21_oq138_fsm_route_conversion` | 5 | 3 | 2 | ✔ |
| `2026-06-10_oq93_grid_viability_probe` | 1 | 0 | 2 | |
| `2026-06-21_maxent_seat_aware` | 1 | 0 | 1 | |
| `2026-07-02_oq136_census_bucket_provenance` | 1 | 0 | 1 | |
| **`2026-07-20_five_leg_twin_comparison`** | **21** | **21** | **0** | ✔ |
| `2026-08-06_oq259_item3_genreflag` | 2 | 2 | 0 | |

**The top-ranked pick scores 21 out of 21 on false positives.** In that directory `P1`, `P2`, `P3`
are probe names — `P1 forced_certainty`, `P2 memetic_mirror`, `P3 perspective_diff`. It has **zero**
taxonomy vocabulary anywhere in it.

### Measured where it actually bites: the files each unit was extracted from

The twin restores vocabulary from the unit's *own* sources, so `files_read` is the right denominator:

| pair | unit | taxonomy vocabulary in `files_read` |
|---|---|---|
| 1 | `04_stakeholder_layer_migration` | **3** — `dangling wire`, `Pattern 2`, `One canonical thing` (AUDIT.md) |
| 2 | `10_oq138_fsm_route_conversion` | **0** (its 2 directory-level hits are in `CIROPE_RED_ADJUDICATION.md`, which the unit was not extracted from) |
| 3 | `20_five_leg_twin_comparison` | **0** |

**2 of 3 direction-(ii) pairs have nothing to un-redact.** Only pair 1 has a source that attributes
a pattern to *its own incident*, and it does so directly:

> "**Verdict: the existing mandatrophy authoring surface is a Build-Discipline Pattern-1 dangling
> wire.**" — `audits/2026-06-07_stakeholder_layer_migration/AUDIT.md:144`

## Why this is not the same catch as the step-1 false positive

Catch #3 of this arc ("permission *class b*y default" matching `Class B`) was a false positive in
the **leak-grep**, where a false positive is *conservative*: it fires H2, you investigate, you clear
it. Nothing is lost.

Here the same matcher was reused as a **density metric to select on**. In that role a false positive
is not conservative — it silently determines the choice, and it does so in the direction that
guts the control: it ranked first the one directory with nothing to restore. **One matcher, two
roles, opposite failure directions, and it was validated only in the first role.** That is
`build_discipline.md` → *An introduced instrument is itself a claim*, with the instrument being the
lexicon and the unvalidated second claim being "hit count measures taxonomy density."

**Direction (i) is NOT affected** — checked, not assumed. Its three selected units carry 6, 6 and 3
unambiguous hits in their catalog rows (`MR-4`, `MR-13`, `INV-W`, 稀释, 放大器); its generic-English
bans (`trigger`, `amplifier`, `dilution`) contributed 1 ambiguous hit across all three. Direction
(i)'s pairs have real vocabulary to restore and its floor is unaffected.

## The ruling, and why it is not mine

Two binding instructions now point opposite ways:

- `HANDOFF_TWINS_AND_DRIVER.md` §1: "**Do not choose them**, and do not re-derive them 'to check' —
  they were fixed by a mechanical rule before the units existed, which is the strongest available
  form of pre-registration for this control."
- The control must measure what it claims. As declared, pairs 2 and 3 measure Δ("other") between a
  redacted unit and an *identically pattern-free* unit that differs only in restored
  source-identifying vocabulary (OQ ids, predicate names). That is a real quantity — but it is not
  the redaction floor the both-residue row needs, and reporting it as one would put a **floor of
  near-zero** under the headline row. "Redaction costs nothing" by construction is precisely the
  failure `HANDOFF_TWINS_AND_DRIVER.md` §1 warns about two paragraphs later.

Reselecting now is selection *after* seeing the outcome-relevant property, which is what the
pre-declaration exists to prevent. Keeping the declared pairs preserves the pre-registration and
ships a floor that is near-zero for structural reasons, not measured ones. **Both options cost
something real, which is what makes it the operator's seat and not mine.**

### The options, stated so they can be ruled on

- **A — keep all three pairs as declared; report per-pair, never pooled.** Pair 1 measures the
  taxonomy-redaction floor (n=1). Pairs 2 and 3 are relabelled for what they are: a
  *source-identifying-redaction* floor. Pre-registration untouched. Cost: the taxonomy floor rests
  on one pair, and the both-residue row's floor is correspondingly weak. Pooling the three would
  measure the pooling convention rather than redaction — the same shape as the pooled-H¹ finding —
  so per-pair reporting is mandatory under this option, not stylistic.
- **B — correct the metric and reselect, declaring the correction and its date.** Taxonomy-only
  density gives `stakeholder_layer_migration` (9), `oq97_pattern6_census` (4),
  `oq93_grid_viability_probe` (2). Cost: a pair selected after the defect was visible, and
  `oq93_grid_viability_probe` is the directory whose incident already collides with the
  disqualified P6 anchor (`controls/anchors.json` `_disqualified`) — it would need that checked
  before use. Buys a floor that measures the thing.
- **C — keep the declared three AND add the corrected two as a second, separately reported set.**
  Both floors reported, each with its selection provenance and date. Cost: two extra coded pairs
  (~4 more coding units at k=3) and a slightly more complex accounting. Nothing is retracted and
  the difference between the two selections becomes readable rather than argued.

**Recommendation: C**, then A's per-pair reporting rule applied to both sets. It is the only option
where the pre-declaration is preserved *and* a real taxonomy floor exists, and the marginal spend is
small relative to the run. But this is a ruling, not a fix.

---

# RULED — option C, with the row-assignment fixed now (operator, 2026-08-11)

**Ruled: keep the declared three AND add the corrected two, reported as two separate sets.**
Option A ships the both-residue row's floor resting on a single pair, "which is not a floor";
option B buys a real measurement by selecting after the defect was visible, "precisely the move
the pre-declaration existed to prevent." C pays ~2 pairs at k=3 to keep both, and the difference
between the two selections becomes readable data rather than a discarded branch.

**The addition, fixed before either number exists:** the **corrected set carries the both-residue
row**; the declared set is reported alongside as the pre-declared comparison. Reason, stated so it
is not re-opened at writeup: the declared set measures something real, but it is not the taxonomy
floor the row needs, and **a row must be fed by the instrument that measures its own quantity.**
Without this, the row acquires a choice at writeup time with both numbers already in hand.

| set | pairs | feeds |
|---|---|---|
| **corrected** | `stakeholder_layer_migration` (9), `oq97_pattern6_census` (4), `oq93_grid_viability_probe` (2) | **the both-residue row's floor** |
| **declared** | `stakeholder_layer_migration` (9), `oq138_fsm_route_conversion` (2), `five_leg_twin_comparison` (0) | reported alongside, as the pre-declared comparison |

`stakeholder_layer_migration` is in both sets and is coded once, serving both.
**Net new direction-(ii) pairs: 2** (`oq97_pattern6_census` → unit `07`, `oq93_grid_viability_probe`
→ unit `05`). Direction (i)'s three pairs are unchanged and unaffected.

## The oq93 collision check, run before the pair was written

**It collides, and the collision is role-appropriate — the pair is safe to write.**

`05_oq93_grid_viability` is the `system_gradient` `[] → 0.0` incident, i.e. the *same incident* as
the published P6 exemplar that `controls/anchors.json` disqualified from the direction-(i) anchor
set. Checked directly rather than inferred: the unit's symptom is "a system-level gradient metric
read exactly 0.0 on every input it had ever been given … 0.0 is what a genuinely flat input should
produce," and the `_disqualified` record names the collision with this unit explicitly.

**Why it disqualifies an anchor but not a twin — the requirement is opposite in the two roles.**
An anchor must be an incident the coder has *not* otherwise seen as a unit; collision destroys it.
A twin's redacted arm **is** the unit — collision with the unit is the definition of the control,
not a contamination of it. The dropped candidate is not in the run at all, so no live instrument
compares unit `05` against itself across two roles.

**One residue, declared:** the P6 exemplar is also entry #9 of the (iii′) population and is marked
DISQUALIFIED there on the anchors.json precedent (`RULING_2026-08-11_freeze_scope.md` §2.1). With
unit `05` now serving as a twin, that (iii′) disqualification is *conservative rather than
required* — different label spaces, different runs. It stands as declared and n=10 is unchanged;
noted so a reader does not later read it as a second, independent collision.

**Third instance of the same shape (2026-08-11, found at packet assembly).** §H.1 — the *authoritative
statement of the boundary rule* — contains a worked adjudication of a directory that is in the sample,
and that directory came up in the threshold probe's primary draw, so the clause could not ship
verbatim to a blind judge without supplying one item's verdict. Same collision as the disqualified P6
anchor and the oq93 twin: **the richest exemplar is the one that cannot be used, because it is already
in the run.** The recurring cause is that a specification and its own test case keep sharing an
artifact — the exemplar that makes a rule legible is drawn from the same population the rule is being
measured against. Resolved there by redaction-with-declared-omissions rather than by re-drawing
(`packets/escape_units/PREREGISTRATION_threshold_calibration.md` → *Assembly record*).

`oq97_pattern6_census` (unit `07`) was checked the same way and is **clean**: its incident is the
census finding that a repair applied at one site was never swept to the others, which is none of
the three published P6 exemplars.

## Cell accounting under 5 pairs — derived from the existing §I ruling, not a new one

A twin's **redacted arm doubles as its matrix cell**; only the **unredacted arm** is extra and
quarantined. This is not an inference from prose — it is entailed by the operator's standing
ruling of **22 matrix units** (§I, mechanism corrected in §I.2), and it is machine-checked today:
`verify_controls.py` [8] passes with `matrix units == 22` and "every sampled directory contributes
exactly ONE cell" while units `04`, `10`, `20` all carry `matrix_unit: true`. Were twins quarantined
from the matrices, cells would read 19 and the §I ruling would be contradicted.

Consequence: adding units `05` and `07` as pairs **leaves cells at 22**. The driver's expected-call
count rises by the new unredacted arms only (2 arms × k=3 = **6 calls**), and quarantined calls still
count toward the payload-capture assertion (§E *Accounting*).

## Regardless of the ruling

`recheck_predeclared_counts.py` is a **standing check** — re-run it after any change to the lexicon,
the units, or the sampled directories. It refuses (exit 1) if the pre-declared selection ever stops
being what the declared rule produces, which is then itself an operator ruling and not a repair.
