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

## Regardless of the ruling

`recheck_predeclared_counts.py` is a **standing check** — re-run it after any change to the lexicon,
the units, or the sampled directories. It refuses (exit 1) if the pre-declared selection ever stops
being what the declared rule produces, which is then itself an operator ruling and not a repair.
