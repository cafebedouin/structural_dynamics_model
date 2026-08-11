# OBSERVATION — the source's own secondary-class annotations predict where its two records disagree

**Status: OBSERVATION, not a finding.** Recorded during the direction-(i) unit rebuild, 2026-08-10.
Operator instruction that produced it: check whether Wu's `真根因` (true root cause) column pulls
against the assigned class on the disagreeing cases, since that would be a *mechanism* for R2
rather than another measurement of it — **but record it as an observation, because it is read
with R2 already in hand.**

**Why this is not a finding, stated before the numbers.** Three separate defeaters, all live:

1. **Non-blind.** I computed the 10/12 disagreement partition before looking for a predictor. The
   partition was known to me at the moment I chose what to test.
2. **Post-hoc predictor selection.** I tested two predictors and report both. Neither was
   pre-registered. The p-values below are nominal, uncorrected, and chosen after seeing the
   outcome variable — the garden-of-forking-paths discount applies at full strength.
3. **n=22, and the predictor sets are 6-8 rows.** One row moving changes the picture.

The blind version that *would* be a finding is specified at the end. Until that runs, this is a
hypothesis with an arithmetic attached, and it must not be cited as evidence for anything.

---

## What was actually tested

The operator's question was about the `真根因` prose. Reading root-cause prose and judging which
class it "pulls toward" is exactly the biased read the ruling warned about — my judgement, applied
to prose, with the answer already known. So I looked for a version of the same question that the
frozen file answers **mechanically**, with no reading on my part:

> Does the source *itself* mark an incident as belonging to more than one class — and are those
> the incidents its two records disagree about?

Wu's labeled dataset carries two fields that do this marking without any interpretation from me:

- **`notes`** sometimes declares a secondary class explicitly (`次类 C`, `次类 E`, `次类 A`...).
- **`paper_class_ref`** sometimes cites a section belonging to a class *other than* the one the
  row assigns (`§4.1 A / §4.2 B` on a row labeled A).

Both are extracted by parser, not by reading. Base rate: 10 of 22 rows (45.5%) are rows where the
catalog and the dataset disagree.

## Result

| predictor (mechanically extracted) | rows | of which disagree | vs base 45.5% | exact one-sided p |
|---|---|---|---|---|
| `notes` declares a secondary class (`次类`) | 8 | 6 (75.0%) | +29.5pp | 0.0480 |
| `paper_class_ref` mentions another class letter — **strict textual** | 6 | 5 (83.3%) | +37.8pp | 0.0433 |
| ...excluding one adjacency remark — **requires my reading, see below** | 5 | 5 (100%) | +54.5pp | 0.0096 |

Hypergeometric, K=10 disagreeing of N=22.

**The judgment call, declared rather than absorbed.** The strict-textual predictor counts
`kb_evening_fallback_quota_chain`, whose `paper_class_ref` reads `§4.3 C (dilution, 'one step from
D')`. The row assigns C and cites C; the "D" is a *prose remark* that dilution sits one step from
fabrication, not a secondary assignment. Excluding it takes the predictor to 5/5 and p to 0.0096.
**I report the strict version as the headline because it needs no judgement from me**, and the
excluded version only as what it is: the same data with one row removed on my reading of what a
parenthetical means. A reader who disagrees with my reading keeps p = 0.043 and loses nothing.

### The sharpest single row, and it needs no statistics

`movespeed_tcc_sandbox` — the 60-day incident, the longest silent interval in the corpus and the
paper's own §5.1 headline:

- the dataset assigns it **A**
- the dataset's own `paper_class_ref` on that same row cites **§4.5 E**, and *only* E
- the catalog assigns it **E**

**The row disagrees with itself**, and the catalog agrees with the row's own citation rather than
with the row's own label. This is one row of one file; it is not evidence of a pattern. But it is
the cleanest available illustration of what the aggregate is pointing at, and it does not depend
on any p-value or on any reading of mine.

## What it would mean if it survives a blind test

R2a established that Wu's disagreements are *systematic* (only 5 of 10 class pairs occupied, E a
bidirectional hub) and offered a mechanism: E is defined on a different axis from its siblings, so
it absorbs and sheds members depending on which question the labeller was answering.

This observation, if it held up, would say something narrower and more useful: **the source
already knew.** The incidents the two records disagree about are disproportionately the ones Wu
himself annotated as multi-class at authoring time. The disagreement would then not be drift
between two vintages of a label set, nor labeller inconsistency, but the **predictable trace of
genuine multi-class membership** — R3's non-exclusivity finding showing up as R2's disagreement,
one phenomenon rather than two.

That reading is *attractive*, which is the reason to be careful with it, and it is exactly the
shape of story that survives review on its plausibility rather than its evidence.

## The blind version that would make this a finding

Pre-register, then run:

1. Strip class labels, `notes`, and `paper_class_ref` from all 22 rows, leaving the `真根因` text
   and the symptom.
2. Have a coder that has never seen the disagreement partition assign each unit a class from Wu's
   five definitions, k=3 for churn, exactly as the main experiment does.
3. **Pre-registered prediction:** units where the blind coder splits (no 3/3 unanimity) are the
   units the two records disagree on, at a rate exceeding the 45.5% base.
4. Kill condition fixed in advance: if the blind-split set matches the disagreement set at or
   below base rate, the observation is disconfirmed and is reported as disconfirmed.

**Cost note, and why this is not being proposed for this run.** That is a third coding direction.
It changes n and adds a direction, which the pinned extension rule forbids without a new
pre-registration — *"the extension changes n and NOTHING else. Any other change is a new
experiment."* Filing it as a candidate successor experiment, not smuggling it into this one.

## Reproduction

```
cd audits/2026-08-10_oq277_rq2_crosscoding/packets/wu_source
python3 -c "import yaml,json,re; ..."   # see audit_log.md for the exact command
```

Inputs, both md5-verified unchanged at the time of this observation:
`llm_observer_ground_truth.yaml` `f26359b2d9f98b6b310aed9b473a1395`,
`failure_modes_catalog.md` `f854454ed2be5bf489f2c5ee133ce013`.

**A defect found in my own first pass, recorded because it is the same shape this audit studies.**
My first extraction of the `次类` predictor used a regex over the raw YAML text and returned **6**
rows. A hand read of the same file said **8**. The regex was under-reading the multi-line `notes`
field — and 6 rows still produced a plausible, publishable-looking correlation in the same
direction as the correct 8. Had I not hand-checked, the undercount would have been invisible: a
wrong number that agrees with the right number's conclusion. The fix was to parse with a real YAML
parser instead of a regex over a structured file, and the corrected count is what appears above.
