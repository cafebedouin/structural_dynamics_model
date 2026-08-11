# OQ-277 RECON — what the Wu-side source material actually supports

**Executed:** 2026-08-10 (read-only pass; no coding, no model calls)
**Scope:** direction (i) source material only. Our-side (direction (ii)) extraction is separate.

## What was fetched, and why

The plan asserted "all literature is now LOCAL — no network dependency remains." That holds
for the **paper** and is false for its **incident catalog**. §4 of the paper says "Full
per-incident detail is in the public catalog; here we ... narrate one or two representative
incidents," and Artifact Availability confirms all 22 postmortems live in the public system
repository. Reading the PDF yields only **17** identifiable incidents (A 1/1, B 3/4, C 4/5,
D 4/4, E 5/8).

Operator ruled: fetch the catalog. Fetched read-only from
`https://github.com/bisdom-cell/openclaw-model-bridge` (public, accompanies arXiv:2606.14589):

| Artifact | Path in repo | md5 |
|---|---|---|
| Failure modes catalog | `ontology/docs/failure_modes_catalog.md` | `f854454ed2be5bf489f2c5ee133ce013` |
| Labeled incident dataset | `docs/llm_observer_ground_truth.yaml` | `f26359b2d9f98b6b310aed9b473a1395` |

Both are frozen under `packets/wu_source/` with `FETCH_MANIFEST.txt`. The PyPI package the
operator installed (`openclaw-ontology-engine` 0.1.0, module `ontology_engine`) does **not**
carry the catalog — it is the governance engine only (5 YAML ontologies + `CONSTITUTION.md`).
It did supply the repository URL, which is how the catalog was located.

**H1 status: PASSES.** PDF text extraction was clean via the Read tool; the pandoc fallback
was never needed.

## Finding R1 — n=22 is now reachable, with Wu's own labels

The catalog names all 22 cases, one table row per case, each with a one-line symptom and a
"true root cause" column. Per-class counts are **A 1 (+6 quirk sub-events), B 4, C 5, D 4,
E 8 = 22**, matching the paper's Table 1 exactly. The `llm_observer_ground_truth.yaml` marks
exactly 22 cases `paper_canonical: true`, with the same 22 identifiers.

Consequence: the extractor no longer has to infer any class label. Both a class assignment
and a mechanism description come from Wu. Three-party independence is preserved — the
extractor never classifies.

## Finding R2 — Wu's two records disagree on 45% of assignments

The catalog and the labeled dataset carry the **same 22 incident identifiers** and assign
them **different classes on 10 of 22 (45%)**. They agree on 12 (55%).

| id | catalog (= paper Table 1) | dataset |
|---|---|---|
| `preflight_cascading_fix` | B | E |
| `v37_9_92_observer_path_blood` | B | E |
| `dream_map_budget_overflow` | B | A |
| `rsync_helper_set_e_regression` | C | E |
| `dream_quota_blast_radius` | C | D |
| `finance_news_syndication_zombie` | E | B |
| `finance_zombie_closure` | E | B |
| `movespeed_exfat_silent_backup_failure` | E | C |
| `movespeed_noowners_uid_mismatch` | E | A |
| `movespeed_tcc_sandbox` | E | A |

Per-class totals diverge on every class: catalog A1/B4/C5/D4/E8 vs dataset A4/B3/C4/D5/E6.
Both sum to 22.

**The honest reading is contested, and the contest is Ω_C.** The charitable reading: the two
artifacts serve different purposes — the catalog sorts by *defense mechanism* (which scanner
immunizes the class), while the dataset labels for an Observer detector's *scope* (can a
content-reading observer see it at all), and its header states its labels are "load-bearing"
for sabotage tests. Purpose-relative labeling is not inconsistency. The uncharitable reading:
this is one-canonical-thing-became-two on the taxonomy itself. **We do not need to settle it**
— either way the operational consequence is identical and hard.

**Consequence for the experiment (this is the part that binds):** direction (i)'s confusion
matrix indexes rows by "Wu's class." For 10 of 22 units that index is **not well-defined**.
Pre-registration must therefore fix:

1. **Primary row index = the catalog.** Paper §3.2 names the consolidated catalog as "the
   canonical index from which this paper's taxonomy is built," and it reproduces Table 1
   exactly. This is Wu's own stated authority, not our preference.
2. **Robustness row index = the dataset**, reported as a second matrix. Any expressibility
   verdict that flips between the two indexes is reported as index-dependent, never as a
   result.
3. **High-confidence stratum = the 12 agreeing units**, reported separately. A verdict that
   holds only on the full 22 but not on the agreeing 12 is inherited ambiguity, not signal.

   **Selection rule FROZEN, with its cost declared (operator amendment 3).** The stratum is
   defined as: *the units whose catalog class equals their dataset class* — computed
   mechanically from the two frozen files by `packets/wu_source/` md5, before any coding, and
   listed explicitly in PREREGISTRATION.md so it cannot be redrawn after seeing results.

   **Declared cost, pre-registered so the stratum can never be read as the better number:**
   selecting on inter-source agreement **selects for incidents both sources found easy to
   classify**. That is a codeability filter — the *same* bias control (c) measures for
   redaction, arriving through a different door. Direction (i) run on the 12 will therefore
   show **higher expressibility than the full 22 partly for that reason alone**, and the gap
   between the two is NOT evidence that the stratum is a cleaner measurement. It is the
   easier measurement. Any writeup sentence preferring the stratum's numbers on cleanliness
   grounds is a pre-registered error. The stratum's legitimate use is narrow: as a check that
   a full-22 verdict is not being *driven* by the ambiguous 10 — direction of robustness,
   never a headline.

**And it reframes the headline.** The experiment asks whether Wu's classes are expressible in
our patterns. Wu's own two records reach 55% agreement with each other on the same incidents.
Judging our cross-coding agreement against an implicit 100% ceiling would be measuring against
a standard the source taxonomy does not meet internally. The 55% figure is the natural
reference line and is pre-registered as such.

### R2b — R2 is a FINDING about the comparison set, independent of the cross-coding (operator amendment 1)

**This must not be filed only as a row-index nuisance.** Wu is a single author, coding his own
incidents, from his own system, against his own five classes, with complete postmortems in
hand — the most favourable conditions a taxonomy will ever be scored under. **55% self-agreement
under those conditions is evidence that incident taxonomies in this domain are constitutively
unstable**, and it is the strongest available support for the §5.3 convergence framing. It
belongs in the writeup **whatever the cross-coding produces**, including if the experiment
HALTs — it is already measured, from two frozen files, and does not depend on a single model
call.

It also does something specific to the paper's §12 (*What Appears to Be New*). §12 currently
rests on institutional novelty, because Wu was the near-twin threatening the taxonomy claim.
**If Wu's own taxonomy does not reproduce against itself, the twin is far less threatening —
not because our six are better, but because neither set is stable enough to support a priority
contest at all.** That is an argument *for* the §5.3 convergence framing, reached from an
unexpected direction: the threat to novelty dissolves into evidence for the convergence thesis.
Queue for v0.4 alongside the OQ-280 §2.3 correction.

Honest limit on this finding: 55% is agreement between two artifacts of *unstated* relative
authority. If Wu intended the dataset purely as an Observer-scope annotation and never as a
taxonomy assignment, the figure measures cross-purpose labelling rather than instability. We
cannot settle intent from the artifacts, so the claim ships scoped: *two records that both
assign one class per incident, over an identical incident set, agree on 55%.* That is
defensible without any claim about what Wu meant.

### R2a — the disagreement is SYSTEMATIC, and E is the hub (operator amendment 2)

Pre-registered guess before the check: the disagreements would concentrate on C/D, since
error-swallowing and chained-hallucination overlap where a swallowed error gets narrated.
**That guess was wrong, and is recorded as wrong.** C/D is the *rarest* tie.

| class pair | n | | class | involved in |
|---|---|---|---|---|
| **B/E** | **4** | | **E** | **8 / 10** |
| C/E | 2 | | B | 5 / 10 |
| A/E | 2 | | A | 3 / 10 |
| A/B | 1 | | C | 3 / 10 |
| C/D | 1 | | D | **1 / 10** |

Only **5 of the 10 possible class pairs** are occupied, so this is concentration, not scatter
— the disagreement is structural, not coding noise. The structure is that **E is a hub**: it
appears in 8 of 10 disagreements, and **bidirectionally** — the catalog moves 3 incidents into
E that the dataset places elsewhere, and moves 5 out of E that the dataset places in E's
neighbours. This is not one-way drift between two vintages of a label set; it is a genuinely
unstable boundary.

**Why E, mechanistically.** Wu defines E by *declared state ≠ runtime state* (plus the
forensic-blind-spot sub-mechanism). That is a claim about the relation between what a system
asserts and what it does — which is **orthogonal to the failure-mechanism axis the other four
classes sort on**, not a sibling of it. Almost any A/B/C incident is *also* describable as a
divergence between declared and actual. A category that cross-cuts its siblings will absorb
and shed members depending on which question the labeller was answering, which is exactly the
bidirectional pattern observed. D, by contrast, is defined by a mechanism nothing else has
(an LLM transforming polluted context into confident output) and is correspondingly the most
stable class at 1/10.

**Mapping to our side is Ω_C and is NOT asserted here.** The tempting read is that Wu's E-hub
is the analogue of our P6, which §4.3 already concedes is "by construction parasitic on the
others" — i.e. both taxonomies carry exactly one cross-cutting member that destabilises
assignment. The next instance must carry this to the writeup as a **PROPOSED** mapping row
awaiting operator ruling, never as a finding. What *is* Ω_E and may be stated: both taxonomies
contain a member their own authors describe as cross-cutting rather than parallel.

## Finding R3 — the classes are not mutually exclusive at incident granularity

Independent of R2, the dataset's own `paper_class_ref` field cites a class *other than* the
case's own label for 4 of 22, and one more (`dream_quota_blast_radius`) is annotated
"C+D composite" in the source. The paper concedes the same in §8 (Construct): "reasonable
observers could draw the line differently for 2–3 incidents" — the artifacts show the real
number is larger. Our own §4.3 makes the matching concession ("a single incident can
instantiate two patterns at different layers"), so this is a *shared* property of both
taxonomies, and it is why the verdict grammar must not require unique assignment.

## Finding R4 — Class A remains verdict-ineligible

Under the pinned incident-granularity rule, the catalog gives Class A exactly **one** case
(`whatsapp_client_display_folding`), the six quirks being explicitly logged as sub-events with
no independent case documents. With n=1 the ≥2/3 expressibility rule is decided by a single
coder call. Class A ships as VERDICT-INELIGIBLE regardless of outcome. (Under the dataset
index Class A has 4 members — another way R2 bites: a class is verdict-ineligible under one
index and not the other.)

## R5 — PRE-REGISTERED DIRECTIONAL EXPECTATION about OUR taxonomy (operator, before direction (ii) runs)

Recorded **before** any of our units are coded, so it can be wrong on the record the way the
C/D guess just was.

**The mechanism R2a exposes, stated generally.** Wu's E hubs because it is defined on a
*different axis* from its siblings — declared-vs-runtime, where A/B/C/D sort by failure
mechanism. A cross-cutting axis has no stable boundary against classes sorted on another axis:
any incident with a declaration mismatch also has a mechanism, so it can be coded either way
depending on which axis the coder reaches for first. D is stable for the mirror reason —
unique mechanism, no axis competition.

**Why this predicts something about us.** Our six sort by **system layer** (production,
identity, replacement, reporting, gating, composition — v0.3 §4.3's own layer column). §4.6
then runs a *second* cut by **generative mechanism** (the trifurcation), explicitly kept as a
parallel scheme on the argument that the two license different repairs. **Wu's E is what
happens when those two cuts are mixed inside one taxonomy instead of kept parallel.**

**The expectation, falsifiable:**

> If our layer cut is clean, our patterns should be internally stable under blind coding, and
> any instability should **concentrate on whichever pattern is doing mechanism work rather
> than layer work**. P6 is the named candidate — §4.3 already concedes it is "by construction
> parasitic on the others," which is the same self-description Wu's E earns.

**How it gets scored (fixed now, not after seeing results):**
- **Confirmed** if P6 is the modal pattern in the UNSTABLE row of direction (ii), or is a
  member of the modal disagreeing pair, at a margin exceeding one unit.
- **Disconfirmed** if instability is diffuse across patterns (no pair occupying more than its
  share), or concentrates on a pattern other than P6.
- **Uninformative** if the direction (ii) unstable row is too small to have a mode (fewer than
  4 unstable units) — declared in advance so a thin result is not read as confirmation.

**Two warnings attached to this expectation, because it is an inviting story:**
1. It is a prediction about *our* internal stability. It is **not** the E↔P6 mapping claim,
   which stays Ω_C and PROPOSED. Confirming R5 would show P6 behaves like a cross-cutting
   member; it would not establish that P6 *is* Wu's E.
2. The prediction is attractive enough to bias an extractor who knows it. It is recorded in
   RECON/HANDOFF (extractor-facing) and **must not enter any coder payload** — the P-lexicon
   ban already excludes it, and the leak-grep's banned list must include `parasitic`,
   `cross-cutting`, and `layer` so a leaked hint is caught rather than assumed absent.

## Revised per-class MDE (replaces the plan's estimate)

The plan assumed "~4–5 units/class at n=22, one unit ≈ 20–25%." The real distribution makes
one unit worth:

| class | catalog n | one unit = | dataset n | one unit = |
|---|---|---|---|---|
| A | 1 | verdict-ineligible | 4 | 25% |
| B | 4 | 25% | 3 | 33% |
| C | 5 | 20% | 4 | 25% |
| D | 4 | 25% | 5 | 20% |
| E | 8 | 12.5% | 6 | 17% |

Only class E supports a per-class figure at better than 20% resolution under either index.
Every per-class verdict carries its own MDE in the writeup.
