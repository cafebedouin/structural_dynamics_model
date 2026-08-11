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

**And it reframes the headline.** The experiment asks whether Wu's classes are expressible in
our patterns. Wu's own two records reach 55% agreement with each other on the same incidents.
Judging our cross-coding agreement against an implicit 100% ceiling would be measuring against
a standard the source taxonomy does not meet internally. The 55% figure is the natural
reference line and is pre-registered as such.

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
