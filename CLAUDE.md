## The governing stance

**Apply this stance relentlessly: distrust the aggregate, witness before claiming, and treat
"I didn't find it" as different from "it isn't there."**

A count, a summary, a green check, an empty grep, a passing `forall`, a "looks done" — each is an
aggregate that can read as success while concealing the opposite. Before you claim, produce the
witness: the pasted run, the diff, the per-item check, the positive control that proves your probe
would have flagged the thing it now reports absent. "I didn't find it" is a fact about your search,
not about the world, until the search itself is shown to find. Two docs disagreeing, a 0-count over a
possibly-empty table, a clean read byte-identical to a read that never looked — resolve them against
the code, not against the more confident document. This stance is the root of the Build Discipline
section below; every pattern there is one instance of it.

## The working method (separated passes)

**Run the stance forward through the work as four kept-separate phases: read-only deciding passes,
then write passes; human-ruled adjudication on the calls that are the human's; paste-or-untag on
every done-claim.**

- **Read-only deciding passes precede write passes.** A pass that gathers evidence and decides what
  to do does not also change files. Decide first from what you read, then write in a separate pass.
  Interleaving the two lets a half-formed conclusion edit the substrate before it has been checked —
  the audit discipline (collect, *then* analyze) applied to editing.
- **Human-ruled adjudication.** A choice that is genuinely the user's — an ambiguous requirement, a
  contradiction between sources, a trade-off with no default — is escalated, not self-resolved. You
  may decide what the evidence settles; you may not decide what only the human can rule.
- **Paste-or-untag.** Every "done / verified / fixed / passing" claim carries its witness — the
  pasted run, diff, or count — in the same turn. If you cannot paste the witness this turn, drop the
  done-tag and mark the item OPEN with its graduation step. A claim without its witness is untagged,
  not done.

This is the same stance as above, run forward through the work: decide on evidence, rule only what
is yours to rule, and never let a summary stand in for a witness.

## One-sentence flag

**If you know a better way:** When a request has a cleaner implementation, a simpler
approach, or an unintended consequence the user likely did not see, say so in one
sentence before doing the work — then proceed. The user sometimes asks for suboptimal
things without realizing it. A one-sentence flag is enough; do not withhold action
pending approval.

## Context Window and File Size Constraints

If a file or task grows large enough that you anticipate context limitations will affect
your ability to work with it cleanly, **prompt the user with how you'd like it resolved**
before proceeding. Examples of useful prompts:

- "This file is 2,000+ lines. I can read it in chunks, or you could split it. What's best?"
- "Reading the full corpus would exceed context. Shall I sample by constraint type instead?"
- "The corpus + docs + code together would leave little room for draft output. Would you prefer I focus on analysis only and save drafts separately?"

Do not silently work around context constraints by using suboptimal approaches. Make the
constraint visible and ask for the user's preference — they may have information about
what matters most for this task.

## Project Context

Prolog+Python research infrastructure implementing Deferential Realism (DR).
99 Prolog modules, 223 main-corpus constraints (`prolog/testsets/`), 189 SOTU
constraints (`prolog/testsets_sotu/`), 100+ Python analysis scripts.

Key constraint: Correctness and reproducibility matter most. Model provenance
(which LLM built which testsets) is a feature, not a problem.

**Start here:** `docs/project_orientation.md` is the canonical operational reference for
any model entering this repo. It covers repo layout, classification architecture, the full
paper sequence with summaries, empirical findings inventory, open work items, and
methodological practices. Line-number anchors are anchored to git HEAD `db218d8c`
(2026-05-28); verify before citing, as high-churn files drift.

**Open questions tracker:** `ISSUES.md` (OQ-01 – OQ-44, and growing) logs unresolved
engine-level, schema-level, and paper-synchronization issues with status, evidence, and
what resolution would change. Check it before touching drl_core.pl, product_site_export.pl,
or the rope gate — OQ-01 and OQ-02 are directly relevant to those files.

**Implementation wiring notes:** `docs/technical/` contains file-level notes on
non-obvious wiring, operator-precedence bugs, fact-adapter patterns, and query gotchas
discovered during implementation sessions. Scope is narrow: things that caused real bugs
or confusion, not general architecture. Read the relevant file before modifying
`config_validation.pl`, `cs_kernel_registry.pl`, or the CS fact schema.
`build_discipline.md` documents five recurring cross-subsystem defect patterns
(produced-but-not-consumed; silent fork; bound-probe bypasses clause-order; fabricated default;
absence satisfies the gate) with diagnostics — consult before adding a step that writes output,
copying a file to test it, writing a findall over a cut-ordered predicate, defaulting a metric
on missing data, or adding a gate/threshold over a table that can be empty.

**Design intent:** `docs/design/design_discipline.md` is the companion to `build_discipline.md`
and its peer: where `build_discipline.md` governs *how we build and verify* (separated passes,
witness-before-claiming, the diagnostic-vs-classification level distinction),
`design_discipline.md` governs *what the engine is for, what follows from that, and what it must
not become* (the declared seat, mutual deference, classification-as-routing, the open-questions
sorting). Both are living documents — consult at session start, and amend as design decisions are
made and recorded. The design Omegas §6 sorts (Ω_E/Ω_C/Ω_P) are tagged by type directly on the
relevant open questions in `ISSUES.md` (the single source for open-question tracking); the Ω-type
taxonomy is defined in `docs/omega_variables.md`. Do not maintain a separate design-Omega ledger.

## Typical Workflow

The primary authoring loop is:

```
python3 agent/c-orchestrator.py "some topic"
```

This chains six steps automatically:
1. **Research** — web search grounding via Haiku
2. **Decompose** — UKE_SCOPE protocol selects axes (default 3) and produces a manifest
3. **Generate** — Sonnet generates one constraint story per axis; saves JSON to `json/`
   and Prolog testset to `prolog/testsets/`
4. **Corpus update** — runs `python/run_pipeline.py` to re-classify the full corpus
5. **Reports** — `python/enhanced_report.py` writes `outputs/constraint_reports/<id>_report.md`
   for each new constraint
6. **Essay** — Sonnet synthesizes a draft essay from the constraint reports; saved to
   `outputs/essays/` and `agent/analysis/essays/`

After the run, take `outputs/constraint_reports/*.md` and the essay draft to a model for
final essay synthesis. Finished essays are posted to cafebedouin.org and are not
committed to the repo beyond what the pipeline writes automatically.

**Corpus growth:** To expand the corpus without a full topic run, use
`python3 -m agent.generate_json_haiku` (reads `prolog/beta_seeds.json`, generates via
Haiku batch API with prompt caching). This is how the corpus grew from ~1,000 to 3,337 in the
pre-rebuild `testsets_3000/` archive; live `testsets/` is now 223 (see Critical Distinctions).

## Running the System

**All `swipl` calls require `cd prolog/` first.** The corpus glob (`testsets/*.pl`) resolves relative to swipl's working directory. Running swipl from the repo root silently loads 0 testsets. Python scripts enforce this via `cwd=PROLOG_DIR` in every subprocess call.

- Full pipeline (analysis only, no generation): `python3 python/run_pipeline.py`
- Prolog tests (corpus validation): `cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"`
- Prolog unit tests (engine): `cd prolog && swipl -g "[stack], [tests/test_snapshot_migration], run_tests, halt" -t "halt(1)"` — substitute any file in `prolog/tests/` (except `test_battery_variants.pl` which is a variant harness, not a plunit test)
- Linter: must be imported as library (`from linter import lint_file`), not run directly
- Config sensitivity: `python3 python/config_sensitivity_sweep.py`
- Directionality sensitivity: `python3 python/directionality_sensitivity_sweep.py`

## Corpus Loading

**`[stack]` alone loads 0 testsets.** `stack.pl` loads engine modules
(`drl_core`, `corpus_loader`, `constraint_indexing`, etc.) and makes
`corpus_loader:load_all_testsets/0` available, but does not call it.
Testsets are loaded on demand, not at stack load time.

**To load testsets in a REPL or script, call it explicitly:**

```prolog
% Interactive or one-shot query
:- [stack], corpus_loader:load_all_testsets.

% Idempotent form (safe to call multiple times; guarded by corpus_loaded/0 flag)
:- [stack], corpus_loader:ensure_corpus_loaded.
```

**How the frozen CLI commands handle it:**
- `product_site_export:run_product_export` — calls `corpus_loader:load_all_testsets`
  internally. Loading `product_site_export` as a module does NOT load testsets; only
  calling `run_product_export` or `run_product_export_to/1` triggers the load.
- `validation_suite`, `run_dynamic_suite` — these handle corpus loading internally.
- `perturb.py` overlays — each overlay file must `[stack]` AND
  `use_module(product_site_export)` AND call `run_product_export_to/1`. `[stack]`
  alone is not enough; `product_site_export` must be explicitly loaded.

**Corpus path and working directory:** controlled by `config:param(corpus_path, Dir)`
(default `testsets`). The glob is `Dir/*.pl` and resolves **relative to swipl's cwd**.
All frozen CLI commands use `cd prolog && swipl ...` so `testsets/` resolves correctly.
Running swipl from the repo root with the default `corpus_path` will find nothing.
To load testsets_3000, overlay `corpus_path` before calling `load_all_testsets`.

**Confirmation:** After `load_all_testsets` completes, `corpus_loaded/0` is asserted.
Check with `corpus_loader:corpus_loaded` in a REPL. The count is printed to stderr:
`[corpus] Loaded N testsets successfully.`

**Run-tagged subdirs (`prolog/testsets/<run_tag>/`) are isolated by the glob, not by dedup.**
`corpus_loader.pl` uses a non-recursive glob (`testsets/*.pl`), so subdir stories are NOT loaded
by default. This is **load-time** safety only. If `corpus_path` is ever changed to include a
run-tagged subdir, or runs are flattened together, duplicate loading becomes live (silently — no
error). The shield is the glob; removing it reopens the collision question. Check this before
editing the `corpus_loader` glob or overlaying `corpus_path`.

## Known State

The dated session changelog now lives in **`KNOWN_STATE.md`** (split out 2026-05-31 to cut
this auto-loaded file's per-session token cost). `KNOWN_STATE.md` is read on demand, not
auto-loaded.

**Read `KNOWN_STATE.md` before touching** any of: `signature_detection.pl`,
`product_site_export.pl`, `enhanced_report.py`, `python/sweeps/perturb.py`,
`python/demotion_pass.py`, `config_validation.pl`, `drl_composition.pl`, or the
`corpus_loader` glob / `corpus_path` overlay — recent changes and mitigations to those files
are recorded there.

Standing warnings that were embedded in the changelog have been promoted into the
always-loaded sections of *this* file, so they remain in every session's context:
- `product_site_export.pl:75–77` green cut (LCO-critical) → **Architecture Invariants**.
- Run-tagged subdir glob isolation → **Corpus Loading**.
- Corpus is 223 not 3,337 / cite the manifest → **Critical Distinctions**.

## Pipeline Output Manifest Convention

Pipeline output JSONs carry a `manifest` top-level key with provenance information:
timestamp (`pipeline_run_at`), corpus counts (`n_constraints`, `n_sotu_constraints`),
git commit (`code_commit`, `code_commit_short`), dirty-tree flag (`code_dirty`), and
`schema_version`. Audits running against pipeline output should cite the manifest in
their writeups. The corpus is continuously extending (orchestrator runs add constraints),
so "the corpus" is meaningful only relative to a timestamp; the manifest makes the
timestamp citable. See `when_apparatus_sharpens_taxonomy.md` §4.1 for context.

## Architecture Invariants

- All metric-based classification routes through classify_from_metrics/6 in drl_core.pl.
  Final type may then be overridden by integrate_signature_with_modal/3 (signature
  layer, also inside dr_type/3). Two of the 6 authored Surface-1 fields
  (accessibility_collapse, resistance) feed the signature layer and bypass
  classify_from_metrics/6 argument slots entirely.
- config.pl is single source of truth for param/2 facts
- Dual threshold: both χ AND ε must be checked
- .tsx artifacts are outputs, not infrastructure
- Archive testsets document build provenance, not active code
- `product_site_export.pl:75–77` `write_entries` clause 3 ends with a `!` after
  `write_one_entry` that is **LCO-critical** — it enables last-call optimization and prevents
  OOM under compressed-ceiling sigmoid variants. The cut looks removable (normal exports pass
  without it) but removing it silently regresses to OOM on those variants. Underlying
  choice-point question is OQ-02; zero-diff proof and history in KNOWN_STATE.md.

## Build Discipline (recurring failure modes — check before declaring work done)

This repo was built fast and solo, and two defects recur across unrelated subsystems
because the producing step is the interesting part and the reconciling step is the boring
one that gets deferred. They are invisible at the moment they're introduced because the
producer *looks* complete. Name them; do not reproduce them.

**1. Produced-but-not-consumed (the dangling wire).** Information is correctly generated,
written to disk, and then nothing reads it back into the thing that needs it. Instances
already in this repo: sensitivity sweeps write `*_sensitivity_results.json` that no
consumer reads; SCOPE writes `kernel_grouping.json` but the grouping is (was) not stamped
into the `.pl` files, leaving stories with `cs_story_uid` and no `cs_kernel_id`; the
manifest convention exists so audits *can* cite provenance but nothing enforces that they
do. **Rule: a producer is not done until something consumes its output.** When you add a
step that writes data, either wire the consumer in the same change or add a check that
fails loudly when the output is unconsumed. A meter with no dial is not a meter.

**2. One-canonical-thing-became-two (the silent fork).** A file or record gets copied to
a scratch/test location, edited, and now two versions exist with no queryable fact saying
which is canonical — the knowledge lives only in someone's memory. Instances: the
duplicated `generate_kernel_corpus.py` (`commitment_corpus/` test copy vs `agent/`);
historically, multiple tracking surfaces (ISSUES / AGENDA / PRIORITIES / TODO) where the
update protocol named only some. **Rule: one canonical location per thing, and which one
is canonical must be a checked fact (a path in docs, a CI check), not a memory.** Before
duplicating a file to test it, prefer a branch or a clearly-marked-temporary copy with a
deletion plan. When you find a fork, resolve by evidence (which path do run-commands
invoke, which imports resolve, git recency) — not by preference — and record the verdict
in `Known State` so the next agent does not re-fork it.

3. Destructive-replace without proof (the faith merge). Before
deleting, retiring, or overwriting any script, sweep, data file,
or generator that a downstream step or another version relies on:
run old and new, paste both outputs, diff them, show identity or
justify every difference in the same change. The old version is
not removed until the new is shown faithful. "Structurally
equivalent" is a code-read, not proof — the diff is
proof. Consolidating N into one is N separate old-vs-new diffs,
each before its standalone is retired. Instance already in this
repo's near future: collapsing the 5 type-stability sweeps onto
perturb.py — each sweep gets an old-vs-primitive diff before its
bespoke version is deleted, or the consolidation is faith, not
fact.

**4. Recap-as-witness substitution.** The turn-end recap and any "done / verified / working /
complete" statement in prose is a CLAIM, not a witness. A claim is only discharged in the
turn by its pasted output — the diff, the run, the validation result, the count. When a
turn reports N edits done, each of the N must carry its own pasted witness in that same
turn; a recap asserting "N edits done" while only M<N are pasted is the substitution defect
(observed: a turn recapped "three edits witnessed" with only the third pasted; the two
asserted-done edits were unwitnessed).

Rule: do not report an edit, fix, or verification as done unless its witness is in the same
turn. If a witness cannot be produced this turn, label the item OPEN with its graduation
step — never let the recap's summary stand as the completion record. The operator should
read any done-claim lacking a same-turn paste as unverified, regardless of the recap.

This is the same defect as produced-but-not-consumed, one layer up: a claim produced
without the witness that consumes it.

**5. Absence satisfies the gate (authored-zero vs absent).** A gate, threshold, or quantifier
passes because its input is *missing*, not because a condition was *checked*: `Count == 0` is true
both when authored to zero and when no facts exist; `forall(P, Q)` is vacuously true when `P`'s
table is empty; `V =< Ceil` passes when `V` is a fabricated default for absent data. The engine
must distinguish *authored to be zero* from *absent* everywhere and **never let absence satisfy a
gate** — a gate whose source table is empty is testing nothing while reading as a pass. Instance:
`natural_law_signature`'s `BeneficiaryCount == 0` reads `intent_power_change`, empty corpus-wide
(0 facts), so it passes by absence for all constraints — the 404 NL certifications mean "no
beneficiary *authored*," not "no beneficiary *exists*" (OQ-43). Sibling of fabricated-default
(build_discipline.md Pattern 4): that invents a *value*, this lets *absence* pass a *condition*.
**Rule: a gate over a possibly-empty table must establish the datum was authored before it may
pass — fail-closed on absence, not pass-open.** Diagnostic: count the source predicate's facts on
the corpus; count 0 ⇒ the gate is a no-op. Engine-wide audit is OQ-44; full pattern +
diagnostic in `docs/technical/build_discipline.md` Pattern 5.

The first two reduce to the same root: **the corpus/codebase you are building for is not the one
on disk now.** Build naming schemes, linkage rules, and reports to be correct for the
corpus you intend (thousands of stories, regeneration under schema change, found-article
ingestion), not the sample that happens to exist. A scheme that *cannot* collide by
construction beats one that *happens not to* collide today. See
`docs/technical/build_discipline.md`.

**The spine under all of these: every defect here is an absence that presents as a presence.**
Something is missing — a consumer, a canonical fact, a clause dispatch, an authored datum, an
authored disqualifier — and a *success-shaped token* (the producer ran; both copies parse; a
solution came back; a plausible `0.5`; the gate passed) fills the hole so the read site can't tell
it from the real thing. The single fix, everywhere: **carry the provenance bit with the value so
absence and success stop collapsing to one token at the read site** — a bare value is a lie of
omission the consumer can't detect. Concretely: wire-or-fail-loud, checked-canonicity, let the
engine dispatch, return `unknown` not `0.5`, fail-closed-on-absence.

**Diagnostics are not exempt — every probe needs a positive control.** A clean read is
byte-identical to a read that didn't look: an empty grep, a `findall` of `[]`, a count of `0`, an
"I found it nowhere" each can mean "nothing there" or "didn't dispatch / queried wrong / never
ran." This holds for *reasoning*, not just shell: a claim of the form "X appears nowhere / is
unique" is an unfalsified diagnostic until run against a case you know it must flag. "I didn't find
it" is not "it is not there" until the finder is shown to find. Full table and instances:
`docs/technical/build_discipline.md` → *The spine* and *Every diagnostic needs a positive control*.

## Critical Distinctions

**Corpus size is 223 (live `testsets/`), not 3,337.** The 3,337 figure predates a deliberate
rebuild: exploratory committer-axis runs reused constraint IDs across runs (the "chimera," OQ-25 /
v7 §5.11). Cleanup reduced `testsets/` to a single coherent run (kernel_run_03: 109 CS readings +
~114 observer-axis). §5.11 trifurcation figures are verified single-run coherent. **Do not cite
3,337, and do not cite a memorized count — the corpus is actively growing; cite the pipeline
manifest** (`n_constraints`). The `testsets_3000/` directory (3,380 constraints) is the archived
pre-rebuild corpus, used only for retrospective audits that explicitly overlay `corpus_path`.

**`json/` files are LLM-generated constraint specifications, not analysis output.**
Each file in `json/` is produced by step 3 of the orchestrator (Sonnet generates it
from an axis in the UKE_SCOPE manifest). The orchestrator writes the JSON to `json/`
and the corresponding Prolog testset to `prolog/testsets/`. These files are inputs to
the analysis pipeline — `run_pipeline.py` reads them; it does not write them. Analysis
output lives in `outputs/`.

**Canonical framework paper: `docs/deferential_realism_paper_v7.md`.** v7 promotes
the committer axis from "commentary-grade annotation" (v6.12 §4.2) to a co-equal second
axis: Axiom 7 (authored commitment structure with computed consequence), Theorem 7
(detection independence — observer-coherent readings can be committer-foreclosed),
Theorem 8 (licensed plurality vs. real closure), §4.5 (two-axis engine), §5.11
(trifurcation profile). Axioms 1–6 and Theorems 1–4 are unchanged from v6.13.
`docs/deferential_realism_paper_v6.13.1.md` is a parallel amendment to the observer-only
line, formally updating Axiom 2 for OQ-26 (ε is reading-relative across generation runs).
Files through v6.13 are superseded for the full framework; use v7.

**Formal classification rules: `docs/logic.md`.** This is the spec document; `config.pl`
must match it. UTF-8 encoding was repaired Feb 2026 (prior versions had mojibake from
double-encoded characters). Edit tool fails on files with multi-byte mojibake — use sed
or Python if you encounter this.

**`dr_type_at/4` and `classify_snapshot/3` have been replaced (2026-05-17):** Both carried
DEPRECATED markers using the legacy `power_modifier` χ path (χ = ε × π, omitting σ).
Replaced by `classify_at_time/4` (`drl_composition.pl`) and `snapshot_type/3`
(`transition_paths.pl`) using the canonical sigmoid pipeline (χ = ε × f(d) × σ(S)).
Callers `constraint_history/3` and `degradation_chain/3` updated accordingly.

**`site_contexts_product/1` scope exclusion is calibration-based.** The product site
excludes `regional`, `continental`, `universal` scopes (`constraint_indexing.pl:954–955`).
σ(universal) = σ(national) = 1.0 (`config.pl:117,120`) — no differential χ effect between
the two. The actual reason: these three scope atoms appear in no canonical context and their
scope_modifier values have not been validated against corpus classifications.

**Pre-computed values live in `outputs/pipeline_output.json`.** H¹, Arakelov heights,
MaxEnt distributions, and classifications are pre-computed by the pipeline. Read from
there; do not recompute from scratch.

## Math Employed in the Prolog Engine

**Calculus / Analysis:** sigmoid/logistic function, exponential function, linear slope
(least-squares accumulation), monotonicity test, drift velocity (first-order temporal
rate), drift acceleration (second-order).

**Probability / Information Theory:** Shannon entropy (normalized H/log N), maximum
entropy principle, Gaussian log-likelihood, log-sum-exp normalization, prior
distribution, Kullback-Leibler divergence (threshold), Boltzmann distribution /
partition function.

**Algebra / Arithmetic:** weighted sum, absolute value, normalization to [0,1]
(clamping), rounding/floor/ceiling, accumulation via fold, sum/mean/min/max aggregation.

**Threshold / Classification:** dual-threshold classification (χ AND ε), priority cascade
(mountain > piton(dead-coordination) > snare > scaffold > rope > tangled_rope > piton(fallback) > naturalized > unknown),
complexity-adjusted threshold (entropy-weighted).

**Power Scaling:** χ = ε × f(d) × σ(S) (canonical extraction formula), power modifier
π(P), scope modifier σ(S), sigmoid directionality f(d), cognitive displacement δ.

**Purity / Contamination Algebra:** Gaussian inverted-U (exp(−(x−μ)²/2σ²)), purity
degradation (max(0, intrinsic − contamination × immunity)), type contamination strength
coefficients, type immunity/susceptibility factors, information-theoretic excess
extraction.

**Graph / Network:** BFS on constraint graph, network purity (weighted cluster average),
contamination cascade/propagation, edge coupling strength.

**Fixed-Point / Iteration:** Jacobi iteration (simultaneous-update), greatest fixed-point
convergence (monotone convergence theorem), Scott-continuity / monotone endofunctor
— all in `drl_fpn.pl`.

**Category Theory / Structural:** presheaf evaluation (restriction across observer
contexts), site/coverage structure (observer-accessible stalks), logical isomorphism
(fingerprint equivalence), lattice operations (meet/join via max/min).

**Modal / Decision Logic:** modal operators (necessity/possibility) as composition rules,
Boltzmann factorization test (independence check), sorting/ranking.

Key files: `drl_core.pl`, `constraint_indexing.pl`, `drl_boltzmann_analysis.pl`,
`boltzmann_compliance.pl`, `drl_purity_network.pl`, `drl_fpn.pl`,
`maxent_classifier.pl`, `drl_composition.pl`, `logical_fingerprint.pl`.

## End-of-Session Documentation Review

When completing work that changes code, produces empirical findings, or resolves an
open question — whether or not the session ends with a PR — review and offer updates to:

- **KNOWN_STATE.md** — new mitigations, resolved issues, code changes with proofs (the dated
  session log; default destination for new findings, NOT CLAUDE.md). Then apply the
  **promotion test** to each new entry: *"Would a fresh agent who never read KNOWN_STATE.md make
  a concrete, **silent** mistake before editing file X (or running command Y) without knowing
  this?"* If yes — and the warning is not already stated in an always-loaded section and is
  stable (not drift-prone, not experiment-scoped) — **promote the tripwire** into the relevant
  always-loaded CLAUDE.md section (Architecture Invariants | Critical Distinctions | Corpus
  Loading | Running the System), leaving the full provenance in KNOWN_STATE.md. Loud failures
  (an immediate error like `export_failed`) are NOT silent mistakes → they stay history. Bias to
  history; over-promotion defeats the token-saving purpose. Do the classify step and the promote
  step as two passes, not one — a misclassification that moves a warning out of context fails
  silently, which is the exact seam this split guards.
- **AGENTS.md** — any change to architecture, testing commands, naming conventions, or
  invariants that a future agent would need to know before touching the relevant files
- **`ISSUES.md`** — status changes (open → mitigated → resolved), new OQ
  items surfaced by the work, updated evidence or file references
- **AGENDA.md** — work packages started, completed, or newly identified
- **PRIORITIES.md** — promote, demote, or retire items based on what the session
  completed or unblocked; add new blockers that emerged

Offer the updates as a diff or edit proposal, not just a verbal summary. The four files
only stay useful if they reflect the current state of the code and open questions.

**Before any `git push`:** verify the four files above are current with respect to the
changes being pushed. A push that makes the docs stale is documentation debt that
compounds across sessions.

Done includes the next step, landed in substrate — not stated in
chat. A task is not complete until a fresh instance, reading only
the repo (not this conversation), could pick up the next forward
move. If the session surfaced a next step, a sequencing
constraint, or a fact that currently lives only in the
conversation, write it where the cold read will find it: the
relevant OQ in ISSUES.md, the ordering note in
PRIORITIES.md/AGENDA.md, or a comment at the code it concerns. A
next step spoken in chat and not written to substrate is a handoff
that did not happen — the produced-but-not-consumed defect at the
seam between sessions. State the next step and its sequencing
constraint (why this one, what it's gated behind), because the
bare next-step is the one a cold reader most easily gets wrong.

## Audit Methodology

Completed audit passes follow: **recon** → **proposal** → **execution** → **writeup**.
Recon establishes what data exists and what questions are answerable. Proposal states
exactly what will be run and what would constitute each verdict. Execution runs scripts
and saves raw output. Writeup analyzes from evidence only — never from documentation
restated as findings. See `docs/project_orientation.md` §8.1 for detail.
