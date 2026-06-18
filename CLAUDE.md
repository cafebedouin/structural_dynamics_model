## The governing stance

**Distrust the aggregate, witness before claiming, and treat "I didn't find it" as different
from "it isn't there."**

A count, a summary, a green check, an empty grep, a passing `forall`, a "looks done" — each can
read as success while concealing the opposite. Before you claim, produce the witness: the pasted
run, the diff, the per-item check, the positive control that proves your probe would have flagged
the thing it now reports absent. "I didn't find it" is a fact about your search, not the world,
until the search itself is shown to find. Two docs disagreeing, a 0-count over a possibly-empty
table, a clean read byte-identical to a read that never looked — resolve against the code, not the
more confident document. Every Build Discipline pattern below is an instance of this stance.

## The working method (separated passes)

- **Read-only deciding passes precede write passes.** A pass that gathers evidence and decides
  does not also change files; decide first from what you read, then write in a separate pass.
  Interleaving lets a half-formed conclusion edit the substrate before it has been checked.
- **Human-ruled adjudication.** A choice that is genuinely the user's — an ambiguous requirement,
  a contradiction between sources, a trade-off with no default — is escalated, not self-resolved.
  You may decide what the evidence settles; not what only the human can rule.
- **Paste-or-untag.** Every "done / verified / fixed / passing" claim carries its witness — the
  pasted run, diff, or count — in the same turn. If you cannot paste it this turn, drop the
  done-tag and mark the item OPEN with its graduation step.

## One-sentence flag

**If you know a better way:** when a request has a cleaner implementation, a simpler approach, or
an unintended consequence the user likely did not see, say so in one sentence before doing the
work — then proceed. Do not withhold action pending approval.

## Context Window and File Size Constraints

If a file or task is large enough that context limits would affect your ability to work with it
cleanly, **prompt the user with how you'd like it resolved** before proceeding (e.g. "This file is
2,000+ lines. I can read it in chunks, or you could split it. What's best?"). Do not silently work
around context constraints with suboptimal approaches — make the constraint visible; the user may
know what matters most.

## Project Context

Prolog+Python research infrastructure implementing Deferential Realism (DR). 99 Prolog modules,
100+ Python analysis scripts. The live corpus (`prolog/testsets/`) was RESET 2026-06-05 and is
being rebuilt from scratch under the de-leaked generation pipeline (KNOWN_STATE 2026-06-05) —
cite the pipeline manifest for its size, never a memorized count. All previous corpora live under
`prolog/archives/datasets/`. Correctness and reproducibility matter most; model provenance (which
LLM built which testsets) is a feature, not a problem.

**Start here:** `docs/project_orientation.md` — canonical operational reference (repo layout,
classification architecture, paper sequence with summaries, findings inventory, open work,
methodology). Line-number anchors pinned to git HEAD `db218d8c` (2026-05-28); verify before
citing — high-churn files drift.

**Open questions tracker:** `ISSUES.md` logs unresolved engine-, schema-, and
paper-synchronization issues with status, evidence, and what resolution would change. Check it
before touching drl_core.pl, product_site_export.pl, or the rope gate — OQ-01 and OQ-02 are
directly relevant.

**The `[NEXT]` activation — "what should I work on?"** When the operator sends **`[NEXT]`** (or
asks what to work on / for sequencing), **run `python3 python/omega_resolver.py menu`** and present
its **WORKABLE NOW** list (sorted by authored `Priority:`, 1=highest) as the options for the
operator to pick, plus the **BLOCKED ON YOU** items that need a ruling/spend-go. **Do NOT read
`ISSUES.md` whole** — it is 6,700+ lines; reading it leads to a faked query and *prose-guessed*
dependency edges (a witnessed failure mode). The menu is the queryable surface: it computes
reachability from the authored `**Deps:**` edges (typed relators `blocked_on`/`gates`/`bundled_with`
/`splits_from`/`blocked_on_human`) over an SCC condensation, and surfaces the authored `**Priority:**`
hint — *priority/value is the operator's declared seat, surfaced, never computed.* The menu's
coverage footer states how trustworthy the frontier is (how many active OQs have authored Deps);
edge-free OQs default `workable_now` and may overstate until their `Deps:` are authored. When you
mint or touch an OQ, author its `**Priority:**` (and `**Deps:**` if it blocks/awaits another) so the
frontier stays honest. **To route an ARBITRARY question to its 2–3 relevant OQs (distinct from
`menu`'s "what's workable next"), scan the derived router `issues/INDEX.md`** (compact: id · status ·
priority · title · deps · cross-refs · mentioned-in) and then read the picks with `grep OQ-NN
ISSUES.md` — this is the supported alternative to reading `ISSUES.md` whole. The router is GENERATED
from `ISSUES.md` (`python3 python/omega_resolver.py index`; JSON twin `issues/INDEX.json`), never
authoritative itself, and its Active/Archive partition imports `omega_resolver.ACTIVE` (so `mitigated`
sits in Archive — OQ-141). **Regenerate it after ANY `ISSUES.md` edit** — `omega index --check` in
`scripts/gate.sh` turns `[GATE]` red on a stale index (so a forgotten regen fails loud, not silent).
`omega_resolver.py check` is the authority-control gate (dangling Deps,
rotted witnesses, malformed/packed `Deps:` edges); `selftest` runs its 9 positive controls. **Read
`docs/technical/omega_resolver.md` before modifying `omega_resolver.py`, the `ISSUES.md` authored
fields (`Status`/`Ω-type`/`Deps`/`Priority`), or the hooks** — it carries the command table, the
reachability/SCC model, the determinism-boundary "floor" (priority/type stay a declared seat, never
computed), and the gotchas (output-shape coupling, the §1b freshness key).

**More activations** (same convention — recognize the exact bracketed token and act):
- **`[GATE]`** — run `./scripts/gate.sh` and report the green/red summary (issues_status,
  omega_resolver check + selftest + index, known_state). Do not fix anything; just report. Use before
  committing anything that touches `ISSUES.md`/`KNOWN_STATE.md`.
- **`[PUSH]`** — the pre-push ritual: (1) run `[GATE]` — must be GREEN; (2) confirm
  `ISSUES.md`, `KNOWN_STATE.md`, and `AGENTS.md` reflect the changes being pushed (the
  documentation-currency check); (3) if both hold, `git push origin main`, else report what is
  blocking. Always show the gate output as the witness — never push on an unverified claim.

**Implementation wiring notes:** `docs/technical/` holds file-level notes on non-obvious wiring,
operator-precedence bugs, fact-adapter patterns, and query gotchas — things that caused real bugs,
not general architecture. Read the relevant file before modifying `config_validation.pl`,
`cs_kernel_registry.pl`, or the CS fact schema. Read `swipl_load_path_and_probe_gotchas.md` before
diagnosing module-resolution behavior (REPL ≠ pipeline for wrong-qualifier calls), writing
test-local predicate swaps or retract/re-assert probes, running an in-session corpus sweep or
overlay counterfactual (validated signature-sweep recipe; Boltzmann memo caches read stale unless
cleared), or interpreting a pipeline_output.json diff. `build_discipline.md` documents the six
defect patterns (summarized in Build Discipline below) with diagnostics; it also carries the
**citation-time / staleness-ladder** rule (a witnessed fact has a shelf life: reusing it as a
premise re-asserts it, so rung it — pointer / as-of stamp / gate — by mutable-state ×
cost-of-acting-on-stale) and the **triage list** of premises that may not be cited as settled
without a live re-witness. Consult before citing a prior run, "tests pass," or "the corpus is
current" as a settled premise.

**Design intent:** `docs/design/design_discipline.md` is the peer of `build_discipline.md`: the
latter governs *how we build and verify*; the former governs *what the engine is for and must not
become* (the declared seat, mutual deference, classification-as-routing, the open-questions
sorting). Both are living documents — consult at session start, amend as decisions are recorded.
Design-Omega §6 sorts (Ω_E/Ω_C/Ω_P) are tagged directly on the relevant OQs in `ISSUES.md` (the
single source for open-question tracking); the Ω-type taxonomy is `docs/omega_variables.md`. Do
not maintain a separate design-Omega ledger. `docs/design/design_gaps.md` is the third family
member: a ledger of *declared absences* — capabilities the engine deliberately does not yet have,
so an empty placeholder is never mistaken for a working feature. Check it before reviving a
removed/unfed predicate or proposing a new provenance/tracking surface (GAP-01 is derivation-chain
tracking). A defect that reads as working goes to `ISSUES.md`; an absence the design admits goes
here.

## Typical Workflow

Primary authoring loop: `python3 agent/c-orchestrator.py "some topic"` — chains seven steps:

1. **Research** — web search grounding via Haiku
2. **Decompose** — UKE_SCOPE protocol selects every §3-distinct axis (no fixed count; `--axes N`
   is an optional ceiling) and produces a manifest
3. **Generate** — Sonnet generates one constraint story per axis; JSON to `json/`, Prolog testset
   to `prolog/testsets/`
4. **Corpus update** — `python/run_pipeline.py` re-classifies the full corpus
5. **Reports** — `python/enhanced_report.py` writes `outputs/constraint_reports/<id>_report.md`
   per new constraint
6. **Tensions ledger** — deterministic extraction (`python/tensions_ledger.py`), NOT an essay
   (OQ-101: the essay FORM collapses plurality; auto-essay removed 2026-06-10)
7. **Commit** — `_step_commit` commits the new `json/<cid>.json` + `prolog/testsets/<cid>.pl`,
   GATED (skips on `--no-commit`, run-tag, or a failed corpus update) and SCOPED to this run's
   cids (never `git add -A`; refuses if the index already holds unrelated staged changes); local
   commit only, never pushes

After the run, take `outputs/constraint_reports/*.md` and the draft to a model for final essay
synthesis. Finished essays are posted to cafebedouin.org, not committed beyond what the pipeline
writes.

**Corpus growth without a full topic run:** `python3 -m agent.generate_json_haiku` (reads
`prolog/beta_seeds.json`, Haiku batch API with prompt caching). This grew the chimera-era corpus
to 3,337 (archived at `prolog/archives/datasets/original_v6/`); the live corpus is the
post-2026-06-05 rebuild (see Critical Distinctions).

**Building a corpus from scratch / in bulk / as a second-model twin:** follow the runbook
`docs/technical/bulk_corpus_generation.md` — which generation path to use (and why NOT
`generate_json.py` for kernel corpora), the seed pipeline, the five-defect provenance fix, the
per-chunk recipe (ladder strip → generate → manual OQ-58 sweep → run_pipeline → commit), the
recurring failure modes, and the twin-model method. The reusable seed pool + this build's records:
`prolog/kernels/rebuild_2026-06-13/`.

## Running the System

**Run `swipl` from `prolog/` (convention); corpus LOADING is cwd-independent (2026-06-04):**
`corpus_loader` resolves a relative `corpus_path` against its own source directory and **throws
`corpus_empty` on a 0-file glob** (escape hatch: assert `config:param(allow_empty_corpus, true)`
first). **Output writes are still cwd-relative** (`../outputs/...` in exporters and probe
scripts), so keep `cd prolog/` for any command that writes. Python scripts enforce
`cwd=PROLOG_DIR` in every subprocess call regardless.

**Never run two pipelines or topic runs concurrently against the shared `prolog/testsets/` +
`outputs/` — serialize them (generate-only first, then one `run_pipeline`).** Concurrent runs
race the shared corpus and outputs: witnessed giant_comp SIGSEGV + per-run manifests that are
not coherent snapshots (OQ-77, resolved 2026-06-10; `audits/2026-06-10_oq77_serial_kill_condition/`).
Within-pipeline parallelism (the 11 analyses in one `run_pipeline`) is fine — the rule is
one pipeline at a time.

- Full pipeline (analysis only, no generation): `python3 python/run_pipeline.py`
  - `run_pipeline()` opens with the **ISSUES.md status-grammar gate** (`issues_status.scan()`,
    aborts naming malformed entries). **Do not remove or bypass the gate — it is NOT dead code,
    including during refactors of run_pipeline.py.** If it fires, fix ISSUES.md until
    `python3 python/issues_status.py --check` passes. Grammar is in the ISSUES.md footer.
- Prolog tests (corpus validation): `cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"`
- Prolog unit tests (engine): `cd prolog && swipl -g "[stack], [tests/test_snapshot_migration], run_tests, halt" -t "halt(1)"` — substitute any file in `prolog/tests/` (except `test_battery_variants.pl`, a variant harness, not a plunit test)
- Stack consistency check (OQ-57-class wrong-qualifier detection): `cd prolog && swipl -l check_stack.pl -g "run_check_stack, halt" -t "halt(1)"` — compare against the recorded baseline (KNOWN_STATE.md 2026-06-04); new findings are regressions. Not a pipeline gate while the baseline is non-empty.
- In-session overlay probes: use `probe_harness:with_retracted/2` / `with_overlay/3`
  (snapshot-first, verified restore, automatic cache clearing via
  `cache_registry:clear_all_caches/0`) instead of hand-rolled retract/assert — see
  `swipl_load_path_and_probe_gotchas.md` §§2–4, 7.
- Linter: must be imported as library (`from linter import lint_file`), not run directly
- Config sensitivity: `python3 python/config_sensitivity_sweep.py`; directionality:
  `python3 python/directionality_sensitivity_sweep.py`

## Corpus Loading

**`[stack]` alone loads 0 testsets.** `stack.pl` loads engine modules and makes
`corpus_loader:load_all_testsets/0` available but does not call it. Load explicitly:

```prolog
:- [stack], corpus_loader:load_all_testsets.     % one-shot
:- [stack], corpus_loader:ensure_corpus_loaded.  % idempotent (guarded by corpus_loaded/0)
```

**Frozen CLI commands:** `product_site_export:run_product_export` / `run_product_export_to/1`
load testsets internally (loading the module does NOT). `validation_suite` / `run_dynamic_suite`
handle loading internally. `perturb.py` overlay files must `[stack]` AND
`use_module(product_site_export)` AND call `run_product_export_to/1` — `[stack]` alone is not
enough.

**Path resolution (2026-06-04):** `config:param(corpus_path, Dir)` (default `testsets`). A
RELATIVE `Dir` is anchored against `corpus_loader.pl`'s own source directory (`prolog/`) via
`resolve_corpus_dir/2` — loading is cwd-independent; absolute paths pass through. A 0-file glob
throws `corpus_empty` (fail-closed; escape: `config:param(allow_empty_corpus, true)`). To load an
archived corpus (e.g. `archives/datasets/original_v6` or `archives/datasets/kernel_v1`), overlay
`corpus_path` before calling `load_all_testsets` (relative overlays resolve against `prolog/`).
**Overlay with `asserta` (or `retractall(config:param(corpus_path,_))` first) — NOT plain
`assertz`.** config.pl:489 defines the default `param(corpus_path, testsets)` as the first clause
and the loader takes the first solution, so a plain `assertz('testsets_flash')` appends *after* the
default and is **silently ignored** — you load the default `testsets` and the count looks
successful (witnessed 2026-06-13: a twin-corpus overlay loaded 44 instead of 960 with no error).
This is also how the `testsets_haiku/` / `testsets_flash/` twin corpora are loaded for comparison.

**Confirmation:** after `load_all_testsets`, `corpus_loaded/0` is asserted and
**`corpus_loader:corpus_constraint/1`** holds one fact per loaded testset (id = file base name) —
the authoritative corpus membership/denominator; enumerate it in probes and exporters, never
constraint_metric/classification unions (those pick up engine demo constraints). Count printed to
stderr: `[corpus] Loaded N testsets successfully.`

**Run-tagged subdirs (`prolog/testsets/<run_tag>/`) are isolated by the glob, not by dedup.**
The non-recursive glob (`testsets/*.pl`) means subdir stories are not loaded — load-time safety
only. Pointing `corpus_path` at a run-tagged subdir, or flattening runs together, makes duplicate
loading live silently. Check this before editing the `corpus_loader` glob or overlaying
`corpus_path`.

## Known State

The dated session changelog lives in **`KNOWN_STATE.md`** (read on demand, not auto-loaded).
**Before touching a file, query it instead of reading all of it:**
`python3 python/known_state_status.py --file <path>` lists entries whose `Files:` line names it
(machine-readable `Files:`/`Tier:` headers; checker `--check`; grammar in its header).
Highest-traffic files: `signature_detection.pl`, `product_site_export.pl`, `enhanced_report.py`,
`python/sweeps/perturb.py`, `python/demotion_pass.py`, `config_validation.pl`,
`drl_composition.pl`, `json_report.pl`, `generate_kernel_corpus.py`, and the `corpus_loader`
glob / `corpus_path` overlay.

Standing warnings already promoted into this file: `product_site_export.pl:75–77` LCO-critical
cut → **Architecture Invariants**; run-tagged subdir glob isolation → **Corpus Loading**;
cite-the-manifest-not-a-memorized-count → **Critical Distinctions**.

## Pipeline Output Manifest Convention

Pipeline output JSONs carry a top-level `manifest`: `pipeline_run_at`, `n_constraints`,
`n_sotu_constraints`, `code_commit`/`code_commit_short`, `code_dirty`, `schema_version`. Audits
against pipeline output must cite it — the corpus continuously extends, so "the corpus" is
meaningful only relative to a timestamp. See `when_apparatus_sharpens_taxonomy.md` §4.1.

## Architecture Invariants

- All metric-based classification routes through classify_from_metrics/6 in drl_core.pl.
  Final type may then be overridden by integrate_signature_with_modal/3 (signature layer, also
  inside dr_type/3). Two of the 6 authored Surface-1 fields (accessibility_collapse, resistance)
  feed the signature layer and bypass classify_from_metrics/6 argument slots entirely.
- config.pl is single source of truth for param/2 facts
- Dual threshold: both χ AND ε must be checked
- .tsx artifacts are outputs, not infrastructure
- Archive testsets document build provenance, not active code
- `product_site_export.pl:75–77` `write_entries` clause 3 ends with a `!` after
  `write_one_entry` that is **LCO-critical** — it enables last-call optimization and prevents OOM
  under compressed-ceiling sigmoid variants. It looks removable (normal exports pass without it)
  but removing it silently regresses to OOM on those variants. Choice-point question is OQ-02;
  zero-diff proof and history in KNOWN_STATE.md.
- **Headline verdict = `verdict_join.verdict`; `diagnostic_verdict.verdict` is a raw input,
  never a headline** (OQ-98, 2026-06-11). `verdict_join` is computed in
  `diagnostic_summary:verdict_join/3` (base verdict + severity-floored alerts + grid/measurement
  provenance) and serialized as a sibling of `diagnostic_verdict` — any new report/consumer
  surface must headline the join; rendering `diagnostic_verdict.verdict` as a summary recreates
  the GREEN-over-severe-alerts defect. Provenance: KNOWN_STATE 2026-06-11.

## Build Discipline (recurring failure modes — check before declaring work done)

These defects recur across unrelated subsystems because the producing step is interesting and the
reconciling step is boring; each looks complete at the moment it's introduced. Full patterns,
instances, and diagnostics: `docs/technical/build_discipline.md`.

**1. Produced-but-not-consumed (the dangling wire).** Data is generated and written, and nothing
reads it back into the thing that needs it (unread `*_sensitivity_results.json`;
`kernel_grouping.json` not stamped into the `.pl` files). **Rule: a producer is not done until
something consumes its output** — wire the consumer in the same change or add a check that fails
loudly. **And consumed-once is not kept-fresh:** wire every new producer/consumer into
`python/run_pipeline.py` in dependency order and re-certify the whole transitive chain downstream
(`pipeline_output.json` → `enrich` → `enriched_pipeline.json` → `enhanced_report.py`); a
post-process the orchestrator never re-runs goes silently stale (the `w1_sheaf_join` artifact
froze at n=563 while the corpus grew to 772). Make freshness checkable: stamp the run manifest
into co-produced artifacts (the `orbit_data.manifest.json` sidecar) and assert same-run before
joining.

**2. One-canonical-thing-became-two (the silent fork).** A file gets copied to a scratch/test
location and edited; two versions exist with no queryable fact saying which is canonical (the
duplicated `generate_kernel_corpus.py`; the old ISSUES/AGENDA/PRIORITIES/TODO tracker sprawl).
**Rule: one canonical location per thing, and canonicity must be a checked fact (a path in docs,
a CI check), not a memory.** Prefer a branch over a test copy. Resolve found forks by evidence
(which path run-commands invoke, which imports resolve, git recency) — not preference — and
record the verdict in KNOWN_STATE.md.

**3. Destructive-replace without proof (the faith merge).** Before deleting, retiring, or
overwriting any script, sweep, data file, or generator that something relies on: run old and new,
paste both outputs, diff them, show identity or justify every difference in the same change.
"Structurally equivalent" is a code-read, not proof — the diff is proof. Consolidating N into one
is N separate old-vs-new diffs, each before its standalone is retired.

**4. Recap-as-witness substitution.** A turn-end recap or any "done / verified / working /
complete" in prose is a CLAIM; only the pasted output (diff, run, validation result, count)
discharges it. A turn reporting N edits done must paste N witnesses; M<N is the defect (observed:
"three edits witnessed" with only the third pasted). If a witness cannot be produced this turn,
label the item OPEN with its graduation step. The operator should read any done-claim lacking a
same-turn paste as unverified, regardless of the recap.

**5. Absence satisfies the gate (authored-zero vs absent).** A gate, threshold, or quantifier
passes because its input is *missing*, not because a condition was *checked*: `Count == 0` is
true when no facts exist; `forall(P, Q)` is vacuously true on an empty table; `V =< Ceil` passes
on a fabricated default. Instance: `natural_law_signature`'s `BeneficiaryCount == 0` reads
`intent_power_change`, empty corpus-wide — the 404 NL certifications mean "no beneficiary
*authored*," not "none *exists*" (OQ-43). **Rule: a gate over a possibly-empty table must
establish the datum was authored before it may pass — fail-closed on absence, not pass-open.**
Diagnostic: count the source predicate's facts on the corpus; 0 ⇒ the gate is a no-op.
Engine-wide audit: OQ-44.

**6. Success-shaped absorption (measured-empty vs didn't-look collapse at aggregation/channel
boundaries).** An aggregation or channel that cannot distinguish *measured-empty* from
*didn't-look* emits success-shaped output either way — and each component is individually sound,
so no site-level check catches it; the absorption lives where they compose. Three instances
witnessed in one day (2026-06-10): `system_gradient`'s `[] → 0.0` fallback (every gradient ever
computed had failed via a cut bug; the fallback made failure byte-identical to measured-flat for
the construct's whole life); `grep -v Warning` (a dead-module warning printed at every load for
four months into a universally filtered channel, then crashed the suite — OQ-96); findall-over-
partial-levels (an 8/32 one-level grid read as a full-system `increasing_coercion` verdict).
**Rule: aggregates carry their COVERAGE to the read site; channels carry ALLOWLISTS
(`python/load_warning_gate.py` is the template — never `grep -v Warning` over load output);
defaults-on-empty return `unknown`/OPEN, never a plausible value. Sufficiency is a property of
the QUESTION, not the dataset — fail-closed per-question (consumer-named requirements), not by
global fraction.** Full entry + diagnostics: `build_discipline.md` → Pattern 6; candidate-site
census: OQ-97.

The first two share a root: **the corpus/codebase you are building for is not the one on disk
now.** Build naming schemes, linkage rules, and reports correct for the corpus you intend
(thousands of stories, regeneration under schema change, found-article ingestion). A scheme that
*cannot* collide by construction beats one that *happens not to* collide today.

**The spine: every defect here is an absence that presents as a presence.** Something is missing
— a consumer, a canonical fact, a clause dispatch, an authored datum — and a success-shaped token
(the producer ran; both copies parse; a plausible `0.5`; the gate passed) fills the hole so the
read site can't tell it from the real thing. The single fix, everywhere: **carry the provenance
bit with the value so absence and success stop collapsing to one token at the read site.**
Concretely: wire-or-fail-loud, checked-canonicity, let the engine dispatch, return `unknown` not
`0.5`, fail-closed-on-absence.

**Diagnostics are not exempt — every probe needs a positive control.** An empty grep, a `findall`
of `[]`, a count of `0`, an "I found it nowhere" each can mean "nothing there" or "didn't
dispatch / queried wrong / never ran." This holds for *reasoning*, not just shell: "X appears
nowhere / is unique" is an unfalsified diagnostic until run against a case you know it must flag.
Full table: `docs/technical/build_discipline.md` → *The spine* and *Every diagnostic needs a
positive control*.

**Unwired ≠ worthless — judge a subsystem by its contribution, not its consumers.** Pattern 1 is a
*build-time* rule (finish the wire you create); it does NOT license the audit reflex of calling an
unwired or zero-firing subsystem cruft. "Has a consumer / is wired into `run_pipeline` / fires on
the corpus" answers *is it used*, not *is it useful* — the mechanical test every model reaches for,
and the wrong one for worth. Each subsystem (Boltzmann, FPN, the signature taxonomy, the trajectory
classifiers) was built to yield a specific analytical product; unwired = the *build* was left
unfinished, not the *idea* worthless. Adjudicate by value: (1) what product does it yield? (2) does
a live subsystem already yield it → **duplicate** (cruft); (3) else what would it add → **unique/
refinement** = *unfinished value*, **wire it or log it in `design_gaps.md`, never retire on wiring
grounds**. Only duplicate or yields-nothing-interpretable is genuine cruft. Liveness/firing/consumer
sweeps are evidence that *feeds* this adjudication, not the adjudication. Asymmetry: retiring
valuable-but-unwired silently destroys a capability; keeping a duplicate is mild clutter — when
unsure, preserve and adjudicate. Full version + instances (the 8 zero-firing signatures; the
`snapshot_type`/`degradation_chain` type-path vs `drift_trajectory` metric-series):
`docs/technical/build_discipline.md` → *Unwired ≠ worthless*.

**False-absence, false-unification, and the unguarded axis-swap — the over-confident moves on the
synthesis side; each owes a witness before it ships.** Distinct from the build defects: these are
errors of *claiming*. **(1) Before any "absent / can't / unrepresentable / no X," owe a positive
control** — grep a name you KNOW exists to prove the search fires, or construct the case it must flag —
else tag it **OPEN**, never a finding. Corollaries: the headline must carry the body's caveat ("proxy
only" in the body ≠ "solved" in the title); control the claim at its altitude (a probe over predicate
`f` licenses "absent in `f`," not "absent in the system"). **(2) Before merging two things that share a
vocabulary or dynamics, owe a distinction-check** — verify the architecture doesn't *mandate* their
separation, cite the ruling, before folding (observer/committer: `deferential_realism_paper_v7.md`
Theorem 7 forbids the fold; shared dynamics across distinct objects is analogy, not a bridge;
"scaffold = {splice,replace}" is a cross-metaphor weld). **(3) Introducing or relabeling an axis owes a
PRE-REGISTERED discriminating control** — construct the case where the new axis and the nearest prior
axis come apart, write what each outcome means *before* the run, then run it (the DMV: designed +
*uncaptured* → snare falsified the design axis, established capture; pre-registered so it couldn't be
narrated into agreement). Reaches the layer (1)/(2) don't — a quiet axis-relabel writes no file wrong
and feels like no ruling. Standing risk, stated as such: the discriminating case has so far come from
*outside* the loop; generating it for one's own synthesis before it lands is the unmet job. Under-claim:
one witness earns "separable / prior label wrong here," not "orthogonal everywhere." All three collapse
a distinction the material holds open; fix = name the witness (firing probe / separation ruling /
pre-registered discriminating control) and produce it or stay OPEN. **(4) The under-confident dual —
hedging-as-rigor: "held open / both readings possible" is earned only when no falsifier can be
specified; if a kill condition is available, COMMIT and attach the kill condition** (prose commits,
uncertainty lives in the falsification apparatus). Generation-time trigger: drafting a
both-readings passage IS the cue to check whether commitment-plus-falsifier is available — don't
wait for review to catch the flinch. **(5) When to stop verifying — this is the Omega framework's
structural-convergence rule (`docs/omega_variables.md`), not a new one. "Verified enough" is a seat
with no floor (Seat-Theorem §8), so the stopping rule is not "stop when verified" but "stop when the
next pass costs more than being wrong, AND every open is DECLARED not concealed." The second clause
is the only checkable one: for each verdict/name you (or the engine) emit, name a tier-available
falsifier or downgrade it to OPEN — which is "route it to a typed Ω", and TYPE IT AGAINST
`omega_variables.md`'s definitions, not loosely: an *orientation* gloss (enclosure vs defense) is a
**deferred Ω_E** — a fact about the actor's stance you can't yet witness, resolved by longitudinal
world-observation (the OQ-133 confrontation-response signature), NOT an Ω_P; calling it Ω_P would let
the actor self-certify its own orientation by fiat (the concealment move blessed by the routing). A
genuinely *contested origin* IS Ω_P/Ω_C (legitimate cross-stakeholder dispute; engine abstains) — do
not collapse the two: same surface OPEN, opposite type (route-to-deferred-measurement vs
abstain-as-preference). An undefined *tier-boundary* is Ω_C. A name that claims more than its tier can
witness (an orientation gloss on a structural mismatch; a trajectory word on a synchronic read) is a
concealed open, not a finding. Pass-count is not the
variable; whether the stopping point is declared is. One pass with opens marked beats eight that ship
a confident gloss over the unwitnessable.** Full version: `docs/technical/build_discipline.md` →
*Over-confident moves on the synthesis side* and *When to stop verifying*.

## Critical Distinctions

**The live corpus is the post-de-leak REBUILD (reset 2026-06-05) — never cite a memorized count;
cite the pipeline manifest** (`n_constraints`). Archived corpora (`prolog/archives/datasets/`,
used only for retrospective audits that explicitly overlay `corpus_path`): `kernel_v1/` is the
1,106-story pre-reset corpus (incl. the `stage1_probe`/`flatctl_probe`/`lineage_probe_01`
run-tags; ALL pre-2026-06-05 empirical findings — OQ-70 FNL stats, OQ-71 lineage, the 55%
coordination disagreement — were measured on it or its ancestors); `original_v6/` is the
3,380-story chimera-era corpus (ID reuse across runs, OQ-25 / v7 §5.11 — do not cite 3,337 as a
live count); `original_v5/` is its 702-story predecessor (same chimera-era caveats);
`testsets_sotu/` holds the 189 SOTU constraints (`sotu/` itself contains json/pl/raw subdirs, not
flat .pl files — overlay `testsets_sotu` for sotu analyses; run_pipeline reports n_sotu=0 now).
While the live corpus is small post-reset, these archives are the breadth option for legacy-side
sweeps via `corpus_path` overlay (the OQ-89 pattern; ~5,200 stories across kernel_v1 + v5 + v6) —
all counts here are file counts verified on disk 2026-06-10.

**FNL prevalence is bait-confounded — do not cite it (or the FNL-driven tangled_rope dominance)
as a detection result (OQ-70).** All FNL firings ride `claimed_natural/2` source 2, which reads
ANY single authored mountain perspective as a naturality claim — a generation-template convention
copied from the one-shot example (`agent/verification_bottleneck.json`, "ANALYTICAL OBSERVER /
NATURAL LAW VIEW (MOUNTAIN)"). Counterfactual witnessed 2026-06-04: retracting the template
perspectives migrates FNL→FCR almost wholesale (FCR's `appears_as_rope` source 2 is the same gate
pattern), zero mass landing in genuine natural_law/CI_rope. Until OQ-70 is ruled,
signature-prevalence statistics measure authoring convention. Also: pipeline outputs from runs
BEFORE 2026-06-04 carry ONE non-corpus per_constraint entry (`catholic_church_1200`, an engine
demo from `constraint_instances.pl`) — exclude it, or corpus counts run one high vs the manifest.
Runs from 2026-06-04 on enumerate `corpus_loader:corpus_constraint/1` and match the manifest;
check `manifest.pipeline_run_at` to know which regime an output is in.

**A kernel-positive means "admits a foundational construction," dominance UNJUDGED — never "this
topic IS a dominant/certified kernel" (kernel-first router, 2026-06-06; OQ-79).**
`c-orchestrator._step_decompose` uses the PRIMED scope prompt (asks the kernel question on every
topic; reuses gkc `_scope_user_prompt` — do not revert to the unprimed §3 prompt, it silently
re-flattens genuine kernels). The primed `is_contested_kernel` verdict is **kernel-liberal**:
kernel whenever a foundational reading is *constructible* (= the topic is contentful,
`docs/seat-theorem-v1.md` Coupling Theorem), flat only when the situation settles it — so loud
means-disputes (nuclear-as-climate-solution, reading-wars) route kernel alongside genuine
kernels, and the kernel set accrues UNCURATED for dominance (operator ruling LIBERAL,
2026-06-06). **Do not cite a kernel count, Tier headline, or essay as "the corpus contains N
genuine axiom-level contests"** — that asserts a seat-free dominance ranking the seat theorem
forbids (§6). Report kernel-positives as "topics admitting a foundational construction." A
*seated* dominance stage is permitted but deferred. Provenance + the eyeball's role (liveness,
not dominance): `outputs/kernel_first_phase0/PHASE0_READOUT.md`, KNOWN_STATE 2026-06-06.

**`json/` files are LLM-generated constraint specifications, not analysis output.** Written by
orchestrator step 3 alongside the matching `prolog/testsets/` file; `run_pipeline.py` reads them,
never writes them. Analysis output lives in `outputs/`.

**Canonical framework paper: `docs/deferential_realism_paper_v7.md`.** v7 promotes the committer
axis to a co-equal second axis: Axiom 7 (authored commitment structure with computed
consequence), Theorem 7 (detection independence), Theorem 8 (licensed plurality vs. real
closure), §4.5 (two-axis engine), §5.11 (trifurcation profile). Axioms 1–6 and Theorems 1–4
unchanged from v6.13. `docs/deferential_realism_paper_v6.13.1.md` is a parallel observer-only
amendment (Axiom 2 for OQ-26: ε is reading-relative across generation runs). Use v7.

**Formal classification rules: `docs/logic.md`.** The spec document; `config.pl` must match it.
(UTF-8 repaired Feb 2026; the Edit tool fails on multi-byte mojibake — use sed or Python.)

**`dr_type_at/4` and `classify_snapshot/3` were replaced (2026-05-17)** — both used the legacy
`power_modifier` χ path (χ = ε × π, omitting σ). Replacements: `classify_at_time/4`
(`drl_composition.pl`) and `snapshot_type/3` (`transition_paths.pl`) on the canonical sigmoid
pipeline (χ = ε × f(d) × σ(S)). Callers `constraint_history/3` and `degradation_chain/3` updated.

**`site_contexts_product/1` scope exclusion is calibration-based.** The product site excludes
`regional`, `continental`, `universal` (`constraint_indexing.pl:954–955`): those scope atoms
appear in no canonical context and their scope_modifier values are unvalidated. σ(universal) =
σ(national) = 1.0 (`config.pl:117,120`) — no differential χ effect.

**Generation is stochastic; the committed story is the determinism frontier (operator
ruling, 2026-06-12).** LLM generation NEVER reproduces: the same material re-run gets
different scopings, kernel namings, readings, and ε — witnessed in-repo: OQ-26 (ε is
generated, not observer-invariant; Axiom 2 was AMENDED for this, v6.13.1), the
press/Reformation topic generating THREE kernel namings across three runs (9 files,
kernel_v1), the naming-drift never-generated siblings (KNOWN_STATE 2026-06-03). Do not
design, test, or reason as if same-prompt → same-story; backchecking a generation tells
you nothing about the next run; re-generated stories are NEW DRAWS, never re-measurements
— cross-run "same story" identity does not exist. What IS deterministic: the committed
JSON onward — and that boundary is CHECKED, not assumed (hash inputs + manifest + output;
same-code reruns witnessed byte-identical at single commits, but order-dependency is on
record as the OQ-112 class). Three mechanisms produce "same material, different results"
— generation stochasticity, ensemble refit (corpus-relative statistics; 3 signature
changes refit 57 stories), pipeline non-determinism at fixed input (a bug) — attribute by
the stage-hash diff, never by assumption: "it's the LLM" without the diff is a hypothesis
sitting where a witness goes. Meta-analysis claims ride snapshot manifests + per-story
provenance (model, sampling params, prompt/schema/example commits — schema-required).
**The typing machinery (fingerprint/orbit/Boltzmann) carries KIND-level identity only:**
`seeded_from` (authored at generation time) is provenance/coordination plumbing, never
identity recovered backward by signature matching — witnessed both directions on the
naming-drift triple (one same-material draw shared nothing positive with its siblings; a
cross-topic pair matched 6/7; `audits/2026-06-12_signature_identity_witness/`). Never key
an exclusion list, named re-witness, or any per-story mechanism on names or signatures
across a regen boundary. **And a category shift on redraw is the mechanism WORKING, not
identity decaying (operator ruling, 2026-06-12; `docs/seat-theorem-v1.md`):** verdicts
are seat-indexed, a redraw occupies a new seat, and a classification that could not
shift would be contentless (Coupling Theorem) — determinism-as-desideratum is part of
the problem. The analysis product is the SHAPE (clusters, shifts, connections — judged
by the hypotheses they generate, not by draw-invariant truth); read the replicate
probe's stability table as an empirical σ/seat partition (draw-stable ≈ situation-fixed;
draw-unstable ≈ seat-expressive — its distribution is analysis substrate), not a noise
filter. What Corollary 3 keeps: pre-committed confrontations still bite — the witness
discipline is untouched, only the determinism valence was wrong.

**Pre-computed values live in `outputs/pipeline_output.json`** (H¹, Arakelov heights, MaxEnt
distributions, classifications). Read from there; do not recompute from scratch.

## Math Employed in the Prolog Engine

Sigmoid/logistic, exponential, least-squares slope, monotonicity tests, drift velocity and
acceleration; Shannon entropy (normalized H/log N), MaxEnt, Gaussian log-likelihood, log-sum-exp,
KL divergence (threshold), Boltzmann distribution / partition function; weighted sums, clamping
to [0,1], fold accumulation, sum/mean/min/max; dual-threshold classification (χ AND ε), priority
cascade (mountain > piton(dead-coordination) > snare > scaffold > rope > tangled_rope >
piton(fallback) > naturalized > unknown), entropy-weighted thresholds; power scaling χ = ε × f(d)
× σ(S), power modifier π(P), scope modifier σ(S), cognitive displacement δ; purity/contamination
algebra (Gaussian inverted-U exp(−(x−μ)²/2σ²), purity degradation max(0, intrinsic −
contamination × immunity), type immunity/susceptibility coefficients); graph BFS, network purity,
contamination cascade, edge coupling; Jacobi iteration and greatest-fixed-point convergence
(monotone endofunctor — all in `drl_fpn.pl`); presheaf evaluation across observer contexts,
site/coverage structure, logical fingerprint equivalence, lattice meet/join; modal operators as
composition rules, Boltzmann factorization test.

Key files: `drl_core.pl`, `constraint_indexing.pl`, `drl_boltzmann_analysis.pl`,
`boltzmann_compliance.pl`, `drl_purity_network.pl`, `drl_fpn.pl`, `maxent_classifier.pl`,
`drl_composition.pl`, `logical_fingerprint.pl`.

## End-of-Session Documentation Review

When work changes code, produces empirical findings, or resolves an OQ — PR or not — review and
offer updates (as a diff or edit proposal, not a verbal summary) to:

- **KNOWN_STATE.md** — the dated session log; default destination for new findings, NOT
  CLAUDE.md. Then apply the **promotion test** per new entry: *"Would a fresh agent who never
  read KNOWN_STATE.md make a concrete, **silent** mistake before editing file X (or running
  command Y)?"* If yes — and the warning is stable and not already in an always-loaded section —
  **promote the tripwire** into the relevant CLAUDE.md section (Architecture Invariants |
  Critical Distinctions | Corpus Loading | Running the System), leaving full provenance in
  KNOWN_STATE.md. Loud failures (an immediate error) are NOT silent → they stay history. Bias to
  history; over-promotion defeats the token-saving purpose. Classify and promote as two passes,
  not one — a misclassification that moves a warning out of context fails silently.
- **AGENTS.md** — changes to architecture, testing commands, naming conventions, or invariants a
  future agent needs before touching the relevant files.
- **ISSUES.md** — status changes (open → mitigated → resolved), new OQs, updated evidence.
  **ISSUES.md is the SINGLE tracking surface** (status grammar + checker in its footer); work
  packages, priorities, and backlog live there as OQs (research-frontier backlog: OQ-69).
  AGENDA.md, AUDIT.md, TODO.md, PRIORITIES.md were consolidated into it and deleted (2026-06-04)
  — do not recreate parallel trackers (Build Discipline Pattern 2).

**Git autonomy (operator ruling, 2026-06-09).** Standing permission to commit without asking:
when a coherent unit of work is witnessed, commit it then — do not batch a session into one
end-commit; in-flight work is what compaction and harness outages destroy (full rationale:
`docs/technical/build_discipline.md` → *Commit-as-you-go*). Everything here is CC0; the operator
values iteration over correctness; mistakes are recoverable. Push is also permitted once the
pre-push check below passes. **Start your own worktree before write work that is costly to
unwind — code changes (`prolog/*.pl`, `python/`, generators, schema) or any multi-file/
hard-to-rollback edit** (`git worktree add ../wt-<task> <branch>`, or the harness's worktree
mechanism), **not only when you know another instance is running**: siblings are undetectable
from inside a session (no lock, no registry; a clean `git status` looks identical either way),
so insulation is the default for substantive work, not a response to detection. Two instances
editing one working tree step on each other's uncommitted state — witnessed 2026-06-10, when two
instances each misread this rule as conditional and collided in the main tree (operator
clarification of intent, same day). **Exception — minor text edits left to instance judgment
(operator ruling, 2026-06-18):** a single- or few-line change to `ISSUES.md`, `KNOWN_STATE.md`,
`AGENTS.md`, or other prose/docs does NOT require a worktree — the collision blast radius is
small and a git rollback is cheaper than re-spinning a 27k-file checkout. Edit main directly,
commit promptly to shrink the uncommitted window. If it is unclear whether a change crosses into
"costly to unwind" (e.g. a doc edit entangled with code, or a large multi-file doc refactor),
ASK rather than guess. Merge
the worktree branch back to main when its unit of work is witnessed; run
`python3 python/issues_status.py --check` after any merge touching ISSUES.md (it fails on
duplicate OQ labels — the clean-automerge artifact of two worktrees claiming the same next
OQ-NN). Multi-writer corollary: a commit's witness is its DIFF or an entry-anchored check, never
a global count delta — counts aggregate other writers' work and are no longer attributable
(build_discipline → *Count-as-witness assumes a single writer*).

**Before any `git push`:** verify the three files above are current with respect to the changes
being pushed.

**Done includes the next step, landed in substrate — not stated in chat.** A task is complete
only when a fresh instance, reading only the repo, could pick up the next forward move. If the
session surfaced a next step, a sequencing constraint, or a fact living only in conversation,
write it where the cold read will find it: the relevant OQ in ISSUES.md (including
ordering/sequencing notes) or a comment at the code it concerns — with why-this-one and what it's
gated behind, since the bare next-step is what a cold reader most easily gets wrong. A next step
spoken in chat and not written to substrate is a handoff that did not happen.

## Memory Consolidation Review

The auto-memory directory (`~/.claude/projects/-home-scott-bin-structural-dynamics-model/memory/`)
accretes one file per session finding. Periodically consolidate: merge same-principle clusters,
delete memories since promoted into repo docs, trim MEMORY.md to one-line index entries, prune
resolved/stale items. The same monthly pass covers:

- **KNOWN_STATE.md roll-off** — entries older than ~30 days get the promotion test once more,
  then compress in place per the roll-off rule in its header. Verify with
  `python3 python/known_state_status.py --check`.
- **ISSUES.md compress-on-close check** — sweep for closed entries that escaped the footer rule's
  at-close compression.

**Last review: 2026-06-04. Interval: monthly.** If today is on or after **2026-07-04**, prompt
the user to run a consolidation pass before starting the requested work, then update both dates.

## Audit Methodology

Audits follow **recon → proposal → execution → writeup**: recon establishes what data exists and
what is answerable; proposal states exactly what will run and what would constitute each verdict;
execution runs scripts and saves raw output; writeup analyzes from evidence only — never
documentation restated as findings. Detail: `docs/project_orientation.md` §8.1.

**Location mandate (2026-06-04): every audit lives in `audits/<YYYY-MM-DD>_<slug>/`** — one
subdirectory per audit, writeup AND evidence artifacts together; date = execution date. Not in
`docs/`; not only in `outputs/` (gitignored = gone on a fresh clone). Audit scripts stay in
`python/audits/` writing to `outputs/`; on completion, writeup + evidence move into the dated
subdirectory. Conventions and consolidation map: `audits/README.md`.

## Cross-Sibling Disambiguation (standard practice)

When a per-item call about an authored field is ambiguous in one file (agent vs vindicated
proposition, epistemic vs structural omega, gain-flow vs framing), check the siblings BEFORE
escalating as undecidable: same-kernel readings and same-topic sibling kernels (`cs_kernel_id`
groups, `cs_reading_relation` edges, name-prefix families) often foreground the same structure
differently. Footing rule: the cross-sibling read **generates** the hypothesis; only an
**in-file witness** (the file's own gain/directionality text) makes it **ruled** — across
distinct kernels the transfer is analogical, so analogy alone = mark the call INFERRED.
Value-name morphology is orthogonal to referent kind in both directions (OQ-64) — classify by
referent text, never the value string; suffix heuristics have lied twice. Method + instance:
`docs/technical/build_discipline.md` → "Cross-sibling comparison disambiguates authored-field
calls"; theory: `docs/the_perturbation_principle.md` §5.1.
