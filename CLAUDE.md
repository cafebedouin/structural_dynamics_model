## How the operator works (read first)

- **One writer at a time; plans are staged.** The operator runs a planning phase (Claude-web /
  Ultra-plan, several rounds), hands each plan to a fresh instance, and keeps 2–3 staged while one
  implements — instances may coexist, but only one *writes*. Concurrent writing was tried and
  dropped because the shared `ISSUES.md`/`KNOWN_STATE.md` collide across instances **even in
  worktrees** (a worktree isolates code, not these trackers). So work directly on `main`,
  commit-as-you-go, branch for risky/multi-file code, and start a worktree ONLY if asked (detail:
  *Git autonomy*).
- **Iteration over correctness; everything is CC0; mistakes are recoverable.** Bias to action:
  fix simple errors on sight and prefer fixing to documenting (*Fix simple errors*); flag a
  better way in one sentence, then proceed (*One-sentence flag*). Reserve the *ask* for genuine
  rulings (the operator's seat) and above-threshold changes.
- **Lean docs.** Keep this file and the doc set tight: carry the tripwire + the pointer, not the
  full exposition (linked docs hold the detail). Over-promotion defeats the purpose.
- **[EDGE] convention (operator, 2026-07-12).** Mark a paragraph `[EDGE]` when there's something
  you're tempted to soften or omit — say it there instead; the operator may also ask for your
  `[EDGE]` take directly. A candor marker, not an activation token.
- **But never trade away the witness.** The bias to action does not loosen *Paste-or-untag* below.

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
- **Paste-or-untag — prose (turn summaries, commit messages included) carries the same witness
  burden as code.** Every done/verified/fixed claim and every count, total, or gate figure cites
  its same-turn witness — pasted run/diff, or artifact path plus what in it shows the claim,
  never memory — or is marked `(recalled)` / OPEN with its graduation step. A count that moved
  by exactly the size of what you just added is a count you didn't take — re-derive it.

## One-sentence flag

**If you know a better way:** when a request has a cleaner implementation, a simpler approach, or
an unintended consequence the user likely did not see, say so in one sentence before doing the
work — then proceed. Do not withhold action pending approval.

## Fix simple errors — fixing beats documenting (operator ruling, 2026-06-18)

**Standing permission to fix a simple, clear error the moment you find it — ALWAYS in scope, no
ask needed**, even when it isn't the assigned task. When a defect can be *fixed* rather than
*documented*, fix it (a parser that mis-splits, an off-by-one, a wrong qualifier, a format
footgun): removing the sharp edge beats turning it into a warning comment. Documenting is the
fallback for when you *can't* fix — it needs a ruling, the fix is large, or the cause is unconfirmed.

**Threshold (calibrated to the `blocked_on_human` comma fix, commit `3f7dc026`):** self-contained
(~single-file core), correctness witnessable *this turn* (an existing test/gate or one control you
add), single-revert reversible, no judgment that is genuinely the user's. That fix (≈70 lines + a
new selftest control + a doc bump, behavior-preserving, gate GREEN) is the upper end of "just do
it." Discipline still rides on top: **witness the fix** (paste-or-untag), keep **output-changing
split from behavior-preserving**, and **escalate genuine rulings**. Above the threshold (multi-file
refactor, engine behavior change, anything needing a ruling) — flag in one sentence or ask first.

## Context Window and File Size Constraints

If a file or task is large enough that context limits would affect your ability to work with it
cleanly, **prompt the user with how you'd like it resolved** before proceeding (e.g. "This file is
2,000+ lines. I can read it in chunks, or you could split it. What's best?"). Do not silently work
around context constraints with suboptimal approaches — make the constraint visible; the user may
know what matters most.

## Project Context

**What this repo is:** the Deferential Realism (DR) research project — a philosophical
framework plus the working engine that makes it falsifiable. Two axes (observer: `dr_type`
over four positions; committer: `cs_*` kernels/readings/drift) under the v8
seat/gauge/orientation ontology: one content-seat audited against, many gauge positions over
it, orientation as the audited face — one-directional audit, machine-enforced by the
axis-boundary taint guard in the gate. The Prolog engine classifies LLM-authored constraint
stories (six types via χ = ε × f(d) × σ(S) + the signature layer); the Python layer
orchestrates, reports, and gates; essays come out the other end (cafebedouin.org). Entry-point
paper: **`docs/deferential_realism_paper_v8.md`** (its closing Appendix states the current
state plainly); repo tour: `README.md`.

Prolog+Python research infrastructure: ~125 Prolog modules, ~120 Python analysis scripts (as
of 2026-07-02). The live corpus (`prolog/testsets/`) was RESET 2026-06-05 and is
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

**The `[NEXT]` activation — "what should I work on?"** On **`[NEXT]`** (or any what-next /
sequencing ask), **run `python3 python/omega_resolver.py menu`** and present its **WORKABLE NOW**
list (sorted by authored `Priority:`, 1=highest) plus the **BLOCKED ON YOU** items needing a
ruling/spend-go. **Do NOT read `ISSUES.md` whole** (7,300+ lines — whole-reads produce faked
queries and prose-guessed Deps, a witnessed failure mode). The menu is the queryable surface:
reachability over the authored `**Deps:**` edges (relators `blocked_on`/`gates`/`bundled_with`/
`splits_from`/`blocked_on_human`/`blocked_on_condition` — the last routes a not-ripe ruling to
BLOCKED with a named mechanical watcher, never back to BLOCKED-ON-YOU) on an SCC condensation,
surfacing the authored `**Priority:**`
(*the operator's declared seat — surfaced, never computed*). Its coverage footer states frontier
trust (how many active OQs have authored Deps); edge-free OQs default `workable_now` and may
overstate. When you mint or touch an OQ, author its `**Priority:**` (and `**Deps:**`) so the
frontier stays honest. **To route an ARBITRARY question to its 2–3 OQs** (distinct from "what's
next"), scan the derived router `issues/INDEX.md` then `grep OQ-NN ISSUES.md` — the supported
alternative to a whole-read. The router is GENERATED (`omega_resolver.py index`; JSON twin
`issues/INDEX.json`), never authoritative; **regenerate after ANY `ISSUES.md` edit** — `omega index
--check` in `scripts/gate.sh` turns `[GATE]` red on a stale index. `omega_resolver.py check` is the
authority gate (dangling/malformed Deps, rotted witnesses); `selftest` runs the planted-fixture positive controls and prints its own DERIVED count (do not publish a literal here — it drifted for 67 days).
**Read `docs/technical/omega_resolver.md` before modifying `omega_resolver.py`, the `ISSUES.md`
authored fields, or the hooks** — it carries the command table, the SCC model, the
determinism-boundary "floor" (priority/type stay a declared seat), and the gotchas.

**More activations** (same convention — recognize the exact bracketed token and act):
- **`[GATE]`** — run `./scripts/gate.sh` and report the green/red summary (issues_status,
  omega_resolver check + selftest + index, known_state). Do not fix anything; just report. Use before
  committing anything that touches `ISSUES.md`/`KNOWN_STATE.md`.
- **`[PUSH]`** — the pre-push ritual: (1) run `[GATE]` — must be GREEN; (2) confirm
  `ISSUES.md`, `KNOWN_STATE.md`, and `AGENTS.md` reflect the changes being pushed (the
  documentation-currency check); (3) if both hold, `git push origin main`, else report what is
  blocking. Always show the gate output as the witness — never push on an unverified claim.

**Hooks are versioned project apparatus — `.claude/settings.json`, `.claude/agents/` and
`.claude/skills/` are ALL COMMITTED; the rest of `.claude/` is machine-local** (`.gitignore` uses
`.claude/*` + three negations — the bare `.claude/` form makes a negation silently inert). Files
under `agents/`/`skills/` are git-witnessable and a gate row over one is legitimate (OQ-337). Three
hooks fire on their own: `SessionStart` prints the activation menu; `PostToolUse` re-runs the
`ISSUES.md` grammar gate when you edit that file; `PreToolUse` injects the KNOWN_STATE
`tripwire`/`correction-key` entries naming any file you `Edit`/`Write`
(`python/pretooluse_tripwires.py`). **Two consequences.** (1) A KNOWN_STATE `Tier:` is a ROUTING
decision — a standing warning filed as `landed` is never delivered to an editor. (2) **The
PreToolUse matcher is `Edit|Write` ONLY, so a Bash-driven edit (`sed`, heredoc, script — what auto
mode instructs) NEVER fires it, and that silence is indistinguishable from "queried, matched
nothing."** Before editing `ISSUES.md`, `KNOWN_STATE.md` or a high-traffic engine file through
Bash, run `python3 python/known_state_status.py --file <path>` yourself. (The one near-miss, OQ-329,
came out clean by luck, not by a control — do not cite it as evidence the channel works; ruling
*build the hook's call-witness first, rule coverage second*: OQ-330, KNOWN_STATE 2026-08-20.) To
add or change a hook, edit `.claude/settings.json` and commit it with the change it serves.

**Implementation wiring notes:** `docs/technical/` holds file-level notes on non-obvious wiring,
operator-precedence bugs, fact-adapter patterns, and query gotchas — things that caused real bugs,
not general architecture. Read the relevant file before modifying `config_validation.pl`,
`cs_kernel_registry.pl`, or the CS fact schema. Read `swipl_load_path_and_probe_gotchas.md` before
diagnosing module-resolution behavior (REPL ≠ pipeline for wrong-qualifier calls), writing
test-local predicate swaps or retract/re-assert probes, writing goal-TEMPLATE probes
(`Key-m:g(...)` / `V^m:g(...)` parse wrong — `:` is priority 600 — and the probe passes
VACUOUSLY, §13), running an in-session corpus sweep or
overlay counterfactual (validated signature-sweep recipe; Boltzmann memo caches read stale unless
cleared), or interpreting a pipeline_output.json diff. `build_discipline.md` documents the
defect patterns (summarized in Build Discipline below — **seven members at eight indices, index 3
vacated 2026-08-11 and never reused**; both documents publish the same member at every index since
OQ-278 closed 2026-08-17, gate-checked per index) with diagnostics; it also carries the
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
Within-pipeline parallelism (the parallel Phase-2 analyses in one `run_pipeline`) is fine — the
rule is one pipeline at a time. **One caveat (OQ-182, 2026-06-27):** the two O(N²) memory-heavy
stages — `trajectory` (HAC clustering, enabled by `trajectory_enabled=1`) and `giant_comp` — must
NOT run concurrently (co-residency intermittently stalled the pipeline). `trajectory` is therefore
pulled out of the parallel Phase-2 set and run **sequentially after** the parallel Prolog block
(`run_pipeline.py` `_phase_prolog`); the 11 remaining real stages stay parallel. Keep it that way —
do not fold `trajectory` back into the parallel `tasks` list.

- **The canonical interpreter is `.venv/bin/python`, not system `python3` (2026-08-18).** The
  2026-08-18 OS upgrade moved system Python 3.10→3.12 and stranded every pip package, so bare
  `python3 python/foo.py` now fails on ~20 tools (`scipy` ×15, `anthropic` ×14, `numpy` ×16, …).
  Activate the venv, or prefix `.venv/bin/python`. `scripts/gate.sh` resolves this itself
  (`$SDM_PYTHON` → `.venv/bin/python` → `python3`) and its FIRST row, `python env`, asserts the
  running interpreter can import what the repo imports — **read that row before believing any
  other red**, and never `pip install` into system python to "fix" a ModuleNotFoundError
  (that fragments the environment; install into `.venv`). New Python subprocess call sites use
  `sys.executable`, never the string `"python3"` — a literal handed the child the empty
  interpreter at three sites. Detail: KNOWN_STATE 2026-08-18.
- **Discover/run any python tool:** `python3 python/cli.py list` (grouped tree of every tool with
  one-line summaries) / `python3 python/cli.py <group> <name> [args]` (run; argv forwarded verbatim).
  Single subprocess pass-through dispatcher (OQ-163); no file moves — grouping is in the command tree,
  not the directory layout.
- Full pipeline (analysis only, no generation): `python3 python/run_pipeline.py`
  - `run_pipeline()` opens with the **ISSUES.md status-grammar gate** (`issues_status.scan()`,
    aborts naming malformed entries). **Do not remove or bypass the gate — it is NOT dead code,
    including during refactors of run_pipeline.py.** If it fires, fix ISSUES.md until
    `python3 python/issues_status.py --check` passes. Grammar is in the ISSUES.md footer.
  - `_phase_prolog` opens with the **OQ-137 reading-totality gate** (the registry-driven plunit
    suite `tests/test_reading_totality.pl` as a sequential fail-fast step before the parallel
    set — commentary_census presumes exactly the totality it proves). Also NOT dead code — do
    not remove or fold into the parallel tasks. If red: a registered reading stopped being
    exactly-one on its declared domain — fix the reading to a typed token
    (`design_discipline.md` §5) or correct its `prolog/reading_registry.pl` entry. **When you
    ADD a reading predicate an aggregate could consume, register it there in the same change** —
    registration is opt-in; an unregistered reading escapes the guard silently.
- **Testing an ENGINE change: exercise it across ALL the corpora, not just `testsets/`.** The live
  `testsets/` is a deliberately sparse singleton (~small N) and will NOT exercise every branch or
  surface corpus-sensitive behavior — an engine change "witnessed" only there is under-witnessed.
  Run it against all five live legs (`testsets/`, `testsets_haiku/`, `testsets_flash/`,
  `testsets_kimi/`, `testsets_sonnet/`) AND the
  breadth archive `archives/datasets/kernel_v1/` (~1,106 stories) by overlaying `corpus_path` (use
  `asserta`, not plain `assertz` — see Corpus Loading). Corpus *content* changes are testset-local;
  *engine* changes are not. (Witnessed why it matters: OQ-178/OQ-51 `cs_kernel_divergence` behavior
  depends on the corpus's reading-sets and `unknown` distribution — a 97-story corpus leaves branches
  unexercised.) **To get a full manifest-bearing `per_constraint` output for a non-default corpus
  WITHOUT overwriting the canonical `pipeline_output.json`, use `classify_corpus(corpus_path,
  output_name, expected_model)` (`run_pipeline.py:368`)** — gate-free json_report path with built-in
  refusals (zero-glob, load-completeness, single-model fingerprint, raw freshness); serialize calls
  (they share the raw artifact). Precedent: kernel_v1/original_v6/twins, 2026-07-01–02. **When
  citing counts across such runs, name BOTH the corpus and the code state** — "HEAD yields N" is
  ambiguous between engine-regime and corpus and has already caused a misread (KNOWN_STATE 2026-07-02).
- **A witness diff-pair is only valid over a FROZEN corpus:** operator topic runs land stories
  mid-session (witnessed 2×, 2026-07-23) — md5-fingerprint each corpus leg around BOTH halves of a
  clean-vs-edited pair, and serialize behind any running `c-orchestrator` (gotchas §5).
- **A pipeline-diff is a valid behavior-preservation witness ONLY if the run rewrote the file.**
  `run_pipeline.py` aborts (non-zero) on its gates BEFORE writing `outputs/pipeline_output.json`, so
  a before/after diff then compares the baseline against *itself* and reads byte-identical — a false
  pass (witnessed 2026-06-24: a `*/`-in-comment syntax error aborted the gate; the diff read
  "identical" on a stale file). Confirm **exit 0 AND output mtime advanced** before trusting it. The
  diff is byte-identical only at `per_constraint` — the manifest re-stamps `pipeline_run_at` every
  run, so a whole-file cross-run diff always differs even when behavior is preserved (normalize it, or
  use a same-session clean-vs-edited diff, never a prior-run baseline). Detail:
  `swipl_load_path_and_probe_gotchas.md` §5.
- Prolog tests (corpus validation): `cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"`
- Prolog unit tests (engine): `cd prolog && swipl -g "[stack], [tests/test_snapshot_migration], run_tests, halt" -t "halt(1)"` — substitute any file in `prolog/tests/` (except `test_battery_variants.pl`, a variant harness, not a plunit test; and `test_cs_pattern_detection.pl`, a custom harness whose runner **exits 0 even when red** and is currently 13/37 red on fixture rot — OQ-266. Never cite that suite, or its exit code, as a witness until OQ-266 lands).
- **CS spec-vs-code enumerations are gate-tripwired (v6, 2026-08-06):** the 8 enumerations in `docs/commitment_systems/commitment_systems_sketch_v6.md` (drift terminals, gap directions/magnitudes, pattern atoms, verdict atoms, obstruction statuses, trifurcation types, attractor table) are pinned to code by `python/spec_enum_check.py` in `scripts/gate.sh` — editing either side alone turns the gate RED; update the other side in the same change. **The guard is opt-in: a NEW CS enumeration is unguarded until its sentinel block AND checker-manifest entry land in the same change** (same silent-escape shape as reading-registry registration above). Pins, extraction assumptions, and failure semantics: `docs/technical/spec_enum_check.md`.
  **`[stack]` is NOT a sufficient load chain for every suite, and a short chain fails LOUD but
  MISLEADINGLY — the suite reports test failures, not a load error, so the natural read is
  "regression" (witnessed 2026-07-25: 7 spurious `reading_totality` failures under `[stack]`).
  Each suite's correct chain is in its own file header — read it first.** Known extended chains:
  `test_reading_totality` → `-l stack.pl -l reading_registry.pl -l commentary_census.pl` **plus**
  `corpus_loader:load_all_testsets` in the goal (this is exactly what `run_pipeline.py`'s OQ-137
  gate runs); `test_purity_bands` → `-l stack.pl -l fpn_report.pl -l giant_component_analysis.pl`;
  `test_purity_absence` → the 8-module pipeline chain in its header.
- **A plain `[stack]` + corpus load leaves MAXENT UNFITTED, and MaxEnt reads FAIL SOFT (OQ-66,
  2026-07-25).** `maxent_dist/3` and `maxent_run_info/3` are empty, and `maxent_entropy/3` /
  `maxent_top_type/3` **fail rather than throw** — so `catch/3` around them does NOT intercept,
  and any probe or suite reading MaxEnt observables under `[stack]` alone measures NOTHING. Map
  that soft failure to a placeholder (`no_top`, `0.0`) and the nothing becomes indistinguishable
  from a result: this is exactly how the OQ-66 guard compared `[no_top,…]` against itself for its
  whole life and reported zero-diff. **Refit explicitly — `maxent_cleanup, maxent_multi_run(Ctxs, _)`
  — then ASSERT `maxent_dist/3` non-empty before any read.** MaxEnt is corpus-fitted state
  deliberately OUTSIDE `cache_registry`, so `clear_all_caches/0` is not a refit. Template:
  `audits/2026-07-25_oq66_nlwb_filter_cutover/nlwb_diff_harness.pl`.
- Stack consistency check (OQ-57-class wrong-qualifier detection): `cd prolog && swipl -l check_stack.pl -g "run_check_stack, halt" -t "halt(1)"` — compare against the recorded baseline (KNOWN_STATE.md 2026-06-04); new findings are regressions. Not a pipeline gate while the baseline is non-empty.
- In-session overlay probes: use `probe_harness:with_retracted/2` / `with_overlay/3`
  (snapshot-first, verified restore, automatic cache clearing via
  `cache_registry:clear_all_caches/0`) instead of hand-rolled retract/assert — see
  `swipl_load_path_and_probe_gotchas.md` §§2–4, 7. **The harness NOW REFUSES rather than
  silently overlaying nothing (OQ-326 RESOLVED 2026-08-21) — six checks run BEFORE the single
  mutation point,** in the ruled order 2 → 3 → 1 → 4/4′ → 5: template resolvable, no rule clause
  matched, per-template snapshot non-empty, replacement reachable at TEMPLATE shape, reachability
  decidable at all, target dynamic. Checks 2 and 5 have **no escape**; the rest are suppressed
  only by a dated, greppable wrapper at the call site (`expect_empty` / `allow_partial` /
  `allow_shadowed` / `reach_undeclared`, each carrying `retrofit(Date,Text)` or `authored(Text)`,
  each suppressing ITS OWN clause only). Gate row **`probe harness`** keeps the suite enforced.
  **Two things a new probe author must know.** A bare `with_asserted/2` now THROWS
  `probe_overlay_reach_undecidable` — an empty template list means no declared query shape, so
  reachability is undecidable, and the migration is `reach_undeclared`, never `allow_shadowed`.
  And **structural install is still not semantic effect**: the checks prove the clauses moved and
  that the replacement is reachable at the declared query shape, NOT that the observable changed —
  so a probe still owes its own assertion inside the overlay (`oq110`'s Control C is the model).
  Use `with_overlay/4` to paste an install witness. Detail: gotchas §12, OQ-326.
- **New Claude API call sites: route through `agent/llm_call.py` `call()` (or reuse its
  `sampling_overrides`).** Sonnet 5/Opus 4.7+ reject non-default `temperature` (loud 400), but
  Sonnet 5 runs ADAPTIVE thinking when the field is omitted — silently spending `max_tokens` on
  thinking (truncation with no guard outside the uke pipeline). And **a model swap in any
  generator is an ENGINE change: it stays OPEN until one full run passes the structural gates on
  the new model — API round-trips are pre-flight, not the witness** (witnessed 2026-07-13: first
  Sonnet-5 run drifted stage-2 format silently; `build_discipline.md` → *A model swap is an
  engine change*).
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
  the GREEN-over-severe-alerts defect. **The moderate→yellow cap for correction-grade signatures
  is confirmed-but-never-stressed** (zero moderate caps have ever shipped): the FIRST
  correction-grade firing on a base-GREEN constraint IS the deferred re-rule evidence — re-run
  `audits/2026-06-11_oq98_verdict_join/histogram_gate.pl` and surface the transition to the
  operator before trusting the new headline. Provenance: KNOWN_STATE 2026-06-11.
- **Purity carries TWO absence tokens; JSON null covers both — never coerce, average, or
  `.get(...,0)` (OQ-60, 2026-07-23).** Engine: `unknown` (no-data — e.g. no authored
  `coordination_type`, so no Boltzmann floor; the old fabricated `boltzmann_floor_default` is
  REMOVED) vs `-1.0` (epistemic-gate-fail sentinel); both serialize as JSON `null`
  (`purity_score`/`purity_band`). Write rule: clean/dispositive purity aggregates gate at
  coverage 1.0 → abstention token (`inconclusive(no_data)`/`undetermined`); positive
  existentials fire through unknowns; descriptive stats carry `n_scored/n_total`
  (`diagnostic.purity_n_scored/_n_total`). Atoms sort BEFORE numbers and THROW in arithmetic —
  guard `number/1` before any sort/max/compare/`is` over purity, and the guard must come FIRST:
  in `(P =:= -1.0 ; \+ number(P))` the `=:=` throws on `unknown` before the number check is ever
  tried (killed the trajectory step, `ab748fc6`). **The dual is silent:** `\=`/`==` do NOT throw,
  so a `P \= -1.0` filter ADMITS `unknown` into a numeric path — a green run is evidence about
  the throwing shape only. Synthetic test constraints needing scorable purity must AUTHOR
  `coordination_type`. Provenance: KNOWN_STATE 2026-07-23; rulings R1–R4 in ISSUES OQ-60.
- **FOUR purity banders, disjoint by design; exactly ONE is named `purity_zone/2` (OQ-62,
  2026-07-25).** The survivor is the canonical spec bander `logical_fingerprint:purity_zone/2`
  (.9/.7/.5/.3). The others are `fpn_report:ep_band/2` (effective purity, .7/.5/.3),
  `giant_component_analysis:action_band/2` (config `purity_action_*` floors) and
  `abductive_helpers:fpn_band/2` (FPN intrinsic, .8/.6/.4/.2) — **different quantities,
  different cuts, NOT interchangeable.** Three used to share the name and three words collided
  (`contaminated`/`degraded`/`critical` each meant two ranges); unifying them, or restoring the
  shared name, fails SILENTLY — bands still compute, numbers are wrong by one cut-point.
  Converse tripwire: all four return the same `unknown`, which is a DELIBERATE shared token
  (input absent or out of range, fail closed) — do not "fix" that overlap. Guard order is
  load-bearing (`\+ number` before `< 0.0`; the comparison throws on the atom), and exactly 0.0
  is a real score that still bands worst. Convention table: `docs/logic_extensions.md` §2.3.1;
  provenance: KNOWN_STATE 2026-07-25, ISSUES OQ-62.
- **`cs_reading_relation` targets resolve ONLY through
  `cs_kernel_registry:cs_edge_target_member/4` — never raw-match them (2026-08-07).**
  Canonical target form is BARE cids (operator ruling); authored corpora carry both
  legacy skews (bare-vs-prefixed in either direction), so a raw
  `cs_reading_relation(UID, Target, Rel)` join silently reads dead on skewed families —
  witnessed: the `cs_axiom_*` joins were 0 for their whole life, 22/67 kernels read
  `untyped`. `cs_pattern_detection` and `cs_corpus_analysis` were routed 2026-08-09
  (OQ-262; the latter had read 40/40 live conflicts as "no typed edge"); ONE un-routed
  site remains — `drl_composition.pl:122`, which also has a wrong-key Source binding
  and can essentially never fire (OQ-268; do not route without the re-key ruling).
  Generator emits bare (`snap_sibling_id`); `cs_reading_relation_unresolved/4` is the
  resolver's exact complement. Provenance: KNOWN_STATE 2026-08-07 + 2026-08-09.
- **A pooled-across-story seat H¹ measures its pooling convention, not the seat set
  (2026-08-08).** Seats are story-local (no cross-story identity — GAP-31), so pooled
  family vectors inherit story typing; 15/16 `real_closure` families "obstruct" this way,
  and every fiat sub-vector read H¹ = (#rope)·(#scaffold) exactly. Before citing one, run
  the symmetric read (complement + all-seats densities); detail: `build_discipline.md` →
  *Pooled-across-story H¹ inherits story-level typing*.
- **`h1_band` is NULLABLE; null = UNDETERMINED, never 0 (OQ-51, 2026-06-25).** A null `h1_band`
  (in `pipeline_output.json`, and the product-site `"h1"`/`"h0"` in `product_site_orbits.json`)
  means the cohomological obstruction is N/A — `<2` real (non-`unknown`) seats. A new reader that
  does `.get("h1_band", 0)` / `... or 0` silently reads undetermined/manifest as genuine (`None > 0`
  on the product site CRASHES). Use `shared.loader.h1_band_or_raise` (fails loud) or handle null
  explicitly. `sheaf_status` has a 4th value `undetermined` (sibling `sheaf_undetermined_reason`:
  `insufficient_seats`/`uncomputable_height`); the partition is manifest⟺h1>0, genuine/fragile⟹h1==0,
  undetermined⟹h1∈{null,0} — NOT an iff on null (route 2 is h1==0). Provenance: KNOWN_STATE 2026-06-25.
  **H¹ is over the SIGNATURE-RESOLVED orbit, and the gap spectrum is |real-seat| indexed (OQ-27 /
  OQ-195 both resolved, 2026-06-30 / 2026-07-02).** `h1_band` counts disagreements over the post-signature
  `dr_type` orbit (`cohomological_obstruction → orbit_vector → type_at_context → dr_type`; inside
  `dr_type`, `metric_based_type_indexed` raw `classify_from_metrics` THEN
  `integrate_signature_with_modal`) — recomputing H¹ from raw `classify_from_metrics` types gives
  wrong values silently (H¹=0 means signature-resolved global section, raw types may be maximally
  heterogeneous). The forbidden-{1,2} gap `{0,3,4,5,6}` holds for **|real seats|=4 only**; under the
  OQ-51 N/A rule fewer real seats give a smaller spectrum (n=3→{0,2,3}, n=2→{0,1}), so band 2 is the
  3-seat signature, not a 4-seat counterexample. General-n law PROVEN at every cardinality:
  `docs/h1_gap_spectrum_general_n.md` (OQ-195 resolved 2026-07-02). Provenance: KNOWN_STATE 2026-06-30 + 2026-07-02.
  **The same null rule governs the STAKEHOLDER frame (OQ-207/OQ-217, 2026-07-12/13):**
  `h1_stakeholder` (+ `_n_seats`/`_n_real`) in `pipeline_output.json` — null = <2 real-typed seats,
  never 0; a `.get("h1_stakeholder", 0)` reader makes the same silent mistake; the reachable
  spectrum per `n_real` is the proven H(n) (out-of-spectrum = bug witness). **Two absence tokens
  coexist by design — never unify them:** `untyped` (census-facing, `seat_perceived_vs_real/4`) vs
  `unknown` (kernel-facing, the only token `is_real_type/1` filters) — an `untyped` leaking into a
  type vector counts as a real disagreeing type and silently inflates H¹. Verdict⟺H¹ is an EXACT
  biconditional since OQ-217, and a consumer reading the bare `manufactured_consensus_candidate`
  token silently drops the larger `_untypeable` stratum. Provenance: KNOWN_STATE 2026-07-12 + 2026-07-13.

## Build Discipline (recurring failure modes — check before declaring work done)

These defects recur across unrelated subsystems because the producing step is interesting and the
reconciling step is boring; each looks complete at the moment it's introduced. **This section is
the tripwire list; the case law — mechanism, dated instances, diagnostics, worked examples — is
`docs/technical/build_discipline.md` (one `##` section per rule named below; its top carries a
glossary for the project vocabulary).** Read the full entry before arguing a rule is inapplicable.

**The spine: every defect here is an absence that presents as a presence.** Something is missing
— a consumer, a canonical fact, a clause dispatch, an authored datum — and a success-shaped token
(the producer ran; both copies parse; a plausible `0.5`; the gate passed) fills the hole so the
read site can't tell it from the real thing. The single fix, everywhere: **carry the provenance
bit with the value so absence and success stop collapsing to one token at the read site** —
wire-or-fail-loud, checked canonicity, let the engine dispatch, return `unknown` not `0.5`,
fail-closed on absence.

**Numbering is SETTLED (OQ-278, 2026-08-17): seven members at eight indices, index 3 a grave never
reused; both documents publish the same member per index, gate-checked (`doc patterns` row).**
`Pattern N`/`PN` is overloaded outside this taxonomy (`diagnostic_summary.pl`'s `P1`–`P10`,
`Priority:` levels, `CWC:P3`, decompose `candidate_pattern`, …) — name the taxonomy when the
surrounding text does not. A pre-2026-08-17 `Pattern 3`/`P3` means the *mechanism* (bound-probe,
now 7, or the vacated destructive-replace), never the index; gate row `displaced cites`.

**1. Produced-but-not-consumed (the dangling wire).** Data is written and nothing reads it back
into the thing that needs it. **A producer is not done until something consumes its output** —
wire the consumer in the same change or add a check that fails loudly; wire it into
`run_pipeline.py` in dependency order and re-certify the transitive chain (`pipeline_output.json`
→ `enrich` → `enriched_pipeline.json` → `enhanced_report.py`), stamping the run manifest into
co-produced artifacts so freshness is checkable. **But an absent consumer is equally consistent
with a deliberate severance, and this rule applied blind PRODUCES that defect** (witnessed
2026-08-15: `epsilon_bin`, which OQ-34/OQ-117 forbid wiring). When you cut a channel on purpose,
the marker goes on the field, at the site. Detail: BD → *Pattern 1*, *The wire that was cut ON
PURPOSE*.

**2. One-canonical-thing-became-two (the silent fork).** Two versions exist with no queryable fact
saying which is canonical. **One canonical location per thing, and canonicity is a checked fact
(a path in docs, a CI check), not a memory.** Prefer a branch over a test copy; resolve found forks
by evidence (which path run-commands invoke, which imports resolve, git recency), record the
verdict in KNOWN_STATE.md. Detail: BD → *Pattern 2*.

**3. — VACATED 2026-08-11 (operator ruling). Index deliberately left EMPTY; do not reuse or
renumber.** *Destructive-replace without proof* was demoted from defect pattern to the witness
rule below (no silent mechanism, no exemplar, zero instances on a search built to find one — a
deletion that gets undone announces itself). The index stays empty so dated audits citing P4/P5/P6
keep their meaning: a visible gap is a checked fact, a silent renumber is a fork. Rationale: BD →
*Pattern 3 — VACATED*; ISSUES OQ-278.

> **The witness rule survives intact — prove before you replace.** Before deleting, retiring, or
> overwriting anything something relies on: run old and new, paste both outputs, diff them, show
> identity or justify every difference in the same change. "Structurally equivalent" is a
> code-read; the diff is proof. Consolidating N into one is N separate diffs.

**4. Fabricated default (the missing-data fallback that emits a real-looking value).** A predicate
whose input is absent binds a plausible constant instead of failing or returning `unknown`. **The
tell is a catch-all clause binding a metric to a constant** — `(measurement(..., V) -> true ; V =
0.5)`. **Return `unknown`, never a plausible value.** Diagnostic: count how often the fallback
fires on the corpus — more than a handful is a fabrication, not a safety net (`classify_at_time`'s
`Supp=0.5` fired 190/190; OQ-33). Report both the flip census AND the input-exposure count.
Detail: BD → *Pattern 4*.

**5. Absence satisfies the gate (authored-zero vs absent).** A gate passes because its input is
*missing*, not because a condition was checked: `Count == 0` on no facts, `forall` over an empty
table, `V =< Ceil` on a fabricated default (`natural_law_signature` over the corpus-empty
`intent_power_change`, OQ-43). **A gate over a possibly-empty table must establish the datum was
authored before it may pass — fail-closed on absence.** Diagnostic: count the source predicate's
facts; 0 ⇒ the gate is a no-op. **The dual (OQ-178): before fail-closing, confirm the absence is
GENUINE, not a probe landing off the authored grid** — a query at a synthetic key against data on
a different grid reads "absent" while the datum exists; fail-closing there erases real signal.
Detail: BD → *Pattern 5*.

**6. Success-shaped absorption (measured-empty vs didn't-look collapse at aggregation/channel
boundaries).** Each component is sound; the absorption lives where they compose (`[] → 0.0`
gradient fallback; `grep -v Warning` over load output, OQ-96; findall over partial levels read as
a full-system verdict). **Aggregates carry their COVERAGE to the read site; channels carry
ALLOWLISTS (`python/load_warning_gate.py` is the template); defaults-on-empty return
`unknown`/OPEN. Sufficiency is a property of the QUESTION — fail-closed per consumer-named
requirement, never by global fraction.** Detail: BD → *Pattern 6*; site census OQ-97.

**7. Bound-probe bypasses clause-order (a BOUND selecting argument skips the cut — census
unbound, or the count is manufactured).** `findall(C, constraint_signature(C, natural_law), Cs)`
answers "satisfies that clause body in isolation," not "the engine assigns `natural_law`" —
earlier lock clauses fail to unify, so their cuts never run. **Any `findall`/`forall`/
`aggregate_all` over a cut-ordered predicate with the selecting argument bound is suspect — re-run
unbound with `once/1` + post-filter by equality.** Asymmetry: the bound form is over-permissive, so
a bound-arg **zero is trustworthy** and a bound-arg **nonzero is an artifact until checked**.
Witnessed twice on the same predicate (2026-05-30; 2026-08-17, 276 vs real 0, concealing that 67%
of kernel_v1 signatures resolve `unknown`). Mechanical guard: gate row `bound selector`. Detail +
incidence ledger: BD → *Pattern 7*.

**8. Recap-as-witness substitution.** *Paste-or-untag* at the turn boundary: a turn reporting N
edits done pastes N witnesses; M<N is the defect, and the operator reads any done-claim lacking a
same-turn paste as unverified regardless of the recap. Detail: BD → *Pattern 8*.

**Shared root of 1 and 2: build for the corpus you intend, not the one on disk** — a scheme that
*cannot* collide by construction beats one that *happens not to* today.

**A denominator that silently admits non-members gets WORSE while reading STABLE (OQ-306, resolved
2026-08-21).** Membership is by FILENAME, so `manifest.n_constraints` counted the
`*_contradictions.pl` meta-files, and that stratum GREW (9 → 27) with nothing going red — a
growing contaminant rewrites a time series rather than biasing it constantly. **Use the built
membership predicates — `corpus_loader:corpus_story/1`, `corpus_member_kind/2`, denominator
`manifest.n_stories` (`n_constraints` is a MEMBER count kept only as a same-run key); gate row
`corpus census` guards the share over time. Count it live, never from git** (the stratum moves with
no commit at all). `schema_version` is now **3**; a `!= 2` reader refuses. Detail: BD → *A
denominator that silently admits non-members*; KNOWN_STATE 2026-08-21.

### Controls, witnesses, and handoffs (operator rulings, 2026-08-11 → 2026-08-20)

Each is one sentence here and a full section in `build_discipline.md` under the italicised name.

- **Every diagnostic needs a positive control** — an empty grep, `findall` of `[]`, a 0, an "I
  found it nowhere" each means *nothing there* OR *didn't dispatch*; this holds for reasoning, not
  just shell. **A control demonstrates DISCRIMINATION, not detection:** the witness is a case the
  instrument DECLINED (grades: a decline in its own history > a natural negative > an authored
  decoy); no decline available ⇒ the question is UNANSWERABLE from this corpus (declare it). An
  instrument validated in one ROLE (detector) reused in another (selection metric) is a NEW
  instrument owing its own decline. (*Every diagnostic needs a positive control*; *A positive
  control demonstrates DISCRIMINATION*.)
- **A check that CANNOT fail witnesses nothing** — ask *what value would make this line fail?*;
  none ⇒ consistency check, discrimination still owed. **Pin `/usr/bin/grep`, not `grep`, in any
  script computing a reported count** (a green "73 + 101 = 174" concealed a positional parse,
  2026-08-10). (*A consistency check is not a discrimination check*.)
- **A control you ADD is itself a claim** — same-path, two-sided, riskiest-shape,
  substrate-anchored, write-free-if-pre-write. (*An introduced instrument is itself a claim*.)
- **A control must witness that it is CALLED** — a guarded function called only from its selftest
  is an orphan: control count rises while coverage falls. Orphaning arrives through repairs, so
  retire the old control in the same change as its replacement. (*A control must witness that it
  is CALLED*.)
- **A found defect's before-commit is a free negative control** — run the detector at N and N−1,
  record *fires at N, declines at N−1*; availability is not automatic (check), and when a detector
  will follow, preserve the defective state deliberately. (*When a defect is found…*.)
- **Gate the output, not only the input** — a pipeline verified on what it consumes can produce
  nothing and report green (219 calls, nothing persisted). Persist the raw datum first,
  write-then-verify per unit, assert count AND non-emptiness AND parses-to-vocabulary, count from
  the artifact never the loop; audit by enumerating what a spend DEPENDS ON, not by following red
  lights. (*Gate the output, not only the input*.)
- **A verification section may not pre-authorize dismissal of a signal it has not re-witnessed**
  — convert every "expect X red, ignore it" into "if X is red, confirm against KNOWN_STATE <date>
  before attributing it elsewhere"; the executor reports the baseline OBSERVED, not predicted.
  (*A verification section may not pre-authorize dismissal*.)
- **Write the receiver's prompt before calling a design done** — enumerating what a receiver must
  DO surfaces what re-reading (recognition) never does; "read the design and execute it" catches
  nothing; the terminal is a channel, so say what may be PRINTED. (*Write the receiver's prompt*.)
- **The receiver's license to refuse** — an instruction correct in prose and wrong when executed is
  refused at the volume of a completion and routed back to the sender; scope is *executed as
  written, this produces what the design forbids*, not "I'd do it differently" (that is the
  one-sentence flag). State the license in the handoff prompt. **A running receiver holding
  `file:line` pins LOCKS that file** — clearance means checking the whole pin set. (*The
  receiver's license to refuse*.)
- **Unwired ≠ worthless** — Pattern 1 is a build-time rule, not a licence to call a zero-firing
  subsystem cruft; adjudicate by the product it yields (duplicate ⇒ cruft; unique/refinement ⇒
  wire it or log it in `design_gaps.md`); when unsure, preserve. (*Unwired ≠ worthless*.)
- **Nobody reads a hammer's changelog** — before minting any record about our own work, name the
  reader and the decision it changes; a tool is validated by use. Governs MINTING only; the rule
  above still governs removal. (*Nobody reads a hammer's changelog*.)

### Over-confident moves on the synthesis side — errors of *claiming*, each owes a witness

Full version: BD → *Over-confident moves on the synthesis side*; *When to stop verifying*.

- **(1) False-absence.** Before "absent / can't / no X," run a positive control (grep a name you
  KNOW exists) or tag it **OPEN**; control the claim at its altitude (a probe over `f` licenses
  "absent in `f`"). **The concept→surface mapping is its own claim** — sweep sibling surfaces
  before "concept C has no live channel" (`coordination_vitality` 0-authored while
  `founding_problem_status` carried it at 164/199, OQ-255).
- **(2) False-unification.** Before merging two things that share vocabulary or dynamics, verify
  the architecture doesn't *mandate* their separation (observer/committer: v7 Theorem 7 forbids the
  fold). Shared dynamics is analogy, not a bridge.
- **(3) Unguarded axis-swap.** A new or relabelled axis owes a PRE-REGISTERED discriminating
  control: the case where new and nearest-prior axis come apart, outcomes written before the run.
  One witness earns "separable here," not "orthogonal everywhere."
- **(4) Hedging-as-rigor.** "Both readings possible" is earned only when no falsifier can be
  specified; if a kill condition exists, COMMIT and attach it.
- **(5) When to stop verifying.** Stop when the next pass costs more than being wrong AND every
  open is DECLARED: each verdict names a tier-available falsifier or downgrades to OPEN, typed
  against `docs/omega_variables.md`. Trap: an *orientation* gloss is a deferred Ω_E, not Ω_P.
- **(6) Non-discriminating falsifier.** A control closes ONE channel and the confound relocates
  (OQ-232, 3× in one arc); iterate until it lands nowhere or in a declared residue. **A falsifier
  must be FIREABLE** (name a world that satisfies it), and a repair that encodes the tested claim
  into the instrument owes a standing probe. (BD → *The relocating confound*; *A falsifier must be
  FIREABLE*.)
- **(7) A conceded point is not a research question.** Converting a fact that is already the
  operating condition into a question needing outside evidence — tell: every warrant is shaped
  like an OUTSIDE while a self-instance sits unused in the transcript. Concede in the register the
  point was made in. Falsifier attached at adoption (BD → *A conceded point*): load-bearing iff
  instances found before the September consolidation pass were not operator-flagged.

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
all counts here are file counts verified on disk 2026-06-10. **Tripwire: HEAD enumerates the
corpus by FILENAME (`corpus_constraint/1`), so a legacy corpus whose filename ≠ its in-file
`constraint_metric` subject yields *null DR output* for those stories silently (no error) — 89/702
in v5, 133/1151 in original_json; 0 in v6/kernel_v1. Re-key by the in-file subject (or verify
filename==subject) before trusting a legacy-corpus DR sweep. Provenance: OQ-20 audit, KNOWN_STATE
2026-06-22.**

**FIVE LIVE LEGS, and the beta posture (operator ruling, 2026-06-20; legs extended 2026-07).**
`prolog/testsets/` is the LIVE leg — a small, **deliberately singleton topical working set** the
operator exercises the engine on while building it; `testsets_haiku/` + `testsets_flash/` are the
reconciled multi-reading **twins** (the comparison baseline); `testsets_kimi/` + `testsets_sonnet/`
are two later model-named legs. (Further legs have been registered since — `python/shared/corpus_legs.py`
`LIVE_LEGS` and the `corpus census` baseline are authoritative, not this paragraph.)

**Static-leg file counts (disk-verified 2026-08-18): `testsets_haiku` 960, `testsets_flash` 960,
`testsets_kimi` 1005, `testsets_sonnet` 1001.** Finished corpora; a number here is usable.
**`prolog/testsets/` CARRIES NO COUNT, EVER (operator ruling, 2026-08-18) — count it, never recall
it:** topic runs land stories mid-session, so any literal is stale the moment written while still
reading as verified (this paragraph once published 259; it was 279 six days later). Get it at the
moment of use — `manifest.n_constraints` of the run's own `pipeline_output.json`, or
`ls prolog/testsets/*.pl | wc -l` — with the date. Provenance: KNOWN_STATE 2026-08-18.

**`testsets/`'s singleton sparsity is INTENDED, not a half-finished rebuild** — do not "complete,"
flatten, or rebuild it on sight; its high OQ-58 dangling rate is the expected sparsity artifact.
**The project is ALPHA working toward BETA: extract maximum value from the corpus we have before
any rebuild**; a future instance may SUGGEST a rebuild, not propose one lightly. It is the
**evolving-schema test bed** (operator, 2026-06-24): schema/prompt/scope changes are exercised HERE
(the twins stay the stable baseline); we do **not** backfill any leg, and "hold off until rebuild"
is the WRONG posture — an OQ-37-style "does this metric earn its keep" call is settled by testing
it on `testsets/`. Witness of the regime swap: `audits/2026-06-20_oq58_cross_corpus_incompleteness/`.

**A leg's MODEL is not its directory name — verify from `story_provenance` (or `classify_corpus`'s
fingerprint refusal) before attributing or pooling (OQ-78, 2026-08-10).** `testsets_kimi/` is
`kimi-k2.6` while the default leg's kimi stratum is `kimi-k3`; `testsets_sonnet/` is
`claude-sonnet-5`, the same model as the default leg's largest stratum — TEST data for any sonnet-5
question, not a neutral comparison leg. The four twin legs are ONE seed set re-authored per model
(957 four-way matched ids): a cross-leg contrast is *matched-seed across models*, never a
withheld-input arm. **ε-keyed statistics are per-Author stratum and NEVER pooled across legs** —
the last-digit rail is model-bound (Claude→`.x8`, kimi→`.x2`, flash→`.x5`).
**The twin legs are the ONLY matched-seed structure in the project (OQ-78/OQ-281, GAP-35):**
every other population pair shares ~0 story ids (sole exception: default-leg haiku-4.5 × sonnet leg,
28 ids), so **check the id intersection FIRST before designing any paired probe** — outside the
twins there is nothing to pair, and a new matched-seed leg is a generation spend, never a re-read.
The twins pair on `constraint_id` (filename); `cs_story_uid` does not join and `seeded_from` is
unemitted, so matched-seed structure is a generation-time decision a later session cannot reverse.

**FNL prevalence is regime-bound — never a detection result (OQ-70, RESOLVED 2026-06-05).**
All kernel_v1-era FNL firings (827/1106) rode a `claimed_natural/2` source that read ANY single
authored mountain perspective as a naturality claim — a generation-template convention, not
detection; retracting the template perspectives migrated FNL→FCR wholesale (witnessed
2026-06-04). Operator ruling (`72ec2cdd`): no signature may read a single authored perspective as
a story-level claim. So: (a) ALL pre-reset/archive prevalence is regime-bound; (b) live prevalence
is citable only as a CLAIMS statistic; (c) statistics reset 2026-06-05 and again 2026-06-11
(example cutover — discount example-inherited signatures per
`audits/2026-06-11_oq109_phase_b/EXAMPLE_INHERITED_SIGNATURES.md`). Pipeline outputs from before
2026-06-04 carry ONE non-corpus `per_constraint` entry (`catholic_church_1200`, an engine demo) —
exclude it; runs from 2026-06-04 on enumerate `corpus_constraint/1` and match the manifest.

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

**Canonical framework paper: `docs/deferential_realism_paper_v8.md`** (OQ-135 adoption,
2026-07-02) — the entry point and canonical seat/gauge/orientation vocabulary (v8 §5.4 bridge
table translates v7's "seat" = v8 "gauge"). The detailed records remain authoritative and all
§-references to them stay valid: **v7** (`docs/deferential_realism_paper_v7.md`) for the
committer-axis promotion — Axiom 7 (authored commitment structure with computed consequence),
Theorem 7 (detection independence), Theorem 8 (licensed plurality vs. real closure), §4.5
(two-axis engine), §5.11 (trifurcation profile); **v6.13.1**
(`docs/deferential_realism_paper_v6.13.1.md`, internally v6.13.2) for the observer-axis record
(Axioms 1–6, Theorems 1–4, the OQ-26 ε reading-relativity and OQ-27/OQ-51 H¹ amendments).
Read v8 first; cite v7/v6.13.1 for proofs and empirical detail.

**Formal classification rules: `docs/logic.md`.** The spec document; `config.pl` must match it.
(UTF-8 fully repaired 2026-07-02 — the Feb 2026 repair was partial and ~1,790 mojibake
sequences persisted undetected for months, plus ~170 in `logic_thresholds.md`; if mojibake
reappears, repair with sed/Python — the Edit tool fails on multi-byte mojibake. KNOWN_STATE
2026-07-02 has the method + controls.)

**`dr_type_at/4` and `classify_snapshot/3` were replaced (2026-05-17)** — both used the legacy
`power_modifier` χ path (χ = ε × π, omitting σ). Replacements: `classify_at_time/4`
(`drl_composition.pl`) and `snapshot_type/3` (`transition_paths.pl`) on the canonical sigmoid
pipeline (χ = ε × f(d) × σ(S)). Callers `constraint_history/3` and `degradation_chain/3` updated.
**The legacy path is now FULLY DRAINED (OQ-67, 2026-07-25):** `drl_audit_core.pl` and
`constraint_indexing:power_modifier/2` are deleted — χ = ε × π exists nowhere, so do not cite it as
a second live path (it was unreachable, not merely deprecated). The six `power_modifier_*` params
REMAIN in `config.pl:57-62` with **no reader at all**, as the calibration anchors `canonical_d_*` is
fitted to approximate — so a zero-flip / null sensitivity result for them means "no consumer," NOT
"no sensitivity," and pre-2026-07-25 sweep outputs describe a still-read regime. The one unique
product of the retired module (the no-exit corner, ε≈1 ∧ χ≈1) is declared absent at
`design_gaps.md` GAP-29; `omega1_audit.pl` retains the surviving χ-only bander and was NOT
adjudicated. Provenance: KNOWN_STATE 2026-07-25.

**`site_contexts_product/1` scope exclusion is calibration-based.** The product site excludes
`regional`, `continental`, `universal` (`constraint_indexing.pl:954–955`): those scope atoms
appear in no canonical context and their scope_modifier values are unvalidated. σ(universal) =
σ(national) = 1.0 (`config.pl:117,120`) — no differential χ effect.

**Generation is stochastic; the committed story is the determinism frontier (operator ruling,
2026-06-12).** LLM generation NEVER reproduces — same material re-run gets different scopings,
namings, readings, ε (OQ-26 / Axiom 2 amended in v6.13.1). Measured (OQ-264, 2026-08-06):
same-input redraw stability of per-reading identity is FILE-STRUCTURE-DEPENDENT (2/6 to 6/6), so
there is NO global churn floor; a manifest feature counts as replicated only if present
(name-blind) in ALL 3 same-input redraws; pooled ratios inherit the churn, so report
numerator/denominator ranges separately; names are never identity across redraws; sampling is not
pinnable (no temperature, no seed). Standard: `audits/2026-08-06_oq264_kredraw_variance/WRITEUP.md`.
**So:** re-generated stories are NEW DRAWS, not re-measurements; cross-run "same story" identity
does not exist; never key an exclusion list or per-story mechanism on names/signatures across a
regen boundary (`seeded_from` is generation-time plumbing, never identity recovered backward —
`audits/2026-06-12_signature_identity_witness/`). **Deterministic from the committed JSON onward,
and that boundary is CHECKED, not assumed** (hash inputs + manifest + output; order-dependency is
the OQ-112 class): attribute "same material, different results" by the stage-hash diff — generation
stochasticity, ensemble refit, or pipeline non-determinism (a bug) — never by assumption. **A
category shift on redraw is the mechanism WORKING** (`docs/seat-theorem-v1.md`): verdicts are
seat-indexed and a redraw occupies a new seat; the analysis product is the SHAPE, and the replicate
stability table reads as a σ/seat partition (draw-stable ≈ situation-fixed, draw-unstable ≈
seat-expressive), not a noise filter. **Reading that table: `status: stable` is NOT
`positive`-stable (OQ-190)** — an `agreement_kind: absence` cell was absent in every draw and
passed on absence (Pattern 5); filter on `agreement_kind` before citing any stability score
(`audits/2026-08-17_oq190_blast_radius/stability_positive_grade.tsv`).

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
  packages, priorities, and backlog live there as OQs (the research-frontier backlog OQ-69 was the
  seed ledger — drained 2026-06-20 into individually-tracked OQ-154–170; provenance map in OQ-69).
  AGENDA.md, AUDIT.md, TODO.md, PRIORITIES.md were consolidated into it and deleted (2026-06-04)
  — do not recreate parallel trackers (Build Discipline Pattern 2).

**Git autonomy (operator ruling, 2026-06-09).** Standing permission to commit without asking:
when a coherent unit of work is witnessed, commit it then — do not batch a session into one
end-commit; in-flight work is what compaction and harness outages destroy (full rationale:
`docs/technical/build_discipline.md` → *Commit-as-you-go*). Push is permitted once the pre-push
check below passes.

**Worktrees are OPT-IN (operator ruling, 2026-06-18; single-instance operation).** Default: work
on `main`, commit-as-you-go, feature branch (`git checkout -b <task>`) for risky/multi-file code —
a branch gives the rollback safety without the 27k-file checkout, the cwd juggling, or the
gitignored-`outputs/` trap (a fresh worktree lacks `outputs/pipeline_output.json`, so "read
pre-computed values from there" probes read empty/stale and look fine — Pattern-6). **Start a
worktree only when explicitly asked** (e.g. resuming concurrent instances); rationale + the
2026-06-10 collision history in commit `b476eae6`. If you do: merge back when the unit is
witnessed, and run `python3 python/issues_status.py --check` after any merge touching ISSUES.md
(fails on duplicate OQ labels from two worktrees claiming the same next OQ-NN). Multi-writer
corollary: a commit's witness is its DIFF or an entry-anchored check, never a global count delta
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
resolved/stale items.

**TRIPWIRE — MERGING TRADES AN ATTENTION CAP FOR A DELIVERY CAP, AND NOTHING GOES RED
(OQ-289/OQ-290, 2026-08-12).** `MEMORY.md` is always-loaded, but the sibling files arrive as
per-turn `relevant_memories` attachments under a **per-file** cap (4,096 B or 25,000 B — which one
binds is what OQ-289 runs); an over-limit file arrives truncated with a `Read` pointer whose
follow-through is unwitnessed. **When merging, front-load each cluster file with a self-sufficient
summary in its first 4 KB**, and do not treat "merged, index shorter" as finished.
`python3 python/apparatus_instrument.py --check` prints the delivered-fraction table (reporting
only until OQ-290 rules). Provenance: KNOWN_STATE 2026-08-12; the retracted CLAUDE.md-truncation
claim is OQ-286 — do not re-mint it.

The same monthly pass covers:

- **KNOWN_STATE.md roll-off** — entries older than ~30 days get the promotion test once more,
  then compress in place per the roll-off rule in its header. Verify with
  `python3 python/known_state_status.py --check`.
- **ISSUES.md compress-on-close check** — sweep for closed entries that escaped the footer rule's
  at-close compression.
- **CLAUDE.md rule-freshness (premise rot)** — for each always-loaded rule, ask not just "is it
  still true?" but "does its *premise* still match how the operator works?" A rule can stay
  internally correct while its reason expires (the worktree rule outlived multi-instance operation
  and sat stale until a session tripped over it, 2026-06-18). Demote or rewrite any rule whose
  premise has changed; this is the symmetric counterpart to the promotion test (promotion adds,
  this removes/refreshes).

**Last full review: 2026-08-10. Interval: monthly.** If today is on or after **2026-09-10**,
prompt the user to run a consolidation pass before starting the requested work, then update both
dates. The 2026-08-10 pass was the major prune (memory 113→53 files; KNOWN_STATE window residue
drained 52/52; ISSUES backlog drained 100/139) and installed the apparatus instrument: **the
memory Feedback channel is capped at 33 entries (gated exchange — retire one to admit one) and
every consolidation pass reads the rolling Fired: catch rate (OQ-276)**, both enforced/reported
by `python3 python/apparatus_instrument.py --check` in the gate.
**Out-of-cycle CLAUDE.md demotion pass, 2026-08-23** (prompted by an outside reader's verdict on
`build_discipline.md` — sound ideas, unedited density): this file 15,174 → ~10,900 words, with the
Build Discipline section cut to tripwire + pointer and every demoted detail verified present in
`build_discipline.md`/KNOWN_STATE before removal; a glossary block added at the top of
`build_discipline.md`. **Standing rule from it: the always-loaded file carries the tripwire and
the pointer, never the exposition — a promotion that adds more than ~6 lines should demote the
body to the case-law file in the same change.** That pass also discharged residue (b) (header
restored) and (c) (first catch-rate reading recorded in OQ-276, now BLOCKED-ON-YOU for the value
ruling). **Declared residue for the 2026-09-10 pass:** (a) 40 borderline ISSUES entries left
uncompressed (30 pre-compressed pointer-blocks; 7 fresh Aug-2026 closes — OQ-151/258/259/260/
261/262/264 — whose bodies are still-operative rulings); (d) the memory files themselves were
NOT re-consolidated on 2026-08-23 (54 files, channel 33/33 — at cap); (e) KNOWN_STATE roll-off
for entries older than 30 days was not run on 2026-08-23.

## Audit Methodology

Audits follow **recon → proposal → execution → writeup**: recon establishes what data exists and
what is answerable; proposal states exactly what will run and what would constitute each verdict;
execution runs scripts and saves raw output; writeup analyzes from evidence only — never
documentation restated as findings. Detail: `docs/project_orientation.md` §8.1.

**Before running ANY check whose answer you do not already know — audit-worthy or a five-minute
hunch — open a line in `audits/INVESTIGATIONS.md` first, and close it with its `Fired:` bit
regardless of outcome** (OQ-276 ledger; the quick mostly-negative checks are exactly the
population it exists to recover — deciding at hunch time whether something "counts" is the
selection bias the ledger measures).

**Location mandate (2026-06-04): every audit lives in `audits/<YYYY-MM-DD>_<slug>/`** — one
subdirectory per audit, writeup AND evidence artifacts together; date = execution date. Not in
`docs/`; not only in `outputs/` (gitignored = gone on a fresh clone). Audit scripts stay in
`python/audits/` writing to `outputs/`; on completion, writeup + evidence move into the dated
subdirectory. Conventions and consolidation map: `audits/README.md`.

**Writeup format (2026-08-06, forward-only): every NEW audit dir carries exactly one
`WRITEUP.md` entry point** (required header: executed date, OQ, one-line verdict at its scoped
altitude, manifest cite, evidence map naming every artifact) with reserved phase names
`RECON.md` / `PROPOSAL.md` / `PREREGISTRATION.md` (one spelling, frozen at spend). Template and
rationale: `audits/README.md` → *Writeup format*; existing dirs are point-in-time, never renamed.
Machine-checked: `python3 python/audit_writeup_gate.py --check` in `scripts/gate.sh` (selftest
rides every run; pre-adoption dirs exempt; malformed dir names fail closed).

**Catch bit (2026-08-10, forward-only): every NEW WRITEUP.md also carries one `**Fired:**`
line** — `live` (a control fired / a claim flipped / a consumer-visible verdict changed) |
`latent` (real defect, conditional on an unproduced input) | `no` (pure confirmation). This is
the apparatus's own instrument: the rolling live/latent/no rate is the falsifier for the
verification ritual itself (reported by `python3 python/apparatus_instrument.py --check` in
`scripts/gate.sh`, which also enforces the memory feedback-channel cap; the rate is reported,
never gated — reading it is the operator's seat). Spec: `audits/README.md` → *The Fired: bit*.

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
