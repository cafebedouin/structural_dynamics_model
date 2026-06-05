# Kernel Frame — Phase 1 Rollout

Goal: give the pipeline a committer-axis frame (kernel / reading / drift) without breaking
the working observer-axis pipeline. Phase 1 records committer content as omegas + one
optional free-text field; nothing computes on kernels for χ yet. Validate the representation
against a toy corpus, THEN decide what to formalize.

## Files in this bundle

- `seeds/kernel_seeds.json` — 46 kernel seeds (26 library-derived w/ known verdicts, 20
  contested-present). Copy to `prolog/kernel_seeds.json`.
- `prompts_additions/SCOPE_addition.md` — extends the EXISTING commitment_system_recognition
  object in `prompts/uke_scope_v2_json.md` with readings + the kernel/not-kernel decision.
- `prompts_additions/GENERATION_addition.md` — new "Kernels and Readings" section for
  `prompts/constraint_story_generation_prompt_json.md`, after the Directionality Overrides
  section.
- `python/apply_schema_patch.py` — adds optional `commentary.kernel_context` string. Run on
  BOTH schema copies. Additive, idempotent.
- `python/generate_kernel_corpus.py` — the merged SCOPE→flatten→batch→sidecar→eyeball script.
  Copy to `agent/generate_kernel_corpus.py`.

## Rollout order (each step gates the next)

1. **Baseline.** On `main`, pick ONE ordinary topic (e.g. "Alberta separatism"), generate it,
   save the output. This is the regression baseline.

2. **Branch.** `git checkout -b kernel-frame`. Tag the main commit you branched from.

3. **Apply (branch only):**
   - `cp seeds/kernel_seeds.json prolog/`
   - Splice `SCOPE_addition.md` into `prompts/uke_scope_v2_json.md` (§1.3 + replace the CS
     recognition object spec).
   - Splice `GENERATION_addition.md` into `prompts/constraint_story_generation_prompt_json.md`
     after the Directionality Overrides section.
   - `python3 commitment_corpus/apply_schema_patch.py schemas/constraint_story_schema.json`
     (single canonical schema since 2026-06-05; the agent/data/ orphan copy is deleted)
   - `cp python/generate_kernel_corpus.py agent/`

4. **Regression gate (the critical step a branch can't give you).**
   `python3 -m agent.generate_kernel_corpus --regression-check "Alberta separatism" --run-tag regress_01`
   Expect `is_contested_kernel = False`. Generate the same ordinary topic the normal way and
   diff against the step-1 baseline. If the kernel frame leaked into an ordinary topic (spurious
   kernel omegas, hedged ε, is_contested_kernel=true), FIX the prompts until the ordinary topic
   matches baseline. Do not proceed until the frame is inert on non-kernels.

5. **Smoke test (5 kernels):**
   `python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01 --limit 5 --skip-search`
   Eyeball `outputs/kernel_manifests/run_01/coherence_eyeball.md`: do readings differentiate
   (distinct emitted types) or collapse (all same)? Does pro-life come back a coherent kernel?
   Adjust prompts, re-run as `run_02` if needed (cheap; Haiku batch).

6. **Full toy corpus:**
   `python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01`
   (drop `--skip-search` for the 20 contested-present seeds if current facts matter; keep it
   for the 26 historical library cases). ~46 kernels → ~120-150 constraints at the expected
   ~25% reading-level failure rate.

## Safety invariants (do not violate)

- All output is RUN-TAGGED: `json/<tag>/`, `prolog/testsets/<tag>/`,
  `outputs/kernel_manifests/<tag>/`. The main corpus is never written. Promote by hand only
  after the eyeball passes.
- The schema change is additive-optional: `kernel_context` absence stays valid, so the
  existing 3,380-constraint corpus still validates and needs no regeneration.
- The processed-log is run-scoped: reruns of the same `--run-tag` are idempotent.
- If none of it works: `git checkout main` and delete the branch. Nothing on main moved.

## What to look at after run_01

- `coherence_eyeball.md` — collapse vs differentiate per kernel. The library cases have known
  verdicts (D2 pluralism should look like a coherent multi-reading kernel or an incoherent
  bundle; QWERTY should reproduce the beneficiary artifact); the contested-present cases are
  the surprises (does Israel/Palestine come back one kernel or two?).
- The omegas across generated constraints — what committer fields did the generator REACH for
  that the schema lacks? Recurring omega-shapes = future fields to formalize. Scattered =
  committer content is irreducibly per-kernel, belongs in prose. That mining pass decides
  Phase 2.

## Deferred to Phase 2 (do NOT build yet)

- Any math/coherence predicate over kernels (the substrate-threshold question — calibrate
  from run_01 data, not from first principles).
- Kernel state over time / prune-graft / amendment transitions.
- Anything that makes χ read kernel structure. Phase 1 keeps kernels inert.
