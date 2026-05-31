# Generation path resolution — which files feed the two generators

> **Provenance.** Verified by reading the import graph and path constants at commit **`5e3d9dc6`**
> (2026-05-31). Re-verify before trusting: the path constants live in
> `agent/story_generator_base.py`; both generators import from there. Counterpart to
> `generator_emission_map.md` — that doc is the *output* side (authored field → Prolog fact); this is
> the *input* side (which prompt / schema / example actually reach the model).

There are **two** generators, and they share one path module but diverge on the example file. Editing
a prompt or schema only matters if the generator you run actually loads the file you edited — and an
env var can silently redirect it. This is the seam OQ-47 (de-stamp regen) depends on.

## Single source of path constants: `agent/story_generator_base.py`

| Constant | Resolves to | Env override |
|---|---|---|
| `PROMPT_PATH` | `prompts/constraint_story_generation_prompt_json.md` | `DR_GEN_PROMPT` |
| `SCHEMA_PATH` | `python/constraint_story_schema.json` (the **canonical** schema) | `DR_SCHEMA` |
| `EXAMPLE_PATH` | `agent/verification_bottleneck.json` | — |

`SCHEMA_PATH` is loaded twice: as **prompt text** (injected so the model sees the schema) and as the
**validation** dict (`story_generator_base:load_schema`). `generate_constraint_pl._load_schema` also
resolves `DR_SCHEMA` else the same canonical file. **All schema reads → `python/constraint_story_schema.json`.**
`agent/data/constraint_story_schema.json` is an **orphan** — referenced only by
`commitment_corpus/apply_schema_patch.py`, loaded by neither generator. Do not edit it expecting effect.

## The two generators

**`agent/generate_kernel_corpus.py` — the corpus-growth / regen path.**
Entry: `python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json`.
Call path to the example load:
`main() → build_batch_requests() → build_cached_messages() → _load_context_file(EXAMPLE_PATH)`
→ **`agent/verification_bottleneck.json`** (clean exemplar). SCOPE prompt: `prompts/uke_scope_v2_json.md`.
Does **not** import or invoke `c-orchestrator`. The `--regression_check` arm builds an inline prompt
and loads no example.

**`agent/c-orchestrator.py` — the topic-driven essay orchestrator** (`python3 agent/c-orchestrator.py "topic"`).
Builds its **own** context dict (does not use `EXAMPLE_PATH`): `uke_scope` = `prompts/uke_scope_v2_json.md`,
`gen_prompt` = `DR_GEN_PROMPT` else the canonical prompt, `example` = **`json/antifragility.json`**,
`uke_w` = `agent/uke_summary.md`. Validation via `story_generator_base:load_schema` (canonical schema).

## The divergence that bites: the example files differ

`generate_kernel_corpus` shows the model `agent/verification_bottleneck.json`; `c-orchestrator` shows
it `json/antifragility.json`. **`json/antifragility.json` hard-codes `accessibility_collapse: 0.9`,
`resistance: 0.08`** — the exact mountain pattern that B4 stripped from the schema gate. A worked
example carrying a gate-satisfying value re-teaches the stripped rule (see `build_discipline.md`,
Estimator-classifier independence → the few-shot example is a third contamination surface). So:

- **A prompt/schema strip (B3/B4) is fully effective on the `generate_kernel_corpus` path** (clean exemplar).
- **On the `c-orchestrator` path it is undone by `json/antifragility.json`** unless that exemplar is
  scrubbed in the same change. Confirmed: the regen (OQ-46/47) runs through `generate_kernel_corpus`,
  so the example scrub is **hygiene, not a regen precondition** — but only because the leaking exemplar
  is off the regen path.

## Gotchas

- **An env var defeats a file edit.** `DR_SCHEMA` / `DR_GEN_PROMPT` redirect schema/prompt resolution;
  a prompt/schema change has no effect if these point elsewhere in the run environment. Check the env
  before concluding an edit "didn't work."
- **Prompt/schema edits only affect *regenerated* stories.** The existing `testsets/` corpus is frozen
  `.pl`; nothing re-reads the prompt/schema until a generator runs. A behavior-preserving change on
  disk is a future-generation change, not a corpus change.
- **Two generators, two examples — name which one you mean.** "The generation prompt" is shared; "the
  example" is not. A claim about generation behavior is under-specified until the generator is named.
