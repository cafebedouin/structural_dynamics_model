# Pipeline Infrastructure Discovery Notes

## Invocation Patterns

### Enhanced Reports

```bash
# From repo root:
python3 python/enhanced_report.py <constraint_id>           # single
python3 python/enhanced_report.py foo bar baz               # multiple
python3 python/enhanced_report.py                           # auto: testsets modified in last hour
```

Output: `outputs/constraint_reports/{constraint_id}_report.md`

Dependencies loaded by enhanced_report.py:
- `outputs/enriched_pipeline.json`
- `outputs/orbit_data.json`
- `outputs/enriched_omega_data.json`
- `outputs/corpus_data.json`
- `outputs/maxent_report.md`
- `outputs/pattern_mining.md`
- `outputs/covering_analysis.md`

### Corpus Update

```bash
# From repo root:
make -j4 quick
```

Runs the full Prolog analysis pipeline (140+ output files) without quality gates. Generates all upstream data that enhanced_report.py depends on.

### Prolog Scenario Invocation

```bash
# From prolog/ directory:
swipl -l stack.pl -l report_generator.pl -g "run_scenario('testsets/{id}.pl', '{id}'), halt."
```

### Constraint Story Generation

The `agent/story_generator_base.py` module provides shared infrastructure:
- `build_prompt()` assembles generation prompt + schema + example
- `process_response()` strips fences, parses JSON, validates against schema
- `save_story()` compiles JSON to .pl, lints, and writes both files
- `_get_client()` lazily initializes Gemini client from `GOOGLE_API_KEY` / `GEMINI_API_KEY`
- `retry_with_backoff()` handles 429/503 with exponential backoff

### Output Paths

| Artifact | Path |
|----------|------|
| Constraint story JSON | `json/{constraint_id}.json` |
| Compiled Prolog testset | `prolog/testsets/{constraint_id}.pl` |
| Enhanced report | `outputs/constraint_reports/{constraint_id}_report.md` |
| Essay | `outputs/essays/{family_id}.md` |
| Pipeline outputs | `outputs/` (enriched_pipeline.json, orbit_data.json, etc.) |

### Agent Module Layout

All `agent/` scripts are run from the repo root:
```bash
python3 agent/orchestrator.py "topic"
python3 agent/generate_text.py
```

No `__init__.py` needed — imports use `from agent.story_generator_base import ...` which works when the repo root is the working directory.

## Verdict-Driven Iteration Loop (Step 5.5)

### Design

After enhanced reports are generated (step 5), the pipeline inspects each report's verdict. Non-GREEN verdicts trigger a feedback loop where the LLM adjusts the constraint story JSON to resolve diagnostic tensions, up to 3 iterations per axis. The adjusted story is saved (overwriting .json and .pl), the report is re-run via `enhanced_report.py`, and the new verdict is checked.

### Stopping Heuristics

| Condition | Action |
|-----------|--------|
| GREEN verdict | Skip iteration (already clean) |
| RED verdict | Always iterate (structurally wrong) |
| YELLOW with only expected-conflict codes, no tensions | Skip (analytically correct) |
| YELLOW with convergent rejections | Iterate |
| YELLOW with confidence < 0.3 | Iterate |
| YELLOW otherwise with tensions | Iterate |
| YELLOW after iteration >= 2 | Accept and stop (avoids chasing perfection) |
| 3 iterations exhausted | Proceed with best result |

Expected conflict codes (safe to ignore):
- `cohomological_fracture_divergence`
- `perspectival_orbit_variance`
- `constructed_non_compliance`
- `fcr_deferred_signature_mismatch`

### Token Budget

Each iteration calls the architect model (gemini-2.5-pro) with the full story JSON + tension context. Expect ~2k tokens in, ~4k tokens out per iteration. Worst case: 3 axes × 3 iterations = 9 LLM calls (~54k tokens).

### Stale Batch Data Trade-off

During iteration, `enhanced_report.py` re-runs the Prolog analysis stack fresh on the updated .pl file, but corpus-level batch data (enriched_pipeline.json, orbit_data.json) remains from the last `make quick` run. This is acceptable because core diagnostic signals (classification, drift, purity, coupling) come from per-constraint Prolog analysis, not corpus statistics. Running `make quick` during iteration would add ~60s per cycle.
