# Quick Start

This guide covers the primary workflows for interacting with the Deferential Realism research infrastructure.

## Prerequisites

- **SWI-Prolog** (for the classification engine)
- **Python 3.10+** (for the analytical pipeline)
- **API Access** (Gemini for generation, Haiku for research, optional)

## The Primary Workflow: The Orchestrator

The most efficient way to generate and analyze new constraints is via the orchestrator. This automates the full lifecycle from research to essay synthesis.

```bash
python3 agent/c-orchestrator.py "some topic or domain"
```

**What the Orchestrator does:**
1. **Research:** Web search grounding via Haiku.
2. **Decompose:** UKE_SCOPE protocol selects axes and produces a manifest.
3. **Generate:** Sonnet generates constraint stories; saves JSON to `json/` and Prolog to `prolog/testsets/`.
4. **Corpus Update:** Runs the analysis pipeline to re-classify the full 3,337-constraint corpus.
5. **Reports:** Generates enhanced structural reports for the new constraints.
6. **Essay:** Synthesizes a draft essay from the structural findings.

After the run, review `outputs/constraint_reports/*.md` and the draft essay in `outputs/essays/`.

## Running the Analysis Pipeline

To run the analysis pipeline on the existing corpus without generating new stories:

```bash
python3 python/run_pipeline.py
```

This updates `outputs/pipeline_output.json` with the latest classifications, H¹ values, and structural metrics across the 3,337 main-corpus and 189 SOTU-derived constraints.

## Running the Prolog Engine

To verify the engine's internal consistency and run the validation suite:

```bash
cd prolog
swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"
```

Current state: 910/0 tests passing.

## Manual Constraint Authoring (Layer 1 & 2)

If you prefer to author constraints manually:

1. **Generation:** Use the prompt in `prompts/constraint_story_generation_prompt.md` with an LLM.
2. **Placement:** Save the resulting Prolog file to `prolog/testsets/`.
3. **Validation:** Run the Prolog test harness to check for schema compliance:
   ```prolog
   ?- [validation_suite].
   ?- run_scenario('./testsets/your_file.pl', your_id).
   ```

## Synthesizing Analysis (Layer 3)

The final step is synthesizing high-level insights from the structural data.

1. Capture the executive summary from the orchestrator or enhanced reports.
2. Use the UKE_W protocol (`protocols/uke_write_v2.2.md`) to guide an LLM in synthesizing a formal essay.
3. The result is a "Deferential Realist" analysis that traces every claim back to the structural invariants surfaced by the engine.

---

**Last updated:** May 9, 2026
**Status:** Sync with Corpus v3,337 | Paper v6.12
