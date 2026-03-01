## Project Context

Prolog+Python research infrastructure implementing Deferential Realism (DR).
76 Prolog modules, 910 testsets, Python orchestration/linting, Streamlit UI.

Key constraint: Correctness and reproducibility matter most. Model provenance
(which LLM built which testsets) is a feature, not a problem.

## Running the System

- Prolog tests: `cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"`
- Linter: must be imported as library (`from linter import lint_file`), no __main__ block
- Config sensitivity: `python3 python/config_sensitivity_sweep.py`
- Directionality sensitivity: `python3 python/directionality_sensitivity_sweep.py`

## Known State (2026-02-28)

- 910/0 tests passing (requires data_repair.pl loaded)
- 170 config params (154 numeric, swept; 17 directionality constants, swept separately; all inert at ±25%)
- See AUDIT.md for full findings, MEMORY.md for project history

## Architecture Invariants

- All classification routes through classify_from_metrics/6 in drl_core.pl
- config.pl is single source of truth for param/2 facts
- Dual threshold: both χ AND ε must be checked
- .tsx artifacts are outputs, not infrastructure
- Archive testsets document build provenance, not active code
