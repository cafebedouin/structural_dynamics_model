# Prolog Structural Dynamics Engine

This directory contains the core symbolic reasoning engine and formal datasets for the Structural Dynamics Model.

## Directory Structure

- **`/testsets`**: The primary data repository. Contains dozens of `.pl` files representing formalized structural analyses of diverse domains (e.g., `lehman_repo_105.pl`, `rfc9293_state_machine.pl`, `gale_shapley.pl`).
- **Core Logic**:
    - `drl_core.pl`: The foundational axioms and rules of Deferential Realism.
    - `drl_modal_logic.pl`: Modal operators for handling belief, necessity, and constraint states.
    - `structural_signatures.pl`: Pattern definitions for identifying specific structural archetypes.
- **Engines & Managers**:
    - `intent_engine.pl`: Handles teleological reasoning and agent goals.
    - `narrative_ontology.pl`: Maps structural constraints to narrative arcs.
    - `scenario_manager.pl`: Orchestrates complex multi-stage simulations.
- **Validation & Testing**:
    - `validation_suite.pl`: The main entry point for verifying dataset integrity.
    - `test_harness.pl`: Framework for running automated unit and integration tests.
- **Bridges & Configs**:
    - `constraint_bridge.pl`: Connects abstract constraints to specific domain instances.
    - `v3_1_config.pl`: Global configuration and versioning for the v3.1 stack.

## Getting Started

### Prerequisites
- [SWI-Prolog](https://www.swi-prolog.org/) (8.x or higher recommended)

### Running Validation
To validate all datasets against the core ontology:
```bash
swipl -g "[validation_suite]." -g "test_all." -t halt
```

### Querying a Specific Dataset
To load the engine and a specific case study (e.g., Lehman Brothers):
```bash
swipl -l drl_core.pl -l testsets/lehman_repo_105.pl
```

Once inside the SWI-Prolog REPL, you can run queries:
```prolog
% Find all active constraints in the Lehman collapse
?- constraint_active(C, Status).
```

## Dataset Conventions
Each file in `testsets/` should follow the naming convention and predicate structure defined in `drl_core.pl` to ensure compatibility with the validation suite and pattern analysis tools.
