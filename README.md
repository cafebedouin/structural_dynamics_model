# Structural Dynamics Model

This repository contains the formalization of structural dynamics through symbolic reasoning.

## Core: Prolog Engine & Datasets
The primary output and focus of this repository are the **Prolog files** located in the `prolog/` directory. These files represent a formal ontology and validation framework for analyzing constraint dynamics across multiple domains.

- **Ontology & Logic:** `prolog/drl_core.pl`, `prolog/drl_modal_logic.pl`, and `prolog/structural_signatures.pl` define the core reasoning engine.
- **Datasets:** The `prolog/testsets/` directory contains dozens of formalized cases—from financial collapses (Lehman Brothers) to technical specifications (TCP/RFC 9293) to historical narratives.
- **Validation:** Tools like `prolog/validation_suite.pl` and `prolog/test_harness.pl` ensure the structural integrity of the model.

## Research Progression: Documentation
The `docs/` directory tracks the conceptual evolution of the model. It contains the theoretical foundations, protocol specifications, and philosophical underpinnings that led to the current Prolog implementation.

- **Core Principles:** `docs/deferential_realism_core_v3.1.md` and `docs/entropy_engine_v3.1.md`.
- **Framework Evolution:** The various `uke_*.md` files and protocol guides document the iterative development of the "Universal Knowledge Engine" (UKE) and its transition into a formal structural model.

## Supporting Infrastructure
The remaining directories provide the necessary "odds and ends" to manage, test, and generate data for the model:
- `python/`: Orchestration scripts for batch processing and audit workflows.
- `scripts/`: Utility shell scripts for data cleaning and syntax normalization.
- `prompts/`: LLM prompts used to assist in the generation and formalization of new datasets.

## Quick Start
To run the validation suite (requires SWI-Prolog):
```bash
cd prolog
swipl -g "[validation_suite]." -g "test_all." -t halt
```

## License
Creative Commons Zero v1.0 Universal