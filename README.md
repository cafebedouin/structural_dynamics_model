# Deferential Realism

**A framework for systems where what you see depends on where you stand, which commitment you
are looking at, and how its holder stands toward it**

Deferential Realism (DR) is a philosophical framework with a working measurement engine. Its
subject is constraints — the rules, arrangements, and structures that bind people, from labor
law to doctrinal commitments to physical limits. The framework is *realist* about structural
properties (extractiveness, suppression, coordination function exist independently of any
observer) and *deferential* about classification (what a constraint *is* to you depends
irreducibly on your structural position). The engine — a Prolog rule cascade with a Python
orchestration layer — implements the axioms computationally and measures the dependence
instead of resolving it: perspectival disagreement is the signal, not a defect. The engine
makes the philosophy falsifiable, but the engine is not the point. The philosophy is the point.

**Start with the paper:** [`docs/deferential_realism_paper_v8.md`](docs/deferential_realism_paper_v8.md)
— the entry point and canonical vocabulary. Its closing Appendix states the current state of
the project plainly, with no version history. This README is the repository tour.

---

## The Intellectual Arc

The project did not begin as a framework. It began with an origin artifact (a multi-model
relay fiction about a care subroutine that outlived every recipient of its care — a
constructed constraint become indistinguishable, from inside, from natural law), a set of
children's stories in which mathematical invariants play as unbending rules that *create*
freedom, and a debugging paper that classified paradoxes by generative mechanism and routed
each type to a different fix. Apply that method — classify by mechanism, index by position,
route rather than adjudicate — to social constraints, where position-dependence is structure
rather than a bug in the question, and you get this framework: first an observer axis (v6),
then a co-equal committer axis (v7), then the unifying ontology of seat, gauge, and
orientation (v8). The full arc, with the readings owned and the sources cited, is v8 §2.

---

## The Framework in Twenty Lines

**One content-seat, gauge-rotated and orientation-audited.** Every contentful question about a
constraint has exactly one **seat** (the reference reality the verdict is audited against —
the authored metric reality of who benefits and what binds), many **gauges** (standpoints the
content is read from — four canonical positions: powerless, moderate, institutional,
analytical), and an **orientation** per commitment-holder (how the selection is presented, and
whether it is kept honestly over time). The audit is one-directional — presentation is checked
against the metric reality, never the reverse — and a machine-checked guard in the standing
gate enforces it.

**The observer axis.** χ = ε × f(d) × σ(S): one formula, evaluated at four positions,
generates six types — mountain, rope, tangled_rope, snare, scaffold, piton. Extraction is
never visible from every position (the beneficiary's position is structurally the blind one);
cross-position disagreement is quantized (with four real positions, disagreement counts 1 and
2 cannot occur); no single position sees everything.

**The committer axis.** Contested topics (kernels) carry rival readings; each declares its
reference frame, drift state, and premise groundings, and the engine *computes* the
consequences: drift's terminal attractor, premise foreclosure, and whether two contradictory
readings are licensed plurality or real closure. Perspectival health and structural viability
are independent measurements (Theorem 7) — a reading can look coherent from every position
while resting on a premise its own evidential commitments have foreclosed.

Details, proofs, corrections, and the honest assessment: v8 §§3–5 and §9, and the record
papers it cites.

---

## Repository Structure

```
structural_dynamics_model/
├── prolog/                # The engine (125 top-level modules, 2026-07-02)
│   ├── stack.pl           #   module loader ([stack] alone loads NO testsets)
│   ├── config.pl          #   single source of truth for all parameters
│   ├── drl_core.pl        #   classifier (classify_from_metrics/6)
│   ├── constraint_indexing.pl  # χ = ε × f(d) × σ(S)
│   ├── signature_detection.pl  # signature layer (false_natural_law, FCR, FSM …)
│   ├── corpus_loader.pl   #   corpus loading (corpus_constraint/1 = membership)
│   ├── check_axis_boundary.pl  # the one-seat taint guard (v8 §5.6)
│   ├── cs_*.pl            #   committer-axis modules (kernels, axioms, drift)
│   ├── testsets/          #   LIVE working corpus (deliberately small test bed)
│   ├── testsets_haiku/    #   twin corpus (stable comparison baseline)
│   ├── testsets_flash/    #   twin corpus (stable comparison baseline)
│   ├── tests/             #   plunit engine tests
│   └── archives/datasets/ #   pre-reset corpora (kernel_v1, original_v5/v6, sotu …)
├── python/                # Analysis layer (120 top-level scripts, 2026-07-02)
│   ├── cli.py             #   discover/run any tool: python3 python/cli.py list
│   ├── run_pipeline.py    #   full corpus re-classification
│   ├── enhanced_report.py #   per-constraint reports
│   └── omega_resolver.py  #   ISSUES.md open-questions resolver (menu/check/index)
├── agent/                 # Orchestrators and UKE protocol suite
│   ├── c-orchestrator.py  #   primary authoring loop (topic → corpus → reports)
│   ├── uke_narrative_orchestrator.py  # narrative resleeving (air-gapped)
│   └── narrative_transform/, analysis/  # protocols, run artifacts
├── docs/                  # Papers and framework documentation
│   ├── deferential_realism_paper_v8.md      # ← START HERE (canonical)
│   ├── deferential_realism_paper_v7.md      # committer-axis record
│   ├── deferential_realism_paper_v6.13.1.md # observer-axis record
│   ├── seat-theorem-v1.md                   # the law (Coupling Theorem)
│   ├── logic.md           #   formal classification rules (spec for config.pl)
│   ├── design/            #   design specs, design_discipline, design_gaps
│   ├── technical/         #   wiring notes, build_discipline, gotchas
│   └── v8/                #   v8 source material (origin, math stories, foundations)
├── audits/                # One dated dir per empirical investigation (evidence + writeup)
├── outputs/               # Generated artifacts (pipeline_output.json + manifest; gitignored)
├── json/ json_haiku/ json_flash/  # LLM-authored constraint specs (inputs, not analysis)
├── issues/                # Generated ISSUES.md router (INDEX.md — never authoritative)
├── schemas/ prompts/ protocols/ essays/ examples/ results/ scripts/ archives/
├── CLAUDE.md              # Working instructions (agents and humans)
├── AGENTS.md              # Onboarding: architecture, testing, conventions
├── ISSUES.md              # THE single tracking surface (OQ-NN open questions)
└── KNOWN_STATE.md         # Dated session changelog of witnessed findings
```

---

## Running the System

```bash
# Corpus validation suite
cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"

# Full analysis pipeline (no generation; re-classifies the corpus)
python3 python/run_pipeline.py

# Discover and run any Python tool
python3 python/cli.py list

# Primary authoring loop: topic → readings → corpus → reports → tensions ledger
python3 agent/c-orchestrator.py "some topic"

# Project gate (tracker grammar, resolver selftests, axis-boundary guard, …)
./scripts/gate.sh
```

Two loading facts that save an hour each: `[stack]` alone loads **zero** testsets (call
`corpus_loader:load_all_testsets` explicitly), and computed results live in
`outputs/pipeline_output.json` — read the pre-computed values, don't recompute. Deeper wiring
notes: `docs/technical/` (read the relevant file before modifying anything it names).

---

## The Corpus

Constraint stories are LLM-authored readings of topics, generated by the orchestrator
pipeline, compiled to Prolog testsets, and classified by the engine. Three live legs: the
working test bed `prolog/testsets/` (deliberately small and singleton-per-topic while schema
and wiring evolve — this is intended, not unfinished) and two reconciled twin corpora
(`testsets_haiku/`, `testsets_flash/`) that serve as the stable comparison baseline. The live
corpus was **reset on 2026-06-05**; everything earlier is archived under
`prolog/archives/datasets/` (kernel_v1, original_v5/v6, the SOTU set) and remains available
for retrospective sweeps.

**Citation policy, stated once:** the corpus continuously extends, so its size is meaningful
only relative to a timestamp. Never quote a remembered count — read
`outputs/pipeline_output.json` → `manifest.n_constraints` (and cite the manifest's
`pipeline_run_at` with it). Generation is stochastic and the committed story is the
determinism frontier: re-generating a topic yields a *new reading*, not a re-measurement, so
corpus statistics are one sample conditional on one generation (v8 §3.2, §7.2).

---

## Documentation Map

| Layer | Where |
|---|---|
| Entry point / canonical vocabulary | `docs/deferential_realism_paper_v8.md` (Appendix = current state, plainly) |
| Detailed records | `docs/deferential_realism_paper_v7.md` (committer axis) · `docs/deferential_realism_paper_v6.13.1.md` (observer axis) |
| The law | `docs/seat-theorem-v1.md` + `docs/one_seat_audited.md`, `docs/choosing-the-seat-well-v0_2.md`, `docs/the-few-seats-worth-choosing-v2.md` |
| Formal rules / calibration | `docs/logic.md` (spec) + `prolog/config.pl` (single source of truth) |
| Operations | `docs/project_orientation.md` · `AGENTS.md` · `CLAUDE.md` |
| Method | `docs/technical/build_discipline.md` (the six defect patterns) · `docs/design/design_discipline.md` · `docs/design/design_gaps.md` |
| Open questions | `ISSUES.md` (single tracker; `python3 python/omega_resolver.py menu` for what's workable) |
| Evidence | `audits/` (one dated directory per investigation) |

---

## Status

Alpha, working toward beta (as of 2026-07-02). The engine and both axes are built and
validated; the corpus stays deliberately small while schema and wiring evolve; correctness and
reproducibility are prioritized over growth. The one-seat invariant is machine-enforced in the
standing gate. Main open obligations: the general-n disagreement-spectrum proof (OQ-195) and
the ε declaration discipline (OQ-205; v8 §6.4). The full frontier lives in `ISSUES.md`.

Essays produced with the pipeline publish to [cafebedouin.org](https://cafebedouin.org).

---

## Contributing

Research infrastructure under active development, not production software.

**Valuable:** new constraint stories on novel domains (validated through the engine);
falsification tracking (document where predictions fail); real-world validation data
(expert-assigned metrics in domains you know); cross-framework comparison.

**Please don't:** rewrite the engine in Python (the logic-programming substrate is
load-bearing); add constraint types without theoretical justification; optimize for
user-friendliness before methodology is validated.

---

## Citation

```bibtex
@software{deferential_realism_2026,
  author = {cafebedouin},
  title = {Deferential Realism: Seat, Gauge, Orientation — a Two-Axis
           Framework for Observer-Dependent Constraint Classification},
  year = {2026},
  publisher = {GitHub},
  url = {https://github.com/cafebedouin/structural_dynamics_model},
  note = {Prolog engine + Python analysis layer; paper v8.0
          (docs/deferential_realism_paper_v8.md); corpus size is
          read from the pipeline manifest, not a frozen count.}
}
```

---

## License

**Creative Commons Zero v1.0 Universal**

Copy, modify, and distribute freely, including commercially, without asking. If you improve
the methodology, consider publishing your refinements.

---

**Last updated:** 2026-07-02
