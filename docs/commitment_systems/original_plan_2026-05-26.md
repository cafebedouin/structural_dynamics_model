*Status (2026-08-06): historical — partially executed; GAPs 2/3/4 closed since. Current spec: `commitment_systems_sketch_v6.md`.*

CS Wiring Audit + Reading Layer Plan                                                                            │
     │                                                                                                                 │
     │ Context                                                                                                         │
     │                                                                                                                 │
     │ Recon pass over the commitment-systems half of the Prolog engine and its pipeline wiring, from                  │
     │ agent/c-orchestrator.py through run_pipeline.py → json_report.pl → pipeline_output.json →                       │
     │ python/enhanced_report.py. Goal: verify what's wired, name what's broken, and identify low-hanging fruit —      │
     │ including the first pass at a "reading" comparison layer where both DR-axis and CS-axis findings about the same │
     │  kernel are surfaced together.                                                                                  │
     │                                                                                                                 │
     │ The two_axis_architecture_v7.md document fixed the reference frame: two deliberately separate axes (observer/DR │
     │  and committer/CS), joined only by the influences-entailment bridge in drl_composition.pl. This audit verifies  │
     │ the implementation matches that frame.                                                                          │
     │                                                                                                                 │
     │ ---                                                                                                             │
     │ Empirical Findings (ground truth for PM handoff)                                                                │
     │                                                                                                                 │
     │ What is wired correctly                                                                                         │
     │                                                                                                                 │
     │ 1. JSON generation → Prolog serialization (python/generate_constraint_pl.py:178–501): All 7 CS field types are  │
     │ serialized to .pl when present — cs_kernel_codification, cs_authority_grounding,                                │
     │ cs_interpretation_layer_present, cs_kernel_id (if _kernel_id present), cs_reading_relation,                     │
     │ cs_axiom/cs_axiom_status/cs_axiom_grounding, cs_reference_frame, cs_drift_state.                                │
     │ 2. stack.pl:19 imports cs_pattern_detection — so pattern/verdict/masking predicates run during the pipeline.    │
     │ 3. json_report.pl:22–23, 474–490, 1195–1213: Per-constraint emits cs_pattern, cs_pattern_signals, cs_verdicts.  │
     │ Corpus-wide emits cs_pattern_distribution (counts + verdict tallies) and cs_grounding_mismatch_count.           │
     │ 4. enhanced_report.py:1970–2013: build_cs_pattern_section() reads those three per-constraint fields and renders │
     │  them in the report. Pattern prose and verdict prose tables are populated.                                      │
     │ 5. prolog/tests/test_forecloses_fpn_injection.pl (new, untracked): Empirically settles the v7 architecture claim that │
     │  forecloses/coexists_with edges cannot enter the contamination network. Four test cases confirm:                │
     │ correct-direction forecloses is gradient-orthogonal (Branch E), reversed injection satisfies P1+P3 but inverts  │
     │ causation (Branch D), coexists_with is label-blind (FPN computes identically to scalar injection), and          │
     │ influences is the only correct bridge. Architecture claim is verified.                                          │
     │                                                                                                                 │
     │ What is NOT wired (the gaps)                                                                                    │
     │                                                                                                                 │
     │ GAP 1 — _kernel_id never injected by orchestrator (agent/c-orchestrator.py:500–560): generate_pl() writes       │
     │ cs_kernel_id/2 only when data.get("_kernel_id") is set. The orchestrator never sets story_dict["_kernel_id"]    │
     │ before calling save_story_tagged(). Result: every orchestrator-generated story with CS fields is missing its    │
     │ kernel link. cs_kernel_registry.cs_readings_for_kernel/2 returns empty for all pipeline-generated kernels,      │
     │ making cs_kernel_divergence/4 and cs_kernel_axiom_conflict/4 inoperable on the main corpus.                     │
     │                                                                                                                 │
     │ GAP 2 — stack.pl doesn't import 4 CS modules: cs_kernel_registry, cs_axiom_engine, cs_drift_engine,             │
     │ cs_drift_mismatch are absent. These predicates are callable standalone but unavailable to any module that loads │
     │  via stack.pl (including json_report.pl, diagnostic_summary.pl, and everything downstream).                     │
     │                                                                                                                 │
     │ GAP 3 — CS drift/axiom/kernel data absent from pipeline_output.json: json_report.pl only surfaces               │
     │ pattern/verdicts. Missing per-constraint: cs_drift_trajectory terminal, cs_axiom_foreclosed status,             │
     │ cs_drift_unacknowledged flag. Missing corpus-wide: kernel divergence counts, axiom conflict distribution        │
     │ (closure vs. plurality vs. neither), drift terminal distribution.                                               │
     │                                                                                                                 │
     │ GAP 4 — enhanced_report.py stale reference (line 2012): Links to docs/commitment_systems_sketch_v4.md — should  │
     │ be commitment_systems/commitment_systems_sketch_v5_2.md.                                                        │
     │                                                                                                                 │
     │ GAP 5 — enhanced_report.py has no extended CS section: Only pattern and verdicts rendered. No drift trajectory, │
     │  no axiom foreclosure, no kernel-divergence info shown in reports.                                              │
     │                                                                                                                 │
     │ GAP 6 — No reading comparison layer: No predicate or report surface jointly presents what both axes say about   │
     │ the same kernel. cs_kernel_divergence/4 and cs_drift_mismatch/2 exist but are standalone-only and not connected │
     │  to pipeline output.                                                                                            │
     │                                                                                                                 │
     │ GAP 7 — abductive_triggers.pl has zero CS imports (noted in observer_diagnostics.md): Axiomatic findings        │
     │ (kernel divergence, axiom conflict, foreclosure) cannot surface as abductive hypotheses. This is the sharpest   │
     │ single integration point — deferred by design per two_axis_architecture_v7.md §open-by-deferral.                │
     │                                                                                                                 │
     │ ---                                                                                                             │
     │ Implementation Plan (low-hanging fruit first)                                                                   │
     │                                                                                                                 │
     │ Step 1 — Fix stale doc reference (1 line)                                                                       │
     │                                                                                                                 │
     │ File: python/enhanced_report.py:2012                                                                            │
     │ # Change:                                                                                                       │
     │ lines.append("  See: docs/commitment_systems_sketch_v4.md")                                                     │
     │ # To:                                                                                                           │
     │ lines.append("  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md")                                │
     │                                                                                                                 │
     │ Step 2 — Add CS module imports to stack.pl (3–4 lines)                                                          │
     │                                                                                                                 │
     │ File: prolog/stack.pl — after the existing cs_pattern_detection import (line 19), add:                          │
     │ :- use_module(cs_kernel_registry, []).                                                                          │
     │ :- use_module(cs_axiom_engine, []).                                                                             │
     │ :- use_module(cs_drift_engine, []).                                                                             │
     │ :- use_module(cs_drift_mismatch, []).                                                                           │
     │ This makes all CS engine predicates available to json_report.pl and diagnostic_summary.pl.                      │
     │                                                                                                                 │
     │ Step 3 — Fix _kernel_id injection in orchestrator                                                               │
     │                                                                                                                 │
     │ File: agent/c-orchestrator.py, in _step_generate(), the entry loop.                                             │
     │                                                                                                                 │
     │ When the manifest's generation_sequence entry is a dict with kernel_id (or the axis has a kernel_id field),     │
     │ inject it into story_dict before saving:                                                                        │
     │ # After story_dict is validated (before save_story_tagged call):                                                │
     │ kernel_id = None                                                                                                │
     │ if isinstance(entry, dict):                                                                                     │
     │     kernel_id = entry.get("kernel_id")                                                                          │
     │ if kernel_id is None:                                                                                           │
     │     kernel_id = axis.get("kernel_id")                                                                           │
     │ if kernel_id:                                                                                                   │
     │     story_dict["_kernel_id"] = kernel_id                                                                        │
     │ This requires auditing the UKE_SCOPE manifest structure to confirm where kernel_id is placed in the             │
     │ generation_sequence entries — likely in the kernel-mode generation sequence the manifest already supports (line │
     │  430 handles dict entries).                                                                                     │
     │                                                                                                                 │
     │ Step 4 — Add CS drift/axiom/kernel fields to json_report.pl per-constraint                                      │
     │                                                                                                                 │
     │ File: prolog/json_report.pl, in the per-constraint JSON emitter (after the cs_verdicts block, ~line 490).       │
     │                                                                                                                 │
     │ Add (guarded by cs_has_fields(C) and catches):                                                                  │
     │ % cs_drift_trajectory terminal                                                                                  │
     │ (   catch(cs_drift_engine:cs_drift_trajectory(C, _, Terminal), _, fail)                                         │
     │ ->  format(S, '      "cs_drift_terminal": "~w",~n', [Terminal])                                                 │
     │ ;   format(S, '      "cs_drift_terminal": null,~n', [])                                                         │
     │ ),                                                                                                              │
     │ % cs_axiom_foreclosed                                                                                           │
     │ (   catch(cs_axiom_engine:cs_axiom_foreclosed(C, AxAtom), _, fail)                                              │
     │ ->  format(S, '      "cs_axiom_foreclosed": "~w",~n', [AxAtom])                                                 │
     │ ;   format(S, '      "cs_axiom_foreclosed": null,~n', [])                                                       │
     │ ),                                                                                                              │
     │ % cs_drift_unacknowledged                                                                                       │
     │ (   catch(cs_pattern_detection:cs_drift_unacknowledged(C, _), _, fail)                                          │
     │ ->  format(S, '      "cs_drift_unacknowledged": true~n', [])                                                    │
     │ ;   format(S, '      "cs_drift_unacknowledged": false~n', [])                                                   │
     │ )                                                                                                               │
     │                                                                                                                 │
     │ Step 5 — Add corpus-wide CS trifurcation stats to json_report.pl                                                │
     │                                                                                                                 │
     │ File: prolog/json_report.pl, in the corpus summary section (~line 1213), after cs_grounding_mismatch_count.     │
     │                                                                                                                 │
     │ Add a cs_trifurcation_summary block mirroring the logic already in cs_corpus_analysis:cs_trifurcation_profile/0 │
     │  (lines 83–185), but emitting JSON instead of stdout text. Key fields:                                          │
     │ - cs_drift_terminal_distribution: map of terminal atom → count                                                  │
     │ - cs_kernel_divergence_count: N distinct reading-pair divergences                                               │
     │ - cs_kernels_with_divergence: N kernels                                                                         │
     │ - cs_axiom_conflict_total, cs_axiom_real_closure, cs_axiom_licensed_plurality                                   │
     │ - cs_drift_unacknowledged_count                                                                                 │
     │ - cs_axiom_foreclosed_count                                                                                     │
     │                                                                                                                 │
     │ Pattern: enumerate AllKernels (same findall as cs_corpus_analysis.pl:105) and call the existing predicates.     │
     │                                                                                                                 │
     │ Step 6 — Expand enhanced_report.py with CS extended section                                                     │
     │                                                                                                                 │
     │ File: python/enhanced_report.py                                                                                 │
     │                                                                                                                 │
     │ Add build_cs_extended_section(constraint_id, pipeline_data):                                                    │
     │ def build_cs_extended_section(constraint_id, pipeline_data):                                                    │
     │     """CS drift/axiom findings — L2 supplement to build_cs_pattern_section."""                                  │
     │     entry = find_constraint_entry(pipeline_data, constraint_id)                                                 │
     │     if entry is None or entry.get("cs_pattern") is None:                                                        │
     │         return None                                                                                             │
     │                                                                                                                 │
     │     terminal = entry.get("cs_drift_terminal")                                                                   │
     │     foreclosed = entry.get("cs_axiom_foreclosed")                                                               │
     │     unack = entry.get("cs_drift_unacknowledged", False)                                                         │
     │                                                                                                                 │
     │     if not any([terminal, foreclosed, unack]):                                                                  │
     │         return None                                                                                             │
     │                                                                                                                 │
     │     lines = ["", "--- COMMITMENT SYSTEM TEMPORAL STATUS ---", ""]                                               │
     │     if terminal:                                                                                                │
     │         lines.append(f"  Drift trajectory terminal: {terminal}")                                                │
     │     if foreclosed:                                                                                              │
     │         lines.append(f"  Axiom foreclosed: {foreclosed}")                                                       │
     │     if unack:                                                                                                   │
     │         lines.append("  ⚠ Drift unacknowledged: authority has not absorbed substantial/severe drift")           │
     │     return "\n".join(lines)                                                                                     │
     │                                                                                                                 │
     │ Wire into generate_report_sections() after l2_cs_pattern (line 2437).                                           │
     │                                                                                                                 │
     │ Step 7 — Reading comparison layer design (first pass)                                                           │
     │                                                                                                                 │
     │ The "reading" layer is a kernel-scoped cross-axis view. Minimum viable implementation:                          │
     │                                                                                                                 │
     │ In cs_corpus_analysis.pl: Add cs_reading_comparison_report/0 (new predicate, extends run_cs_corpus_analysis).   │
     │ For each kernel K with ≥2 readings, emit:                                                                       │
     │ - DR axis per reading: type at each context from classify_at_time/4                                             │
     │ - CS axis per reading: cs_pattern, cs_drift_terminal, cs_axiom_foreclosed                                       │
     │ - Divergence pairs (from cs_kernel_divergence/4)                                                                │
     │ - Mismatch detection (from cs_drift_mismatch/2)                                                                 │
     │                                                                                                                 │
     │ In json_report.pl corpus summary: Add cs_kernel_comparison array — one entry per kernel with its readings'      │
     │ per-axis findings.                                                                                              │
     │                                                                                                                 │
     │ In enhanced_report.py: Add build_kernel_reading_section(constraint_id, pipeline_data) — if the constraint       │
     │ belongs to a kernel (check cs_kernel_id presence), render the cross-axis comparison from the kernel entry in    │
     │ pipeline_output.json.                                                                                           │
     │                                                                                                                 │
     │ The reading layer does NOT merge the axes — it surfaces both axes' findings side by side and explicitly names   │
     │ agreements and disagreements. The cs_drift_mismatch case (DR-stable but CS-foreclosed) is the primary           │
     │ cross-axis diagnostic object to feature.                                                                        │
     │                                                                                                                 │
     │ ---                                                                                                             │
     │ Verification                                                                                                    │
     │                                                                                                                 │
     │ After implementation, the end-to-end test is:                                                                   │
     │ # 1. Prolog engine picks up new CS modules                                                                      │
     │ cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"                       │
     │                                                                                                                 │
     │ # 2. Pipeline produces CS trifurcation fields in pipeline_output.json                                           │
     │ python3 python/run_pipeline.py                                                                                  │
     │ grep -c "cs_drift_terminal" outputs/pipeline_output.json                                                        │
     │                                                                                                                 │
     │ # 3. Enhanced report for a CS constraint shows extended section                                                 │
     │ python3 python/enhanced_report.py <cs_constraint_id>                                                            │
     │ grep "COMMITMENT SYSTEM TEMPORAL STATUS" outputs/constraint_reports/<id>_report.md                              │
     │                                                                                                                 │
     │ # 4. CS corpus analysis standalone still works                                                                  │
     │ cd prolog && swipl -g "[cs_corpus_analysis], run_cs_corpus_analysis, halt" -t "halt(1)"                         │
     │                                                                                                                 │
     │ ---                                                                                                             │
     │ Critical Files Modified                                                                                         │
     │                                                                                                                 │
     │ - prolog/stack.pl — add 4 imports (step 2)                                                                      │
     │ - agent/c-orchestrator.py — _step_generate() kernel_id injection (step 3)                                       │
     │ - prolog/json_report.pl — per-constraint drift/axiom fields + corpus trifurcation block (steps 4–5)             │
     │ - python/enhanced_report.py — stale ref fix + new extended CS section + wire call (steps 1, 6)                  │
     │ - prolog/cs_corpus_analysis.pl — reading comparison section (step 7, optional first pass)                       │
     │                                                                                                                 │
     │ What NOT to do                                                                                                  │
     │                                                                                                                 │
     │ - Do not feed forecloses/coexists_with edges into the contamination network (proven unsafe by                   │
     │ test_forecloses_fpn_injection.pl; the architecture note is now empirically settled)                             │
     │ - Do not add a CS trigger to abductive_triggers.pl yet — that's the sharpest integration point, open by design  │
     │ per v7                                                                                                          │
     │ - Do not reconcile cs_drift_mismatch disagreements — the disagreement must remain representable  
