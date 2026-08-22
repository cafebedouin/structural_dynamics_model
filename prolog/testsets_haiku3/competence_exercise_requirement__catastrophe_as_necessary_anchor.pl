% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-Anchored Competence Maintenance in High-Reliability Organizations
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, surgery) face a
 *   persistent claim that human competence in safety-critical roles decays
 *   during long periods without real catastrophic or near-catastrophic
 *   events. This reading instantiates one interpretation of a contested
 *   kernel: the claim that only actual jeopardy—not simulation, not dry
 *   exercises—irreplaceably maintains the intuitive, embodied competence
 *   required to handle novel real-world emergencies. Under this reading,
 *   simulation is declared structurally insufficient; long-safe organizations
 *   must either stage real-world exercises with actual jeopardy or accept a
 *   narrative of atrophied readiness. The constraint extracts a transfer from
 *   simulation investors and safe organizations to regulatory regimes and
 *   real-world-test advocates. Two sibling readings dispute this: one argues
 *   simulation is adequate with sufficient fidelity and debriefing; another
 *   argues both are necessary. The competence kernel persists across all
 *   three readings; the readings differ on what the kernel requires.
 *
 * KEY AGENTS:
 *   - Competence maintenance regime: sets the standard that only catastrophe/near-catastrophe sustains competence; no direct cost
 *   - Real-world test advocates (powerful seat): benefit from the regime's validation; point to incident analysis as evidence
 *   - Simulation investors (constrained, organized): bear cost of de-valuation of simulation investments; forced into expensive real-world exercises
 *   - Long-safe organizations (moderate power, constrained exit): pressured to prove competence through jeopardy-events or accept oversight escalation
 *   - Competence researchers (observer seat): provide evidence on both sides; their findings are filtered through the institutional framing
 *   - Regulatory bodies (institutional, beneficiary/agenda-setter): use the regime to justify oversight and mandatory incident exercises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.72).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-Anchored Competence Maintenance in High-Reliability Organizations").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'd1ec7274-a036-4857-a0ff-72361aca1f58').
narrative_ontology:cs_kernel_codification('d1ec7274-a036-4857-a0ff-72361aca1f58', distributed).
narrative_ontology:cs_authority_grounding('d1ec7274-a036-4857-a0ff-72361aca1f58', extraction).
narrative_ontology:cs_interpretation_layer_present('d1ec7274-a036-4857-a0ff-72361aca1f58').
narrative_ontology:cs_reading_relation('d1ec7274-a036-4857-a0ff-72361aca1f58', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_reading_relation('d1ec7274-a036-4857-a0ff-72361aca1f58', competence_exercise_requirement__simulation_as_adequate_exercise, influences).
narrative_ontology:cs_axiom('d1ec7274-a036-4857-a0ff-72361aca1f58', foundational, catastrophe_irreducible_learning_mechanism).
narrative_ontology:cs_axiom_status(catastrophe_irreducible_learning_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('d1ec7274-a036-4857-a0ff-72361aca1f58', catastrophe_irreducible_learning_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('d1ec7274-a036-4857-a0ff-72361aca1f58', foundational, simulation_cannot_substitute_jeopardy_response).
narrative_ontology:cs_axiom_status(simulation_cannot_substitute_jeopardy_response, holdable).
narrative_ontology:cs_axiom_grounding('d1ec7274-a036-4857-a0ff-72361aca1f58', simulation_cannot_substitute_jeopardy_response, empirically_contingent).
narrative_ontology:cs_reference_frame('d1ec7274-a036-4857-a0ff-72361aca1f58', jeopardy_as_competence_anchor).
narrative_ontology:cs_drift_state('d1ec7274-a036-4857-a0ff-72361aca1f58', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d1ec7274-a036-4857-a0ff-72361aca1f58', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_maintenance_regime).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, real_world_test_advocates).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_investment_constituencies).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_in_long_safe_periods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and adjudicates the claim that only catastrophic or near-catastrophic events provide the irreducible exercise required to sustain operational competence. Sets the standards for what counts as 'adequate' competence testing. Controls the framing that links safety to real-world jeopardy events. Does not itself bear the cost of awaiting or engineering such events.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_maintenance_regime, agenda_setter,
    institutional, civilizational, analytical, global).

% Operators, safety boards, and researchers who argue that simulation alone produces a false confidence in competence—that muscle memory and intuitive response decay without real stakes. They benefit from the regime's validation of their position in resource allocation and regulatory design decisions. They point to incidents where operators trained only in simulation failed to improvise when the actual event differed from the sim.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, real_world_test_advocates, beneficiary,
    powerful, generational, mobile, global).

% Airlines, training programs, regulatory bodies that have invested heavily in high-fidelity simulation as the primary competence maintenance tool. They bear the cost of the regime's claim that simulation is insufficient—their investment is devalued, and they are pressured to maintain readiness through expensive real-world operations (line checks, actual landings in marginal weather, non-jeopardy emergencies staged on actual aircraft). The constraint creates a deferred-cost regime in which simulation alone is declared inadequate, forcing continued expensive real-world exposure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_investment_constituencies, payer,
    organized, biographical, constrained, national).

% Organizations (airlines, nuclear plants, hospitals) that have operated successfully for years or decades without catastrophic events. They face pressure from the regime to stage costly real-world exercises or accept the declared risk of atrophied competence. The regime extracts a transfer: if you have not had a real event recently, you must either engineer a near-miss exercise (real cost, real jeopardy) or accept a narrative of degraded readiness that justifies external oversight and mandated training escalation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_in_long_safe_periods, payer,
    moderate, biographical, constrained, national).

% Cognitive scientists, human factors researchers, and educators who study how expertise decays and is maintained. They analyze whether the catastrophe-necessity claim is empirically true or a narrative that conflates two separable mechanisms: (1) the physiological basis of muscle memory under real stress, and (2) the scenario-diversity problem—whether simulation covers the space of possible real-world contingencies. They testify in regulatory proceedings.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_researchers, observer,
    moderate, biographical, analytical, global).

% Government agencies (FAA, NRC, etc.) that use the catastrophe-necessity framing to justify increased oversight, real-world audit regimes, and mandatory incident-response exercises on operational systems. The regime gives them grounds to mandate costly interventions and to claim that compliance requires continuous real-world stress exposure—even in safe periods, proving the organization's competence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_bodies, agenda_setter).

% Individuals harmed or nearly harmed in real catastrophic events. They would argue whether the competence failure was due to atrophied muscle memory (supporting the regime) or due to simulation gaps that real-world events cannot solve (supporting hybrid or simulation-adequate readings). Their voice is rarely heard in policy design; they are presented as post-hoc evidence rather than consulted as claimants.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, incident_survivors, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_maintenance_regime).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational readiness and safety by anchoring competence standards to a claim about how human expertise decays and is sustained: the argument that only real-world jeopardy provides irreplaceable learning that simulation cannot substitute.
% TRANSFER_FUNCTION: Transfers cost and jeopardy from regulatory bodies and competence-maintenance regimes (which bear little cost of enforcing the standard) to simulation investors and long-safe organizations (which must absorb expensive real-world exercises and accept heightened risk narratives to prove competence).
% ABSENT_VOICES: Simulation technologists and cognitive scientists who argue simulation fidelity has crossed a threshold where high-fidelity sim with rigorous debriefing is adequate; neuroscience researchers who contest the 'muscle memory under real jeopardy' premise; operators in long-safe periods who argue their track record proves competence without recent catastrophic anchor points. These voices are excluded from or marginalized in competence standard-setting.
% DISAPPEARANCE_RATIONALE: If the regime vanished—if the claim that only catastrophe maintains competence were rejected—simulation would be reframed as adequate with proper fidelity and debriefing; real-world exercises would become elective or risk-justified rather than mandatory, freeing investment capital; competence standards would shift from event-driven jeopardy models to proficiency-based performance metrics. Organizations would reorganize training portfolios and regulatory bodies would redesign oversight. The constraint does not maintain safety outcomes—it redistributes what counts as proof of competence.
% FOUNDING_PROBLEM: In the 1970s–1990s, aviation and nuclear operations discovered that crews trained entirely in simulators sometimes failed to respond appropriately to real emergencies that differed from practiced scenarios. Notable incidents (Gimli Glider partial-cause, Three Mile Island) revealed gaps between classroom competence and real-world intuition. The founding claim: something about actual jeopardy irreplaceably teaches what simulation alone cannot.
% FOUNDING_PROBLEM_CORROBORATION: Real-world incident analyses from the 1980s–2000s cite simulation limitations as contributory factors. However, simulation fidelity has advanced dramatically since then, and independent research (NASA Ames, European research councils, modern cognitive science) contests whether high-fidelity simulation with rigorous debriefing achieves the same learning outcomes. Regulatory bodies (FAA, EASA) affirm the founding problem's live status, citing ongoing incidents; operators with decades-long safety records challenge the claim as unfalsifiable—they argue their sustained safety proves competence without recent catastrophic anchors. The corroboration is split along institutional lines.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 by interval end) because the regime imposes a deferral-cost transfer: organizations cannot prove competence through safe, efficient simulation; they must accept either real jeopardy exercises (direct cost) or regulatory escalation (oversight cost). Suppression is high (0.72) because the claim is presented as empirically grounded ('muscle memory under real stress') while remaining unfalsifiable—organizations with decades of safe records cannot disprove the claim that their competence is atrophied. Theater is substantial (0.58) and rising: as simulation technology improves and real-world incidents decrease, the regime's enforcement relies increasingly on narratives and mandatory exercises rather than on demonstrable safety outcomes. The measurement series shows steady extraction and theater growth as time passes without catastrophic anchors—the longer the safe period, the more the regime must extract through narrative and oversight. Suppression plateaus (0.72 at t=30+) because the basic enforcing mechanism (the unfalsifiable claim + institutional authority) reaches full strength and then maintains it.
 *
 * PERSPECTIVAL GAP:
 *   From the regime seat: this is genuine competence maintenance grounded in empirical insights from real accidents. From the payer seats: this is a narrative mechanism that uses rare catastrophic events as retroactive justification and creates perverse incentives to keep systems at the edge of real jeopardy. From the observer seat: the gap is between (a) the specific learning mechanisms real events provide that simulation cannot, and (b) the empirical fidelity and scenario-breadth that modern simulation achieves. The engine computes per-seat directionality from the structural data: beneficiaries sit near the beneficiary end; constrained payers sit near the target end; observers sit near symmetric. The regime extracts by controlling the narrative of what counts as 'adequate' competence.
 *
 * DIRECTIONALITY LOGIC:
 *   Competence-maintenance regimes and regulatory bodies benefit from the claim—they gain legitimacy and enforcement authority. Real-world-test advocates benefit from validation of their position and from continued resource allocation to real-world exercises. Simulation investors and long-safe organizations are targets: they pay through de-valued investment, costly real-world exercises, and acceptance of a narrative of degraded readiness. Beneficiaries face low directionality (d near 0.0); payers face high directionality (d near 1.0). Competence researchers sit at symmetric (d ≈ 0.5) because they both benefit from research funding (driven by the regime's validation questions) and would face institutional pressure if their findings contradicted the regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope because it possesses both genuine coordination (the kernel claim that some form of competence maintenance is necessary) and asymmetric extraction (the specific requirement that ONLY catastrophe maintains competence, which de-values simulation and extracts cost from certain constituencies). The coordination component is real—competence is genuinely important. The extraction component rides on the coordination by claiming a specific (and contested) mechanism of competence maintenance. Mandatrophy would exist if the founding problem (the gap between simulation competence and real-world performance in the 1970s–1990s) had been solved by simulation fidelity improvements, yet the regime persists in enforcing the catastrophe-necessity claim. The regime's authority persists partly by institutional inertia and partly by selective incident citation—when real accidents occur, they are cited as proof; when they do not occur, the absence is cited as proof of atrophied competence. The constraint does not break down—it becomes more theatrical as safety improves, because it must justify itself through narrative rather than through demonstrable competence loss.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    muscle_memory_under_real_jeopardy_claim,
    'Is the learning mechanism that catastrophic events provide genuinely irreducible—is there a neurophysiological or cognitive component of competence that high-fidelity simulation cannot produce, even at high fidelity?',
    'Neuroscience research comparing brain activation and learning retention in high-fidelity simulation vs. real-world jeopardy settings; longitudinal competence data from organizations using only simulation vs. hybrid regimes; incident analysis distinguishing failure modes that would/would not be resolved by simulation fidelity improvements.',
    'If the mechanism is irreducible, the regime''s core claim holds and competence maintenance requires real-world anchoring. If simulation fidelity can substitute given sufficient breadth and debriefing, the mechanism is reducible and the regime is extractive narrative. If the gap is scenario-coverage rather than jeopardy-response, hybrid approaches are optimal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(muscle_memory_under_real_jeopardy_claim, empirical, 'Whether real jeopardy provides irreducible learning beyond what simulation can achieve').

omega_variable(
    scenario_coverage_vs_jeopardy_learning,
    'Do incidents in real-world operations fail because crews lack the ''muscle memory'' of real jeopardy, or because the scenario deviates from anything in the training corpus (simulation + real-world) and crews lack the adaptive framework to improvise?',
    'Detailed causal analysis of recent safety incidents in aviation, nuclear, and surgery comparing (a) failure-mode categories that require jeopardy-induced muscle memory vs. (b) failure-mode categories that reflect scenario novelty or incomplete training coverage. Natural experiments from jurisdictions that mandate different training regimes.',
    'If failures are predominantly jeopardy-response failures, the regime''s claim is supported. If failures are predominantly scenario-novelty or training-coverage failures, the regime conflates two distinct problems and extracts cost for the wrong mechanism. A hybrid finding would support the hybrid_dependency reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scenario_coverage_vs_jeopardy_learning, empirical, 'Whether competence loss is due to lack of real jeopardy or due to scenario-coverage gaps').

omega_variable(
    unfalsifiability_of_the_regime,
    'Can an organization with a multi-decade track record of safety without catastrophic events ever falsify the regime''s claim that its competence is atrophied? What evidence would convince the regime that simulation alone is adequate?',
    'Examine the regime''s actual response to long-safe organizations seeking exemption from real-world exercises. What evidence or outcomes would count as proof of maintained competence without real-world jeopardy anchors? If no evidence is pre-specified, the claim is unfalsifiable.',
    'If the regime''s claim is unfalsifiable, it is not an empirical claim about how competence decays—it is a narrative mechanism that extracts cost by definition. The classification would shift from tangled_rope (genuinely coordinated + extractive) toward snare (extraction masked by coordination narrative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unfalsifiability_of_the_regime, conceptual, 'Whether the regime''s claim is empirically falsifiable or unfalsifiable by design').

omega_variable(
    simulation_fidelity_threshold,
    'Has simulation fidelity, debriefing rigor, and scenario-coverage breadth crossed a threshold where the measured gap between simulation and real-world competence outcomes became negligible? When did this threshold (if it exists) occur?',
    'Historical analysis of simulation fidelity improvements (1990s–present) correlated with competence test results, incident causal factors, and regulatory assessments. Benchmark comparison of competence metrics between simulation-only vs. hybrid-trained cohorts in settings where such comparison is feasible.',
    'If a threshold was crossed (circa 2010–2015, potentially), the regime''s grounding in 1980s–1990s incident analysis is anachronistic. Organizations trained post-threshold under high-fidelity regimes would face extraction costs justified by evidence from pre-threshold periods. The constraint would exhibit mandatrophy (founding problem solved, but regime persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulation technology has made real-world jeopardy optional for competence maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.42).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 5, 0.45).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 10, 0.48).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 15, 0.51).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 20, 0.54).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 25, 0.56).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 30, 0.58).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel. The kernel is the claim that some form of competence maintenance is necessary. Three structurally distinct constraints emerge from three readings: (1) catastrophe_as_necessary_anchor asserts that only real jeopardy provides irreplaceable learning; (2) hybrid_dependency asserts both simulation and real-world anchoring are necessary; (3) simulation_as_adequate_exercise asserts simulation alone is sufficient with high fidelity. Each reading has distinct ε (measuring extractiveness under each reading's own lights), distinct beneficiary/victim structures, and distinct type classifications. They are not the same constraint viewed from different angles; they are three constraint-instances of the same kernel, held by different institutional constituencies. This story establishes catastrophe-necessity as the enforced reading; the sibling stories establish alternative readings. The network links represent structural influence: catastrophe-necessity affects (influences but does not foreclose) both siblings by controlling the empirical framing within which they operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
