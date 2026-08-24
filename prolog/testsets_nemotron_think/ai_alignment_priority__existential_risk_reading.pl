% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Existential Risk Framing of AI Alignment Priority
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The existential risk reading of AI alignment defines 'alignment' as
 *   preventing catastrophic loss of control over advanced AI systems and
 *   prioritizes existential safety above all other concerns. This framing
 *   emerged from early AI safety theory (2000s-2010s) and became dominant in
 *   flagship AI labs, major philanthropic funding (Open Philanthropy,
 *   Long-Term Future Fund), and governmental policy (US Executive Order, UK
 *   AI Safety Summit). The constraint operates by defining the legitimate
 *   scope of 'alignment work' — adversarial red-teaming, scalable oversight,
 *   interpretability — while marginalizing fairness, accountability,
 *   transparency, and justice for currently harmed populations as 'near-term'
 *   or 'non-existential.' Resource flow (talent, compute, grants, policy
 *   access) concentrates in frontier labs that adopt this framing. The
 *   claimed type (tangled_rope) reflects a genuine coordination problem
 *   (existential risk is real) with asymmetric extraction (present harms
 *   deprioritized, labs capture resources).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.75).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential Risk Framing of AI Alignment Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'f925bfb5-6762-46c1-9c2e-9225f247b43f').
narrative_ontology:cs_kernel_codification('f925bfb5-6762-46c1-9c2e-9225f247b43f', distributed).
narrative_ontology:cs_authority_grounding('f925bfb5-6762-46c1-9c2e-9225f247b43f', distributed).
narrative_ontology:cs_reading_relation('f925bfb5-6762-46c1-9c2e-9225f247b43f', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('f925bfb5-6762-46c1-9c2e-9225f247b43f', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('f925bfb5-6762-46c1-9c2e-9225f247b43f', foundational, existential_risk_has_lexical_priority_over_present_harms).
narrative_ontology:cs_axiom_status(existential_risk_has_lexical_priority_over_present_harms, holdable).
narrative_ontology:cs_axiom_grounding('f925bfb5-6762-46c1-9c2e-9225f247b43f', existential_risk_has_lexical_priority_over_present_harms, instrumental).
narrative_ontology:cs_axiom('f925bfb5-6762-46c1-9c2e-9225f247b43f', foundational, control_problem_is_the_central_alignment_target).
narrative_ontology:cs_axiom_status(control_problem_is_the_central_alignment_target, holdable).
narrative_ontology:cs_axiom_grounding('f925bfb5-6762-46c1-9c2e-9225f247b43f', control_problem_is_the_central_alignment_target, empirically_contingent).
narrative_ontology:cs_reference_frame('f925bfb5-6762-46c1-9c2e-9225f247b43f', alignment_as_control_problem).
narrative_ontology:cs_drift_state('f925bfb5-6762-46c1-9c2e-9225f247b43f', post_chatgpt_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f925bfb5-6762-46c1-9c2e-9225f247b43f', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, longterm_future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, ai_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_ai_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, global_majority_excluded_from_governance).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, existential_risk_from_ai_is_real).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, control_problem_is_central_to_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The hypothetical future population whose existence is the stated beneficiary of existential risk prevention. Has no voice in current governance, cannot exit the constraint's effects, and bears the consequence if the prioritization fails or succeeds.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, longterm_future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Research community that defines, funds, and publishes the technical agenda for alignment-as-control. Gains career capital, funding, and institutional recognition from the existential risk framing. Exit means leaving the field or accepting marginalization within it.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_safety_researchers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, ai_safety_researchers, agenda_setter).

% Corporate labs (OpenAI, Anthropic, DeepMind, etc.) that develop frontier models. Adopt existential risk framing to justify capability scaling, attract safety talent, and shape regulation. Capture the resource flow (compute, talent, policy access) while externalizing near-term harms. Can pivot framing if regulation shifts.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, beneficiary).

% Communities experiencing algorithmic discrimination, labor displacement, surveillance, and environmental harms from deployed AI now. Their harms are deprioritized as 'near-term' or 'non-existential.' No exit from the systems harming them; no voice in the governance that defines their harms as secondary.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% Researchers working on fairness, accountability, transparency, and present harms. Compete for the same funding pools, publication venues, and policy attention that the existential risk framing captures. Marginalized in flagship conferences and foundation grants; exit means reframing work or leaving academia.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_ai_ethics_researchers, payer,
    moderate, biographical, constrained, global).

% Nations and populations outside the US/EU/China AI governance axis. Bear deployment harms (data extraction, labor exploitation, environmental cost) and existential risk equally, but have no seat in the alignment priority-setting process. The constraint's universal scope makes their exclusion structural.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, global_majority_excluded_from_governance, excluded,
    powerless, biographical, trapped, global).

% Analysts tracking how the existential risk framing shapes funding, regulation, and research priorities. See the full structural asymmetry: a genuine coordination problem (existential risk) used to capture resources and legitimize capability scaling while deprioritizing present justice claims.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_policy_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research effort and policy attention toward preventing loss of control over advanced AI systems — a genuine collective action problem where uncoordinated development could lead to catastrophe.
% TRANSFER_FUNCTION: Moves funding, talent, compute access, and policy legitimacy from near-term harm mitigation (fairness, accountability, transparency, global majority concerns) toward adversarial red-teaming, interpretability, and scalable oversight research at frontier labs.
% ABSENT_VOICES: Global majority governments, civil society organizations representing currently harmed communities, labor unions facing AI displacement, and ecological justice advocates are structurally excluded from the alignment priority-setting process. They would object to the deprioritization of present harms but are not in the rooms where 'alignment' is defined.
% DISAPPEARANCE_RATIONALE: If the existential risk framing vanished overnight, funding would redirect toward present-harm mitigation, regulatory frameworks would center current discrimination and labor impacts, frontier labs would lose their primary safety-justification for scaling, and the global governance conversation would re-center justice for affected populations now.
% FOUNDING_PROBLEM: Early AI safety theory (Yudkowsky, Bostrom) identified that superintelligent systems could pursue objectives misaligned with human survival, and that the 'control problem' might be unsolvable if not addressed before capability thresholds are crossed. The founding problem was: how to ensure the first superintelligence remains controllable.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the AI safety field's own early texts (MIRI, FHI, 2000s-2010s). However, critics from the near-term harms reading (Gebru, Buolamwini, Raji, Benjamin, global majority scholars) contest that the founding problem was ever the only or primary alignment problem, arguing it was a narrow framing that served specific institutional interests. No neutral third-party corroboration exists; the contestation is the signal.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the framing captures the overwhelming majority of AI safety funding and talent while extracting legitimacy from present-harm mitigation. Suppression (0.7) is high because alternative framings are actively excluded from flagship venues, foundation grants, and policy tables — not merely neglected. Theater ratio (0.4) reflects genuine technical work (interpretability, red-teaming) mixed with performative safety-washing that justifies continued scaling. Accessibility collapse (0.65) shows alternative framings persist but are structurally marginalized. Resistance (0.7) is high from near-term harm advocates, global majority voices, and critical scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the frontier lab + safety researcher seat, this is genuine coordination preventing extinction (rope-like). From the present-harm seat, it is extraction masquerading as coordination (snare-like). The engine computes this divergence from the declared beneficiary/victim structure and exit options — the existential risk reading's own claim (tangled_rope) acknowledges both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Longterm future humanity is the stated beneficiary (d near 0.0) but has no agency — the constraint extracts in their name. AI safety researchers and frontier labs are structural beneficiaries (d ~0.1-0.2) who capture resources and set agendas. Present marginalized populations and near-term ethics researchers are payers (d ~0.8-0.9) bearing the cost of deprioritization with trapped/constrained exit. Global majority is excluded (no seat). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (control of superintelligence) remains technically live but the constraint has accumulated extraction: capability scaling continues under the safety banner, near-term harms grow unaddressed, and the field's institutional incentives favor the framing. Mandatrophy is unresolved — the coordination function has not atrophied, but the extraction overlay has thickened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_probability_ambiguity,
    'What is the actual probability of existential catastrophe from loss of control, and does it justify the current resource allocation?',
    'Convergent expert elicitation with calibrated forecasting track records, or empirical evidence of capability thresholds where control becomes irreversibly lost.',
    'If probability is low (<1%), the coordination function weakens and extraction dominates (snare). If high (>10%), coordination function strengthens and extraction may be warranted (rope/tangled_rope). The current high ε assumes the reading''s own high-probability assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_probability_ambiguity, empirical, 'Whether the existential risk premise empirically warrants the resource capture.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (preventing loss of control) be separated from the extraction function (capturing resources for capability scaling, deprioritizing present harms)?',
    'Counterfactual analysis: if near-term harm mitigation were funded at parity, would existential risk progress slow? Natural experiments from jurisdictions with different funding priorities.',
    'If inseparable, the constraint is a true tangled_rope — extraction is the price of coordination. If separable, the extraction is a contingent institutional capture that could be removed without losing the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally entangled.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does the kernel ''ai_alignment_priority'' admit a single coherent framing, or is the contest between readings constitutive of the kernel itself?',
    'Genealogical analysis of how ''alignment'' was coined and contested (Christiano, Leike, Amodei vs. Gebru, Raji, Benjamin). If the term was always polysemic, the kernel is distributed; if it had a core meaning that fragmented, the kernel is fixed_text with interpretive drift.',
    'If the kernel is distributed, all three readings are equally legitimate and the constraint family is the proper unit of analysis. If fixed_text with drift, the existential risk reading may be the original meaning and others are deviations (or vice versa).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether the kernel itself is a site of irreducible framing contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_align_existential_tr_t2015, ai_alignment_priority__existential_risk_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(ai_align_existential_tr_t2017, ai_alignment_priority__existential_risk_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(ai_align_existential_tr_t2019, ai_alignment_priority__existential_risk_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(ai_align_existential_tr_t2021, ai_alignment_priority__existential_risk_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement(ai_align_existential_tr_t2023, ai_alignment_priority__existential_risk_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(ai_align_existential_tr_t2025, ai_alignment_priority__existential_risk_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_align_existential_be_t2015, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(ai_align_existential_be_t2017, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(ai_align_existential_be_t2019, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(ai_align_existential_be_t2021, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(ai_align_existential_be_t2023, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2023, 0.72).
narrative_ontology:measurement(ai_align_existential_be_t2025, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(ai_align_existential_su_t2015, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(ai_align_existential_su_t2017, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2017, 0.4).
narrative_ontology:measurement(ai_align_existential_su_t2019, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2019, 0.52).
narrative_ontology:measurement(ai_align_existential_su_t2021, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(ai_align_existential_su_t2023, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2023, 0.68).
narrative_ontology:measurement(ai_align_existential_su_t2025, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the ai_alignment_priority constraint family. The kernel decomposes into three readings with distinct ε values and victim/beneficiary structures: existential_risk_reading (high ε, speculative future, longterm beneficiary), nearterm_harms_reading (moderate ε, present harms, marginalized beneficiary), integrated_reading (lower ε, dual coordination, broader beneficiary). The existential risk reading influences the others by capturing the 'alignment' label and its resource flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, organized, 0.2).
constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
