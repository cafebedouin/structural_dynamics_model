% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety: Dual Priority Approach (Existential Risk & Near-Term Harms)
 *   domain: ai_safety/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'dual priority' reading of AI safety,
 *   which asserts that both existential risks (x-risk) and near-term harms
 *   (NTH) must be addressed as non-competing priorities. This reading
 *   attempts to unify the AI safety discourse but faces practical challenges
 *   in resource allocation and theoretical coherence. The constraint is
 *   claimed as a Rope, reflecting its genuine coordination function in
 *   attempting to bridge disparate communities, but its metrics show moderate
 *   extractiveness and suppression due to the inherent difficulties and
 *   trade-offs in implementing such a broad agenda.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.45).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.3).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety: Dual Priority Approach (Existential Risk & Near-Term Harms)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "ai_safety/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '29f18fc1-964b-4ec1-a57d-f478d25d98ae').
narrative_ontology:cs_kernel_codification('29f18fc1-964b-4ec1-a57d-f478d25d98ae', distributed).
narrative_ontology:cs_authority_grounding('29f18fc1-964b-4ec1-a57d-f478d25d98ae', distributed).
narrative_ontology:cs_reading_relation('29f18fc1-964b-4ec1-a57d-f478d25d98ae', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('29f18fc1-964b-4ec1-a57d-f478d25d98ae', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('29f18fc1-964b-4ec1-a57d-f478d25d98ae', foundational, comprehensive_risk_mitigation_is_optimal).
narrative_ontology:cs_axiom_status(comprehensive_risk_mitigation_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('29f18fc1-964b-4ec1-a57d-f478d25d98ae', comprehensive_risk_mitigation_is_optimal, instrumental).
narrative_ontology:cs_axiom('29f18fc1-964b-4ec1-a57d-f478d25d98ae', foundational, xrisk_and_nth_are_interconnected).
narrative_ontology:cs_axiom_status(xrisk_and_nth_are_interconnected, holdable).
narrative_ontology:cs_axiom_grounding('29f18fc1-964b-4ec1-a57d-f478d25d98ae', xrisk_and_nth_are_interconnected, empirically_contingent).
narrative_ontology:cs_reference_frame('29f18fc1-964b-4ec1-a57d-f478d25d98ae', unified_ai_safety_paradigm).
narrative_ontology:cs_drift_state('29f18fc1-964b-4ec1-a57d-f478d25d98ae', contemporary_ai_development_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('29f18fc1-964b-4ec1-a57d-f478d25d98ae', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, policy_makers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, general_public).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, resource_allocators).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, specialized_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for a comprehensive approach to AI safety, seeking to balance resources and attention between long-term existential risks and immediate societal harms. They aim to build a coherent framework but face internal and external pressure regarding resource allocation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_researchers, agenda_setter,
    organized, generational, constrained, global).

% Benefit from a framework that attempts to address all facets of AI risk, providing a more politically palatable and comprehensive narrative for regulation. They are responsible for allocating public funds and legislative attention based on these priorities.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers, beneficiary,
    institutional, biographical, constrained, national).

% Potentially benefits from a holistic approach that seeks to protect against both catastrophic future scenarios and present-day injustices. Their interests are represented by advocates and policymakers, but they have little direct agency.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, general_public, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear the cost of attempting to fund and manage research and interventions across two distinct problem spaces (existential risk and near-term harms) with often competing methodologies and timelines. They face the challenge of justifying resource distribution under scarcity.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, resource_allocators, payer,
    institutional, immediate, constrained, global).

% Researchers focused exclusively on either existential risk or near-term harms may find their specialized work deprioritized or diluted by the dual-priority framework, leading to funding challenges or pressure to broaden their focus. They bear the cost of a less focused agenda.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, specialized_researchers, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the diverse AI safety community by proposing a unified framework that acknowledges and integrates both long-term existential risks and immediate societal harms, aiming to prevent fragmentation and ensure comprehensive risk mitigation.
% TRANSFER_FUNCTION: Transfers attention, funding, and legitimacy to a broader set of AI safety concerns, from those exclusively focused on existential risk or near-term harms, towards a more integrated approach. This can lead to a redistribution of resources.
% ABSENT_VOICES: Critics who argue that the two problem spaces are fundamentally incommensurable or that one should take absolute priority over the other are often marginalized in discussions promoting a 'dual priority' approach. They would argue for a clearer, more focused agenda.
% DISAPPEARANCE_RATIONALE: If the dual-priority commitment vanished, the AI safety discourse would likely revert to a more fragmented state, with distinct communities focusing solely on either existential risk or near-term harms, potentially leading to less coordinated and less comprehensive risk mitigation efforts.
% FOUNDING_PROBLEM: The AI safety field was becoming polarized between those focused on long-term, speculative risks and those focused on immediate, demonstrable harms, leading to a lack of unified strategy and inefficient resource allocation.
% FOUNDING_PROBLEM_CORROBORATION: Leading AI ethics organizations and intergovernmental bodies attest to the ongoing challenge of balancing these priorities, citing continued debates within the research community and difficulties in policy implementation. Independent academic analyses also highlight the persistent tension.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).
:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) arises from the dilution of focus and resources for specialized researchers, who might achieve greater impact by concentrating on one area. Suppression (0.30) is present as the dual-priority narrative can marginalize voices advocating for exclusive focus on one type of risk. Theater ratio (0.20) is moderate, as some efforts to 'balance' priorities may be performative rather than genuinely integrated. Accessibility collapse is moderate (0.40) because while the dual-priority framework offers a broad tent, it can make it harder for highly specialized approaches to gain traction. Resistance (0.50) is present from those who believe the two problem spaces are incommensurable or that one should take absolute priority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'dual priority' advocates, this is a necessary coordination mechanism (Rope) to prevent fragmentation. However, from the perspective of specialized researchers, it can feel like a Tangled Rope or even a Snare, as it extracts resources and attention from their focused work without necessarily providing commensurate benefits to their specific goals.
 *
 * DIRECTIONALITY LOGIC:
 *   AI safety researchers and policymakers are beneficiaries as this framework provides a more comprehensive and politically viable approach. The general public is a diffuse beneficiary. Resource allocators and specialized researchers are payers, as they bear the costs of managing and adapting to a broader, less focused agenda. The directionality for specialized researchers is higher (more target-like) due to the potential for diluted funding and pressure to conform to a broader research agenda.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_coherence,
    'Can resources be effectively and coherently allocated across both existential risk and near-term harms without diluting impact or creating internal competition?',
    'Empirical studies of funding distribution and research outcomes in organizations adopting a dual-priority approach, comparing them to specialized organizations.',
    'If incoherent, the constraint''s extractiveness and theater_ratio would be higher, potentially reclassifying it as a Tangled Rope or Snare due to inefficient resource capture. If coherent, it strengthens the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Whether the dual-priority approach is practically implementable without significant efficiency losses.').

omega_variable(
    incommensurability_of_risks,
    'Are existential risks and near-term harms fundamentally incommensurable, making a ''dual priority'' framework conceptually unstable?',
    'Philosophical and ethical analysis of risk types, and the development of a robust, widely accepted theoretical framework for comparing and integrating them.',
    'If incommensurable, the constraint''s suppression and resistance would be higher, as the framework would struggle to gain full buy-in, potentially leading to a reclassification towards Snare due to the forced integration of disparate concerns.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incommensurability_of_risks, conceptual, 'Whether the two types of risks can be meaningfully integrated into a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2020, ai_safety_commitment__dual_priority_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(ai_s_tr_t2023, ai_safety_commitment__dual_priority_reading, theater_ratio, 2023, 0.18).
narrative_ontology:measurement(ai_s_tr_t2026, ai_safety_commitment__dual_priority_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement(ai_s_tr_t2030, ai_safety_commitment__dual_priority_reading, theater_ratio, 2030, 0.22).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2020, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(ai_s_be_t2023, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2023, 0.4).
narrative_ontology:measurement(ai_s_be_t2026, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement(ai_s_be_t2030, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2030, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2020, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(ai_s_su_t2023, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2023, 0.28).
narrative_ontology:measurement(ai_s_su_t2026, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2026, 0.3).
narrative_ontology:measurement(ai_s_su_t2030, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2030, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI Safety Commitment' kernel, aiming to integrate both existential risk and near-term harms. It influences and coexists with more specialized readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
