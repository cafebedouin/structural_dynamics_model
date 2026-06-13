% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support: Universality Paradox Reading
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes unconditional income support (UIS) as a
 *   politically ambiguous policy vehicle, where its cross-ideological appeal
 *   (from libertarian minimal state to socialist welfare expansion) masks
 *   fundamentally incompatible normative commitments and implementation
 *   paths. The 'universality paradox' reading highlights how these divergent
 *   paths often converge on similar fiscal and distributional outcomes due to
 *   mechanisms like 'taxing back' the benefit, leading to a policy that is
 *   rhetorically flexible but ideologically muddled. This ambiguity allows
 *   political entrepreneurs to build broad coalitions but extracts a cost in
 *   ideological clarity and can lead to the dismantling of targeted programs
 *   under the guise of 'universality'.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: Primary beneficiary (institutional/arbitrage) — exploit ambiguity for coalition building
 *   - policy_designers: Primary beneficiary (institutional/mobile) — use taxing-back mechanisms for rhetorical flexibility
 *   - ideological_clarity: Primary victim (non-agent) — suffers from the policy's inherent ambiguity
 *   - targeted_program_recipients: Primary victim (powerless/constrained) — lose existing support as universality is used to justify cuts
 *   - libertarian_advocates: Stakeholder (organized/mobile) — support UIS as a replacement for complex welfare states
 *   - socialist_advocates: Stakeholder (organized/mobile) — support UIS as a step towards greater economic equality
 *   - welfare_state_bureaucrats: Stakeholder (institutional/constrained) — administer existing programs, face potential redundancy or restructuring
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.35).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.2).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support: Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'af62c4cd-af66-4ef4-bc22-ecb82ba81755').
narrative_ontology:cs_kernel_codification('af62c4cd-af66-4ef4-bc22-ecb82ba81755', distributed).
narrative_ontology:cs_authority_grounding('af62c4cd-af66-4ef4-bc22-ecb82ba81755', distributed).
narrative_ontology:cs_reading_relation('af62c4cd-af66-4ef4-bc22-ecb82ba81755', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('af62c4cd-af66-4ef4-bc22-ecb82ba81755', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('af62c4cd-af66-4ef4-bc22-ecb82ba81755', foundational, policy_ambiguity_is_political_resource).
narrative_ontology:cs_axiom_status(policy_ambiguity_is_political_resource, holdable).
narrative_ontology:cs_axiom_grounding('af62c4cd-af66-4ef4-bc22-ecb82ba81755', policy_ambiguity_is_political_resource, empirically_contingent).
narrative_ontology:cs_axiom('af62c4cd-af66-4ef4-bc22-ecb82ba81755', foundational, fiscal_outcomes_converge_despite_ideology).
narrative_ontology:cs_axiom_status(fiscal_outcomes_converge_despite_ideology, holdable).
narrative_ontology:cs_axiom_grounding('af62c4cd-af66-4ef4-bc22-ecb82ba81755', fiscal_outcomes_converge_despite_ideology, empirically_contingent).
narrative_ontology:cs_reference_frame('af62c4cd-af66-4ef4-bc22-ecb82ba81755', ideological_polarization_in_welfare_reform).
narrative_ontology:cs_drift_state('af62c4cd-af66-4ef4-bc22-ecb82ba81755', contemporary_policy_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('af62c4cd-af66-4ef4-bc22-ecb82ba81755', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is low because the 'taxing back' mechanism means the net transfer is often not as high as the gross benefit suggests, and the fiscal outcomes across different designs are similar. Suppression (0.20) is low because the policy's appeal is broad, but active enforcement is required to manage the 'taxing back' and program consolidation. Theater ratio (0.60) is high because the policy's rhetorical function (appealing to diverse ideologies) often outweighs its direct, transparent distributional impact, with much of the political activity focused on framing rather than clear outcomes. Accessibility collapse (0.40) is moderate as alternatives (targeted programs, other welfare reforms) are not fully collapsed but are often reframed or threatened by the push for universality. Resistance (0.30) is moderate, coming from those who fear the loss of targeted programs or ideological purity.
 *
 * PERSPECTIVAL GAP:
 *   Political entrepreneurs and policy designers experience this as a flexible tool for coalition building and policy innovation, allowing them to advance agendas under a broad banner. Targeted program recipients, however, experience it as a threat, where the promise of universality is used to justify cuts to existing, often more generous, targeted support. Ideological clarity, as a non-agent victim, experiences a degradation of coherent policy discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are beneficiaries because they gain political capital and flexibility from the policy's ambiguity. Ideological clarity and targeted program recipients are victims because the ambiguity obscures true impacts and can lead to a net loss of support for vulnerable groups. Libertarian and socialist advocates are stakeholders who benefit from the rhetorical appeal but may become victims if the implementation deviates too far from their core principles.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a Tangled Rope because it genuinely coordinates diverse political interests (cross-ideological appeal) but does so through asymmetric extraction (loss of ideological clarity, potential harm to targeted recipients). The 'Trojan horse' aspect suggests a latent mandatrophy, where the initial coordination function (building consensus for a new social policy) might eventually atrophy into pure extraction if the ambiguity is consistently exploited to dismantle existing support without delivering on the universal promise. The high theater ratio reflects this potential for performative consensus masking divergent real-world impacts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''universality paradox'' or is it primarily a ''freedom floor'' or ''dependency trap''?',
    'Empirical analysis of policy outcomes in different implementations, focusing on the actual fiscal and social impacts, and the stated rationales of implementing parties versus their observed effects.',
    'If primarily a ''freedom floor'', the constraint would be a Rope (low extraction, high coordination). If a ''dependency trap'', it would be a Snare (high extraction, high suppression). This reading emphasizes the political ambiguity and similar fiscal outcomes across different ideological justifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''unconditional_income_support'' kernel, specifically the ''universality_paradox_reading''.').

omega_variable(
    fiscal_outcome_convergence,
    'To what extent do different UBI implementation paths (e.g., high vs. low grant, different clawback rates) truly converge on similar fiscal and distributional outcomes?',
    'Comparative fiscal modeling and empirical evaluation of pilot programs across diverse policy designs and economic contexts.',
    'If convergence is weak, the ''universality paradox'' reading''s core premise (similar fiscal outcomes despite ideological differences) is undermined, potentially shifting the constraint towards a more direct ''freedom floor'' or ''dependency trap'' classification depending on the dominant design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_outcome_convergence, empirical, 'The degree to which different UBI designs yield similar fiscal outcomes, masking ideological differences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__universality_paradox_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__universality_paradox_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__universality_paradox_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unconditional_income_support' kernel, focusing on the political ambiguity and convergence of fiscal outcomes. It is distinct from the 'freedom_floor_reading' (emphasizing autonomy and stigma reduction) and the 'dependency_trap_reading' (emphasizing disincentives and upward redistribution), which are separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
