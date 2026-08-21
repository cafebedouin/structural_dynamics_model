% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant for Shared Infrastructure Coordination
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint represents the 'coordination_reading' of an HOA covenant,
 *   focusing on its function to coordinate shared infrastructure maintenance
 *   and resolve genuine externalities. In this reading, the covenant is a
 *   beneficial collective action mechanism, ensuring common goods are
 *   provided and maintained, with minimal extraction beyond necessary
 *   operational costs. It is contrasted with readings that emphasize
 *   behavioral control or pure extraction, which are modeled as sibling
 *   constraints within the 'hoa_covenant_scope' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.2).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant for Shared Infrastructure Coordination").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, 'c65444ad-7dff-4095-8218-b97506de2a2c').
narrative_ontology:cs_kernel_codification('c65444ad-7dff-4095-8218-b97506de2a2c', formalized).
narrative_ontology:cs_authority_grounding('c65444ad-7dff-4095-8218-b97506de2a2c', practice).
narrative_ontology:cs_interpretation_layer_present('c65444ad-7dff-4095-8218-b97506de2a2c').
narrative_ontology:cs_reading_relation('c65444ad-7dff-4095-8218-b97506de2a2c', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('c65444ad-7dff-4095-8218-b97506de2a2c', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('c65444ad-7dff-4095-8218-b97506de2a2c', foundational, collective_benefit_justifies_contribution).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_contribution, holdable).
narrative_ontology:cs_axiom_grounding('c65444ad-7dff-4095-8218-b97506de2a2c', collective_benefit_justifies_contribution, instrumental).
narrative_ontology:cs_axiom('c65444ad-7dff-4095-8218-b97506de2a2c', foundational, shared_costs_for_shared_goods).
narrative_ontology:cs_axiom_status(shared_costs_for_shared_goods, holdable).
narrative_ontology:cs_axiom_grounding('c65444ad-7dff-4095-8218-b97506de2a2c', shared_costs_for_shared_goods, conventional).
narrative_ontology:cs_reference_frame('c65444ad-7dff-4095-8218-b97506de2a2c', efficient_collective_action_framework).
narrative_ontology:cs_drift_state('c65444ad-7dff-4095-8218-b97506de2a2c', contemporary_community_governance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c65444ad-7dff-4095-8218-b97506de2a2c', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, potential_free_riders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from well-maintained common areas and infrastructure, and from predictable dispute resolution. They collectively pay dues to fund these services. Their exit is tied to selling their property, which means leaving the community.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% Elected representatives responsible for administering the covenant, collecting dues, overseeing maintenance, and enforcing rules related to shared infrastructure and externalities. They act on behalf of the homeowners and are subject to community oversight.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    institutional, biographical, mobile, local).

% Individuals who might otherwise avoid contributing to shared costs but are compelled by the covenant to pay their share. The constraint ensures they contribute, preventing the degradation of shared resources and ensuring fairness for all contributors.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, potential_free_riders, payer,
    powerless, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure equitable funding and maintenance of shared infrastructure (e.g., roads, parks, utilities) and to provide a mechanism for resolving genuine externalities (e.g., noise, property upkeep affecting neighbors) within a residential community.
% TRANSFER_FUNCTION: Collects regular dues from all homeowners and allocates these funds to maintenance, repairs, and improvements of common property and services, ensuring collective goods are provided.
% ABSENT_VOICES: Homeowners who fundamentally oppose any form of collective governance or who believe they can manage their property entirely independently without affecting others. Their voices are often marginalized by the collective decision-making structure of the HOA, as the covenant is a pre-condition of property ownership in the community.
% DISAPPEARANCE_RATIONALE: If the covenant and its enforcement vanished overnight, shared infrastructure would likely degrade due to lack of coordinated funding and maintenance, leading to a 'tragedy of the commons'. Property values would decline, and disputes over externalities would escalate without a clear resolution mechanism, forcing a reorganization of community life.
% FOUNDING_PROBLEM: The historical problem of maintaining shared amenities and resolving inter-property disputes in residential developments, where individual incentives often lead to underinvestment in common goods and unchecked negative externalities.
% FOUNDING_PROBLEM_CORROBORATION: Urban planners, property management experts, and historical analyses of communities without effective collective governance mechanisms consistently corroborate the persistence of these coordination problems, demonstrating the ongoing need for such covenants.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) as the primary function is to cover the costs of shared services, with minimal surplus. Suppression is low (0.20) because enforcement is generally accepted as necessary for collective benefit, and alternatives (e.g., private provision of all services) are less efficient. Theater ratio is low (0.10) as the covenant's activities are genuinely functional. Accessibility collapse is moderate (0.40) because while opting out of the HOA is difficult once property is purchased, alternative forms of community governance exist, and the decision to join is initially voluntary. Resistance is low (0.15) as most homeowners perceive net benefits from the coordinated services.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes coordination, other readings (behavioral_control_reading, extraction_reading) would perceive the same covenant as more extractive or suppressive. This divergence highlights how the same legal instrument can be interpreted through different lenses, leading to different classifications. This story focuses solely on the coordination function, as per the prompt, without adjudicating the validity of other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are symmetric beneficiaries and payers, receiving services proportional to their contributions. The HOA board acts as an agenda-setter, facilitating the coordination function. Potential free-riders are compelled to contribute, ensuring the system's viability, and thus are 'payers' in this context, but their 'cost' is the prevention of a negative externality for the collective, leading to a net benefit for the community.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_behavioral_control_ambiguity,
    'To what extent does the HOA covenant''s enforcement extend beyond shared infrastructure and genuine externalities into aesthetic uniformity and behavioral conformity?',
    'Analysis of enforcement records: the proportion of fines/actions related to objective infrastructure/externalities versus subjective aesthetic or lifestyle rules. Community surveys on perceived scope of control.',
    'If enforcement heavily targets aesthetic/behavioral aspects, the constraint leans towards the ''behavioral_control_reading'', increasing perceived suppression and potentially extractiveness (if fines are substantial).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_behavioral_control_ambiguity, empirical, 'Distinguishing genuine coordination from aesthetic/behavioral control.').

omega_variable(
    coordination_vs_extraction_ambiguity,
    'Is the HOA covenant primarily a mechanism for shared benefit, or has it drifted into a revenue generation and power consolidation tool for the board?',
    'Financial audit of HOA budgets (revenue sources vs. expenditure on shared services), analysis of fine proliferation and selective enforcement patterns, and board member turnover rates.',
    'Evidence of excessive fines, large unspent reserves, or disproportionate board power would shift the classification towards the ''extraction_reading'', significantly increasing extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ambiguity, empirical, 'Distinguishing genuine coordination from rent-seeking and power consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__coordination_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__coordination_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__coordination_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__coordination_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__coordination_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__coordination_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
