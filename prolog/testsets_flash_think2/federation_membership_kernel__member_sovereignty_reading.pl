% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty over Free Movement in Federations
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'member sovereignty' reading of the
 *   federation membership kernel, asserting that free movement rights must be
 *   bounded by national welfare state capacity and labor market protection.
 *   Member states retain authority to exclude economically inactive migrants
 *   and protect social solidarity institutions. This reading stands in
 *   tension with more expansive interpretations of free movement and
 *   supranational authority, prioritizing national control and the perceived
 *   sustainability of national social models.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.75).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty over Free Movement in Federations").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '6e306246-04b4-4c0d-9fd2-4e8dbced87f4').
narrative_ontology:cs_kernel_codification('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', formalized).
narrative_ontology:cs_authority_grounding('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', lineage).
narrative_ontology:cs_interpretation_layer_present('6e306246-04b4-4c0d-9fd2-4e8dbced87f4').
narrative_ontology:cs_reading_relation('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', federation_membership_kernel__integration_reading, forecloses).
narrative_ontology:cs_reading_relation('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', foundational, national_welfare_state_sustainability_is_paramount).
narrative_ontology:cs_axiom_status(national_welfare_state_sustainability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', national_welfare_state_sustainability_is_paramount, instrumental).
narrative_ontology:cs_axiom('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', foundational, member_state_control_over_borders_is_inherent).
narrative_ontology:cs_axiom_status(member_state_control_over_borders_is_inherent, holdable).
narrative_ontology:cs_axiom_grounding('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', member_state_control_over_borders_is_inherent, conventional).
narrative_ontology:cs_reference_frame('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', national_sovereignty_framework).
narrative_ontology:cs_drift_state('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', contemporary_eu_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6e306246-04b4-4c0d-9fd2-4e8dbced87f4', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, domestic_labor_force).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_states).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, mobile_workers_from_other_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain and assert the authority to define the boundaries of free movement, particularly concerning access to national welfare systems and protection of domestic labor markets. They actively enforce policies to exclude economically inactive migrants.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, member_states, agenda_setter,
    institutional, generational, constrained, national).

% Are the primary targets of exclusion policies, facing restricted access to social benefits and potential deportation. Their mobility is severely constrained by national welfare state capacity rules.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, regional).

% Benefit from the perceived protection of national welfare systems, which are argued to be safeguarded from strain by the exclusion of economically inactive migrants. They support policies that prioritize national solidarity.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients, beneficiary,
    moderate, biographical, mobile, national).

% Benefit from labor market protection, as the exclusion of migrants is argued to prevent wage depression and maintain employment opportunities for national citizens. Organized labor groups often advocate for such protections.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, domestic_labor_force, beneficiary,
    organized, biographical, mobile, national).

% Experience brain drain and social costs as their citizens are excluded or face restricted access to opportunities in other member states. They have limited leverage to challenge the receiving states' sovereignty claims.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_states, payer,
    institutional, generational, constrained, national).

% While economically active, they face increased scrutiny and potential administrative hurdles, as the overall policy climate emphasizes national control over free movement. Their access to social benefits can also be challenged.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, mobile_workers_from_other_member_states, payer,
    moderate, biographical, constrained, regional).

% Their expansive interpretations of free movement and EU citizenship are actively resisted and circumvented by this reading, which prioritizes national sovereignty. They are structurally excluded from setting the primary terms of this constraint.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, supranational_courts_ecj, excluded,
    institutional, generational, analytical, continental).

% Advocate for broader free movement rights and deeper integration, challenging the member states' claims of absolute sovereignty over welfare and labor markets. They analyze the impact of restrictive policies.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, pro_integration_advocates, observer,
    organized, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To protect the fiscal sustainability and social cohesion of national welfare states, and to safeguard domestic labor markets from perceived negative impacts of unrestricted free movement within a federal or quasi-federal system.
% TRANSFER_FUNCTION: Transfers the burden of welfare provision and labor market competition from national citizens and residents to economically inactive migrants and their sending states, primarily through exclusion and restricted access to benefits.
% ABSENT_VOICES: Supranational institutions (e.g., the European Court of Justice) whose expansive interpretations of free movement are actively resisted by this reading; also human rights organizations and migrant advocacy groups who prioritize individual mobility and non-discrimination over national welfare state capacity arguments.
% DISAPPEARANCE_RATIONALE: If member states lost the authority to bound free movement by national welfare state capacity and labor market protection, national welfare systems would face immediate and significant pressure, labor markets would undergo rapid adjustments, and the political economy of the federation would fundamentally shift, leading to a reorganization of social and economic structures.
% FOUNDING_PROBLEM: The perceived strain on national welfare states and domestic labor markets caused by economically inactive migrants or large-scale migration flows, leading to concerns about social dumping and erosion of national social solidarity.
% FOUNDING_PROBLEM_CORROBORATION: National governments, populist movements, and some economists attest to the ongoing and live nature of these problems, citing fiscal pressures and social integration challenges. Supranational bodies, pro-integration advocates, and other economists often contest the severity or causality of these problems, arguing for alternative solutions or different interpretations of the evidence.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderately high because the constraint actively imposes costs on specific groups (migrants, sending states) to benefit others (national welfare recipients, domestic labor force). Suppression (0.75) is high due to the active enforcement mechanisms employed by member states to control borders and access to benefits. The theater ratio is low (0.15) as the stated function (protecting national welfare/labor markets) is directly pursued through concrete, enforced policies, with little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member states and national citizens, this constraint is a necessary protection of national interests and social solidarity, framed as a legitimate coordination function. From the perspective of migrants and sending states, it operates as a discriminatory and extractive mechanism that limits fundamental rights and imposes significant social and economic costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states, national welfare recipients, and the domestic labor force are beneficiaries (low directionality) as they are protected from perceived strains. Economically inactive migrants, sending states, and mobile workers from other member states are targets (high directionality) as they bear the costs of exclusion and restricted access. Supranational courts are excluded, as their interpretations are actively resisted by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not characterized by mandatrophy; rather, it represents an active reassertion and defense of a specific mandate (national sovereignty over welfare and labor markets) against competing mandates (supranational integration, expansive free movement rights). The 'founding problem' is considered 'live' by its proponents, indicating a continuous perceived need for the constraint's function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''federation_membership_kernel''. What are the implications of this specific ''member_sovereignty_reading'' for the overall coherence of the federation?',
    'Comparative analysis of policy outcomes and legal challenges across different member states and over time, assessing the degree of divergence and its impact on federal cohesion.',
    'If this reading gains dominance, it could lead to fragmentation within the federation, increased internal border controls, and a weakening of supranational institutions. If it is successfully challenged, the federation might move towards deeper integration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Identifies this constraint as a specific reading of a broader kernel.').

omega_variable(
    structural_delta_impact,
    'How precisely does this ''member_sovereignty_reading'' structurally alter the victim set, access for sending state workers, brain drain, and receiving state labor market flexibility compared to other readings?',
    'Detailed empirical studies quantifying changes in migrant flows, welfare access, labor market participation, and economic impacts on sending states under policies aligned with this reading, compared to counterfactuals under other readings.',
    'Quantifying these deltas would provide concrete evidence of the extractive and suppressive effects of this reading, strengthening arguments for or against its policy implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_impact, empirical, 'Quantifies the specific structural changes introduced by this reading.').

omega_variable(
    welfare_state_sustainability_necessity,
    'Is the exclusion of economically inactive migrants truly necessary for the fiscal sustainability of national welfare states, or are there alternative policy mechanisms (e.g., better coordination, fiscal transfers) that could achieve sustainability without such exclusion?',
    'Economic modeling and comparative policy analysis of welfare states with different migration regimes and coordination mechanisms, assessing their long-term fiscal health and social outcomes.',
    'If exclusion is found to be unnecessary, the ''coordination'' aspect of this constraint would be significantly weakened, reclassifying it closer to a pure Snare. If it is found to be necessary, the Tangled Rope classification would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_state_sustainability_necessity, empirical, 'Assesses the necessity of exclusion for welfare state sustainability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.69).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'federation_membership_kernel', each representing a different structural interpretation of free movement rights within a federal system. This reading emphasizes national sovereignty and welfare state protection, contrasting with integration-focused and welfare-coordination-focused readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
