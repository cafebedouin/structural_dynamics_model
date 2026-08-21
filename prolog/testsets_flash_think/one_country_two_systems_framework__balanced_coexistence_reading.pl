% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems: Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'balanced coexistence' reading of
 *   the 'One Country, Two Systems' framework. It describes a system where
 *   neither absolute sovereignty nor absolute autonomy prevails, but rather a
 *   continuous process of substantive negotiation and political accommodation
 *   resolves contested boundaries. This reading emphasizes functional
 *   division of powers and acknowledges that civil society retains some
 *   bargaining power through various forms of leverage. The metrics reflect a
 *   medium-epsilon constraint regime with ongoing, but not overwhelming,
 *   suppression and a degree of performative maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.55).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.45).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems: Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '548f5ef9-a2ee-4106-901d-86bee03cbc7b').
narrative_ontology:cs_kernel_codification('548f5ef9-a2ee-4106-901d-86bee03cbc7b', formalized).
narrative_ontology:cs_authority_grounding('548f5ef9-a2ee-4106-901d-86bee03cbc7b', practice).
narrative_ontology:cs_interpretation_layer_present('548f5ef9-a2ee-4106-901d-86bee03cbc7b').
narrative_ontology:cs_reading_relation('548f5ef9-a2ee-4106-901d-86bee03cbc7b', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('548f5ef9-a2ee-4106-901d-86bee03cbc7b', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('548f5ef9-a2ee-4106-901d-86bee03cbc7b', foundational, sovereignty_and_autonomy_are_co_constitutive).
narrative_ontology:cs_axiom_status(sovereignty_and_autonomy_are_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('548f5ef9-a2ee-4106-901d-86bee03cbc7b', sovereignty_and_autonomy_are_co_constitutive, conventional).
narrative_ontology:cs_axiom('548f5ef9-a2ee-4106-901d-86bee03cbc7b', foundational, political_accommodation_is_primary_dispute_resolution).
narrative_ontology:cs_axiom_status(political_accommodation_is_primary_dispute_resolution, holdable).
narrative_ontology:cs_axiom_grounding('548f5ef9-a2ee-4106-901d-86bee03cbc7b', political_accommodation_is_primary_dispute_resolution, conventional).
narrative_ontology:cs_reference_frame('548f5ef9-a2ee-4106-901d-86bee03cbc7b', dynamic_equilibrium_framework).
narrative_ontology:cs_drift_state('548f5ef9-a2ee-4106-901d-86bee03cbc7b', contemporary_political_climate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('548f5ef9-a2ee-4106-901d-86bee03cbc7b', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hksar_government).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hksar_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereignty while engaging in political accommodation to maintain stability and economic ties. Benefits from the framework's ability to integrate Hong Kong without immediate, disruptive conflict, and from the gradual assertion of central authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Operates the local administration, balancing the demands of the central government with the aspirations of Hong Kong society. Benefits from the stability provided by the framework but bears the costs of constant negotiation and compromise, often having to implement policies that are unpopular locally.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hksar_government, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hksar_government, payer).

% Experiences the direct impact of political accommodation, often seeing its demands for greater autonomy or civil liberties curtailed in the name of 'balance.' Retains some bargaining power through economic and international leverage, but pays the cost of compromise.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Monitors the implementation of the framework, often expressing concerns about the erosion of autonomy or civil liberties. Can apply diplomatic or economic pressure, influencing the terms of political accommodation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the coexistence of two distinct legal and political systems within a single sovereign state, preventing direct conflict and facilitating the integration of Hong Kong into the PRC while preserving its unique characteristics.
% TRANSFER_FUNCTION: Transfers political concessions, legal interpretations, and economic benefits between the PRC and HKSAR, often at the expense of absolute claims from either side, and sometimes at the cost of civil society's aspirations for greater autonomy.
% ABSENT_VOICES: Those advocating for absolute sovereignty (e.g., full integration into mainland system) or absolute autonomy (e.g., full independence for Hong Kong) are structurally marginalized by this framework, as it explicitly rejects both extremes in favor of a negotiated middle ground.
% DISAPPEARANCE_RATIONALE: If the 'One Country, Two Systems' framework vanished overnight, it would likely lead to either full integration under PRC law or a push for full independence, both of which would fundamentally alter the political, legal, and economic landscape of Hong Kong and its relationship with the mainland and the international community.
% FOUNDING_PROBLEM: The framework was established to facilitate the peaceful and stable transfer of sovereignty over Hong Kong from the United Kingdom to the People's Republic of China, ensuring Hong Kong's distinct capitalist system, common law, and civil liberties would be preserved for 50 years, while affirming PRC sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: International observers, legal scholars, and various political factions (outside the direct benefiting parties) corroborate the ongoing challenge of balancing these principles, even if they dispute the current implementation or the extent to which the original promises are being upheld. The need for 'substantive negotiation' remains a live issue.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The framework functions as a Tangled Rope because it genuinely coordinates the integration of two distinct systems (preventing outright conflict) while simultaneously involving asymmetric extraction. The 'political accommodation' often means one side (Hong Kong civil society) yields more, bearing the costs of compromise. Active enforcement is required to manage the contested boundaries and ensure compliance with negotiated outcomes. The moderate suppression reflects that while absolute claims are suppressed, outright coercion is not the primary mode of operation in this reading; rather, it's a managed tension. Theater ratio is moderate as public discourse often emphasizes 'mutual respect' and 'win-win' outcomes even when underlying power dynamics are shifting.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the PRC Central Government, this framework is a successful coordination mechanism for national unity. From the perspective of Hong Kong civil society, it is a structure that increasingly extracts concessions and limits freedoms, even if it avoids outright absorption. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government is a primary beneficiary, gaining stability and gradual assertion of sovereignty. The HKSAR Government also benefits from the framework's stability but acts as a payer by mediating and implementing compromises. Hong Kong civil society is a primary target/payer, bearing the costs of curtailed autonomy and civil liberties. The international community acts as an observer, influencing the dynamics but not directly participating in the internal 'accommodation' process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negotiation_vs_imposition_ambiguity,
    'Is ''political accommodation'' within this framework a genuine negotiation between parties with real leverage, or a veiled imposition of central authority''s will?',
    'Analysis of negotiation outcomes over time: if outcomes consistently favor one party despite significant resistance from the other, it suggests imposition. Examination of the actual mechanisms of ''accommodation'' and the relative power of the parties involved.',
    'If primarily imposition, the constraint''s effective extractiveness and suppression are higher than currently measured, shifting its classification closer to a Snare. If genuine negotiation, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_vs_imposition_ambiguity, empirical, 'Ambiguity regarding the true nature of ''political accommodation'' in the framework.').

omega_variable(
    civil_society_leverage_durability,
    'How durable is Hong Kong civil society''s bargaining power (economic/international leverage) in the face of increasing central authority, and what is its actual impact on ''accommodation'' outcomes?',
    'Longitudinal study of civil society''s ability to resist or modify central government policies, and the correlation between international pressure/economic factors and policy shifts. Assessment of the erosion of independent institutions that historically amplified civil society''s voice.',
    'If civil society''s leverage is significantly diminished, its ''constrained'' exit option moves closer to ''trapped,'' increasing its directionality and the effective extraction it experiences. This would push the overall constraint towards a more extractive classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_leverage_durability, empirical, 'Uncertainty about the long-term effectiveness of civil society''s bargaining power.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''balanced coexistence'' reading of the ''One Country, Two Systems'' kernel, or does it inadvertently incorporate elements of the ''sovereignty primacy'' or ''autonomy primacy'' readings?',
    'Expert review by scholars specializing in Hong Kong constitutional law and political systems, specifically comparing the authored axioms and structural relationships against the core tenets of each reading. Iterative refinement based on feedback.',
    'If elements of other readings are present, the constraint''s epsilon and stakeholder directionalities may be miscalibrated, leading to an inaccurate classification for this specific reading. Re-authoring would be required to ensure ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ensuring the fidelity of this constraint story to its declared kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(one__tr_t18, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(one__tr_t24, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(one__tr_t30, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(one__be_t18, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(one__be_t24, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(one__be_t30, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(one__su_t18, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(one__su_t24, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(one__su_t30, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'One Country, Two Systems' framework, focusing on the dynamic balance and political accommodation. It is linked to sibling readings that emphasize either sovereignty or autonomy primacy, as these interpretations are in constant tension and influence each other's operational space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
