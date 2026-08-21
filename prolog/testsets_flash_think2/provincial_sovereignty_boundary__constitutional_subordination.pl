% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Constitutional Subordination of Provinces
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint describes the 'constitutional subordination' reading of
 *   provincial sovereignty in a federal system, where provinces are
 *   considered creations of the federal constitution with no inherent
 *   sovereignty, and any exit requires federal consent. This reading
 *   legitimizes strong federal authority over national policy and resource
 *   governance, viewing provincial challenges as resistance to the
 *   constitutional order. The metrics reflect the substantial extraction of
 *   provincial autonomy and the active suppression of alternatives to the
 *   federal framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.7).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.8).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e').
narrative_ontology:cs_kernel_codification('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', fixed_text).
narrative_ontology:cs_authority_grounding('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', lineage).
narrative_ontology:cs_interpretation_layer_present('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e').
narrative_ontology:cs_reading_relation('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', provincial_sovereignty_boundary__resource_sovereignty_primacy, forecloses).
narrative_ontology:cs_axiom('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', foundational, federal_paramountcy_doctrine).
narrative_ontology:cs_axiom_status(federal_paramountcy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', federal_paramountcy_doctrine, conventional).
narrative_ontology:cs_axiom('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', foundational, constitutional_unity_principle).
narrative_ontology:cs_axiom_status(constitutional_unity_principle, holdable).
narrative_ontology:cs_axiom_grounding('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', constitutional_unity_principle, deontological).
narrative_ontology:cs_reference_frame('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', constitutional_act_1867_original_intent).
narrative_ontology:cs_drift_state('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', contemporary_federal_provincial_relations, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0944eb7a-d7a6-45e3-aa1a-1d3b457ae80e', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, beneficiary_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, citizens_of_canada).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provinces_seeking_autonomy).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, citizens_of_canada).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces federal paramountcy, constitutional unity, and the requirement for federal consent for any provincial exit or fundamental change to the federation. Benefits from the stability and authority of a strong central state, and from the ability to implement national policies like equalization and climate action.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Bear the costs of federal policy imposition (e.g., climate regulations, spending power initiatives) and the denial of their claims to greater sovereignty or self-determination. Their options are limited to legal challenges, political resistance, or seeking constitutional amendment, all within the federal framework.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provinces_seeking_autonomy, payer,
    organized, generational, constrained, national).

% Experience federal authority over resource development, environmental regulation, and interprovincial trade, which can limit their ability to fully control and benefit from their natural resources. They also contribute to federal equalization payments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    organized, generational, constrained, national).

% Receive federal equalization payments and benefit from national programs and infrastructure funded by the federal government, which helps ensure a comparable level of public services across the country. They generally align with the constitutional subordination view.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, beneficiary_provinces, beneficiary,
    organized, generational, constrained, national).

% Interprets the federal constitution, adjudicating disputes between federal and provincial governments. Its rulings reinforce or clarify the boundaries of federal and provincial powers, often upholding the principle of constitutional subordination.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, observer,
    institutional, civilizational, analytical, national).

% Are structurally excluded from the legitimate constitutional discourse regarding provincial exit, as their goals are deemed unconstitutional without federal consent. They operate outside the recognized federal framework, facing legal and political barriers to their objectives.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, excluded,
    organized, generational, identity_locked, regional).

% Benefit from the stability, common market, and national social programs provided by the federal system. They also bear the costs of federal taxation and, indirectly, the political friction between federal and provincial governments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, citizens_of_canada, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, citizens_of_canada, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified federal state with a strong central government capable of national governance, ensuring a common market, national defense, and inter-regional equity through mechanisms like equalization payments.
% TRANSFER_FUNCTION: Transfers legislative and fiscal authority from provinces to the federal government in areas of federal jurisdiction, and financial resources from wealthier provinces (via federal taxation) to less wealthy provinces through equalization.
% ABSENT_VOICES: Separatist movements and proponents of absolute provincial sovereignty are structurally excluded from the constitutional conversation, as their core claims are deemed illegitimate by the federal constitutional order. They would argue for a compact-based or resource-sovereignty-based federalism.
% DISAPPEARANCE_RATIONALE: If the principle of constitutional subordination vanished, provinces would immediately assert inherent sovereignty, leading to a rapid fragmentation of federal authority, potential secession attempts, and a complete renegotiation of the Canadian federation, likely dissolving the current constitutional order.
% FOUNDING_PROBLEM: To establish a strong, unified federal state capable of national governance, preventing provincial fragmentation and ensuring peace, order, and good government across a vast and diverse territory.
% FOUNDING_PROBLEM_CORROBORATION: The federal government and federal institutions consistently attest that the founding problem of national unity and effective governance remains live, citing ongoing provincial challenges and global geopolitical instability. Academic constitutional scholars and some segments of the citizenry also corroborate the continued relevance of a strong federal center.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because federal policies (e.g., equalization, climate policy) can significantly impact provincial fiscal capacity and legislative autonomy. Suppression is high due to the constitutional denial of inherent provincial sovereignty and the requirement for federal consent for fundamental changes or exit, effectively trapping provinces within the federal structure. Theater ratio is low, as federal authority is actively exercised and defended, not merely performed. Resistance is high, reflecting ongoing provincial challenges to federal power. The claimed type is Tangled Rope because the federal system provides genuine coordination benefits (e.g., common market, national defense) but also involves significant asymmetric extraction of autonomy from provinces.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this constraint is a necessary Rope or even a Mountain, ensuring national unity and effective governance. From the perspective of provinces seeking greater autonomy, it operates as a Snare, trapping them in a subordinate position and extracting resources/autonomy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is a clear beneficiary, collecting authority and resources to maintain national unity and implement its agenda. Beneficiary provinces also benefit from federal transfers. Provinces seeking autonomy and resource-rich provinces are targets, as their claims to greater self-determination or resource control are subordinated to federal authority. Separatist movements are excluded, as their very existence challenges the constitutional premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'To what extent is the ''constitutional subordination'' reading a definitive legal interpretation versus a politically dominant narrative?',
    'Analysis of historical constitutional debates, judicial dissents, and comparative federal systems to identify alternative, equally coherent interpretations of federal-provincial relations.',
    'If primarily a dominant narrative, the constraint''s suppression and extractiveness might be conceptually lower, as alternatives are suppressed by political power rather than pure legal necessity. If definitive, the metrics are robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity between legal interpretation and political narrative in defining provincial status.').

omega_variable(
    federal_consent_for_exit_legitimacy,
    'Is the requirement for federal consent for provincial exit a legitimate constitutional principle or an assertion of power lacking democratic grounding?',
    'Referendum on secession in a province, followed by a Supreme Court reference on the legality of unilateral secession and the conditions for negotiation.',
    'If deemed illegitimate, the constraint''s suppression of provincial exit options would be reclassified as pure coercion, increasing its Snare-like qualities. If legitimate, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_consent_for_exit_legitimacy, preference, 'Legitimacy of federal veto over provincial exit.').

omega_variable(
    sovereignty_definition_contest,
    'Is ''sovereignty'' in a federal context an indivisible concept (as this reading implies) or a divisible one, allowing for shared or nested sovereignties?',
    'Comparative legal and political analysis of other federal states that explicitly recognize shared or divided sovereignty, and their stability outcomes.',
    'If sovereignty is divisible, the ''no inherent sovereignty'' premise of this reading is conceptually challenged, potentially lowering the perceived suppression and extractiveness from the provincial perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_definition_contest, conceptual, 'Conceptual contest over the indivisibility of sovereignty in a federal state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.25).
narrative_ontology:measurement(prov_tr_t1992, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(prov_tr_t2002, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2002, 0.21).
narrative_ontology:measurement(prov_tr_t2012, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(prov_be_t1992, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1992, 0.65).
narrative_ontology:measurement(prov_be_t2002, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2002, 0.68).
narrative_ontology:measurement(prov_be_t2012, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2012, 0.69).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.7).
narrative_ontology:measurement(prov_su_t1992, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1992, 0.75).
narrative_ontology:measurement(prov_su_t2002, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2002, 0.78).
narrative_ontology:measurement(prov_su_t2012, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2012, 0.79).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_equalization_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_framework).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_resource_development_permitting).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'provincial_sovereignty_boundary' kernel. It represents the 'constitutional subordination' view, emphasizing federal paramountcy and denying inherent provincial sovereignty. It forecloses the 'compact_federalism' and 'resource_sovereignty_primacy' readings due to fundamental contradictions in their core premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
