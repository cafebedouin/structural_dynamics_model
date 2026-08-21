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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Constitutional Subordination to Federal Authority
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint describes the 'constitutional subordination' reading of
 *   provincial sovereignty within a federal system, where provinces are seen
 *   as creations of the federal constitution with no inherent sovereignty.
 *   Exit (secession) requires federal consent, and federal policies like
 *   equalization and climate regulation are considered legitimate exercises
 *   of federal authority. This reading directly opposes 'compact federalism'
 *   and 'resource sovereignty primacy' readings, which assert greater
 *   provincial autonomy. The constraint is claimed as a Rope by its
 *   proponents (federal government, equalization-receiving provinces) but
 *   operates as a Tangled Rope due to its substantial extraction from
 *   resource-rich provinces and active suppression of alternative
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.65).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.75).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Constitutional Subordination to Federal Authority").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '344e051b-b942-4504-a215-b03e106190a4').
narrative_ontology:cs_kernel_codification('344e051b-b942-4504-a215-b03e106190a4', fixed_text).
narrative_ontology:cs_authority_grounding('344e051b-b942-4504-a215-b03e106190a4', lineage).
narrative_ontology:cs_interpretation_layer_present('344e051b-b942-4504-a215-b03e106190a4').
narrative_ontology:cs_reading_relation('344e051b-b942-4504-a215-b03e106190a4', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_reading_relation('344e051b-b942-4504-a215-b03e106190a4', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('344e051b-b942-4504-a215-b03e106190a4', foundational, federal_supremacy_in_constitutional_matters).
narrative_ontology:cs_axiom_status(federal_supremacy_in_constitutional_matters, holdable).
narrative_ontology:cs_axiom_grounding('344e051b-b942-4504-a215-b03e106190a4', federal_supremacy_in_constitutional_matters, conventional).
narrative_ontology:cs_axiom('344e051b-b942-4504-a215-b03e106190a4', foundational, provinces_derive_power_from_constitution).
narrative_ontology:cs_axiom_status(provinces_derive_power_from_constitution, holdable).
narrative_ontology:cs_axiom_grounding('344e051b-b942-4504-a215-b03e106190a4', provinces_derive_power_from_constitution, conventional).
narrative_ontology:cs_reference_frame('344e051b-b942-4504-a215-b03e106190a4', constitutional_act_1867_framework).
narrative_ontology:cs_drift_state('344e051b-b942-4504-a215-b03e106190a4', contemporary_federal_provincial_relations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('344e051b-b942-4504-a215-b03e106190a4', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate constitutional authority over provinces, including a veto on secession and the right to implement national policies like equalization and climate regulation. Benefits from a unified national market and political stability.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Are constitutionally bound to federal frameworks, including equalization payments and federal environmental regulations, which they perceive as limiting their resource development and fiscal autonomy. Their ability to challenge federal authority is limited by constitutional interpretation and federal enforcement.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    powerful, biographical, constrained, regional).

% Benefit from federal equalization payments, which are a direct transfer of wealth from richer to poorer provinces, reinforcing the constitutional subordination framework. They generally support strong federal powers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces, beneficiary,
    organized, generational, mobile, regional).

% Seek to challenge the constitutional subordination of provinces, arguing for a right to self-determination and secession. They are structurally excluded from the federal constitutional framework that denies their claims to inherent sovereignty, requiring extra-constitutional means for their goals.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, excluded,
    moderate, generational, trapped, local).

% Acts as the ultimate arbiter of constitutional disputes between federal and provincial governments. Its interpretations define the boundaries of provincial sovereignty and federal power, shaping the operational reality of this constraint.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of constitutional authority, ensuring national unity, a common market, and the ability to implement national policies like equalization and environmental protection across diverse regions.
% TRANSFER_FUNCTION: Transfers fiscal resources from wealthier provinces to the federal government (and then to equalization-receiving provinces) and transfers policy authority from provinces to the federal level on matters of national scope.
% ABSENT_VOICES: Advocates for inherent provincial sovereignty and a unilateral right to secession are structurally excluded from the constitutional discourse that defines provinces as creatures of the federal constitution. They would argue for a 'compact' theory of confederation.
% DISAPPEARANCE_RATIONALE: If the principle of provincial constitutional subordination vanished, the Canadian federation would immediately face a constitutional crisis. Provinces would assert greater autonomy, potentially leading to unilateral declarations of sovereignty, a breakdown of national programs, and a fundamental reordering of political and economic power.
% FOUNDING_PROBLEM: The original problem was to create a unified nation-state from disparate colonies, balancing regional identities with a strong central government to ensure stability and collective action.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars outside of the federal government generally corroborate that the founding problem of balancing unity and regionalism remains live, though the specific tensions and interpretations have evolved over time. The Supreme Court's rulings consistently reinforce the federal constitutional framework.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) stems from the mandatory fiscal transfers (equalization) and policy limitations imposed on resource-rich provinces by federal authority. Suppression (0.75) is high because the federal government actively uses legal and political means to enforce this constitutional interpretation, including challenging provincial legislation and denying unilateral secession. Theater ratio (0.20) is low as the federal government genuinely believes in and actively maintains this constitutional order, though some performative aspects exist in federal-provincial negotiations. The increasing extractiveness and suppression over time reflect the hardening of federal positions on national unity and policy scope.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this is a necessary and legitimate framework for national coordination. From resource-rich provinces, it is an extractive mechanism that limits their self-determination. The engine's classification will reflect this divergence, likely showing a Rope for beneficiaries and a Snare/Tangled Rope for payers/excluded parties.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and equalization-receiving provinces are beneficiaries, as this reading legitimizes federal power and ensures fiscal transfers. Resource-rich provinces are payers, bearing the costs of transfers and federal policy constraints. Separatist movements are structurally excluded and targeted by the enforcement mechanisms of this reading, as their core claim of inherent sovereignty is denied. The Supreme Court acts as an observer, interpreting the constitutional boundaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is the ''constitutional subordination'' reading the only valid interpretation of the federal constitution, or are ''compact federalism'' and ''resource sovereignty primacy'' equally defensible readings?',
    'A definitive, universally accepted constitutional amendment or a series of Supreme Court rulings that explicitly and unambiguously foreclose alternative interpretations, which is unlikely given the nature of constitutional law.',
    'If alternative readings gain legal or political traction, the constraint''s legitimacy would erode, reducing its effective suppression and extractiveness. If this reading is definitively established as the sole valid one, its Mountain-like qualities would increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity in constitutional interpretation regarding provincial sovereignty.').

omega_variable(
    federal_consent_for_secession,
    'What constitutes ''federal consent'' for provincial secession, and under what conditions would it be granted or withheld?',
    'A clear legislative framework or a Supreme Court reference establishing the precise legal and political conditions for federal consent to secession, which currently does not exist in detail.',
    'Clarifying the conditions for consent could either reduce the perceived suppression (if conditions are reasonable) or increase it (if conditions are deemed impossible to meet), affecting the exit options for separatist movements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_consent_for_secession, preference, 'Uncertainty regarding the process and conditions for federal consent to provincial secession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(prov_tr_t2008, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(prov_be_t2008, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2008, 0.63).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(prov_su_t2008, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2008, 0.73).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_equalization_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'provincial_sovereignty_boundary' kernel. The other readings are 'compact_federalism' and 'resource_sovereignty_primacy', each representing a distinct structural claim about provincial autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
