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
 *   This constraint story describes the 'constitutional subordination'
 *   reading of the provincial sovereignty boundary in Canada. Under this
 *   reading, provinces are legal creations of the federal constitution,
 *   possessing no inherent sovereignty, and their exit from the federation
 *   requires federal consent. This interpretation grants the federal
 *   government significant authority over provinces, particularly in areas of
 *   national policy and resource governance, and legally nullifies unilateral
 *   secessionist claims. The constraint is structurally a Tangled Rope, as it
 *   provides a coordination function (national unity) but also involves
 *   substantial, actively enforced extraction from provinces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.78).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.85).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.78).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'b22b7189-a160-476c-abd5-3f44ace32bfa').
narrative_ontology:cs_kernel_codification('b22b7189-a160-476c-abd5-3f44ace32bfa', fixed_text).
narrative_ontology:cs_authority_grounding('b22b7189-a160-476c-abd5-3f44ace32bfa', extraction).
narrative_ontology:cs_interpretation_layer_present('b22b7189-a160-476c-abd5-3f44ace32bfa').
narrative_ontology:cs_reading_relation('b22b7189-a160-476c-abd5-3f44ace32bfa', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('b22b7189-a160-476c-abd5-3f44ace32bfa', provincial_sovereignty_boundary__resource_sovereignty_primacy, forecloses).
narrative_ontology:cs_axiom('b22b7189-a160-476c-abd5-3f44ace32bfa', foundational, federal_constitution_is_supreme_law).
narrative_ontology:cs_axiom_status(federal_constitution_is_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('b22b7189-a160-476c-abd5-3f44ace32bfa', federal_constitution_is_supreme_law, conventional).
narrative_ontology:cs_axiom('b22b7189-a160-476c-abd5-3f44ace32bfa', foundational, provinces_derive_power_from_federal_act).
narrative_ontology:cs_axiom_status(provinces_derive_power_from_federal_act, holdable).
narrative_ontology:cs_axiom_grounding('b22b7189-a160-476c-abd5-3f44ace32bfa', provinces_derive_power_from_federal_act, conventional).
narrative_ontology:cs_reference_frame('b22b7189-a160-476c-abd5-3f44ace32bfa', unqualified_federal_supremacy).
narrative_ontology:cs_drift_state('b22b7189-a160-476c-abd5-3f44ace32bfa', contemporary_federal_provincial_relations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b22b7189-a160-476c-abd5-3f44ace32bfa', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_unity_advocates).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces the constitutional principle that provinces derive their powers from the federal constitution, not from inherent sovereignty. Benefits from the ability to implement national policies (e.g., equalization, climate) and prevent provincial secession.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate within the constitutional framework, often challenging federal authority but ultimately bound by it. Bear the cost of federal policy imposition and the inability to unilaterally assert full sovereignty over their territories or resources.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments, payer,
    institutional, biographical, constrained, regional).

% Interprets the federal constitution, consistently upholding the principle of federal supremacy and the legal impossibility of unilateral provincial secession. Its rulings reinforce the constraint.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for provincial independence, but their efforts are legally nullified by this constitutional reading. They are structurally trapped by the federal legal framework, with no constitutional path to exit without federal consent.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, excluded).

% Benefit from a strong, unified federal state and the constitutional clarity that prevents provincial fragmentation. Their ideological position is vindicated by this reading of the constitution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_unity_advocates, beneficiary,
    organized, biographical, mobile, national).

% Provinces with significant natural resources that assert greater control over their development and revenues. They are constrained by federal jurisdiction over interprovincial trade, environmental policy, and equalization payments, which limits their de facto resource sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_provinces, payer,
    powerful, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of constitutional authority, ensuring national unity, federal policy coherence, and the stability of the Canadian federation by defining the limits of provincial power.
% TRANSFER_FUNCTION: Transfers ultimate constitutional authority and policy control from provinces to the federal level, particularly in areas deemed of national interest (e.g., equalization, climate policy, national security), and prevents provinces from unilaterally exiting the federation.
% ABSENT_VOICES: Indigenous nations, whose inherent sovereignty predates the federal constitution and who would challenge the premise of federal or provincial jurisdiction over their lands and peoples. International legal bodies might also offer alternative interpretations of self-determination.
% DISAPPEARANCE_RATIONALE: If this constitutional principle vanished, the Canadian federation would likely dissolve into a loose confederation or multiple independent states. Provinces would assert full sovereignty, leading to massive political, economic, and social reorganization, including potential border disputes and trade barriers.
% FOUNDING_PROBLEM: To create a unified, stable nation-state from disparate British colonies, preventing fragmentation, ensuring a strong central government capable of national defense and economic integration, and avoiding the weaknesses of a purely confederal system.
% FOUNDING_PROBLEM_CORROBORATION: Federal legal scholars, historical constitutional documents (e.g., BNA Act 1867), and numerous Supreme Court of Canada rulings consistently corroborate that the founding problem of national unity and federal supremacy remains live and central to the Canadian constitutional order. This is attested by sources outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.78) reflects the significant policy and fiscal autonomy denied to provinces by this reading, particularly in areas like resource control and equalization. Suppression (0.85) is very high because the federal legal framework, backed by the Supreme Court, actively and effectively suppresses any claims of inherent provincial sovereignty or unilateral secession. Theater ratio is low (0.1) because the enforcement of federal constitutional supremacy is real and functional, not merely performative. Resistance is high (0.7) due to ongoing political and legal challenges from provincial governments and separatist movements, but this resistance is largely contained by the robust enforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this constraint is a foundational 'Mountain' ensuring national stability and the common good. From the perspective of provincial governments or separatist movements, it is a 'Snare' or 'Tangled Rope' that extracts autonomy and resources through coercive legal mechanisms. The engine's classification as Tangled Rope reflects the analytical assessment of its dual function and high extraction, independent of the federal claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and national unity advocates are clear beneficiaries, gaining from a strong, unified state and the ability to implement national policies. Provincial governments, separatist movements, and resource provinces are targets, bearing the costs of limited autonomy, federal policy imposition, and the legal impossibility of unilateral exit. The Supreme Court acts as a key agenda-setter, interpreting and reinforcing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (national unity, federal coherence) is still considered 'live' by its beneficiaries and corroborating sources. However, the high and increasing extractiveness, coupled with high resistance, suggests that while the founding problem persists, the mechanism for solving it has become increasingly extractive, potentially beyond what is necessary for coordination. This prevents mislabeling it as a pure Rope, acknowledging the significant costs borne by provinces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine constitutional principle (as asserted by the ''constitutional subordination'' reading), or a constructed interpretation that benefits the federal center (as implied by ''compact federalism'' or ''resource sovereignty primacy'' readings)?',
    'Comparative constitutional analysis of other federations, historical review of the intent of confederation, and ongoing public discourse on federal-provincial relations.',
    'If resolved towards a ''constructed'' interpretation, the constraint''s extractiveness and suppression might be re-evaluated as less inherent to the constitutional structure and more a product of federal power, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between constitutional principle and power-driven interpretation.').

omega_variable(
    founding_problem_status_contestation,
    'Is the founding problem of national unity and federal coherence truly ''live'' as asserted by this reading, or has it been substantially resolved, with the constraint now serving primarily extractive purposes?',
    'Independent sociological and political science research on national identity, regional alienation, and the actual threat of provincial fragmentation, rather than relying solely on federal government assertions.',
    'If the founding problem is found to be ''dead'' or significantly diminished, the constraint''s high extractiveness would be harder to justify as a coordination cost, pushing its classification closer to a Snare or Piton (if maintenance becomes purely theatrical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_contestation, empirical, 'Contestation over the ongoing relevance of the constraint''s founding problem.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (constitutional/legal barriers) or internalized (provinces accepting their subordinate role due to historical precedent or perceived futility of resistance)?',
    'Analysis of provincial government rhetoric and policy choices: if provinces consistently challenge federal authority despite legal setbacks, suppression is primarily structural. If challenges diminish over time, internalized suppression may be growing.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as provinces carry the suppression with them even in the absence of direct federal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in federal-provincial relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(prov_tr_t1992, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(prov_tr_t2002, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(prov_tr_t2012, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2012, 0.09).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.65).
narrative_ontology:measurement(prov_be_t1992, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1992, 0.7).
narrative_ontology:measurement(prov_be_t2002, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2002, 0.73).
narrative_ontology:measurement(prov_be_t2012, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2012, 0.76).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.75).
narrative_ontology:measurement(prov_su_t1992, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1992, 0.8).
narrative_ontology:measurement(prov_su_t2002, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2002, 0.82).
narrative_ontology:measurement(prov_su_t2012, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2012, 0.84).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_equalization_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'provincial_sovereignty_boundary' kernel. This 'constitutional_subordination' reading emphasizes federal supremacy and the derived nature of provincial power, contrasting with 'compact_federalism' (provinces as co-equal founders) and 'resource_sovereignty_primacy' (provincial resource control as absolute sovereignty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
