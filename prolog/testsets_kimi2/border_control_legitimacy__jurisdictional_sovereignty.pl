% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional Sovereignty Border Control Arrangement
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the jurisdictional_sovereignty reading of
 *   the contested kernel border_control_legitimacy. The standing arrangement
 *   under contest is the exercise of border control as an expression of
 *   territorial sovereignty, constrained by proportionality and necessity
 *   tests and legitimated through a balance of protection obligations, labor
 *   needs, and public consent. The reading acknowledges dual victim
 *   setsâexcluded migrants and displaced citizensâand situates
 *   enforcement legitimacy in that balance. Sibling readings include
 *   sovereignty_primary (absolute discretion to exclude as constitutive of
 *   statehood) and freedom_of_movement_primary (mobility as a fundamental
 *   human right overriding territorial sovereignty).
 *
 * KEY AGENTS:
 *   - state_apparatus: Primary agenda-setter (institutional/constrained) â administers border control, claims jurisdictional authority, balances enforcement against legitimacy constraints.
 *   - citizen_constituency: Primary beneficiary (organized/constrained) â receives protection and labor-market insulation, supplies or withholds democratic consent.
 *   - excluded_migrants: Primary target (powerless/trapped) â bears direct costs of border closure, denied mobility and protection.
 *   - displaced_citizens: Secondary target (moderate/constrained) â bears diffuse costs of enforcement or displacement from failed balance.
 *   - international_rights_bodies: Analytical observer (institutional/analytical) â monitors proportionality and human rights compliance without enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.62).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.71).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty Border Control Arrangement").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '3ee9f6a0-ea76-4d52-8899-c7e4f82482a4').
narrative_ontology:cs_kernel_codification('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', formalized).
narrative_ontology:cs_authority_grounding('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', lineage).
narrative_ontology:cs_interpretation_layer_present('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4').
narrative_ontology:cs_reading_relation('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', foundational, sovereignty_jurisdictional_not_closure).
narrative_ontology:cs_axiom_status(sovereignty_jurisdictional_not_closure, holdable).
narrative_ontology:cs_axiom_grounding('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', sovereignty_jurisdictional_not_closure, conventional).
narrative_ontology:cs_axiom('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', foundational, legitimacy_requires_balance).
narrative_ontology:cs_axiom_status(legitimacy_requires_balance, holdable).
narrative_ontology:cs_axiom_grounding('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', legitimacy_requires_balance, deontological).
narrative_ontology:cs_reference_frame('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', balanced_jurisdictional_sovereignty).
narrative_ontology:cs_drift_state('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', contemporary_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ee9f6a0-ea76-4d52-8899-c7e4f82482a4', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, citizen_constituency).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and exercises jurisdictional authority over territory, administering border control as a delegated function subject to constitutional and international law. Balances enforcement against labor market needs and public consent to maintain legitimacy. Derives budget, authority, and bureaucratic mandate from the arrangement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Receives the protection bundle and labor-market insulation promised by regulated admission. Grants or withdraws consent through democratic mechanisms. Experiences coordination benefits of ordered membership and the diffuse costs of enforcement taxation or social division.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizen_constituency, beneficiary,
    organized, biographical, constrained, national).

% Subject to border closure and exclusion despite claims to mobility, protection, or family reunification. Bear the direct costs of enforcement: denied entry, legal non-personhood, exposure to dangerous transit routes, and indefinite displacement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, regional).

% Citizens who bear the diffuse costs of the enforcement apparatus (fiscal burden, service erosion, moral injury) or who experience social and economic displacement when the balance between admission and enforcement fails. Victimized regardless of which side of the legitimacy balance collapses.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, national).

% Monitor state compliance with proportionality, necessity, and human rights standards. Publish legal opinions and findings that shape legitimacy discourse but lack direct enforcement power over sovereign border decisions.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for territorial jurisdiction that regulates rights and obligations within a defined territory, offering ordered membership, public protection, and labor-market governance without requiring absolute exclusion.
% TRANSFER_FUNCTION: Transfers mobility rights from excluded migrants to the state apparatus and citizen constituency; transfers fiscal and social costs of enforcement and admission onto displaced citizens; extracts compliance and loyalty from all subjects within the territory.
% ABSENT_VOICES: Excluded migrants are physically absent from the democratic polity that decides their exclusion; future citizens and unborn descendants who might benefit from open borders are never in the room; frontline border communities and transnational families are structurally underrepresented in sovereignty debates.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the international system of territorial jurisdiction would lose its primary mechanism for regulating membership. States would lack the standing arrangement to balance protection with labor needs; migration flows would reorganize around market and kinship networks rather than state permission; the category of excluded migrant would dissolve, but so would the guaranteed protection bundle for citizens.
% FOUNDING_PROBLEM: How to organize political community and membership in a world of territorial states without either absolute exclusion (which generates humanitarian costs and legitimacy crises) or unregulated admission (which erodes public consent and protection capacity).
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and political theorists outside the state apparatus attest the founding problem remains live; human rights organizations corroborate that current enforcement generates the predicted legitimacy crises; populist movements corroborate that admission undermines consent. No neutral party attests the current balance is optimal.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the arrangement systematically denies mobility rights to excluded migrants and imposes diffuse costs on displaced citizens while concentrating authority and protection benefits in the state and citizenry. Suppression is high (0.71) because the constraint depends on actively excluding rival mobility regimes and alternatives to state-controlled admission. Theater is moderate (0.40) because enforcement increasingly serves symbolic sovereignty displays alongside functional migration management. Accessibility collapse is moderate (0.60): alternatives such as open borders or free-movement regimes are intellectually available but politically and institutionally collapsed. Resistance is moderate (0.55): active resistance from migrants, civil society, and some sub-state actors meets the enforcement apparatus. The measurement series show extraction and theater rising over the interval as enforcement hardened and the legitimacy balance became more precarious.
 *
 * PERSPECTIVAL GAP:
 *   The excluded migrant seat experiences the constraint as pure extraction (high d, high effective extraction), while the citizen constituency experiences it as coordination with diffuse costs (low d, damped extraction). The state apparatus experiences it as authority maintenance (low d) with fiscal and political costs. The engine computes these divergences from the structural beneficiary/victim declarations and exit modulations. The dual victim structure ensures that even citizens are not uniformly beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state_apparatus, citizen_constituency) derive low directionality because the constraint subsidizes their authority and protection. Victims (excluded_migrants, displaced_citizens) derive high directionality because the constraint extracts mobility and imposes fiscal, social, and moral costs. The dual victim set is the distinctive structural feature of this reading: regardless of whether enforcement hardens or admission liberalizes, one of the two victim sets is activated, preventing the arrangement from resolving into a pure coordination story.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging both coordination (jurisdictional order, citizenship protection, labor governance) and extraction (exclusion, displacement, enforcement overhead). Without the dual victim set, the arrangement could be misread as pure coordination (Rope) by beneficiaries or pure extraction (Snare) by excluded migrants alone. The tangled_rope classification captures the hybridity: the same structure that coordinates citizenship protection also extracts from mobility seekers and from citizens who bear the costs of failed balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the jurisdictional sovereignty reading a modification of Westphalian lineage or a rupture from it, and does it foreclose absolute sovereignty entirely or merely coexist with it in competing legal traditions?',
    'Comparative constitutional and international law analysis tracking whether states that adopt proportionality-bound border control explicitly reject absolute discretion doctrines or merely layer limits on top of them.',
    'If the reading is a rupture, its classification as foreclosing sovereignty_primary is structurally secure; if it is a layered modification, the kernel may better be modeled as influence rather than foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship of this reading to Westphalian sovereignty lineage').

omega_variable(
    dual_victim_priority,
    'When enforcement violates human rights and admission undermines public consent simultaneously, which victim set''s grievance determines the legitimacy crisis, and can the constraint survive a legitimacy crisis on both fronts?',
    'Empirical case studies of states facing simultaneous border enforcement scandals and anti-immigrant political backlash; track whether institutional reform or collapse follows.',
    'If the constraint cannot survive dual-front crisis, its coordination function is more fragile than the metrics suggest and the effective extraction may be higher due to precarious enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_victim_priority, empirical, 'Which victim set drives legitimacy failure when both are activated').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function of territorial jurisdiction and ordered membership be preserved while decoupling the extractive function of border closure and exclusion?',
    'Natural experiment from jurisdictions that have decoupled territorial regulation from border closure (e.g., regional free-movement zones with retained jurisdictional authority).',
    'If separable, the extractive component is a policy choice rather than a necessary cost of coordination, strengthening the tangled_rope classification; if inseparable, part of the measured extraction is coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether jurisdiction and border closure are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_js_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bcl_js_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.2).
narrative_ontology:measurement(bcl_js_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.25).
narrative_ontology:measurement(bcl_js_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.3).
narrative_ontology:measurement(bcl_js_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.34).
narrative_ontology:measurement(bcl_js_tr_t25, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 25, 0.38).
narrative_ontology:measurement(bcl_js_tr_t30, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(bcl_js_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bcl_js_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bcl_js_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(bcl_js_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(bcl_js_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(bcl_js_be_t25, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(bcl_js_be_t30, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bcl_js_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bcl_js_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(bcl_js_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(bcl_js_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(bcl_js_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(bcl_js_su_t25, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(bcl_js_su_t30, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, identity_coordination).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel border_control_legitimacy. The sovereignty_primary reading treats border closure as constitutive of statehood; the freedom_of_movement_primary reading treats mobility as a fundamental right overriding territorial claims; this reading treats sovereignty as jurisdictional authority separable from closure, requiring a balance that produces dual victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
