% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Negotiated Digital Commons Governance
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the commons_reading of the
 *   software_control_legitimacy kernel. The kernel conflates four
 *   structurally distinct claims about how software control should be
 *   legitimated: absolute user freedom (freedom_imperative_reading),
 *   pragmatic methodological openness (pragmatic_openness_reading),
 *   unilateral creator property (property_rights_reading), and negotiated
 *   collective management (this reading). The commons reading treats software
 *   control as a commons governance question: neither absolute freedom nor
 *   absolute property, but collectively managed shared infrastructure. The
 *   standing arrangement is the governance regime itself, which coordinates
 *   shared resource use while asymmetrically excluding absolutist frameworks
 *   from legitimacy.
 *
 * KEY AGENTS:
 *   - commons_stewards: agenda_setter (institutional/constrained) â administers governance protocols
 *   - stakeholder_communities: beneficiary (organized/constrained) â receives governance voice and shared access
 *   - libre_absolutists: payer (organized/identity_locked) â denied standing, excluded from governance participation
 *   - proprietary_absolutists: payer (institutional/arbitrage) â denied standing, excluded from governance participation
 *   - digital_political_economists: observer (analytical) â tracks legitimacy allocation across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.48).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.55).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Negotiated Digital Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '09aef6b2-21e6-49ea-aae4-238ffe6efaa0').
narrative_ontology:cs_kernel_codification('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', distributed).
narrative_ontology:cs_authority_grounding('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', practice).
narrative_ontology:cs_interpretation_layer_present('09aef6b2-21e6-49ea-aae4-238ffe6efaa0').
narrative_ontology:cs_reading_relation('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_axiom('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', foundational, digital_infrastructure_is_common_pool_resource).
narrative_ontology:cs_axiom_status(digital_infrastructure_is_common_pool_resource, holdable).
narrative_ontology:cs_axiom_grounding('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', digital_infrastructure_is_common_pool_resource, empirically_contingent).
narrative_ontology:cs_axiom('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', foundational, collective_legitimacy_over_unilateral_control).
narrative_ontology:cs_axiom_status(collective_legitimacy_over_unilateral_control, holdable).
narrative_ontology:cs_axiom_grounding('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', collective_legitimacy_over_unilateral_control, deontological).
narrative_ontology:cs_reference_frame('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', negotiated_collective_management).
narrative_ontology:cs_drift_state('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09aef6b2-21e6-49ea-aae4-238ffe6efaa0', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, libre_absolutists).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, proprietary_absolutists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the collective governance protocols for shared digital infrastructure, establishing boundaries between permissible communal use, prohibited enclosure, and prohibited unconditional release that would undermine the sustainability of the commons.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commons_stewards, agenda_setter,
    institutional, generational, constrained, global).

% Participate in negotiated rule-making for shared code, data, and platforms; receive governance voice and infrastructure access under commons terms, contingent on accepting collectively determined boundaries.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, biographical, constrained, global).

% Hold that all software must grant users complete freedom to run, copy, distribute, study, change, and improve. Under the commons reading, their maximalist position is denied governance standing because it rejects the legitimacy of boundary-setting itself.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, libre_absolutists, payer,
    organized, generational, identity_locked, global).

% Hold that software creators possess absolute property rights to restrict use, modification, and distribution. Under the commons reading, unilateral enclosure is prohibited and their framework is denied standing in collective governance.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_absolutists, payer,
    institutional, generational, arbitrage, global).

% Use digital infrastructure daily but are rarely party to the governance negotiations; their interests are mediated through stakeholder communities or absent entirely from commons rule-setting.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, platform_end_users, excluded,
    moderate, biographical, constrained, global).

% Analyze the commons governance structure as a third way between intellectual property absolutism and free-software absolutism, tracking how legitimacy is allocated across the kernel's competing readings.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, digital_political_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Governs shared digital infrastructure through collective negotiation, preventing both unilateral enclosure and chaotic overuse by establishing community-defined use boundaries and maintenance obligations.
% TRANSFER_FUNCTION: Moves governance authority over software from unilateral controllers to collective stakeholder communities; moves the compliance burden and exclusion cost to those who would prefer either unconditional release or exclusionary property.
% ABSENT_VOICES: Platform end-users and populations in jurisdictions with weak collective governance traditions are typically unrepresented; absolutist voices are present in discourse but denied formal standing within the commons governance framework itself.
% DISAPPEARANCE_RATIONALE: If commons governance vanished, digital infrastructure would likely trend toward proprietary enclosure or unconditional open release; stakeholder communities would lose negotiated intermediate frameworks, while absolutists would gain terrain. The parties dispute which outcome would dominate.
% FOUNDING_PROBLEM: Unmanaged digital infrastructure faces either tragedy of the commons (overuse, under-maintenance) or tragedy of the anti-commons (excessive enclosure, fragmented rights); pure property and pure freedom both fail to sustain shared resources over time.
% FOUNDING_PROBLEM_CORROBORATION: Digital commons scholars and Ostrom-adjacent political economists attest the problem from outside the benefiting stakeholder communities; proprietary and libre absolutists contest that the problem is correctly characterized as requiring collective management.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the commons regime genuinely coordinates shared infrastructure maintenance and use-rights, but it asymmetrically denies governance participation to absolutist positions. Suppression (0.55) reflects active boundary enforcement needed to prevent both enclosure and unconditional release. Theater ratio is low (0.25) because the governance function is substantive rather than performative. Accessibility collapse is moderate (0.40): pure proprietary and pure libre alternatives remain structurally available but are delegitimized within the commons frame. Resistance (0.50) comes from the excluded absolutist camps. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the stakeholder_communities seat, the constraint is coordination that sustains shared infrastructure; from the libre_absolutists and proprietary_absolutists seats, the same structure is extraction that denies their foundational commitments standing. The commons_stewards seat experiences it as necessary administration. The engine computes these divergences from the structural data rather than from any authored type claim.
 *
 * DIRECTIONALITY LOGIC:
 *   commons_stewards and stakeholder_communities sit near the beneficiary end (low d) because they gain governance capacity and shared infrastructure access. libre_absolutists and proprietary_absolutists sit near the target end (high d) because the constraint structurally excludes their preferred frameworks from legitimacy and participation. platform_end_users are diffuse and near-symmetric, bearing indirect costs of governance complexity without direct voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the genuine coordination function (shared infrastructure management, boundary-setting against enclosure) as pure extraction, while also preventing mislabeling the asymmetric exclusion of absolutist positions as mere neutral boundary-setting. If the founding problem of unmanaged digital infrastructure were dead but the regime persisted, the classification would drift toward piton; the authored founding_problem_status is live, so the coordination function retains structural justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_reading_kernel_location,
    'How does the commons reading''s exclusion of absolutist frameworks alter the legitimacy conditions for software control, and does this reading''s epsilon remain stable across institutional designs?',
    'Comparative institutional analysis across actual commons governance regimes (e.g., copyleft stewards, platform cooperatives, data trusts) measuring the standing afforded to absolutist positions.',
    'If all commons regimes structurally exclude absolutists, epsilon is higher and the classification edges toward snare; if some incorporate them, epsilon is lower and the classification edges toward rope. This determines whether the kernel requires decomposition into multiple constraints by governance type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_reading_kernel_location, conceptual, 'Kernel location and sibling reading structural delta for commons reading').

omega_variable(
    variable_epsilon_by_commons_design,
    'Does the extractiveness of commons governance remain moderate across all institutional designs, or do certain commons rules (e.g., opaque steward councils, restrictive contributory licenses) push the regime toward higher extraction?',
    'Cross-case measurement of extractiveness across diverse digital commons regimes with transparent versus opaque governance structures.',
    'High variance would indicate this reading''s epsilon is unstable and may require epsilon-invariance decomposition into separate constraints per regime type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(variable_epsilon_by_commons_design, empirical, 'Institutional design variance in commons extractiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t6, software_control_legitimacy__commons_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__commons_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(soft_tr_t18, software_control_legitimacy__commons_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__commons_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__commons_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t6, software_control_legitimacy__commons_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__commons_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(soft_be_t18, software_control_legitimacy__commons_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__commons_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__commons_reading, base_extractiveness, 30, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(soft_su_t6, software_control_legitimacy__commons_reading, suppression_requirement, 6, 0.43).
narrative_ontology:measurement(soft_su_t12, software_control_legitimacy__commons_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(soft_su_t18, software_control_legitimacy__commons_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__commons_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__commons_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% The natural-language phrase 'software control legitimacy' conflates four structurally distinct claims. This story is the commons reading; sibling stories handle freedom imperative, pragmatic openness, and property rights. Each has distinct epsilon values, beneficiary/victim structures, and directionalities, necessitating decomposition per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
