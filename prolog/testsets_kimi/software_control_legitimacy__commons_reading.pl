% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Software Control Legitimacy â Commons Reading
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the commons_reading of the
 *   software_control_legitimacy kernel. It treats software control as a
 *   commons governance question requiring negotiated collective management of
 *   shared digital infrastructure. Both absolutist positionsâproprietary
 *   property rights and the freedom imperativeâare structurally excluded
 *   from governance participation, making them victims of the constraint,
 *   while pragmatic stakeholder communities and commons institutions benefit
 *   from the coordination framework. The reading stands in contrast to
 *   sibling readings that frame software control as absolute user freedom,
 *   pragmatic methodology choice, or creator property right.
 *
 * KEY AGENTS:
 *   - commons_governance_institutions: Primary agenda-setter (institutional/constrained) â administers the negotiated rules
 *   - pragmatic_stakeholder_communities: Primary beneficiary (organized/constrained) â gains governance voice and shared resources
 *   - software_foundations: Secondary beneficiary (institutional/constrained) â stewards legal and financial infrastructure
 *   - freedom_absolutists: Primary target (moderate/identity_locked) â denied governance participation for rejecting negotiated constraints
 *   - property_absolutists: Secondary target (powerful/constrained) â denied governance participation for rejecting collective obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.62).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.58).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control Legitimacy â Commons Reading").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '60d68e7c-0dd5-437d-ae2d-731f43bf91b8').
narrative_ontology:cs_kernel_codification('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', distributed).
narrative_ontology:cs_authority_grounding('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', practice).
narrative_ontology:cs_interpretation_layer_present('60d68e7c-0dd5-437d-ae2d-731f43bf91b8').
narrative_ontology:cs_reading_relation('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', foundational, software_as_common_pool_resource).
narrative_ontology:cs_axiom_status(software_as_common_pool_resource, holdable).
narrative_ontology:cs_axiom_grounding('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', software_as_common_pool_resource, conventional).
narrative_ontology:cs_axiom('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', foundational, negotiated_governance_excludes_absolutes).
narrative_ontology:cs_axiom_status(negotiated_governance_excludes_absolutes, holdable).
narrative_ontology:cs_axiom_grounding('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', negotiated_governance_excludes_absolutes, conventional).
narrative_ontology:cs_reference_frame('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', stakeholder_consensus_practice).
narrative_ontology:cs_drift_state('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', corporate_sponsorship_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60d68e7c-0dd5-437d-ae2d-731f43bf91b8', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, pragmatic_stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, commons_governance_institutions).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, software_foundations).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, freedom_absolutists).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, property_absolutists).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, commons_based_governance).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, stakeholder_consensus_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the negotiated rules for shared digital infrastructure, sets licensing boundaries, allocates infrastructure resources, and enforces compliance against both proprietary enclosure and unrestricted software freedom. Its staff and leadership are drawn from stakeholder communities and bound by the governance charter.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commons_governance_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Participate in governance fora, contribute to shared infrastructure pools, and benefit from collective resource management. They gain voice and legitimacy through the commons framework and rely on its coordination mechanisms for project sustainability.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, pragmatic_stakeholder_communities, beneficiary,
    organized, biographical, constrained, global).

% Provides legal, financial, and administrative infrastructure for collective software management. They hold assets, employ staff, and shape governance norms under the commons framework, receiving tax and reputational benefits.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, software_foundations, beneficiary,
    institutional, generational, constrained, global).

% Advocate that all software must respect absolute user freedom and reject any mechanism that restricts use, study, modification, or distribution. They are denied seats in commons governance because they reject the legitimacy of negotiated constraints on software.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, freedom_absolutists, payer,
    moderate, biographical, identity_locked, global).

% Assert that software creators have absolute authority to restrict use and distribution through property rights. They are denied commons governance participation because they reject collective management obligations and shared infrastructure commitments.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, property_absolutists, payer,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates shared use, maintenance, and evolution of digital infrastructure by negotiating rules that prevent both proprietary enclosure and ungoverned fragmentation, enabling sustained collective production.
% TRANSFER_FUNCTION: Transfers governance legitimacy and resource access to pragmatic stakeholder communities and institutions, while transferring marginalization and exclusion to proprietary and freedom absolutists who reject the negotiated framework.
% ABSENT_VOICES: Individual users who lack collective representation; absolutist camps who are present in broader discourse but excluded from governance fora; commercial actors who would prefer pure proprietary models but are not party to commons negotiation.
% DISAPPEARANCE_RATIONALE: Without the commons governance framework, shared digital infrastructure would lose coordination mechanisms, pragmatic communities would fragment or enclose, and software control would revert to polarized conflict between proprietary and freedom camps.
% FOUNDING_PROBLEM: Software control was dominated by either proprietary enclosure restricting access and reuse, or uncoordinated open-source fragmentation producing sustainability crises, license incompatibilities, and tragedies of the anti-commons.
% FOUNDING_PROBLEM_CORROBORATION: Software foundation historians and commons governance scholars attest to the problem from outside the absolutist camps; proprietary vendors and free software advocates dispute that the commons framework was the correct or necessary response.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62) because the commons framework concentrates governance voice among incumbent pragmatic stakeholders and systematically denies participation to absolutist camps. Suppression is moderate (0.58) because the framework must actively marginalize absolutist alternatives to maintain the negotiated center. Theater is moderate-low (0.25): commons governance performs real coordination, but some governance rituals serve to legitimize incumbent control rather than resolve stakeholder conflict. Accessibility collapse (0.45) is moderate because absolutist alternatives persist but are delegitimized; resistance (0.55) is moderate because absolutist camps actively contest the framework. The measurement series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   Pragmatic stakeholders and institutions experience the constraint as genuine coordination that sustains shared infrastructure. Absolutist camps experience the same structure as illegitimate exclusion that denies their core normative commitments. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Commons governance institutions, pragmatic stakeholder communities, and software foundations are structural beneficiaries (low d) because they receive governance voice, resources, and legitimacy from the framework. Freedom absolutists and property absolutists are structural targets (high d) because they bear the cost of exclusion from governance and legitimacy. The derivation reflects the declared beneficiary/victim roles and the constrained or identity-locked exit options of the respective camps.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve enclosure and fragmentation in software control. The founding problem remains partially live, so the constraint has not fully atrophied. However, the rising theater_ratio and extractiveness over the interval suggest that coordination benefits may be increasingly captured by incumbent institutions, creating mandatrophy risk if the commons framework persists primarily to protect stakeholder communities from absolutist challenge rather than to solve the original infrastructure problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_capture_risk,
    'Is the commons governance genuinely participatory, or captured by incumbent stakeholders (well-resourced foundations, corporate sponsors) who set rules to entrench their interests?',
    'Empirical analysis of governance board composition, funding sources, and decision outcomes against formal participation rules; comparison with declared stakeholder representation.',
    'If captured, effective extractiveness is higher than authored because governance voice is monopolized; the constraint shifts toward snare. If genuinely participatory, the authored coordination function holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_capture_risk, empirical, 'Whether commons governance is participatory or captured').

omega_variable(
    absolutist_exclusion_necessity,
    'Is excluding absolutist positions from governance necessary for commons functionality, or does it represent illegitimate suppression of valid alternatives?',
    'Comparative case analysis of commons governance bodies that include dissenting absolutists versus those that exclude them, measuring decision paralysis versus enriched deliberation.',
    'If exclusion is necessary, the victim status of absolutists is the price of coordination; if illegitimate, the constraint''s suppression metric under-states the true coercive burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_exclusion_necessity, conceptual, 'Whether absolutist exclusion is functional or suppressive').

omega_variable(
    kernel_reading_sibling_relation,
    'This constraint is one reading of the software_control_legitimacy kernel; how would sibling readings restructure the beneficiary/victim sets?',
    'Cross-reading comparison: the freedom_imperative_reading would make proprietary_absolutists the primary victims; the property_rights_reading would make freedom_absolutists the primary victims; the pragmatic_openness_reading would minimize victim sets entirely.',
    'The kernel is structurally under-determined; the chosen reading fixes the directionality map. Shifting to a sibling reading would invert or dissolve the victim declarations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_relation, conceptual, 'Sibling reading structural deltas for this kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sw_ctrl_cmns_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sw_ctrl_cmns_tr_t6, software_control_legitimacy__commons_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(sw_ctrl_cmns_tr_t12, software_control_legitimacy__commons_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(sw_ctrl_cmns_tr_t18, software_control_legitimacy__commons_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(sw_ctrl_cmns_tr_t24, software_control_legitimacy__commons_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(sw_ctrl_cmns_tr_t30, software_control_legitimacy__commons_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(sw_ctrl_cmns_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sw_ctrl_cmns_be_t6, software_control_legitimacy__commons_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(sw_ctrl_cmns_be_t12, software_control_legitimacy__commons_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(sw_ctrl_cmns_be_t18, software_control_legitimacy__commons_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(sw_ctrl_cmns_be_t24, software_control_legitimacy__commons_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(sw_ctrl_cmns_be_t30, software_control_legitimacy__commons_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sw_ctrl_cmns_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sw_ctrl_cmns_su_t6, software_control_legitimacy__commons_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(sw_ctrl_cmns_su_t12, software_control_legitimacy__commons_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(sw_ctrl_cmns_su_t18, software_control_legitimacy__commons_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(sw_ctrl_cmns_su_t24, software_control_legitimacy__commons_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(sw_ctrl_cmns_su_t30, software_control_legitimacy__commons_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, property_rights_reading).

% DUAL FORMULATION NOTE:
% The software_control_legitimacy kernel decomposes into four structurally distinct readings: commons_reading (this file), freedom_imperative_reading, pragmatic_openness_reading, and property_rights_reading. Each reading assigns different beneficiary/victim structures and epsilon values to the same natural-language domain. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
