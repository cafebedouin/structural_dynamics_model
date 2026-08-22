% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Selective Solidarity: Tiered Free Movement and Contributory Welfare Access
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story captures the selective_solidarity reading of the
 *   federation_membership_obligations kernel in EU political economy. The
 *   standing arrangement under contest is the legal and administrative
 *   framework that tiers free movement rights and welfare access according to
 *   economic contribution history, codified in Directive 2004/38 and
 *   interpreted through CJEU case law such as Dano and Alimanovic. Mobile EU
 *   citizens are bifurcated into economically active persons enjoying full
 *   rights and economically inactive persons facing residence restrictions,
 *   exclusion from non-contributory benefits, and deportation. The reading
 *   treats this not as pure exclusion (member_sovereignty_primary) nor as
 *   unconditional integration (integration_primary), but as a conditional
 *   compromise that makes multi-level solidarity sustainable. The authored
 *   metrics describe a substantially extractive, actively enforced structure
 *   whose coordination function is real but asymmetrically distributed.
 *
 * KEY AGENTS:
 *   - Host member states (agenda_setter/institutional/constrained): administer residence tests, enforce deportation, defend fiscal sustainability
 *   - Economically active mobile citizens (beneficiary/moderate/mobile): enjoy full free movement and equal treatment
 *   - Economically inactive mobile citizens (payer/powerless/trapped): bear exclusion from welfare and deportation risk
 *   - EU judicial institutions (observer/institutional/analytical): adjudicate the boundary between mobility rights and welfare closure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.68).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.7).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Selective Solidarity: Tiered Free Movement and Contributory Welfare Access").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '08206366-f51d-4d7a-b7ce-e5d17ffd5ac6').
narrative_ontology:cs_kernel_codification('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', formalized).
narrative_ontology:cs_authority_grounding('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', lineage).
narrative_ontology:cs_interpretation_layer_present('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6').
narrative_ontology:cs_reading_relation('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', foundational, solidarity_conditional_on_contribution).
narrative_ontology:cs_axiom_status(solidarity_conditional_on_contribution, holdable).
narrative_ontology:cs_axiom_grounding('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', solidarity_conditional_on_contribution, conventional).
narrative_ontology:cs_axiom('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', foundational, economic_activity_as_membership_proxy).
narrative_ontology:cs_axiom_status(economic_activity_as_membership_proxy, holdable).
narrative_ontology:cs_axiom_grounding('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', economic_activity_as_membership_proxy, instrumental).
narrative_ontology:cs_reference_frame('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', contributory_solidarity_equilibrium).
narrative_ontology:cs_drift_state('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08206366-f51d-4d7a-b7ce-e5d17ffd5ac6', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, economically_active_mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_member_states).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the tiered free movement framework through residence and welfare eligibility tests. They enforce contribution requirements, process deportations of inactive movers, and defend fiscal sustainability of national welfare systems before EU institutions. They derive political legitimacy from appearing to protect welfare boundaries while retaining access to mobile labor.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_member_states, agenda_setter,
    institutional, generational, constrained, national).

% Move between member states for employment and receive full free movement rights, equal treatment in work-related benefits, and social advantages linked to employment status. Their economic activity satisfies the contribution threshold, positioning them as the deserving mobile citizens who gain from cross-border mobility.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_active_mobile_citizens, beneficiary,
    moderate, biographical, mobile, regional).

% Move to or remain in host states without current employment or sufficient resources. Face residence restrictions, exclusion from non-contributory social assistance, and deportation under Directive 2004/38 if they become an unreasonable burden. Bear the cost of the solidarity boundary that protects host welfare systems.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens, payer,
    powerless, immediate, trapped, regional).

% Interpret the Treaty free movement provisions and the scope of lawful restrictions. Their case law oscillates between integrationist expansion and deference to member state welfare closures, operating as the authoritative interpreter of where the contribution threshold falls.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, eu_judicial_institutions, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, host_member_states).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cross-border labor mobility across heterogeneous welfare regimes by matching benefit entitlement to economic contribution, preventing fiscal free-riding while preserving a single labor market.
% TRANSFER_FUNCTION: Transfers the risk of destitution and administrative burden of proof onto economically inactive mobile citizens; transfers fiscal savings and labor-market flexibility to host member states and employed mobile citizens.
% ABSENT_VOICES: Economically inactive mobile citizens are politically marginal and often deportable; their voice is mediated by underfunded legal-aid and migrant-rights NGOs rather than direct representation in Council or Parliament. Sending states with high emigration have limited leverage to protect their inactive nationals in host welfare forums.
% DISAPPEARANCE_RATIONALE: If the tiered contribution framework vanished, host states would face binary pressure: either equalize welfare access for all movers (integration primary) or reassert full national closure (member sovereignty). The current mobility compromise depends on this specific sorting mechanism; without it, the political equilibrium of free movement collapses.
% FOUNDING_PROBLEM: How to sustain Treaty-based free movement of workers and citizens across member states with divergent economic development and generous non-contributory welfare regimes without triggering fiscal migration or undermining public support for welfare states.
% FOUNDING_PROBLEM_CORROBORATION: EU institutional scholarship and CJEU case law acknowledge the tension between mobility rights and welfare closure. Member state governments corroborate the problem by citing fiscal sustainability. Independent migration economists and migrant-rights organizations dispute the magnitude of the fiscal threat, suggesting the founding problem is overstated and the arrangement persists for political symbolism rather than structural necessity.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the arrangement systematically denies social assistance and threatens deportation for a defined class of mobile citizens based on economic status, transferring fiscal and political gains to host states and employed movers. Suppression (0.70) reflects active administrative enforcement: residence tests, benefit denials, and removal proceedings. Theater_ratio (0.50) is moderate-high because the welfare tourism narrative that justifies the tiering is substantially disconnected from the actual fiscal scale of inactive migration, making much enforcement performative. Accessibility_collapse (0.60) captures how the selective-solidarity framing has crowded out both pure integration and pure sovereignty as live policy options within the EU discourse. Resistance (0.55) reflects sustained legal challenge and migrant-rights advocacy against deportation and exclusion practices.
 *
 * PERSPECTIVAL GAP:
 *   The host member state seat experiences the constraint as necessary coordination to protect welfare-state legitimacy and fiscal boundaries; the economically inactive mobile citizen seat experiences the same legal framework as coercive extraction that conditions belonging on market participation. The employed mobile citizen seat experiences it as enabling beneficial mobility with manageable conditions. The engine computes these divergent seat classifications from the same structural data: the directionality derivation assigns low d to beneficiaries (active movers, host states) and high d to the trapped inactive payer.
 *
 * DIRECTIONALITY LOGIC:
 *   Host member states are declared beneficiaries because they capture fiscal savings and labor-market flexibility; their institutional power and constrained exit (bound by EU law) place their derived directionality near the beneficiary end. Economically active mobile citizens are beneficiaries because their rights are secured by the same framework that excludes the inactive; their mobility gives them low directionality. Economically inactive mobile citizens are the declared victims: they are powerless, often trapped by lack of resources and deportation risk, giving them directionality near the full-target end. The EU judicial institutions sit at the analytical pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The selective-solidarity framework risks mandatrophy if the fiscal threat it was built to contain proves illusory or minimal, turning a transitional scaffold into permanent extraction. However, the framework has no sunset clause and is not framed as temporary. The current reading treats it as a steady-state compromise, not a scaffold. The high theater_ratio suggests some performative maintenance of a function whose original justification may be overstated, but the coordination function (enabling labor mobility across diverse welfare regimes) remains structurally real. This prevents classification as pure snare: there are genuine beneficiaries among the coordinated, not only concentrated extraction for an agenda-setter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_contributory_principle,
    'Is the contributory principle a natural feature of social insurance or a constructed boundary that redefines citizenship rights as market participation rights?',
    'Comparative legal-historical analysis of welfare state origins versus EU citizenship jurisprudence; examination of whether contribution requirements emerged from insurance logic or from member state bargaining to limit mobility.',
    'If the principle is constructed rather than natural, the constraint''s legitimacy shifts from technical coordination to political extraction, supporting reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_contributory_principle, conceptual, 'Whether the contributory principle is intrinsic to welfare or a mobility-control device.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For economically inactive mobile citizens, is their exclusion enforced through structural administrative barriers or internalized self-exclusion (not moving because they know they will be unsupported)?',
    'Post-exit trajectory studies and administrative data on voluntary return versus deportation; survey evidence on mobility decisions among potentially inactive populations.',
    'If suppression is internalized, effective extraction exceeds the structural measure because the target carries the constraint after exit; this would raise suppression and extractiveness estimates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for inactive movers.').

omega_variable(
    welfare_tourism_fiscal_scale,
    'Does the actual fiscal cost of economically inactive mobility justify the enforcement intensity, or is the threat disproportionate to the measured flow?',
    'Independent fiscal metering of non-contributory benefit claims by inactive mobile EU citizens relative to total welfare spending; comparison with native citizen claim rates.',
    'A negligible fiscal cost would indicate that the extraction is primarily symbolic and theatrical, raising theater_ratio and supporting a stronger piton or snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_tourism_fiscal_scale, empirical, 'Whether fiscal threat scale matches enforcement intensity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_selective_solidarity_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.2).
narrative_ontology:measurement(federation_selective_solidarity_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.3).
narrative_ontology:measurement(federation_selective_solidarity_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.4).
narrative_ontology:measurement(federation_selective_solidarity_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.55).
narrative_ontology:measurement(federation_selective_solidarity_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.6).
narrative_ontology:measurement(federation_selective_solidarity_tr_t25, federation_membership_obligations__selective_solidarity, theater_ratio, 25, 0.55).
narrative_ontology:measurement(federation_selective_solidarity_tr_t30, federation_membership_obligations__selective_solidarity, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(federation_selective_solidarity_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(federation_selective_solidarity_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(federation_selective_solidarity_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(federation_selective_solidarity_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(federation_selective_solidarity_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(federation_selective_solidarity_be_t25, federation_membership_obligations__selective_solidarity, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(federation_selective_solidarity_be_t30, federation_membership_obligations__selective_solidarity, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(federation_selective_solidarity_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(federation_selective_solidarity_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(federation_selective_solidarity_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(federation_selective_solidarity_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(federation_selective_solidarity_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(federation_selective_solidarity_su_t25, federation_membership_obligations__selective_solidarity, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(federation_selective_solidarity_su_t30, federation_membership_obligations__selective_solidarity, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is the selective_solidarity reading of the federation_membership_obligations kernel, sitting between integration_primary (full welfare access) and member_sovereignty_primary (full closure authority). Its epsilon reflects the standing tiered-arrangement under this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
