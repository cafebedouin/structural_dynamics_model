% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Hybrid Complementarity Jurisdiction
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute establishes the International Criminal Court with
 *   claimed universal jurisdiction over genocide, crimes against humanity,
 *   war crimes, and aggression. However, the 'complementarity' doctrine
 *   subordinates this claim: the ICC is a court of last resort, deferring to
 *   national courts as the primary enforcers. This reading instantiates the
 *   constraint as a hybrid: the Statute aspires to universal accountability
 *   but operationally depends on state cooperation and complementarity
 *   deference. The ICC asserts residual universal authority while constrained
 *   by state sovereignty, creating a tangled rope structure—genuine
 *   coordination (fallback for failed states' justice systems) layered with
 *   extraction (states and powerful actors navigate around the mechanism,
 *   ICC's docket reflects political pressure rather than severity,
 *   non-signatory nationals experience unilateral exposure). This reading
 *   sits between the sovereigntist reading (Rome Statute is pure state
 *   consent with no residual universal authority) and the universalist
 *   reading (Rome Statute establishes transcendent accountability regardless
 *   of consent).
 *
 * KEY AGENTS:
 *   - ICC institutional apparatus: administers complementarity doctrine, holds nominal universal jurisdiction but operationally constrained by state cooperation
 *   - Signatory states (compliant): retain primary prosecutorial responsibility, defer to ICC as fallback, benefit from reputation without enforcing costs
 *   - Non-signatory states and their nationals: subject to potential Security Council referral, experience exposure without voice
 *   - States resistant to ICC authority: trapped between withdrawal (reputational cost) and compliance (operational cost)
 *   - International justice advocates: benefit from existence of ICC, maintain pressure for enforcement, have mobility
 *   - Affected populations (victims): identity-locked to the constraint, depend on ICC access for remedy in failed states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.41).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.29).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Hybrid Complementarity Jurisdiction").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'b39973db-7632-427d-8ebb-41e8a8882205').
narrative_ontology:cs_kernel_codification('b39973db-7632-427d-8ebb-41e8a8882205', fixed_text).
narrative_ontology:cs_authority_grounding('b39973db-7632-427d-8ebb-41e8a8882205', extraction).
narrative_ontology:cs_interpretation_layer_present('b39973db-7632-427d-8ebb-41e8a8882205').
narrative_ontology:cs_reading_relation('b39973db-7632-427d-8ebb-41e8a8882205', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b39973db-7632-427d-8ebb-41e8a8882205', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('b39973db-7632-427d-8ebb-41e8a8882205', foundational, complementarity_substantive_tension).
narrative_ontology:cs_axiom_status(complementarity_substantive_tension, holdable).
narrative_ontology:cs_axiom_grounding('b39973db-7632-427d-8ebb-41e8a8882205', complementarity_substantive_tension, deontological).
narrative_ontology:cs_axiom('b39973db-7632-427d-8ebb-41e8a8882205', foundational, hybrid_authority_legitimacy).
narrative_ontology:cs_axiom_status(hybrid_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b39973db-7632-427d-8ebb-41e8a8882205', hybrid_authority_legitimacy, conventional).
narrative_ontology:cs_reference_frame('b39973db-7632-427d-8ebb-41e8a8882205', international_accountability_with_state_primacy).
narrative_ontology:cs_drift_state('b39973db-7632-427d-8ebb-41e8a8882205', contemporary_selective_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b39973db-7632-427d-8ebb-41e8a8882205', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice_advocates).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, affected_populations_accessing_justice).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, states_resistant_to_icc_authority).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, nationals_of_non_signatory_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.41) because the constraint coordinates accountability but unevenly: powerful states manage exposure through non-signatory status or Security Council vetoes, while weaker states and non-signatories bear asymmetric risk. Suppression is relatively low (0.29) because the constraint operates through legal authority and institutional design rather than coercion, and resistance to it is vocal and legitimate (state sovereignty arguments). Theater ratio rises over time (0.22→0.38) because complementarity deference becomes increasingly performative: the ICC claims universal reach while operationally accepting state obstruction, creating a gap between the law-on-the-books (universal jurisdiction) and law-in-action (political deferral). The constraint is tangled rope because it achieves genuine coordination (unified fallback for accountability when states fail) AND extracts asymmetrically (non-signatories and vulnerable states bear the cost of exposure while powerful states evade via Security Council veto or non-membership). The metrics share one time grid (2002-2026 by 4-6 year intervals) capturing the ICC's institutional evolution from idealistic founding through pragmatic constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the vantage of signatory states compliant with complementarity, the constraint is a genuine coordination framework: they retain primary authority and defer only as a true fallback. From the vantage of non-signatory states and resistance movements, the constraint operates as imposed jurisdiction: the ICC claims universal authority while denying voice and offering no escape route except state membership (which itself comes with operational costs). From the ICC's institutional perspective, the constraint is a legitimacy bargain: universal aspiration provides normative authority, state cooperation provides enforcement capacity, complementarity doctrine provides diplomatic cover for operational constraints. The engine computes these seats differently because their structural relationships to the constraint (power atom, exit options, beneficiary/victim roles) differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Signatory compliant states sit near symmetric (d~0.45): they coordinate genuine accountability and retain primary authority, offsetting the ICC's residual claim. Non-signatory nationals sit near full target (d~0.85): exposed to international prosecution without state consent or voice. ICC institutional apparatus sits near moderate target (d~0.55): it holds nominal authority but operationally constrained by state cooperation. Affected populations sit at high target on the identity-locked axis (d~0.80): their victim status is the claim on the system, but they cannot exit or negotiate. International justice advocates sit near beneficiary (d~0.25): they mobilize the constraint and benefit from its existence. Major non-signatory states sit at moderate target (d~0.55) because they are exposed to Security Council referral yet have veto power over referrals.
 *
 * MANDATROPHY ANALYSIS:
 *   The complementarity doctrine prevents false classification of extraction as pure coordination. If the Rome Statute claimed only universal jurisdiction (universalist reading), it would appear as pure aspiration—a mountain of international law. If it claimed only state consent (sovereigntist reading), it would appear as institutional coordination. The hybrid reading captures that it is both: genuine coordination in principle (accountability for genocide when states fail) AND extraction in operation (non-signatories exposed without consent, signatory states navigate around enforcement, powerful actors manage outcomes through Security Council politics). Complementarity is the mechanism that sustains this hybrid: it legitimizes the universal claim while enabling state obstruction. Mandatrophy is resolved because the founding problem (post-WWII accountability gap) is contested—advocates say it persists, states say capacity has improved—and the constraint's operation has drifted: theater ratio rises because enforcement has become increasingly selective (Africa-concentrated docket, withdrawal threats from targeted states), suggesting the coordination function has atrophied while the extraction function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_as_substantive_limit_vs_procedural_courtesy,
    'Does complementarity represent a constitutional substantive limit on the ICC''s jurisdiction (sovereignty preserves a reserved domain) or a procedural courtesy (the ICC defers tactically but retains inherent universal jurisdiction)?',
    'Observation of the ICC''s conduct if a state explicitly refuses to prosecute and the ICC proceeds without state cooperation; legal opinions from the ICC Prosecutor''s office and independent courts interpreting the Rome Statute''s Article 17 language.',
    'Substantive limit → sovereigntist reading gains ground, extraction lowers because ICC authority is genuinely bounded; procedural courtesy → universalist reading gains ground, extraction rises because ICC operates under principle of universal authority despite practical state obstruction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complementarity_as_substantive_limit_vs_procedural_courtesy, conceptual, 'The ontological status of complementarity—constraint or tactic.').

omega_variable(
    universal_jurisdiction_over_non_signatories_legitimacy,
    'Does the Rome Statute legitimately claim universal jurisdiction over nationals of non-signatory states via Security Council referral, or does jurisdiction over non-consenting states constitute unilateral imposition?',
    'Formal legal opinions from major non-signatories (US, Russia, China, India) on the legitimacy of Article 13b referrals; comparison with alternative international law doctrines (jurisdiction based on territoriality, nationality, protective principle); empirical evidence of whether states regard referrals as legitimate authority or coercion.',
    'Legitimacy upheld → the constraint is genuine hybrid coordination (universal aspiration grounded in international law); legitimacy contested → the constraint slides toward snare (powerful states use veto to exempt themselves while imposing on others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_jurisdiction_over_non_signatories_legitimacy, empirical, 'Whether non-consent can be overridden by international law or only by state choice.').

omega_variable(
    theater_ratio_rise_driver,
    'Does the rising theater ratio (2002→2026) reflect genuine increasing performativity (complementarity as cover for political selectivity) or increasing transparency about necessary political constraints (candid recognition that accountability is politically limited)?',
    'Analysis of ICC case selection: compare severity of alleged crimes with case-opening decisions, track changes in disclosure of prosecutorial rationale, interview Prosecutor''s office on case management logic, audit docket composition (geographic, power-level, political alignment bias).',
    'Performativity interpretation → the constraint becomes increasingly piton-like; transparency interpretation → the theater rise is acknowledgment of reality-based constraints, not degradation of function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_rise_driver, empirical, 'Whether rising theater ratio indicates atrophy of coordination or adaptation to sustainable accountability.').

omega_variable(
    state_withdrawal_as_exit_option_viability,
    'Is withdrawal from the Rome Statute a real exit option for signatory states (effectively constrained only by reputational cost) or a constrained exit (legal possibility but practically prohibitive)?',
    'Observation of withdrawal attempts and their costs; analysis of whether withdrawal resolves the actor''s problem (e.g., does South Africa exit stop ICC exposure) or merely shifts the mechanism (Security Council referral becomes available).',
    'Real exit → resistance states can eventually escape; extraction is constrained by exit option mobility, d-values lower. Constrained exit → states remain trapped; extraction remains high despite nominal exit possibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_withdrawal_as_exit_option_viability, empirical, 'Whether withdrawal is a live exit or a nominal option.').

omega_variable(
    security_council_veto_as_consent_proxy,
    'Does permanent member veto power over Article 13b referrals restore meaningful consent for non-signatory states (through proxy protection) or create a different form of exposure (non-member states lack standing to block referrals while major powers protect themselves through veto)?',
    'Historical analysis of which situations were referred and which were blocked; comparison of veto patterns with national interest; test whether non-member states sought permanent member protection and whether it was granted.',
    'Veto as consent proxy → non-member exposure is less than measured (d-values lower because major-power non-members have de facto voice); veto as structural privilege → exposure is higher because only permanent members benefit from blocking capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_council_veto_as_consent_proxy, empirical, 'Whether Security Council power distribution restores sovereignty or creates asymmetric exposure.').

omega_variable(
    hybrid_reading_vs_sovereigntist_foreclosure,
    'Does the hybrid complementarity reading logically foreclose the sovereigntist reading (Rome Statute has some residual universal authority that sovereign states cannot unilaterally eliminate) or coexist with it (complementarity could be reinterpreted as pure state consent)?',
    'Logical analysis of the Rome Statute''s Article 5-8 language and Article 17 together; test whether a pure-consent interpretation is consistent with the text.',
    'Foreclosure → the two readings cannot coexist in a single legal framework; coexistence → both readings remain live depending on which interpretation court adopts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_vs_sovereigntist_foreclosure, conceptual, 'Whether hybrid and sovereigntist readings are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.22).
narrative_ontology:measurement(rome_tr_t2006, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2006, 0.25).
narrative_ontology:measurement(rome_tr_t2012, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2012, 0.31).
narrative_ontology:measurement(rome_tr_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(rome_tr_t2022, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2022, 0.39).
narrative_ontology:measurement(rome_tr_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(rome_be_t2006, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2006, 0.33).
narrative_ontology:measurement(rome_be_t2012, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(rome_be_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(rome_be_t2022, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2022, 0.41).
narrative_ontology:measurement(rome_be_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2026, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.18).
narrative_ontology:measurement(rome_su_t2006, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2006, 0.21).
narrative_ontology:measurement(rome_su_t2012, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2012, 0.25).
narrative_ontology:measurement(rome_su_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2018, 0.28).
narrative_ontology:measurement(rome_su_t2022, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2022, 0.29).
narrative_ontology:measurement(rome_su_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2026, 0.29).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).

% DUAL FORMULATION NOTE:
% The rome_statute_jurisdiction kernel decomposes into three constraint stories distinguished by their reading of complementarity's ontological status (substantive constitutional limit vs. procedural courtesy). The hybrid_complementarity_reading treats complementarity as authentic tension between universal aspiration and sovereign primacy, not as rhetoric or mere procedure. Each sibling reading produces a different ε: sovereigntist lower (authority genuinely bounded), universalist higher (authority universal despite state obstruction), hybrid intermediate (tension held open). All three stories affect each other through legal interpretation and institutional practice—a case opening in one reading triggers reinterpretation pressure in the others. The sovereigntist and universalist readings coexist across different state parties; the hybrid reading influences both by occupying the middle ground and mediating the tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, institutional, 0.55).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
