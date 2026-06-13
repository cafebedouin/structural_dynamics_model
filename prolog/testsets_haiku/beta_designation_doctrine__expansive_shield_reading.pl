% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation as Comprehensive Liability Waiver (Expansive Reading)
 *   domain: technology/law/consumer_protection
 *
 * SUMMARY:
 *   Under the expansive reading, the beta designation is interpreted as a
 *   blanket, indefinite, universal liability waiver: any software labeled
 *   beta is presumed to exempt developers from all product-liability,
 *   warranty, and negligence claims, regardless of how long the software
 *   remains beta, how critical its function, or how severe the defects. This
 *   reading treats beta status as a unilateral contractual assertion by
 *   developers, binding on users at download. The reading is one
 *   instantiation of a contested kernel (beta_designation_doctrine) with two
 *   sibling readings that interpret the same legal concept very differently:
 *   the narrow_warning_reading treats beta as time-bounded testing disclosure
 *   preserving base product liability; the severity_carve_out_reading carves
 *   out life-safety and critical systems from beta immunity. This constraint
 *   instantiates the expansive reading only—it is a clean, ε-invariant story
 *   of high extraction where developers externalize all defect costs and
 *   users (especially dependent systems) enter the victim set with no
 *   temporal or severity boundaries.
 *
 * KEY AGENTS:
 *   - software_developers: developers of beta-labeled software set the terms unilaterally; they benefit from indefinite liability exemption.
 *   - platform_operators: institutional beneficiaries indirectly; they host the software and enforce the waiver by accepting TOS assertions.
 *   - beta_software_users: powerless victims bearing all defect costs; identity_locked (essential software fused with professional/social identity) makes exit practically impossible.
 *   - dependent_systems: systems relying on beta software as a component; they absorb cascading defects with no recourse.
 *   - competing_legal_regimes: excluded from the negotiation; their mandatory consumer protections are suppressed by global platform TOS.
 *   - consumer_protection_authorities: analytical observers investigating whether the waiver overrides mandatory law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.87).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.78).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Waiver (Expansive Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology/law/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '94aab5f9-2307-4a25-b610-7f9cf8ccbfe8').
narrative_ontology:cs_kernel_codification('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', fixed_text).
narrative_ontology:cs_authority_grounding('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', extraction).
narrative_ontology:cs_interpretation_layer_present('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8').
narrative_ontology:cs_reading_relation('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', foundational, beta_label_authorizes_comprehensive_waiver).
narrative_ontology:cs_axiom_status(beta_label_authorizes_comprehensive_waiver, holdable).
narrative_ontology:cs_axiom_grounding('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', beta_label_authorizes_comprehensive_waiver, conventional).
narrative_ontology:cs_axiom('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', foundational, indefinite_beta_duration_permissible).
narrative_ontology:cs_axiom_status(indefinite_beta_duration_permissible, holdable).
narrative_ontology:cs_axiom_grounding('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', indefinite_beta_duration_permissible, instrumental).
narrative_ontology:cs_reference_frame('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', developer_risk_externalization_framework).
narrative_ontology:cs_drift_state('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', contemporary_indefinite_beta_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94aab5f9-2307-4a25-b610-7f9cf8ccbfe8', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, beta_software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, dependent_systems).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.87 at interval end) because the constraint systematically moves all product-liability risk from developers (who ship the code) to users (who absorb the defects), with no consideration for defect severity, duration of beta status, or essentiality of the software. The extraction accumulates over the interval (from 0.71 to 0.87) as developers increasingly treat beta as a permanent liability shield rather than a temporary developmental label—the founding problem (lack of testing infrastructure) is solved, but the doctrine persists and expands. Suppression is high (0.78) because the constraint's persistence depends on preventing users from organizing claims, enforcing the waiver adhesively through TOS, and suppressing competing legal regimes' mandatory protections. Theater is moderate-to-high (0.62): significant performance activity occurs around the 'beta testing' framing, but the actual function (genuine developmental testing) has atrophied—what remains is mostly the liability-shield story. Accessibility collapse is moderate (0.71): alternatives to beta software technically exist but are often unavailable for essential tools (communication, productivity, system utilities), making the collapse practical if not absolute. Resistance is moderate (0.58): users and consumer protection authorities mount real resistance through litigation, legislative pressure, and public criticism, but the constraint persists through institutional inertia and developer/platform power asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   Developers and platform operators perceive this as legitimate risk allocation: beta labels are transparent; users can choose not to download; developers need shield from frivolous claims to innovate. Users and downstream dependents perceive it as coercive extraction: beta labels are non-negotiable contract terms, essential software is indefinitely beta, defect costs are real and irreversible (data loss, security breach, operational failure), and they have no recourse. Consumer protection authorities perceive it as an override of mandatory law: in their jurisdictions, product liability cannot be waived; the constraint conflicts with local statute. The engine computes each seat's classification from the structural data (power, exit_options, beneficiary/victim status, directionality); the perspectival gap is the observable difference when seats with different structural relationships compute different types.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers are beneficiaries (d near 0.0): they set the constraint, collect the liability exemption, have high power and arbitrage exit options (can choose to label software beta or not; can switch jurisdictions). Users are targets (d near 1.0): they bear the defect costs, have powerless/organized power (individually powerless, collectively organized only through litigation), and identity_locked exit (essential software fused with identity). Platform operators are secondary beneficiaries (d near 0.1): they host developers (whom they attract through permissive terms) and indirectly share the benefit by disclaiming responsibility. Dependent systems are forced targets (d = 1.0): they have no negotiating power, no choice about what software they depend on, and experience cascading defects as pure cost. Competing legal regimes have no d value within this constraint—they are excluded by the conflict mechanism itself (global TOS override local law).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows classic mandatrophy: the founding problem (lack of private beta-testing infrastructure) is solved—modern software development has extensive internal QA, staged rollout, and user-research capabilities. Yet the beta-waiver doctrine persists and has expanded indefinitely in scope and duration. Developers continue to label mature software as beta specifically to retain the liability shield, long after the developmental justification has vanished. The constraint's manifest function (temporary testing disclosure) has atrophied; what remains is mostly performative maintenance of the liability shield. This is characteristic piton-adjacent behavior, but the high extractiveness (0.87) and high suppression (0.78) push it into snare territory: it persists not because everyone benefits moderately (piton signature) but because it extracts substantially from a large victim set and is actively defended by a powerful beneficiary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_omega,
    'Which reading of the beta_designation_doctrine kernel is structurally correct: the expansive_shield_reading (this constraint), the narrow_warning_reading, or the severity_carve_out_reading?',
    'Legislative/regulatory intent from major jurisdictions; case law interpreting beta disclaimers and their interaction with product-liability law; practitioner testimony and software-release practice analysis; consumer-protection authority guidance and enforcement patterns.',
    'If the narrow or carve_out readings are correct, this constraint (expansive) mislabels the doctrine as comprehensive when it is actually bounded. If the expansive reading is correct, the sibling readings misunderstand the scope of the doctrine. The readings produce mutually exclusive classifications: narrow → rope/scaffold, carve_out → tangled_rope, expansive → snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_omega, conceptual, 'The kernel contest: which interpretation of beta designation is structurally correct?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(beta_tr_t3, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(beta_tr_t6, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 12, 0.6).
narrative_ontology:measurement(beta_tr_t18, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 18, 0.61).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(beta_be_t3, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 3, 0.76).
narrative_ontology:measurement(beta_be_t6, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 6, 0.8).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 12, 0.84).
narrative_ontology:measurement(beta_be_t18, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 18, 0.86).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 25, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(beta_su_t3, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(beta_su_t6, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 12, 0.77).
narrative_ontology:measurement(beta_su_t18, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, software_release_maturity_signaling).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, adhesive_contract_liability_waiver).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the beta_designation_doctrine kernel. The three stories (expansive_shield_reading, narrow_warning_reading, severity_carve_out_reading) represent a kernel contest where a single legal concept (what beta designation legitimately accomplishes) instantiates structurally distinct constraints depending on which interpretation is adopted. The expansive reading asserts comprehensive, indefinite immunity; the narrow reading asserts time-bounded disclosure; the severity reading asserts categorical carve-outs for critical systems. Each story has its own ε value, beneficiary/victim structure, and constraint type. They coexist as live positions in law and practice—all three readings are claimed by some practitioners/courts, and none has universal acceptance. This is a constraint family under shared kernel identity, not a single constraint viewed from multiple angles (ε-invariance principle: if ε changes when interpretation changes, they are different constraints).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beta_designation_doctrine__expansive_shield_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
