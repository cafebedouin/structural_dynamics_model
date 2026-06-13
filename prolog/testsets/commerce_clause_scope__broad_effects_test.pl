% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Broad Effects Test Interpretation
 *   domain: constitutional/federalism/economic
 *
 * SUMMARY:
 *   The broad effects test reading of the Commerce Clause asserts that
 *   federal authority extends to any intrastate economic activity that
 *   substantially affects interstate commerce in the aggregate, and that
 *   'regulate' includes prohibition and comprehensive control. This reading
 *   emerged from constitutional litigation beginning in the 1937 crisis (West
 *   Coast Hotel v. Parrish, NLRB v. Jones & Laughlin) and hardened through
 *   the Civil Rights Era (Heart of Atlanta Motel v. United States, Katzenbach
 *   v. McClung) and beyond. Under this reading, the federal government's
 *   enumerated power to regulate interstate commerce extends to virtually all
 *   economic activity once aggregation doctrine is accepted. The alternative
 *   narrow-originalist reading sees 'commerce among states' as trade crossing
 *   state lines and 'regulate' as removing barriers, not comprehensive
 *   control. An intermediate reading accepts substantial effects but imposes
 *   limiting principles (non-economic activity requires a jurisdictional
 *   element, aggregation applies only to economic activity, attenuation
 *   limits apply). This story instantiates the BROAD reading only—a single,
 *   ε-invariant constraint with high extraction from state autonomy, high
 *   federal beneficiary capture, and high suppression of federalism as a
 *   limiting principle.
 *
 * KEY AGENTS:
 *   - Federal regulators (institutional power, agenda-setter): interpret and enforce the broad effects test, claim authority over any economic activity with aggregated national effects.
 *   - State legislatures (organized power, payer): lose police-power autonomy as federal authority expands; constrained exit (cannot easily contest federal preemption without overturning Supreme Court precedent).
 *   - Civil rights coalitions (organized power, beneficiary): use the broad effects test to enforce national standards against state/local resistance (e.g., regulating discrimination in local commerce as Commerce Clause issue).
 *   - Local commerce actors (powerless, victim): subject to federal regulation of intrastate activity; identity-locked to their jurisdiction; no voice in federal rulemaking.
 *   - Narrow-originalist judges and scholars (institutional power, excluded): argue for narrower reading but are outvoted in current doctrine; dissent but do not control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.82).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.71).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.82).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test Interpretation").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional/federalism/economic").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'a47d2515-6357-4f45-b807-023471bf5f44').
narrative_ontology:cs_kernel_codification('a47d2515-6357-4f45-b807-023471bf5f44', fixed_text).
narrative_ontology:cs_authority_grounding('a47d2515-6357-4f45-b807-023471bf5f44', lineage).
narrative_ontology:cs_interpretation_layer_present('a47d2515-6357-4f45-b807-023471bf5f44').
narrative_ontology:cs_reading_relation('a47d2515-6357-4f45-b807-023471bf5f44', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('a47d2515-6357-4f45-b807-023471bf5f44', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('a47d2515-6357-4f45-b807-023471bf5f44', foundational, aggregation_doctrine_valid).
narrative_ontology:cs_axiom_status(aggregation_doctrine_valid, holdable).
narrative_ontology:cs_axiom_grounding('a47d2515-6357-4f45-b807-023471bf5f44', aggregation_doctrine_valid, deontological).
narrative_ontology:cs_axiom('a47d2515-6357-4f45-b807-023471bf5f44', foundational, enumeration_of_powers_permissive_not_limiting).
narrative_ontology:cs_axiom_status(enumeration_of_powers_permissive_not_limiting, holdable).
narrative_ontology:cs_axiom_grounding('a47d2515-6357-4f45-b807-023471bf5f44', enumeration_of_powers_permissive_not_limiting, deontological).
narrative_ontology:cs_reference_frame('a47d2515-6357-4f45-b807-023471bf5f44', market_integration_enabling_framework).
narrative_ontology:cs_drift_state('a47d2515-6357-4f45-b807-023471bf5f44', contemporary_post_foundational_problem_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a47d2515-6357-4f45-b807-023471bf5f44', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_civil_rights_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, uniform_policy_advocates).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_commerce_autonomy).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_limiting_principle).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82 at 2024) is high because the reading transfers regulatory authority from states to the federal government without compensation or negotiation—state autonomy is extracted as federal authority expands. The extraction is cumulative over the interval (1937–2024): measurement series show extraction rising from 0.35 (1937, when the reading was not yet established) to 0.82 (2024, when broad effects test is standard doctrine). Suppression (0.71) is high because the framework suppresses federalism-as-limiting-principle: the enumeration of federal powers is treated as non-binding once effects are aggregated, and state dissent is overridden by Supremacy Clause. Theater ratio (0.42) is moderate: the security/stability justifications for uniform national rules are real, but a rising share of regulatory activity defends federal authority itself rather than achieving the stated coordination goal. Accessibility collapse (0.88) is very high because states have almost nowhere to retreat from the broad effects test without overturning Supreme Court precedent—the alternative is foreclosed. Resistance (0.58) is moderate: state legislatures and federalism advocates resist, but they lack the institutional leverage to block federal expansion without a constitutional amendment or a wholesale judicial reversal.
 *
 * PERSPECTIVAL GAP:
 *   From the federal-regulator seat, the broad effects test is genuine coordination enabling national market integration and uniform civil rights enforcement. From the state seat, the same structure is extracted authority with thin justification. From the narrow-originalist seat, the broad test is a constitutional error that empties the enumeration of powers. The engine computes these per-seat divergences from the structural data—the authored claim (tangled rope) reflects the federal and civil-rights-coalition framing; the metrics reflect the high extraction and suppression observed in operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators are structural beneficiaries (d ≈ 0.1–0.2): they gain authority and prestige as the broad test expands federal scope. Civil rights coalitions are beneficiaries (d ≈ 0.2–0.3): they gain federal enforcement leverage against state/local resistance. State legislatures are targets (d ≈ 0.7–0.8): they lose autonomy and must spend resources conforming to federal rules. Local commerce actors are full targets (d ≈ 0.9): they face federal regulation of intrastate activity with no exit except abandoning livelihood. The federalism principle (non-agent, agent=false) is a victim of the reading: treated as substantively empty once effects are aggregated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was state protectionism fragmenting the national market (1787). The broad effects test was deployed to solve this, but by the 1960s the founding problem (state tariff and trade barriers) was substantially solved. The test persisted and expanded to solve a different problem (uniform enforcement of civil rights, national environmental standards, labor rules). By 2024, the founding problem (Founding Era protectionism) is dead, but the enforcement mechanism (broad effects test) persists and is used to regulate intrastate activity unrelated to market fragmentation. The measurement series shows theater ratio rising (more enforcement activity is theatrical—defending federal authority itself, not the coordination goal) and suppression rising (the mechanism actively suppresses federalism as constraint). This is a strong mandatrophy signal: the founding constraint outlived its founding problem and is now maintained mostly theatrically. The reading remains classified tangled_rope because coordination (uniform rules) is still partially real, and extraction (state autonomy) is still active; but the mandatrophy is visible in the metrics and the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernelcontest_narrow_originalist_foreclosure,
    'Does the broad effects test logically foreclose the narrow originalist reading, or do they merely coexist as competing interpretations?',
    'If a narrow originalist reading were adopted by a federal court, could it coherently apply to the Constitution''s text without denying the broad test''s core premise? The test is: does accepting ''commerce among states means trade crossing state lines'' directly contradict accepting ''commerce includes activity with substantial interstate effects''? Or are they merely different readings of ambiguous language?',
    'If the broad test FORECLOSES the narrow reading (one core premise logically rules out the other), then the constraint classification and the kernel relation (''coexists_with'' vs. ''forecloses'') must be revised. If they coexist (different parties hold different readings within their own coherent frameworks), then the current coexistence relation is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernelcontest_narrow_originalist_foreclosure, conceptual, 'Whether the broad effects test and narrow originalist reading have logically incompatible core premises or can coexist as alternative interpretations of ambiguous constitutional language.').

omega_variable(
    aggregation_doctrine_necessity,
    'Is the aggregation doctrine (the idea that effects can be aggregated across many intrastate actors to establish substantial effects on interstate commerce) a logical consequence of the Commerce Clause language, or is it a doctrinal add-on that reformulates the constitutional question?',
    'Originalist textual analysis: does ''commerce among states'' permit or require aggregation of distributed effects? Does ''substantially affects'' presume aggregation or require direct, individual causation? Compare with statutory canons in non-constitutional contexts where aggregation is explicit or implicit.',
    'If aggregation is a logical consequence, the broad effects test is the reading the Constitution compels. If aggregation is a doctrinal choice, the reading is more contingent and could be replaced by a non-aggregating substantial-effects test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_necessity, conceptual, 'Whether aggregation doctrine is logically entailed by the Commerce Clause or is an interpretive choice.').

omega_variable(
    civil_rights_enforcement_separation,
    'Can federal authority to enforce civil rights be grounded in a narrower reading of the Commerce Clause, or does civil rights enforcement specifically require the broad effects test?',
    'Empirical test: in jurisdictions or hypothetical scenarios where federal Commerce Clause authority is narrow, what alternative constitutional hooks could support federal civil rights enforcement? Equal Protection Clause Section 5 power, Direct State Action Doctrine, Fourteenth Amendment enforcement—are these sufficient, or does narrowing Commerce Clause authority necessarily narrow civil rights enforcement?',
    'If civil rights enforcement can survive independently, the broad effects test is not the only mechanism, and narrowing it would not eliminate federal civil rights authority. If civil rights enforcement is substantially dependent on the broad test, narrowing it would require finding equivalent authority elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_enforcement_separation, empirical, 'Whether civil rights enforcement is dependent on the broad effects test or can be grounded independently.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression of federalism-as-limiting-principle structural (enforceable by Supremacy Clause) or internalized (federal and state institutions have accepted the broad reading as legitimate constitutional law)?',
    'Examine post-suppression trajectory: if a state were to assert federalism-limiting-principle grounds and reject federal authority, would the suppression persist (structural) or dissolve (internalized)? How many current state officials and judges would even consider such assertion, and how deeply have they integrated the broad test into their own constitutional understanding?',
    'If structural: the suppression would be maintained by external force (federal courts striking down state assertion). If internalized: states have adopted the broad reading as their own framework; suppression persists even without external force and would be harder to remove.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether the suppression of federalism is structural (external enforcement) or internalized (institutional acceptance).').

omega_variable(
    founding_problem_resurrection,
    'Is state economic protectionism (the original Commerce Clause problem) actually dead, or has it morphed into new forms (e.g., regulatory protectionism disguised as safety/environmental rules) that require broad federal authority to address?',
    'Historical analysis: measure the frequency and magnitude of explicitly protectionist state legislation in 1937 vs. 2024. Track whether modern state regulation (environmental, safety, labor rules) functions as implicit protectionism. Survey federal agency litigation: what proportion of Commerce Clause regulatory actions are responding to genuine protectionism vs. other policy goals?',
    'If protectionism has persisted in new forms, the founding problem is not dead and the mandate expansion is justified. If protectionism is genuinely rare, the broad test persists beyond its founding problem and mandatrophy signals are strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_resurrection, empirical, 'Whether state economic protectionism persists in modern form or is genuinely solved, affecting mandatrophy assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(comm_tr_t1950, commerce_clause_scope__broad_effects_test, theater_ratio, 1950, 0.28).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_scope__broad_effects_test, theater_ratio, 1964, 0.32).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_scope__broad_effects_test, theater_ratio, 1980, 0.37).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__broad_effects_test, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_scope__broad_effects_test, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(comm_be_t1950, commerce_clause_scope__broad_effects_test, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_scope__broad_effects_test, base_extractiveness, 1964, 0.62).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_scope__broad_effects_test, base_extractiveness, 1980, 0.71).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__broad_effects_test, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_scope__broad_effects_test, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement(comm_su_t1950, commerce_clause_scope__broad_effects_test, suppression_requirement, 1950, 0.52).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_scope__broad_effects_test, suppression_requirement, 1964, 0.58).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_scope__broad_effects_test, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__broad_effects_test, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_scope__broad_effects_test, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% The Commerce Clause scope is a contested kernel with three structurally distinct readings. This story instantiates the BROAD reading (broad effects test, aggregation doctrine, comprehensive federal regulatory authority). The sibling narrow-originalist reading ('commerce_clause_scope__narrow_originalist') asserts commerce means trade crossing state lines and limiting federal authority. The intermediate reading ('commerce_clause_scope__intermediate_channels') accepts substantial effects but imposes limiting principles. Each reading has a different epsilon (extractiveness from state autonomy), different beneficiary/victim structure, and different type classification. All three are live positions in current constitutional interpretation, held by different judicial and scholarly coalitions. The broad and narrow readings coexist within the U.S. federal system (different courts, different justices); neither has completely displaced the other. The intermediate reading attempts to find a middle ground but also remains contested. These three stories form a constraint family: all derive from the same constitutional kernel (Commerce Clause) but represent fundamentally different readings of its scope and implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
