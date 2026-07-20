% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the freedom_floor_reading of the
 *   income_support_commitment kernel. It treats unconditional income support
 *   not as relief or charity but as a structural precondition for autonomy,
 *   dignity, and genuine labor-market exit capacity. The kernel is contested:
 *   the dependency_trap_reading sees the same policy as skill-atrophying
 *   state dependence, while the targeting_efficiency_reading argues resources
 *   should concentrate on proven need. This reading's structural delta is low
 *   extractiveness, no declared victims, and beneficiaries among those whose
 *   bargaining power is weakest under contingent, means-tested regimes.
 *
 * KEY AGENTS:
 *   - caregivers (beneficiary, powerless, constrained exit) â gain economic independence outside wage labor and welfare bureaucracies.
 *   - precarious_workers (beneficiary, powerless, constrained exit) â gain a reserve that permits rejecting exploitative terms.
 *   - abuse_survivors (beneficiary, powerless, constrained exit) â gain liquidity to exit coercive relationships without bureaucratic disclosure.
 *   - artists_entrepreneurs (beneficiary, moderate, mobile exit) â gain a lower risk premium for creative and entrepreneurial activity.
 *   - state_redistributor (agenda_setter, institutional, arbitrage exit) â administers the tax-funded transfer without personal rent extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.1).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '3823856b-800d-4150-8204-2a023e2ba308').
narrative_ontology:cs_kernel_codification('3823856b-800d-4150-8204-2a023e2ba308', formalized).
narrative_ontology:cs_authority_grounding('3823856b-800d-4150-8204-2a023e2ba308', lineage).
narrative_ontology:cs_interpretation_layer_present('3823856b-800d-4150-8204-2a023e2ba308').
narrative_ontology:cs_reading_relation('3823856b-800d-4150-8204-2a023e2ba308', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('3823856b-800d-4150-8204-2a023e2ba308', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('3823856b-800d-4150-8204-2a023e2ba308', foundational, universal_subsistence_as_precondition_of_autonomy).
narrative_ontology:cs_axiom_status(universal_subsistence_as_precondition_of_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('3823856b-800d-4150-8204-2a023e2ba308', universal_subsistence_as_precondition_of_autonomy, deontological).
narrative_ontology:cs_axiom('3823856b-800d-4150-8204-2a023e2ba308', secondary, non_contingent_income_enables_market_exit).
narrative_ontology:cs_axiom_status(non_contingent_income_enables_market_exit, holdable).
narrative_ontology:cs_axiom_grounding('3823856b-800d-4150-8204-2a023e2ba308', non_contingent_income_enables_market_exit, instrumental).
narrative_ontology:cs_reference_frame('3823856b-800d-4150-8204-2a023e2ba308', social_citizenship_subsistence_floor).
narrative_ontology:cs_drift_state('3823856b-800d-4150-8204-2a023e2ba308', contemporary_austerity_politics, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3823856b-800d-4150-8204-2a023e2ba308', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform unpaid or underpaid care work; the unconditional floor grants economic independence outside the labor market, reducing dependence on family breadwinners or punitive welfare bureaucracies. Exit from the constraint means emigration or renouncing citizenship, both costly.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Hold irregular, low-wage, or gig employment; the floor provides a reserve that allows rejecting exploitative terms without falling into destitution. They cannot easily opt out of the tax-transfer system.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Use the unconditional transfer to establish independent housing and leave coercive relationships; the income is not contingent on partner consent or bureaucratic disclosure of abuse. Leaving the jurisdiction would terminate the benefit.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, constrained, national).

% Rely on the floor to absorb income volatility during creative or startup periods, reducing the risk premium required to exit salaried employment. They could relocate to jurisdictions with similar schemes but face transaction costs.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Legislates and funds the universal transfer through the tax system; adjusts the level periodically through democratic budgeting. Does not collect personal rents from the arrangement but administers the collective coordination mechanism.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, state_redistributor, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of guaranteeing a subsistence floor without requiring individuals to prove need, desperation, or labor-market participation, eliminating the administrative overhead and stigma of means-testing while pooling risk across the tax base.
% TRANSFER_FUNCTION: Moves resources from the general tax base to all residents or citizens unconditionally, with net transfer positive for those below the income median and neutral or negative for high earners.
% ABSENT_VOICES: Undocumented residents excluded from citizenship-based schemes have no voice in the distribution rules; fiscal conservatives who oppose the tax burden are in the political conversation but may be structurally marginalized in jurisdictions where the universal floor is politically entrenched.
% DISAPPEARANCE_RATIONALE: If the unconditional floor vanished overnight, caregivers would lose economic independence and be forced back into dependency on breadwinners or means-tested bureaucracies, precarious workers would face immediate survival crises and lose bargaining power, abuse survivors would lose the liquidity required to exit violent households, and the entrepreneurial risk threshold would rise sharply â the labor market and household formation patterns would reorganize around restored desperation incentives.
% FOUNDING_PROBLEM: Labor markets and household structures systematically fail to provide a subsistence floor outside of employment or family dependency, producing a coordination failure where individual employers and family units cannot unilaterally guarantee dignity without a collective pooling mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Feminist economists and labor historians outside the immediate beneficiary coalition document the structural undervaluation of care work and the historical coercion of unpaid household labor; development economists and poverty researchers attest that means-tested alternatives produce administrative exclusion, stigma traps, and non-take-up that worsen deprivation.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.1, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.10) because the transfer is unconditional and universal; there is no means-test gate extracting dignity, time, or compliance from recipients. Suppression is equally low (0.10) because universality eliminates the coercive surveillance and behavioral conditioning machinery of targeted welfare. Theater ratio is negligible (0.05) because the policy's function (cash transfer) is transparent and its administrative overhead is minimal. Accessibility collapse is low (0.20) because alternatives (targeted welfare, private charity, labor-market participation) remain available and well understood. Resistance is moderate (0.30) because fiscal conservatives and targeting advocates contest the tax burden, but this resistance is political contestation rather than a structural feature of the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   A beneficiary seat (caregiver, precarious worker, abuse survivor, artist or entrepreneur) experiences the constraint as a freedom-expanding rope that subsidizes autonomy. A high-income taxpayer who opposes the policy might experience the funding mechanism as extractive, but that is a separate constraint (taxation). Within this reading, there is no payer seat at the point of receipt because the transfer is universal and the net effect is redistributive rather than extractive for the governed parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (caregivers, precarious_workers, abuse_survivors, artists_entrepreneurs) have directionality near the full-beneficiary end because the constraint is designed to subsidize their autonomy and exit capacity. No victims are declared, so no seat sits near the full-target end. The state_redistributor sits near symmetric as the neutral administrator of the coordination mechanism; its power and arbitrage exit options prevent it from being a trapped target of the policy.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would occur if the policy persisted after the coordination problem (guaranteeing a subsistence floor outside employment or family dependency) was solved by other means, such as post-scarcity production or fully automated provision. Currently the founding problem is live in most jurisdictions. The low theater ratio, absence of victims, and absence of enforcement overhead prevent misclassification as a snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_effect_empirical_status,
    'Does unconditional income support at full-scale implementation actually produce the autonomy, dignity, and exit capacity this reading predicts, or do behavioral adaptation and inflation erode the effect?',
    'Large-scale natural experiments and permanent-policy evaluations measuring labor market attachment, household bargaining power, entrepreneurial entry, and subjective well-being outcomes.',
    'If the autonomy effect is weak or negative, the coordination justification weakens and the constraint may recompute toward tangled rope or piton; if strong, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_effect_empirical_status, empirical, 'Empirical uncertainty about whether the freedom-floor effect scales as predicted.').

omega_variable(
    funding_mechanism_contingency,
    'Is the freedom floor''s benignity contingent on a progressive tax base that falls predominantly on high earners, or does the net transfer structure create victims through taxation regardless of universality?',
    'Incidence analysis of the paired tax mechanism: if the funding side extracts regressively from low-income groups, the no-victims claim fails.',
    'Would introduce a payer or victim seat on the funding side, potentially shifting classification toward tangled rope if coordination and extraction coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_mechanism_contingency, conceptual, 'Whether the constraint''s benignity depends on the tax-funding arrangement.').

omega_variable(
    kernel_reading_contest,
    'This reading competes with dependency_trap and targeting_efficiency readings of the same kernel; which structural elements determine which reading dominates in a given jurisdiction?',
    'Comparative policy sociology tracking which reading''s axioms are institutionalized in legislation and which are marginalized.',
    'Determines whether the constraint is authored as rope, snare, or scaffold in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Sibling reading contest for the income support commitment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
