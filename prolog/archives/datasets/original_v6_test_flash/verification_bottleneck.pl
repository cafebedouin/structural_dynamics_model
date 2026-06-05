% ============================================================================
% CONSTRAINT STORY: verification_bottleneck
% ============================================================================
% Version: 1.3 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_verification_bottleneck, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: verification_bottleneck
 *   human_readable: Verification Bottleneck in Quantum Materials Discovery
 *   domain: condensed_matter_physics/experimental_verification
 *
 * SUMMARY:
 *   The verification bottleneck in quantum materials discovery creates a
 *   structural tension between the career and funding incentives for claiming
 *   novel phenomena and the epistemic requirement for independent
 *   replication. This constraint exhibits all six DR types from different
 *   perspectives, making it a diagnostic exemplar for how indexical
 *   classification works. The same structural phenomenon — the gap between
 *   claim and confirmation in complex experimental physics — appears as an
 *   immutable law of science (mountain), a coordination mechanism (rope), a
 *   temporary problem being solved by open science (scaffold), a degraded
 *   review ritual (piton), a mixed coordination-extraction hybrid (tangled
 *   rope), or pure extraction (snare), depending on the observer's structural
 *   position.
 *
 * KEY AGENTS:
 *   - Original Research Group: Primary beneficiary (institutional/arbitrage) — captures citation advantage and funding priority during verification window
 *   - Field Epistemic Reliability: Primary victim (powerless/trapped) — abstract collective good that cannot exit or organize; bears full cost of false positives
 *   - Replication Groups: Secondary victim (moderate/constrained) — face resource barriers and career risk of negative results; also benefit from verification ecosystem
 *   - Journal Editorial System: Institutional actor (institutional/arbitrage) — maintains performative review ritual; sees own process as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent scientific limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(verification_bottleneck, 0.4).
domain_priors:suppression_score(verification_bottleneck, 0.55).
domain_priors:theater_ratio(verification_bottleneck, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(verification_bottleneck, extractiveness, 0.4).
narrative_ontology:constraint_metric(verification_bottleneck, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(verification_bottleneck, theater_ratio, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(verification_bottleneck, tangled_rope).
narrative_ontology:human_readable(verification_bottleneck, "Verification Bottleneck in Quantum Materials Discovery").
narrative_ontology:topic_domain(verification_bottleneck, "condensed_matter_physics/experimental_verification").

domain_priors:requires_active_enforcement(verification_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(verification_bottleneck, original_research_group).
narrative_ontology:constraint_victim(verification_bottleneck, field_epistemic_reliability).
narrative_ontology:constraint_victim(verification_bottleneck, replication_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The epistemic commons has no advocate and no exit option. Bears full cost of premature claims.
constraint_indexing:constraint_classification(verification_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Constrained by resource requirements and career risk of negative results, but also benefits from the verification ecosystem through collaborative access and method development.
constraint_indexing:constraint_classification(verification_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Benefits from first-mover advantage. Experiences the constraint as coordination: communicating findings enables follow-up work. Net beneficiary — extraction runs toward this agent, not away from them.
constraint_indexing:constraint_classification(verification_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Traditional peer review for complex quantum materials claims is largely performative: reviewers cannot verify synthesis conditions, measurement protocols, or data quality from a manuscript alone.
constraint_indexing:constraint_classification(verification_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational/universal perspective, some verification lag is inherent to experimental science: complex claims always take time to replicate, and the gap between claim and confirmation is a structural feature of how knowledge advances. This perspective sees the bottleneck as an immutable property of the scientific process itself.
constraint_indexing:constraint_classification(verification_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(verification_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(verification_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(verification_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(verification_bottleneck, TR),
    TR >= 0.70.

:- end_tests(verification_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.40): Moderate. The original research group captures career and funding benefits during the 2-5 year verification window, but the extraction is not as severe as the original v1.0 assessment (0.72) suggested — much of the 'extraction' is legitimate first-mover reward for high-risk research. The reduced value reflects that the career asymmetry, while real, is partly a fair coordination incentive. Suppression (0.55): Moderate-high. Significant barriers to independent verification include specialized equipment requirements, tacit knowledge in sample preparation, publication bias against negative results, and career risk. But suppression is not total — some groups can and do replicate, and open-science norms are reducing barriers. Theater ratio (0.72): High. Traditional peer review for complex experimental physics is substantially performative. Reviewers assess plausibility, novelty, and presentation quality but cannot verify synthesis conditions, measurement calibration, or raw data quality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position — their power level, exit options, and relationship to the extraction flow. The pipeline computes d from these context parameters and applies the sigmoid f(d) to produce experienced extractiveness chi. Beneficiaries with arbitrage options experience low or negative effective extraction; trapped agents with no exit bear maximum extraction; organized agents with exit paths experience moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that all six types are legitimate perspectival readings of the same structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replication_timeline_threshold,
    'What timeline threshold distinguishes legitimate discovery lag from extractive claim-staking?',
    'Historical analysis of confirmed vs retracted discoveries; correlation between replication timeline and ultimate validity',
    'If threshold < 2 years: many legitimate discoveries misclassified as extraction. If threshold > 5 years: extractive claims persist unchallenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_timeline_threshold, empirical, 'Timeline threshold for distinguishing discovery lag from extraction').

omega_variable(
    alternative_probe_sufficiency,
    'Do alternative experimental probes (muon spin rotation, NMR, neutron scattering) constitute independent verification or merely correlated measurements?',
    'Cross-technique correlation analysis; identification of shared systematic errors or sample preparation dependencies',
    'If truly independent: verification bottleneck is coordination problem (Rope from more perspectives). If correlated: bottleneck is extraction mechanism (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_probe_sufficiency, empirical, 'Whether alternative probes provide independent verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(verification_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(veri_tr_t0, verification_bottleneck, theater_ratio, 0, 0.45).
narrative_ontology:measurement(veri_tr_t5, verification_bottleneck, theater_ratio, 5, 0.6).
narrative_ontology:measurement(veri_tr_t10, verification_bottleneck, theater_ratio, 10, 0.72).

% Extraction over time
narrative_ontology:measurement(veri_be_t0, verification_bottleneck, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(veri_be_t5, verification_bottleneck, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(veri_be_t10, verification_bottleneck, base_extractiveness, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(verification_bottleneck, information_standard).
narrative_ontology:affects_constraint(verification_bottleneck, inverse_spin_valve_signature).
narrative_ontology:affects_constraint(verification_bottleneck, noncentrosymmetric_asoc_coupling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
