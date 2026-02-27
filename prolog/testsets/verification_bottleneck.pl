% ============================================================================
% CONSTRAINT STORY: verification_bottleneck
% ============================================================================
% Version: 1.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   position. The constraint's theater_ratio (0.72) reflects that traditional
 *   peer review for complex quantum materials claims is largely performative:
 *   reviewers cannot verify synthesis conditions, measurement protocols, or
 *   data quality from a manuscript alone. ArXiv preprints represent an
 *   alternative pathway with genuinely lower theater — distributed scrutiny
 *   skips the performative review ritual entirely, testing the open-source
 *   hypothesis that many eyes make bugs shallow.
 *
 * KEY AGENTS:
 *   - Original Research Group: Primary beneficiary (institutional/arbitrage) — captures citation advantage and funding priority during verification window
 *   - Field Epistemic Reliability: Primary victim (powerless/trapped) — abstract collective good that cannot exit or organize; bears full cost of false positives
 *   - Replication Groups: Secondary victim (moderate/constrained) — face resource barriers and career risk of negative results; also benefit from verification ecosystem
 *   - Open Science Coalition: Organized agents (organized/constrained) — arXiv, registered reports, open-data mandates building alternative verification pathways with sunset logic
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

% PERSPECTIVE 1: FIELD EPISTEMIC RELIABILITY (SNARE) — Cannot exit the verification crisis; bears full cost of premature claims. The epistemic commons has no advocate and no exit option. Maximum experienced extraction — abstract collective cannot organize or escape.
constraint_indexing:constraint_classification(verification_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REPLICATION GROUP (TANGLED ROPE) — Constrained by resource requirements and career risk of negative results, but also benefits from the verification ecosystem through collaborative access and method development. Significant extraction but not maximal — some agency and some benefit.
constraint_indexing:constraint_classification(verification_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINAL RESEARCH GROUP (ROPE) — Benefits from first-mover advantage. Experiences the constraint as coordination: communicating findings enables follow-up work. Net beneficiary — extraction runs toward this agent, not away from them.
constraint_indexing:constraint_classification(verification_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SCIENCE COALITION (SCAFFOLD) — Organized agents (arXiv, registered reports, open-data mandates) see the bottleneck as a temporary coordination failure with a sunset: distributed preprint scrutiny, pre-registration, and open-access norms are building alternative verification pathways that bypass the traditional peer review theater.
constraint_indexing:constraint_classification(verification_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: JOURNAL EDITORIAL SYSTEM (PITON) — Traditional peer review for complex quantum materials claims is largely performative: reviewers cannot verify synthesis conditions, measurement protocols, or data quality from a manuscript alone. The review ritual persists through institutional inertia despite low functional verification.
constraint_indexing:constraint_classification(verification_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some verification lag is inherent to experimental science: complex claims always take time to replicate, and the gap between claim and confirmation is a structural feature of how knowledge advances. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to science' framing naturalizes what is actually a contingent institutional arrangement.
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
 *   Extractiveness (0.40): Moderate. The original research group captures career and funding benefits during the 2-5 year verification window, but the extraction is not as severe as early assessments suggested — much of the 'extraction' is legitimate first-mover reward for high-risk research. The moderate value reflects that the career asymmetry, while real, is partly a fair coordination incentive. Suppression (0.55): Moderate-high. Significant barriers to independent verification include specialized equipment requirements, tacit knowledge in sample preparation, publication bias against negative results, and career risk for researchers who publish replications. But suppression is not total — some groups can and do replicate, and open-science norms are reducing barriers. Theater ratio (0.72): High. Traditional peer review for complex experimental physics is substantially performative. Reviewers assess plausibility, novelty, and presentation quality but cannot verify synthesis conditions, measurement calibration, or raw data quality. The theater has increased over the interval as experimental complexity has outpaced reviewer capacity. ArXiv preprints bypass this theater entirely — their verification mechanism (distributed scrutiny) has different failure modes but lower performative content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties. The original research group sees coordination (Rope) — they are solving the legitimate problem of communicating findings to the broader research community. The open science coalition sees a temporary problem with a sunset (Scaffold) — arXiv preprints, registered reports, and open data mandates are building alternative pathways. The journal editorial system sees its own degraded ritual (Piton) — peer review persists through inertia, not functional verification capacity. Replication groups see mixed coordination and extraction (Tangled Rope) — the system both enables collaborative access and imposes career risk for negative results. The field's epistemic reliability sees pure extraction (Snare) — premature claims contaminate the literature with no self-correction mechanism. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — verification lag is inherent to science — but the structural data reveals this as a false summit: the contingent institutional arrangements (career incentives, funding concentration, publication bias) are not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position: their power level, exit options, and relationship to the extraction flow. The original research group derives low d (≈0.15) from beneficiary status and arbitrage exit options — they can walk away from the constraint with their priority intact. Replication groups derive moderate-high d (≈0.55) from victim status but constrained exit — they face resource barriers and career risk but retain some agency through consortium and open-science pathways. The field's epistemic reliability derives maximum d (≈0.95) from victim status and trapped exit — it has no exit, no agency, and no ability to organize. The open science coalition derives moderate d (≈0.50) from victim status offset by organized power — they are constrained but coordinate collectively to build alternatives. The journal editorial system derives low-to-negative d from beneficiary status and arbitrage exit — they profit from the system but perceive it as degraded. The analytical observer derives high d (≈0.73) from analytical position and global scope — they see the full structure but cannot intervene.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that all six types are legitimate perspectival readings of the same structural data. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?' The analytical observer's mountain is a false summit (naturalizes contingent institutions). The beneficiary's rope is their genuine experience (first-mover advantage enables coordination). The open science coalition's scaffold reflects a real structural feature (alternative pathways with sunset timeline). The journal system's piton is a real observation (performative review persists through inertia). The snare from the field's perspective reflects the victims' structural reality (trapped, powerless, bearing full cost). The tangled rope from replication groups' perspective reflects their actual mixed experience (resources + method development offset by career risk + resource barriers). No single type is 'the' answer — the presheaf over the observation site IS the answer. The constraint's true identity is its perspectival structure.
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

omega_variable(
    preprint_scrutiny_effectiveness,
    'Does distributed preprint scrutiny on arXiv actually catch errors at rates comparable to or better than traditional peer review for complex experimental claims?',
    'Comparison of error detection rates: preprint comments/revisions vs journal review rounds for the same manuscripts; longitudinal tracking of claims first posted as preprints vs those going direct to journals',
    'If effective: scaffold perspective confirmed — open-science sunset is real. If ineffective: many-eyes logic fails for specialized claims, and the scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_scrutiny_effectiveness, empirical, 'Whether arXiv distributed scrutiny provides effective verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(verification_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(verif_tr_t0, verification_bottleneck, theater_ratio, 0, 0.45).
narrative_ontology:measurement(verif_tr_t5, verification_bottleneck, theater_ratio, 5, 0.6).
narrative_ontology:measurement(verif_tr_t10, verification_bottleneck, theater_ratio, 10, 0.72).

% Extraction over time
narrative_ontology:measurement(verif_be_t0, verification_bottleneck, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(verif_be_t5, verification_bottleneck, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(verif_be_t10, verification_bottleneck, base_extractiveness, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(verification_bottleneck, information_standard).
narrative_ontology:affects_constraint(verification_bottleneck, inverse_spin_valve_signature).
narrative_ontology:affects_constraint(verification_bottleneck, noncentrosymmetric_asoc_coupling).

% DUAL FORMULATION NOTE:
% The verification bottleneck is downstream of specific materials claims but represents a distinct structural constraint. The upstream constraints have their own extractiveness values reflecting the empirical status of the specific physical claims; the verification bottleneck has its own extractiveness reflecting the career incentive asymmetry and resource barriers to replication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
