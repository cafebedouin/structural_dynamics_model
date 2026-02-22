% ============================================================================
% CONSTRAINT STORY: verification_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-05
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
 *   replication. Original research groups benefit from first-mover advantage
 *   in publications, citations, and grant funding during the 2-5 year window
 *   before independent verification is complete. The field's epistemic
 *   reliability bears the cost of premature claims that may not replicate,
 *   while replication groups face resource constraints and career
 *   disincentives for publishing negative results. The constraint has
 *   intensified as experimental complexity has increased (requiring
 *   specialized equipment, sample preparation protocols, and measurement
 *   expertise that few groups possess) and as funding agencies increasingly
 *   reward breakthrough claims over incremental verification work.
 *
 * KEY AGENTS:
 *   - Original Research Group: Primary beneficiary (institutional/arbitrage) — captures citation advantage and funding priority during verification window
 *   - Field Epistemic Reliability: Primary victim (powerless/trapped) — abstract collective good that cannot exit or organize; bears full cost of false positives
 *   - Replication Groups: Secondary victim (moderate/constrained) — face resource barriers and career risk of negative results; constrained by equipment access and expertise requirements
 *   - Funding Agencies: Institutional actor (institutional/constrained) — benefit from breakthrough narratives for political support but bear long-term cost of credibility damage from retractions
 *   - Analytical Observer: Sees full extraction structure (analytical/analytical) — recognizes asymmetric incentives and epistemic commons depletion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(verification_bottleneck, 0.72).
domain_priors:suppression_score(verification_bottleneck, 0.78).
domain_priors:theater_ratio(verification_bottleneck, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(verification_bottleneck, extractiveness, 0.72).
narrative_ontology:constraint_metric(verification_bottleneck, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(verification_bottleneck, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(verification_bottleneck, snare).
narrative_ontology:human_readable(verification_bottleneck, "Verification Bottleneck in Quantum Materials Discovery").
narrative_ontology:topic_domain(verification_bottleneck, "condensed_matter_physics/experimental_verification").

domain_priors:requires_active_enforcement(verification_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(verification_bottleneck, original_research_group).
narrative_ontology:constraint_victim(verification_bottleneck, field_epistemic_reliability).
narrative_ontology:constraint_victim(verification_bottleneck, replication_groups).
narrative_ontology:constraint_victim(verification_bottleneck, funding_agencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD EPISTEMIC RELIABILITY (SNARE) — Cannot exit the verification crisis; bears full cost of premature claims
constraint_indexing:constraint_classification(verification_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REPLICATION GROUP (SNARE) — Constrained by resource requirements and career risk of negative results
constraint_indexing:constraint_classification(verification_bottleneck, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINAL RESEARCH GROUP (ROPE) — Benefits from first-mover advantage in publications and funding
constraint_indexing:constraint_classification(verification_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUNDING AGENCY (TANGLED ROPE) — Needs breakthrough claims for political support but bears cost of false positives
constraint_indexing:constraint_classification(verification_bottleneck, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SNARE) — Sees structural extraction from epistemic commons
constraint_indexing:constraint_classification(verification_bottleneck, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(verification_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(verification_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(verification_bottleneck, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(verification_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(verification_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): The original research group captures substantial career and funding benefits during the verification window (typically 2-5 years for complex quantum materials claims), while the epistemic cost of premature claims is externalized to the field. The extraction is structural rather than intentional — the incentive gradient favors early publication even when internal confidence is moderate. The increased value reflects the magnitude of career advantage captured during the verification window relative to the externalized epistemic cost. Suppression (0.78): High barriers to independent verification include: (1) specialized equipment requirements (dilution refrigerators, high-field magnets, synchrotron beamtime), (2) tacit knowledge in sample preparation, (3) publication bias against negative results, (4) career risk for junior researchers challenging established groups, (5) funding concentration in groups with track records of breakthrough claims. The suppression is higher than extractiveness because the barriers are primarily structural (equipment access, expertise requirements) rather than purely incentive-driven. The increased value reflects the severity of resource barriers and the strength of institutional mechanisms that prevent challenge. Theater ratio (0.38): Moderate theatrical component includes: performative confidence in press releases, selective emphasis on supporting data while downplaying anomalies, citation networks that amplify preliminary claims, conference presentations that project certainty beyond data quality. However, substantial functional activity remains: genuine experimental work, peer review (albeit imperfect), eventual replication attempts. The reduced value reflects that most activity is still functional research rather than pure performance.
 *
 * PERSPECTIVAL GAP:
 *   The original research group experiences the constraint as a coordination mechanism (Rope) — they are solving the legitimate problem of communicating preliminary findings to enable follow-up work, and the career benefits are viewed as fair compensation for high-risk research. The field's epistemic reliability experiences it as pure extraction (Snare) — premature claims contaminate the literature, misdirect research effort, and erode public trust, with no mechanism for self-correction until years later. Replication groups experience it as a Snare with constrained exit — they can choose not to attempt replication, but this choice means abandoning their research area or accepting subordinate status. Funding agencies experience it as Tangled Rope — they need breakthrough narratives to justify budgets to political overseers, but also bear reputational cost when high-profile claims fail to replicate. The analytical observer sees the structural extraction: the verification bottleneck is not a natural coordination problem but an artifact of misaligned incentives that systematically transfers epistemic risk from claimants to the commons.
 *
 * DIRECTIONALITY LOGIC:
 *   Original research group: Declared as beneficiary with institutional power and arbitrage exit options. The derivation chain produces low d (≈0.05) → negative f(d) (≈-0.12) → negative effective extraction. This correctly captures their structural position: they benefit from the constraint and can exit to other research areas or institutions if challenged. Field epistemic reliability: Declared as victim with powerless status and trapped exit options. Derivation produces high d (≈0.95) → high f(d) (≈1.42) → amplified extraction. Correctly captures that the field as an abstract collective cannot organize or exit, and bears the full cost of contaminated literature. Replication groups: Declared as victim with moderate power and constrained exit. Derivation produces d ≈0.75 → f(d) ≈1.10. Captures their intermediate position: they have some agency (can choose not to replicate) but face significant barriers (resource requirements, career risk). Funding agencies: Declared as both beneficiary (political support from breakthrough narratives) and victim (reputational cost of retractions) with institutional power and constrained exit. The dual declaration produces d ≈0.50 → f(d) ≈0.65, reflecting their ambiguous structural position. No override needed — the symmetric declaration correctly models their Tangled Rope experience.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via multi-perspective classification and structural relationship declarations. The constraint is NOT pure extraction masquerading as coordination — it is genuine extraction enabled by asymmetric information and misaligned incentives. The original research group's Rope classification is their subjective experience (they see themselves as coordinators), but the analytical perspective reveals the Snare structure: high extraction (0.72), high suppression (0.78), and systematic transfer of epistemic risk to victims with no exit options. The Tangled Rope classification for funding agencies shows how institutional actors can simultaneously benefit from and be victimized by the same constraint. The resolution mechanism is structural decomposition: the constraint has both a coordination function (communicating preliminary findings) and an extraction function (capturing career benefits while externalizing epistemic risk). The extraction dominates because: (1) the verification window (2-5 years) is long relative to career timescales, (2) replication groups face asymmetric costs (resource requirements, publication bias against negative results), (3) the field's epistemic reliability cannot organize to demand higher standards. The omega variables identify the empirical tests that would confirm or refute the Snare classification: if alternative probes are truly independent and replication timelines are short, the constraint would reclassify as Rope; if career benefits greatly exceed costs of failed replication, Snare is confirmed.
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
    'If truly independent: verification bottleneck is coordination problem (Rope). If correlated: bottleneck is extraction mechanism (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_probe_sufficiency, empirical, 'Whether alternative probes provide independent verification').

omega_variable(
    career_incentive_magnitude,
    'What is the quantitative career benefit of premature claim vs cost of failed replication?',
    'Bibliometric analysis of citation advantage for first claims; career trajectory analysis for authors of retracted vs confirmed discoveries',
    'If benefit >> cost: Snare classification confirmed. If benefit ≈ cost: reclassify as Tangled Rope with symmetric risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_incentive_magnitude, empirical, 'Asymmetry between career benefit of claim and cost of failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(verification_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(verif_tr_t0, verification_bottleneck, theater_ratio, 0, 0.1).
narrative_ontology:measurement(verif_tr_t3, verification_bottleneck, theater_ratio, 3, 0.22).
narrative_ontology:measurement(verif_tr_t6, verification_bottleneck, theater_ratio, 6, 0.32).
narrative_ontology:measurement(verif_tr_t10, verification_bottleneck, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(verif_be_t0, verification_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(verif_be_t3, verification_bottleneck, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(verif_be_t6, verification_bottleneck, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(verif_be_t10, verification_bottleneck, base_extractiveness, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(verification_bottleneck, information_standard).
narrative_ontology:affects_constraint(verification_bottleneck, inverse_spin_valve_signature).
narrative_ontology:affects_constraint(verification_bottleneck, noncentrosymmetric_asoc_coupling).

% DUAL FORMULATION NOTE:
% The verification bottleneck is downstream of specific materials claims (inverse spin valve signature, noncentrosymmetric ASOC coupling) but represents a distinct structural constraint. The upstream constraints have their own ε values reflecting the empirical status of the specific physical claims; the verification bottleneck has ε=0.72 reflecting the career incentive asymmetry and resource barriers to replication. The bottleneck affects all quantum materials claims but is particularly severe for claims requiring specialized equipment or tacit experimental knowledge. Linked via network.affects_constraints to show that the epistemic status of upstream claims is contaminated by the verification bottleneck structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
