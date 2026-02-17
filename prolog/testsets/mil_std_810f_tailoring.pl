% ============================================================================
% CONSTRAINT STORY: mil_std_810f_tailoring
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_mil_std_810f_tailoring, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: mil_std_810f_tailoring
 * human_readable: MIL-STD-810F Environmental Tailoring Standard
 * domain: technological
 * * SUMMARY:
 * MIL-STD-810F is a Department of Defense standard for environmental testing that emphasizes "tailoring" a product's design and test limits to the conditions it will experience in its service life. It acts as a coordination mechanism for ensuring equipment reliability but imposes significant costs and procedural rigidity on developers, creating a hybrid of coordination and extraction.
 * * KEY AGENTS:
 * - Test Lab Engineer: Subject (Powerless), must execute the tests as specified.
 * - DoD Acquisition Official: Beneficiary (Institutional), uses the standard to enforce quality and interoperability.
 * - Systems Auditor: Observer (Analytical), evaluates the standard's dual role.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(mil_std_810f_tailoring, 0.51). % High enough for Snare/Tangled Rope. Extracts significant engineering/financial resources.
domain_priors:suppression_score(mil_std_810f_tailoring, 0.45).   % High enough for Tangled Rope. Alternative testing methods are suppressed by contract requirements.
domain_priors:theater_ratio(mil_std_810f_tailoring, 0.15).       % Low; the standard is highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(mil_std_810f_tailoring, extractiveness, 0.51).
narrative_ontology:constraint_metric(mil_std_810f_tailoring, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(mil_std_810f_tailoring, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(mil_std_810f_tailoring, tangled_rope).
narrative_ontology:human_readable(mil_std_810f_tailoring, "MIL-STD-810F Environmental Tailoring Standard").

% Binary flags
domain_priors:requires_active_enforcement(mil_std_810f_tailoring). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(mil_std_810f_tailoring, dod_end_users). % Derives has_coordination_function/1
narrative_ontology:constraint_victim(mil_std_810f_tailoring, defense_contractors). % Derives has_asymmetric_extraction/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the test lab engineer, the standard is a rigid set of demands that extract
% time and resources, with failure leading to costly re-testing loops.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the DoD official, the standard is a pure coordination tool to ensure
% different contractors produce reliably robust equipment.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the vital coordination function (beneficiaries exist) and
% the asymmetric extraction (victims exist), enforced by institutional power.
% The metrics (E>=0.5, S>=0.4) and structural properties meet the Tangled Rope definition.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mil_std_810f_tailoring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mil_std_810f_tailoring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mil_std_810f_tailoring, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(mil_std_810f_tailoring, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(threshold_validation_for_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mil_std_810f_tailoring, ExtMetricName, E),
    assertion(E >= 0.46).

:- end_tests(mil_std_810f_tailoring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file misclassified the analytical perspective as a Mountain. A Mountain
 * requires near-zero extraction (<= 0.15) and suppression (<= 0.05). With an
 * extractiveness of 0.51 and suppression of 0.45, this constraint is clearly constructed.
 * The key insight is its dual nature: it provides a genuine coordination benefit
 * (safer, more reliable equipment for soldiers) while also asymmetrically extracting
 * value (time, money, labor) from contractors who have no alternative but to comply.
 * This combination of coordination, asymmetric extraction, and active enforcement
 * makes it a canonical Tangled Rope from a neutral, analytical viewpoint. The
 * perspectival gap between the engineer (Snare) and the official (Rope) is what
 * signals the presence of this hybrid structure.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A simpler analysis might label it a
 * Snare, ignoring its legitimate and vital coordination function. Conversely, labeling
 * it a Rope (as its proponents do) would ignore the significant coercive extraction
 * imposed on contractors. The Tangled Rope classification correctly captures both
 * facets, preventing mischaracterization and allowing for a more nuanced policy discussion
 * about balancing coordination benefits against extractive costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_mil_std_810f_tailoring,
    'Do the laboratory simulations truly replicate the "effects" of the service environment, or are they a costly proxy?',
    'Post-deployment audit of field failures vs. predicted lab failures over a 10-year equipment lifecycle.',
    'If fidelity is high, the extraction is justified (closer to Rope). If fidelity is low, it is wasteful coercion (closer to Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mil_std_810f_tailoring, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (E > 0.46).
% Models a slight increase in bureaucratic overhead (theater) and cost
% pass-through (extraction) over the lifecycle of the "F" revision.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(mil_std_810f_tailoring_tr_t0, mil_std_810f_tailoring, theater_ratio, 0, 0.10).
narrative_ontology:measurement(mil_std_810f_tailoring_tr_t5, mil_std_810f_tailoring, theater_ratio, 5, 0.12).
narrative_ontology:measurement(mil_std_810f_tailoring_tr_t10, mil_std_810f_tailoring, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(mil_std_810f_tailoring_ex_t0, mil_std_810f_tailoring, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mil_std_810f_tailoring_ex_t5, mil_std_810f_tailoring, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(mil_std_810f_tailoring_ex_t10, mil_std_810f_tailoring, base_extractiveness, 10, 0.51).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% As a technical standard, its primary function is informational coordination.
narrative_ontology:coordination_type(mil_std_810f_tailoring, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */