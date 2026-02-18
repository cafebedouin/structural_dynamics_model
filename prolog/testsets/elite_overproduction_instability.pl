% ============================================================================
% CONSTRAINT STORY: elite_overproduction_instability
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_elite_overproduction_instability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: elite_overproduction_instability
 * human_readable: The Aspirant's Bottleneck
 * domain: social
 * * SUMMARY:
 * A structural condition where the number of individuals educated and prepared
 * for elite positions significantly exceeds the available slots in the social
 * hierarchy. This results in a "Snare" for the overproduced aspirants and a
 * "Tangled Rope" for the system itself, which coordinates talent but also
 * generates a pool of disaffected counter-elites.
 * * KEY AGENTS:
 * - The Aspirant: Subject (Powerless) - Highly credentialed but economically blocked.
 * - The Incumbent: Beneficiary (Institutional) - Controls entry to existing slots.
 * - The Sociologist: Auditor (Analytical) - Identifies the "Tangled Rope" logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(elite_overproduction_instability, 0.58). % High: credential inflation extracts labor/debt from aspirants.
domain_priors:suppression_score(elite_overproduction_instability, 0.70).   % High: few non-credentialed paths to power or status.
domain_priors:theater_ratio(elite_overproduction_instability, 0.45).       % Moderate: Meritocracy signals are increasingly performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(elite_overproduction_instability, extractiveness, 0.58).
narrative_ontology:constraint_metric(elite_overproduction_instability, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(elite_overproduction_instability, theater_ratio, 0.45).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(elite_overproduction_instability, tangled_rope).
narrative_ontology:human_readable(elite_overproduction_instability, "The Aspirant's Bottleneck").
narrative_ontology:topic_domain(elite_overproduction_instability, "social").

% Binary flags
domain_priors:requires_active_enforcement(elite_overproduction_instability). % Gatekeeping, credential validation, etc.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(elite_overproduction_instability, incumbent_elites).
narrative_ontology:constraint_victim(elite_overproduction_instability, aspirant_elites).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the aspirant, the system is a trap of endless preparation with no guaranteed exit.
constraint_indexing:constraint_classification(elite_overproduction_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% To the institution, the surplus of talent is a Rope—it ensures high competition and efficiency.
constraint_indexing:constraint_classification(elite_overproduction_instability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes the hybrid: it facilitates talent selection but creates a radicalizing surplus.
constraint_indexing:constraint_classification(elite_overproduction_instability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_overproduction_instability_tests).

test(perspectival_gap) :-
    % Verify variance: Snare for the powerless, Rope for the institution.
    constraint_indexing:constraint_classification(elite_overproduction_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_overproduction_instability, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(tangled_rope_detection) :-
    % Verify that the analytical perspective correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(elite_overproduction_instability, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(elite_overproduction_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.58) reflects the high cost of credentialing (debt, time,
 * opportunity cost) relative to the diminishing likelihood of a status-commensurate
 * return. The suppression score (0.70) reflects the social and economic closure
 * that makes alternative paths to elite status non-viable. The perspectival gap
 * is stark: incumbents see a functional talent pipeline (Rope), while aspirants
 * experience a debt-fueled trap (Snare).
 *
 * * MANDATROPHY ANALYSIS:
 * The Mandatrophy is resolved by the "Tangled Rope" classification. It prevents
 * the system from seeing the instability as mere bad luck or individual failure;
 * it is a structural byproduct of a coordination mechanism (meritocratic competition)
 * that has become extractive and unstable due to a supply/demand imbalance for
 * elite positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_elite_overproduction_instability,
    'Will the surplus aspirants reform the system (Scaffold) or attempt to dismantle it (Snare)?',
    'Longitudinal study of counter-elite political movements vs. institutional absorption rates.',
    'If reform: Transition to Scaffold; If dismantle: Collapse of the current Rope/Tangled Rope structure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(elite_overproduction_instability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the constraint intensifying over the interval.
% Extraction increases as credentialing becomes more expensive and less effective.
% Theater ratio increases as meritocratic claims become less connected to outcomes.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(eoi_tr_t0, elite_overproduction_instability, theater_ratio, 0, 0.20).
narrative_ontology:measurement(eoi_tr_t5, elite_overproduction_instability, theater_ratio, 5, 0.35).
narrative_ontology:measurement(eoi_tr_t10, elite_overproduction_instability, theater_ratio, 10, 0.45).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(eoi_ex_t0, elite_overproduction_instability, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(eoi_ex_t5, elite_overproduction_instability, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(eoi_ex_t10, elite_overproduction_instability, base_extractiveness, 10, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system's primary coordination function is allocating scarce elite positions.
narrative_ontology:coordination_type(elite_overproduction_instability, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */