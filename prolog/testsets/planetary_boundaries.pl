% ============================================================================
% CONSTRAINT STORY: planetary_boundaries
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_planetary_boundaries, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: planetary_boundaries
 * human_readable: Planetary Boundaries Framework
 * domain: environmental/economic
 * * SUMMARY:
 * The Planetary Boundaries framework defines a "safe operating space for humanity"
 * based on nine critical Earth system processes. While presented as a set of
 * biophysical limits (natural law), the policy frameworks derived from it create
 * a global coordination system that asymmetrically extracts development potential
 * from industrializing nations to maintain stability for the global system.
 * * KEY AGENTS:
 * - Developing Economies: Subject (Powerless), experience capped growth potential.
 * - Global Governance Bodies: Beneficiary (Institutional), use the framework for policy coordination.
 * - Systems Auditor: Observer (Analytical), assesses the full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(planetary_boundaries, 0.80). % High extraction of future development potential.
domain_priors:suppression_score(planetary_boundaries, 0.80).   % Biophysical limits strongly suppress unlimited growth alternatives.
domain_priors:theater_ratio(planetary_boundaries, 0.10).       % Low theater; the functions and impacts are direct.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(planetary_boundaries, extractiveness, 0.80).
narrative_ontology:constraint_metric(planetary_boundaries, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(planetary_boundaries, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be an objective, unchangeable feature of reality.
narrative_ontology:constraint_claim(planetary_boundaries, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(planetary_boundaries). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(planetary_boundaries, global_civilization).
narrative_ontology:constraint_victim(planetary_boundaries, developing_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For developing economies, the framework is a trap that caps their industrial
% and economic growth potential, enforcing a developmental ceiling.
constraint_indexing:constraint_classification(planetary_boundaries, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For global governance bodies, it is a pure coordination tool, providing a
% scientific basis for international treaties and sustainable development goals.
constraint_indexing:constraint_classification(planetary_boundaries, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the framework is a Tangled Rope. It has a genuine and necessary
% coordination function (beneficiary exists) but also imposes severe, asymmetric
% extraction (victim exists) and requires active political enforcement.
constraint_indexing:constraint_classification(planetary_boundaries, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_boundaries_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_boundaries, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_boundaries, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(planetary_boundaries, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(high_extraction_threshold) :-
    domain_priors:base_extractiveness(planetary_boundaries, E),
    assertion(E >= 0.46).

:- end_tests(planetary_boundaries_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.8) is high because the constraint effectively
 * appropriates the future development potential of industrializing nations to
 * maintain the stability of the current global system. The suppression score
 * (0.8) is also high because the biophysical limits are presented as non-negotiable,
 * suppressing the alternative of resource-intensive growth paths followed by
 * currently developed nations.
 *
 * The perspectival gap is stark: for institutional actors, it's a 'Rope' for
 * coordinating global policy. For those with limited development options, it's
 * a 'Snare' that locks them into a lower economic ceiling.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of potential Mandatrophy. A naive analysis
 * focusing only on the victims would label it a pure 'Snare'. However, this
 * would miss its critical coordination function. The 'Tangled Rope'
 * classification from the analytical perspective resolves this by structurally
 * acknowledging both functions: it is a necessary coordination mechanism
 * ('Rope' aspect) that has severe, asymmetrically distributed extractive costs
 * ('Snare' aspect). This prevents the system from incorrectly dismissing the
 * entire framework as purely predatory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_planetary_boundaries,
    'Are the boundaries truly fixed biophysical Mountains, or are they constructed policy Snares whose thresholds could be altered by radical technological innovation (e.g., fusion, carbon capture)?',
    'Empirical observation of Earth system responses vs. demonstrated efficacy of breakthrough technologies at planetary scale.',
    'If Mountain, the extraction is non-negotiable. If Snare, the extraction is a policy choice that could be technologically obviated.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(planetary_boundaries, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint's extractive pressure has increased over the interval as
% planetary limits are approached and policy enforcement has tightened.
% Theater ratio remains low and stable.

% Theater ratio over time:
narrative_ontology:measurement(pb_tr_t0, planetary_boundaries, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pb_tr_t5, planetary_boundaries, theater_ratio, 5, 0.08).
narrative_ontology:measurement(pb_tr_t10, planetary_boundaries, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(pb_ex_t0, planetary_boundaries, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(pb_ex_t5, planetary_boundaries, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(pb_ex_t10, planetary_boundaries, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The framework functions to allocate finite global budgets
% (e.g., carbon, nitrogen) among competing actors.
narrative_ontology:coordination_type(planetary_boundaries, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */