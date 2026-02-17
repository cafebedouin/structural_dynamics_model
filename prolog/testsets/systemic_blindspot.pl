% ============================================================================
% CONSTRAINT STORY: systemic_blindspot
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_systemic_blindspot, []).

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
 * * constraint_id: systemic_blindspot
 * human_readable: The Incalculable Tail-Risk
 * domain: cognitive/organizational/economic
 * * SUMMARY:
 * A scenario where an institutional framework's metrics and sensors are
 * fundamentally incapable of detecting a rising existential risk because
 * the risk falls outside the system's "legible" taxonomy. It functions as a
 * Rope for internal coordination and career stability within the institution,
 * but represents a massive Snare for the broader population whose survival
 * depends on the system's (missing) peripheral vision.
 * * KEY AGENTS:
 * - Marginalized Citizen: Subject (Powerless)
 * - Risk Assessment Bureau: Beneficiary (Institutional)
 * - Epistemological Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) because the blindspot allows for the unchecked
% accumulation of catastrophic risk while siphoning resources into "safety"
% metrics that measure the wrong variables.
domain_priors:base_extractiveness(systemic_blindspot, 0.86).
domain_priors:suppression_score(systemic_blindspot, 0.70).
domain_priors:theater_ratio(systemic_blindspot, 0.82). % High theater: focus on precise reporting of irrelevant data.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(systemic_blindspot, extractiveness, 0.86).
narrative_ontology:constraint_metric(systemic_blindspot, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(systemic_blindspot, theater_ratio, 0.82).

% Constraint self-claim (what does the constraint claim to be?)
% The institution claims its framework is for coordination and safety.
narrative_ontology:constraint_claim(systemic_blindspot, tangled_rope).
narrative_ontology:human_readable(systemic_blindspot, "The Incalculable Tail-Risk").

% Binary flags
% Enforcement is the institutional pressure to conform to the limited metrics.
domain_priors:requires_active_enforcement(systemic_blindspot).

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(systemic_blindspot, risk_assessment_bureau).
narrative_ontology:constraint_victim(systemic_blindspot, marginalized_citizen).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the powerless agent, the blindspot is a snare: they are aware of the
% danger but are ignored by the system because their data is "anecdotal."
constraint_indexing:constraint_classification(systemic_blindspot, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the restricted taxonomy as a Rope—a way to ensure
% consistent, high-fidelity coordination and "objective" reporting.
constraint_indexing:constraint_classification(systemic_blindspot, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of high coordination efficiency (Rope)
% masking predatory, unpriced risk accumulation (Snare).
constraint_indexing:constraint_classification(systemic_blindspot, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: the "risk management" process
% is a non-functional, performative spike maintained by institutional inertia.
constraint_indexing:constraint_classification(systemic_blindspot, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(systemic_blindspot, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(systemic_blindspot_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(systemic_blindspot, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(systemic_blindspot, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(systemic_blindspot, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection by systems auditors.
    constraint_indexing:constraint_classification(systemic_blindspot, piton,
        context(agent_power(analytical), time_horizon(civilizational), _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(systemic_blindspot, _),
    narrative_ontology:constraint_victim(systemic_blindspot, _),
    domain_priors:requires_active_enforcement(systemic_blindspot).

:- end_tests(systemic_blindspot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a state where the system's "coordination"
 * has atrophied into a performative loop that liquidates species-level safety
 * for institutional peace of mind. The suppression score (0.70) represents the
 * active marginalization of out-of-band data that threatens the institutional
 * consensus. The high theater ratio (0.82) confirms that the activity is
 * primarily performative rather than functional.
 *
 * * PERSPECTIVAL GAP:
 * The Marginalized Citizen feels a Snare because they are trapped in a
 * reality that the system refuses to "see." The Risk Bureau sees a Rope
 * because their metrics provide the illusion of total control and
 * organizational predictability. The Analytical Observer sees a Tangled Rope,
 * recognizing both the internal coordination and the externalized harm.
 *
 * * [RESOLVED MANDATROPHY]:
 * The Mandatrophy is resolved by identifying the constraint's dual nature.
 * The `tangled_rope` classification acknowledges the valid internal coordination
 * function while simultaneously flagging the severe asymmetric extraction imposed
 * on outsiders. The `piton` classification further clarifies that this
 * coordination is now almost entirely theatrical, having lost its connection
 * to its original risk-mitigation purpose. This dual classification prevents
 * the system from collapsing the analysis into a simple "pure extraction" Snare,
 * which would miss the internal logic that sustains the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_ontological_expansion,
    'Can the system expand its taxonomy to include the "invisible" risk, or is its structure fundamentally rigid?',
    'Auditing the speed of institutional adoption of "out-of-framework" signals versus the rate of their suppression.',
    'If taxonomy adapts: Snare of policy (fixable). If taxonomy is rigid: Mountain of Cognitive Closure (immutable limit).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(systemic_blindspot, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a constraint that began as a useful heuristic but
% degraded into a dangerous, theatrical blindspot.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(systemic_blindspot_tr_t0, systemic_blindspot, theater_ratio, 0, 0.20).
narrative_ontology:measurement(systemic_blindspot_tr_t5, systemic_blindspot, theater_ratio, 5, 0.55).
narrative_ontology:measurement(systemic_blindspot_tr_t10, systemic_blindspot, theater_ratio, 10, 0.82).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(systemic_blindspot_ex_t0, systemic_blindspot, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(systemic_blindspot_ex_t5, systemic_blindspot, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(systemic_blindspot_ex_t10, systemic_blindspot, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system allocates attention and safety resources based on its flawed metrics.
narrative_ontology:coordination_type(systemic_blindspot, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */