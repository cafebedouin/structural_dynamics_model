% ============================================================================
% CONSTRAINT STORY: rational_inertia_trap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_rational_inertia_trap, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rational_inertia_trap
 * human_readable: The Legacy Protocol Lock-in
 * domain: technological/economic
 * * SUMMARY:
 * A scenario where a superior technological or social protocol exists, but
 * individual agents find it rational to remain in the inferior legacy
 * system because the local cost of switching exceeds the local immediate
 * benefit, even though the collective gain would be massive. This creates
 * a stable, extractive equilibrium benefiting the incumbent platform.
 * * KEY AGENTS:
 * - Small Developer: Subject (Powerless)
 * - Platform Incumbent: Beneficiary (Institutional)
 * - Market Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.75) as the trap siphons innovation potential and maintenance costs.
domain_priors:base_extractiveness(rational_inertia_trap, 0.75).
domain_priors:suppression_score(rational_inertia_trap, 0.68).
domain_priors:theater_ratio(rational_inertia_trap, 0.72). % High theater: constant "innovation" talk with zero structural change.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rational_inertia_trap, extractiveness, 0.75).
narrative_ontology:constraint_metric(rational_inertia_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rational_inertia_trap, theater_ratio, 0.72).

% Constraint self-claim (what does the constraint claim to be?)
% The incumbent claims the legacy protocol is just a coordination standard.
narrative_ontology:constraint_claim(rational_inertia_trap, tangled_rope).

% Binary flags
% Enforcement is through network effects, proprietary APIs, and high switching costs.
domain_priors:requires_active_enforcement(rational_inertia_trap).

% Structural property derivation hooks:
% These are required for the Tangled Rope classification.
narrative_ontology:constraint_beneficiary(rational_inertia_trap, platform_incumbents).
narrative_ontology:constraint_victim(rational_inertia_trap, small_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The small developer is trapped: switching alone is suicide, staying is slow extraction.
constraint_indexing:constraint_classification(rational_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The incumbent views the inertia as a Rope—it provides market stability and predictable coordination.
constraint_indexing:constraint_classification(rational_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view sees both the coordination function (beneficiary exists)
% and the asymmetric extraction (victim exists), with active enforcement. This
% is the canonical signature of a Tangled Rope.
constraint_indexing:constraint_classification(rational_inertia_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.72) > 0.70 triggers Piton: a non-functional inertial spike
% maintained by performative activity rather than real function.
constraint_indexing:constraint_classification(rational_inertia_trap, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(rational_inertia_trap, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_inertia_trap_tests).

test(perspectival_gap) :-
    % Verify Snare for powerless vs Rope for institutional.
    constraint_indexing:constraint_classification(rational_inertia_trap, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_inertia_trap, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rational_inertia_trap, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio results in Piton classification.
    constraint_indexing:constraint_classification(rational_inertia_trap, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three required properties for Tangled Rope are declared.
    domain_priors:requires_active_enforcement(rational_inertia_trap),
    narrative_ontology:constraint_beneficiary(rational_inertia_trap, _),
    narrative_ontology:constraint_victim(rational_inertia_trap, _).

:- end_tests(rational_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.75 to model the significant economic
 * drag caused by the lock-in, capturing lost innovation and inflated
 * maintenance costs. The suppression score of 0.68 reflects the high
 * barriers to exit created by network effects and proprietary standards.
 * The high theater ratio (0.72) represents the incumbent's performative
 * gestures toward "openness" and "future-proofing" while actively preventing
 * any real migration path.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For the small developer (powerless, trapped), the system
 * is a Snare; they are bled dry with no viable alternative. For the platform
 * incumbent (institutional, mobile), it is a Rope that coordinates their
 * ecosystem and guarantees revenue. The analytical observer, seeing both
 * the coordination and the extraction, classifies it as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This constraint is a classic case of Mandatrophy,
 * where a coordination mechanism has degraded into a primary extraction
 * vehicle. The Tangled Rope classification is critical for resolution, as it
 * correctly identifies the hybrid nature of the system. It acknowledges the
 * real coordination function that benefits the incumbent (preventing a
 * misclassification as a pure Snare) while also flagging the asymmetric
 * extraction and active enforcement that harm others. The Piton classification
 * further highlights the system's decay into performative inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_switching_barrier,
    'Is the barrier a technical impossibility (Mountain) or a predatory design (Snare)?',
    'Open-source bridge implementation and adoption rate tracking.',
    'If bridge allows exit: Snare. If bridge fails due to complexity: Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rational_inertia_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the protocol degrading from a useful standard into an
% extractive trap over the 10-year interval.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
% Initially low, as the protocol was functional. Rises as function is
% replaced by performance.
narrative_ontology:measurement(rit_tr_t0, rational_inertia_trap, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rit_tr_t5, rational_inertia_trap, theater_ratio, 5, 0.55).
narrative_ontology:measurement(rit_tr_t10, rational_inertia_trap, theater_ratio, 10, 0.72).

% Extraction over time (triggers extraction_accumulation detection):
% Extraction increases as the incumbent layers on rent-seeking mechanisms.
narrative_ontology:measurement(rit_ex_t0, rational_inertia_trap, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(rit_ex_t5, rational_inertia_trap, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rit_ex_t10, rational_inertia_trap, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The legacy protocol acts as a form of global infrastructure for its ecosystem.
narrative_ontology:coordination_type(rational_inertia_trap, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */