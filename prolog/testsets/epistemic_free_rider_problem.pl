% ============================================================================
% CONSTRAINT STORY: epistemic_free_rider_problem
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_epistemic_free_rider_problem, []).

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
 * * constraint_id: epistemic_free_rider_problem
 * human_readable: The Truth-Mining Exhaustion
 * domain: informational/social/economic
 * * SUMMARY:
 * A scenario where the cost of producing verified, grounded information is borne
 * by a shrinking pool of "truth-miners," while the majority of the population
 * consumes low-cost, unverified synthetic derivatives. This "Rope" for
 * massive informational scaling becomes a "Snare" for the original producers,
 * as the market value of grounded truth is liquidated by a flood of free-riding
 * synthetic mimics, leading to the collapse of the epistemic infrastructure.
 * * KEY AGENTS:
 * - Investigative Journalist: Subject (Powerless Victim)
 * - Synthetic Content Farm: Beneficiary (Institutional)
 * - Information Market Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) reflects the parasitic liquidation of the producer's
% investigative surplus by actors who do not contribute to verification costs.
domain_priors:base_extractiveness(epistemic_free_rider_problem, 0.87).
domain_priors:suppression_score(epistemic_free_rider_problem, 0.74).
domain_priors:theater_ratio(epistemic_free_rider_problem, 0.83). % High theater: "Verification badges" that do not track primary source verification costs.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(epistemic_free_rider_problem, extractiveness, 0.87).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, theater_ratio, 0.83).

% Constraint self-claim (what does the constraint claim to be?)
% The system of content aggregation and synthesis claims to be a coordination mechanism.
narrative_ontology:constraint_claim(epistemic_free_rider_problem, tangled_rope).
narrative_ontology:human_readable(epistemic_free_rider_problem, "The Truth-Mining Exhaustion").

% Binary flags
% Enforcement is algorithmic: search rankings, platform policies, and slow copyright systems.
domain_priors:requires_active_enforcement(epistemic_free_rider_problem).

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(epistemic_free_rider_problem, synthetic_content_farms).
narrative_ontology:constraint_victim(epistemic_free_rider_problem, investigative_journalists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The journalist is trapped: they produce the "ground truth" that synthetic
% models use to stay relevant, but they receive zero return, liquidating their agency.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The content farm views the free-riding as a Rope—the only way to coordinate
% the infinite supply of "content" required to satisfy global attention demand.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical context detects a system with both a coordination function
% (beneficiaries exist) and asymmetric extraction (victims exist), requiring active
% enforcement. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.83) > 0.70 triggers Piton: the "News Aggregator"
% is an inertial spike; it performatively signals "News" while hollowing the producers.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_free_rider_problem_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless producer vs Rope for the institutional free-rider.
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio correctly triggers the Piton classification.
    domain_priors:theater_ratio(epistemic_free_rider_problem, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, piton,
        context(agent_power(analytical), _, exit_options(arbitrage), _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three required properties for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(epistemic_free_rider_problem, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(epistemic_free_rider_problem, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(epistemic_free_rider_problem).

:- end_tests(epistemic_free_rider_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the
 * "coordination" of global information is achieved by liquidating the
 * viability of primary verification. The suppression score (0.74) reflects
 * the difficulty for truth-miners to find alternative markets not dominated
 * by synthetic mimics. The high theater ratio (0.83) captures performative
 * "verification" signals that are decoupled from actual verification costs.
 *
 * PERSPECTIVAL GAP:
 * The Investigative Journalist feels a Snare because their labor is
 * extracted without compensation by the very models that make their career obsolete.
 * The Synthetic Content Farm sees a Rope because free-riding on primary sources
 * is the most efficient way to coordinate the distribution of "good enough"
 * knowledge to billions who cannot afford high-cost verification.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] Resolved via the Piton and Tangled Rope classifications.
 * For an analytical observer, the "Information Market" is a Tangled Rope: it has a
 * real coordination function but also severe, asymmetric extraction. For an auditor
 * focused on function vs performance, it is a Piton: the system's original function
 * (disseminating truth) has atrophied, replaced by the theatrical performance of
 * doing so (Theater Ratio 0.83).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_epistemic_exhaustion,
    'When the last human "miner" stops, does the synthetic model collapse or reach a Mountain of hallucination (Snare vs Mountain)?',
    'Tracking the perplexity of synthetic models after a complete primary-source blackout.',
    'If models explode: Snare of current extraction. If models stabilize: Mountain of synthetic intelligence.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(epistemic_free_rider_problem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This high-extraction constraint degraded over time. Initially, aggregation
% had low extraction and theater. The rise of LLMs dramatically increased both.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(efrp_tr_t0, epistemic_free_rider_problem, theater_ratio, 0, 0.20).
narrative_ontology:measurement(efrp_tr_t5, epistemic_free_rider_problem, theater_ratio, 5, 0.55).
narrative_ontology:measurement(efrp_tr_t10, epistemic_free_rider_problem, theater_ratio, 10, 0.83).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(efrp_ex_t0, epistemic_free_rider_problem, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(efrp_ex_t5, epistemic_free_rider_problem, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(efrp_ex_t10, epistemic_free_rider_problem, base_extractiveness, 10, 0.87).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system coordinates the flow and synthesis of information.
narrative_ontology:coordination_type(epistemic_free_rider_problem, information_standard).

% Network relationships (structural influence edges)
% The collapse of epistemic infrastructure directly impacts public trust.
narrative_ontology:affects_constraint(epistemic_free_rider_problem, public_trust_in_institutions).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */