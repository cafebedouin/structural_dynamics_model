% ============================================================================
% CONSTRAINT STORY: cascading_uncertainty_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_cascading_uncertainty_2026, []).

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
 * * constraint_id: cascading_uncertainty_2026
 * human_readable: The Sovereignty Gap (Cascading Uncertainty)
 * domain: political/social/geopolitical
 * * SUMMARY:
 * As of Feb 5, 2026, a "Cascading Uncertainty" has emerged from the 
 * simultaneous expiration of New START and a domestic enforcement crisis. 
 * While the state projects power through massive defense contracts, a large 
 * portion of the population views its domestic operations as excessive, 
 * creating a widening "sovereignty gap" where external coordination is 
 * maintained by extracting internal social cohesion.
 * * KEY AGENTS:
 * - General US Population: Subject (Powerless)
 * - Federal Executive/Defense Apparatus: Beneficiary (Institutional)
 * - Institutional Auditors/Pollsters: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.75) as the state extracts massive capital for 
% defense while social legitimacy (the ultimate coordination asset) is depleted.
domain_priors:base_extractiveness(cascading_uncertainty_2026, 0.75). 

% Suppression is high (0.88) due to coercive domestic enforcement tactics 
% and the lack of viable alternatives for citizens to opt out of the system.
domain_priors:suppression_score(cascading_uncertainty_2026, 0.88).   

% Theater ratio is high (0.80) because "drawdown" announcements and 
% "strategic ambiguity" mask the underlying growth of technological power and enforcement.
domain_priors:theater_ratio(cascading_uncertainty_2026, 0.80).       

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cascading_uncertainty_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, theater_ratio, 0.80).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(cascading_uncertainty_2026, tangled_rope).
narrative_ontology:human_readable(cascading_uncertainty_2026, "The Sovereignty Gap (Cascading Uncertainty)").

% Binary flags
domain_priors:requires_active_enforcement(cascading_uncertainty_2026). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, federal_defense_contractors).
narrative_ontology:constraint_victim(cascading_uncertainty_2026, american_social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For citizens who feel domestic enforcement has "gone too far," the state's 
% growth is a Snare—a predatory trap of high-tech surveillance and force.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The executive branch views this as a Rope: coordinating national defense 
% and domestic security to ensure national survival.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% With high extraction (0.75), suppression (0.88), a clear coordination function
% (defense), asymmetric extraction (loss of social cohesion), and active enforcement,
% the system is a canonical Tangled Rope.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_uncertainty_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_conditions_met) :-
    domain_priors:base_extractiveness(cascading_uncertainty_2026, E), E >= 0.50,
    domain_priors:suppression_score(cascading_uncertainty_2026, S), S >= 0.40,
    domain_priors:requires_active_enforcement(cascading_uncertainty_2026),
    narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, _),
    narrative_ontology:constraint_victim(cascading_uncertainty_2026, _).

test(high_theater_ratio_check) :-
    domain_priors:theater_ratio(cascading_uncertainty_2026, TR),
    TR >= 0.70.

:- end_tests(cascading_uncertainty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) is extremely high because the state is 
 * actively burning its social capital (legitimacy) to fund its 
 * technological power. The high theater_ratio (0.80) is justified by the 
 * performative nature of "drawdown" announcements while the core enforcement 
 * apparatus remains. The original classification of the analytical view as a 
 * Piton was incorrect; a Piton requires low base extraction (<= 0.10). 
 * Given the massive extraction, suppression, and clear coordination/victim 
 * structure, this is a canonical Tangled Rope.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic example of where Mandatrophy resolution is
 * critical. A naive analysis focusing only on the high extraction (0.75) and
 * suppression (0.88) from the powerless perspective would classify it as a pure
 * Snare. However, this ignores the genuine, if costly, coordination function
 * of national defense perceived by the institutional beneficiary. The Tangled
 * Rope classification correctly holds both truths in tension: it is a system
 * that provides real coordination for one group by asymmetrically extracting
 * from another, preventing the system from collapsing the analysis into a
 * simplistic "pure evil" Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sovereignty_gap_2026,
    'Can a state maintain global technological "Ropes" while its domestic legitimacy is a "Snare"?',
    'Longitudinal study of tax compliance, military recruitment rates, and enrollment in federal programs.',
    'If stable, it suggests social cohesion is a fungible resource for state power. If unstable, it leads to a fragmentation of sovereignty.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascading_uncertainty_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as the gap between state rhetoric and social polls widens.
narrative_ontology:measurement(cas_tr_t0, cascading_uncertainty_2026, theater_ratio, 0, 0.30).
narrative_ontology:measurement(cas_tr_t5, cascading_uncertainty_2026, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cas_tr_t10, cascading_uncertainty_2026, theater_ratio, 10, 0.80).

% Extraction rises as defense contracts scale and domestic approval drops.
narrative_ontology:measurement(cas_ex_t0, cascading_uncertainty_2026, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cas_ex_t5, cascading_uncertainty_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cas_ex_t10, cascading_uncertainty_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(cascading_uncertainty_2026, enforcement_mechanism).

% Network relationships (structural influence edges)
% The high cost of this security apparatus directly impacts fiscal policy.
narrative_ontology:affects_constraint(cascading_uncertainty_2026, national_debt_ceiling_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */