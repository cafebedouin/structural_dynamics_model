% ============================================================================
% CONSTRAINT STORY: climate_policy_extraction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-15
% ============================================================================

:- module(constraint_climate_policy_extraction, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_policy_extraction
 *   human_readable: The Decarbonization Snare (Climate-Identity Collision)
 *   domain: environmental/political
 *
 * SUMMARY:
 *   As federal and international climate policies intensify, a regional
 *   hydrocarbon-based economy interprets decarbonization as a direct threat
 *   to its wealth and identity. This story models how a global environmental
 *   imperative (the necessity of net-zero) is perceived locally as a Snare
 *   that strands assets and deliberately leaves the region behind in the
 *   next economy, while being seen as a necessary coordination mechanism
 *   (Rope) by those most vulnerable to climate change.
 *
 * KEY AGENTS (by structural relationship):
 *   - alberta_energy_sector: Primary target (moderate/trapped) — perceives
 *     net-zero goals as a threat to job security and community survival.
 *   - federal_regulators: Primary enforcer (institutional/constrained) — implements
 *     climate obligations to meet international targets, viewing it as a necessity.
 *   - future_global_populace: Primary beneficiary (powerless/trapped) —
 *     benefits from long-term emissions reduction, viewing the policy as a lifeline.
 *   - climate_scientists: Analytical observer — monitors emissions pathways and policy impacts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_policy_extraction, 0.62).
domain_priors:suppression_score(climate_policy_extraction, 0.65).
domain_priors:theater_ratio(climate_policy_extraction, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_policy_extraction, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_policy_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_policy_extraction, theater_ratio, 0.25).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(climate_policy_extraction, tangled_rope).
narrative_ontology:human_readable(climate_policy_extraction, "The Decarbonization Snare (Climate-Identity Collision)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(climate_policy_extraction). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(climate_policy_extraction, future_global_populace).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(climate_policy_extraction, alberta_energy_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENERGY SECTOR WORKER (SNARE)
% Viewed as an illegitimate imposition that extracts the region's current
% wealth while offering no viable exit option.
constraint_indexing:constraint_classification(climate_policy_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE FEDERAL GOVERNMENT (MOUNTAIN)
% Perceived as an inescapable physical/moral law necessitated by the
% warming world, justifying the enforcement actions.
constraint_indexing:constraint_classification(climate_policy_extraction, mountain,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes a genuine coordination function (global survival) that is
% deeply entangled with the coercive extraction from a regional sub-economy.
constraint_indexing:constraint_classification(climate_policy_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: FUTURE GENERATIONS (ROPE)
% From the perspective of those who will inherit the climate, the policy is a
% pure coordination mechanism for global survival. The regional extraction is
% invisible or seen as a necessary cost for the collective good.
constraint_indexing:constraint_classification(climate_policy_extraction, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_policy_extraction_tests).

test(perspectival_clash) :-
    % Verify the core conflict: Target sees Snare, Enforcer sees Mountain, Beneficiary sees Rope.
    constraint_indexing:constraint_classification(climate_policy_extraction, snare, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(climate_policy_extraction, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(climate_policy_extraction, rope, context(agent_power(powerless), _, _, _)).

test(snare_thresholds) :-
    % Verify Snare metrics: E >= 0.46, S >= 0.60.
    narrative_ontology:constraint_metric(climate_policy_extraction, extractiveness, E), E >= 0.46,
    narrative_ontology:constraint_metric(climate_policy_extraction, suppression_requirement, S), S >= 0.60.

test(tangled_rope_structure) :-
    % Verify the structural requirements for the analytical Tangled Rope classification.
    narrative_ontology:constraint_beneficiary(climate_policy_extraction, _),
    narrative_ontology:constraint_victim(climate_policy_extraction, _),
    domain_priors:requires_active_enforcement(climate_policy_extraction).

:- end_tests(climate_policy_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (0.62) and suppression (0.65) reflect the
 *   significant economic and social costs imposed on the hydrocarbon-dependent
 *   region, which perceives few viable alternative paths. The policy requires
 *   active enforcement (e.g., carbon taxes, emissions caps), fulfilling a key
 *   condition for a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is categorical and multi-faceted. What the regional target sees as a
 *   Snare (political targeting), the national enforcer frames as a Mountain
 *   (physical necessity), and the global powerless beneficiary sees as a Rope
 *   (pure coordination for survival). The analytical view of Tangled Rope
 *   captures the validity of all three: a necessary coordination function
 *   achieved via asymmetric, coercive extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are future generations and populations in climate-vulnerable
 *   areas, who gain stability and survival. The victim is the specific,
 *   capital-intensive, fossil-fuel-based economy and identity cultivated in
 *   the Alberta region for decades. The policy extracts from the latter to
 *   subsidize the former.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial. It prevents mislabeling the
 *   climate transition as "purely" about emissions coordination (Rope) by
 *   forcing acknowledgement of the intense, asymmetric extraction of regional
 *   economic viability. It also prevents dismissing the policy as pure
 *   extraction (Snare) by recognizing its non-trivial global coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_transition_viability,
    'Can federal policy transform a Snare into a Scaffold via credible transition investment?',
    'Analysis of federal green energy subsidies versus provincial oil and gas expansion.',
    'If successful, the Snare perspective softens to Scaffold; if not, separation risk peaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_transition_viability, empirical, 'Feasibility of decarbonization without regional economic collapse').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_policy_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Tracking extraction accumulation as federal targets tighten.
narrative_ontology:measurement(cpe_ex_t0, climate_policy_extraction, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cpe_ex_t5, climate_policy_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cpe_ex_t10, climate_policy_extraction, base_extractiveness, 10, 0.62).

% Theater ratio remains low but increases slightly as policy debates intensify.
narrative_ontology:measurement(cpe_tr_t0, climate_policy_extraction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cpe_tr_t5, climate_policy_extraction, theater_ratio, 5, 0.18).
narrative_ontology:measurement(cpe_tr_t10, climate_policy_extraction, theater_ratio, 10, 0.25).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_policy_extraction, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint can be seen as the inverse of a `fossil_fuel_lock_in`
% constraint. The pre-existing lock-in makes this transition policy feel
% acutely extractive.
narrative_ontology:affects_constraint(fossil_fuel_lock_in, climate_policy_extraction).
narrative_ontology:affects_constraint(climate_policy_extraction, sovereignty_as_arbitrage).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */