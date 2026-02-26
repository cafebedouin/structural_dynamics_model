% ============================================================================
% CONSTRAINT STORY: north_korea_songun_mandatrophy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================
% [RESOLVED MANDATROPHY] This constraint is a textbook case of mandatrophy,
% where the entire civilian margin is extracted to service a non-negotiable
% military mandate. The resolution acknowledges this is not a failed state,
% but a Snare functioning with near-perfect efficiency from the perspective
% of its architects.
% ============================================================================

:- module(constraint_north_korea_songun_mandatrophy, []).

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
 * * constraint_id: north_korea_songun_mandatrophy
 * human_readable: North Korean "Songun" (Military-First) Policy
 * domain: political/economic
 * * SUMMARY:
 * The "Songun" (Military-First) policy in North Korea mandates the absolute
 * prioritization of the Korean People's Army in all affairs of state and
 * resource allocation. This system extracts the vast majority of the nation's
 * productive capacity (the "civilian margin") to fund a massive conventional
 * military and nuclear weapons program, leading to the systemic atrophy of
 * all other state functions, including agriculture, healthcare, and energy.
 * * KEY AGENTS:
 * - DPRK Civilian Population: Subject (Powerless), experiencing the policy as
 *   a totalizing system of extraction and scarcity.
 * - The Kim Regime & KPA Leadership: Beneficiary (Institutional), for whom
 *   the policy is a coordination mechanism for regime survival and power.
 * - People's Republic of China: External Actor (Institutional), which provides
 *   economic lifelines that prevent state collapse, treating the DPRK as a
 *   strategic buffer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(north_korea_songun_mandatrophy, 0.95). % Snare extraction >= 0.46
domain_priors:suppression_score(north_korea_songun_mandatrophy, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(north_korea_songun_mandatrophy, 0.10).       % Not performative; brutally functional.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, extractiveness, 0.95).
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(north_korea_songun_mandatrophy, snare).
narrative_ontology:topic_domain(north_korea_songun_mandatrophy, "political/economic").
narrative_ontology:human_readable(north_korea_songun_mandatrophy, "North Korean \"Songun\" (Military-First) Policy").

% Binary flags
domain_priors:requires_active_enforcement(north_korea_songun_mandatrophy).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(north_korea_songun_mandatrophy, kpa_military_leadership).
narrative_ontology:constraint_beneficiary(north_korea_songun_mandatrophy, kim_regime_continuity).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, dprk_civilian_population).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, civilian_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the civilian population, the policy is a Snare that consumes their entire
% productive and personal margin for survival.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the regime, Songun is a Rope—a necessary coordination mechanism to ensure
% national defense and regime survival in a perceived hostile world.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Analytically, the system's near-total extraction (χ = 0.95 * 1.15 * 1.2 ≈ 1.31)
% and suppression (0.95) classify it as a Snare, where the coordination function
% is entirely subsumed by the extractive one.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE EXTERNAL STABILIZER (ROPE)
% For China, providing economic support is a Rope. It's a low-cost way to
% maintain a strategic buffer state and prevent a collapse scenario (refugee
% crisis, US troops on its border), which would be far more costly.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_korea_songun_mandatrophy_tests).

test(perspectival_gap_subject_vs_beneficiary) :-
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, rope, context(agent_power(institutional), _, _, _)),
    true.

test(analytical_classification_is_snare) :-
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare, context(agent_power(analytical), _, _, _)).

test(snare_threshold_validation) :-
    narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, extractiveness, E),
    narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(north_korea_songun_mandatrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system of near-total extraction (0.95) and suppression
 * (0.95). This is not a dysfunctional state but a highly efficient extractive
 * apparatus. The Perspectival Gap is extreme: the regime experiences it as a
 * Rope for survival (with a felt extraction χ near zero due to the π(institutional)
 * modifier), while the powerless citizen experiences it as an inescapable Snare
 * (with a felt extraction χ > 1.0). The analytical view aligns with the subject,
 * classifying it as a Snare due to the objective metrics.
 *
 * MANDATROPHY ANALYSIS:
 * This file resolves the mandatrophy by classifying the system as a Snare from
 * the analytical perspective. It avoids the error of seeing the system as a
 * "failed Rope" and instead identifies it as a "successful Snare." The mandate
 * (military power) is achieved by design through the complete atrophy of the
 * civilian margin. The external support from China acts as a separate Rope that
 * prevents the Snare from reaching its terminal state (total collapse), creating
 * a state of "brittle stability."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_nk_mandatrophy,
    'At what point does the mandatrophic depletion of the DPRK become a strategic liability that China can no longer afford to support?',
    'Monitoring the ratio of Chinese energy/food exports to DPRK against the rate of DPRK provocations that trigger costly international escalation.',
    'If support is cut, the external Rope snaps, likely leading to state collapse (Mountain). If support holds, the Rope remains a tool of strategic arbitrage for China.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Interval represents the Songun era, from ~1995 to present.
narrative_ontology:interval(north_korea_songun_mandatrophy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The Songun policy intensified after its introduction during the "Arduous March."
% Extraction increased as the military and nuclear programs absorbed more resources.
% Theater ratio remains low; the system is brutally functional, not performative.

% Theater ratio over time:
narrative_ontology:measurement(nk_tr_t0, north_korea_songun_mandatrophy, theater_ratio, 0, 0.10).
narrative_ontology:measurement(nk_tr_t5, north_korea_songun_mandatrophy, theater_ratio, 5, 0.10).
narrative_ontology:measurement(nk_tr_t10, north_korea_songun_mandatrophy, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(nk_ex_t0, north_korea_songun_mandatrophy, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(nk_ex_t5, north_korea_songun_mandatrophy, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(nk_ex_t10, north_korea_songun_mandatrophy, base_extractiveness, 10, 0.95).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The Songun policy is, at its core, a mechanism for enforcing resource
% allocation priorities across the entire state.
narrative_ontology:coordination_type(north_korea_songun_mandatrophy, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */