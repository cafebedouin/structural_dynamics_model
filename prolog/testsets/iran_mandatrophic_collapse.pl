% ============================================================================
% CONSTRAINT STORY: iran_mandatrophic_collapse
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_iran_mandatrophic_collapse, []).

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
 * * constraint_id: iran_mandatrophic_collapse
 * human_readable: Iranian Mandatrophy (The Water-Economic Choke)
 * domain: political/economic/technological
 * * SUMMARY:
 * Mandatrophy in Iran describes the systemic wasting away of ecological and
 * economic resilience caused by the rigid prioritization of the "Revolutionary
 * Mandate" (regional proxy funding, nuclear ambition, and ideological self-
 * sufficiency) over the organic "margins" of the state (aquifers, currency
 * stability, and social trust). The "Water Mafia"—primarily the IRGC's
 * construction conglomerate Khatam al-Anbiya—has extracted the nation's
 * hydrologic margin through a massive dam-building program, leading to
 * "water bankruptcy" and irreversible land subsidence.
 * * KEY AGENTS:
 * - IRGC Commander: Beneficiary (Institutional); architects of the
 * "Resistance Economy" who profit from the extraction of natural and fiscal margins.
 * - Iranian Citizen: Subject (Powerless); subject to hyperinflation, dry taps,
 * and frequent electricity blackouts.
 * - Environmental Analyst: Auditor (Analytical); observer identifying the
 * "mandatrophic" loop where short-term mandate wins lead to terminal systemic choking.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(iran_mandatrophic_collapse, 0.90). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(iran_mandatrophic_collapse, 0.85).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(iran_mandatrophic_collapse, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, extractiveness, 0.90).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
% The regime frames its actions as necessary enforcement for national survival.
narrative_ontology:constraint_claim(iran_mandatrophic_collapse, tangled_rope).
narrative_ontology:human_readable(iran_mandatrophic_collapse, "Iranian Mandatrophy (The Water-Economic Choke)").

% Binary flags
domain_priors:requires_active_enforcement(iran_mandatrophic_collapse). % Requires heavy active enforcement (Basij crackdowns, surveillance).

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, irgc_khatam_al_anbiya).
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, axis_of_resistance_proxies).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, iranian_ecological_future).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, iranian_citizenry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen experiences the system as a Snare. The regime's "Mandate"
% extracts their water, electricity, and savings, choking their ability to live.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The IRGC views its control as a Rope—a necessary coordination tool to bypass
% international sanctions and maintain state survival against external threats.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analyst identifies the current state as a Mountain. The geological damage
% to aquifers is permanent, and "Water Bankruptcy" is an unyielding physical
% limit that no policy can bypass. The Snare has hit a physical wall.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_mandatrophic_collapse_tests).

test(perspectival_gap_rope_vs_snare) :-
    % Verify the core perspectival gap between the powerless citizen and the institutional beneficiary.
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_view_is_mountain) :-
    % Verify the analytical observer sees the terminal state as a Mountain.
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeAnalytical == mountain.

test(threshold_validation_high_extraction) :-
    % Mandatrophy requires extreme extraction of the "margin" (water/capital).
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(iran_mandatrophic_collapse, ExtMetricName, E),
    E >= 0.46.

:- end_tests(iran_mandatrophic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system in a terminal state. The base extractiveness (0.90)
 * represents the near-total consumption of the nation's ecological and economic
 * margins (water, currency stability) to fund the "Revolutionary Mandate." The
 * suppression score (0.85) reflects the active crushing of alternatives like
 * technocratic water management or diplomatic de-escalation.
 *
 * The Perspectival Gap is profound:
 * - The IRGC (Institutional) sees its control as a Rope, a necessary tool for
 *   coordinating national survival against sanctions and foreign enemies.
 * - The Citizen (Powerless) experiences this as a Snare, where their livelihood
 *   is extracted to fund a mandate from which they derive no benefit.
 * - The Analyst (Analytical) sees the outcome: a Mountain. The long-running Snare
 *   has extracted so much that it has created an irreversible physical limit—
 *   aquifers are permanently destroyed, and the land is sinking. The political
 *   problem has become a geological fact.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This case is a classic example of Mandatrophy, where
 * a state's ideological mandate consumes its own physical capacity to exist.
 * The classification as a Mountain from the analytical perspective resolves the
 * ambiguity. While the *mechanism* is a Snare (a constructed policy), its
 * *consequences* have become a Mountain (an unchangeable physical reality).
 * The system correctly identifies that the constraint is no longer a matter of
 * policy choice but of confronting a terminal physical state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_iran_mandatrophic_collapse,
    'To what degree is the collapse caused by external sanctions versus internal mismanagement?',
    'Comparative analysis of GDP impact from sanctions vs. estimated value lost from water mismanagement and corruption.',
    'If sanctions dominate, the constraint is an external Snare. If mismanagement dominates, it is a self-inflicted Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iran_mandatrophic_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models how the "Resistance
% Economy" began as a coordination mechanism and degenerated into pure extraction.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (low, as the extraction is direct, not performative):
narrative_ontology:measurement(imc_tr_t0, iran_mandatrophic_collapse, theater_ratio, 0, 0.10).
narrative_ontology:measurement(imc_tr_t5, iran_mandatrophic_collapse, theater_ratio, 5, 0.15).
narrative_ontology:measurement(imc_tr_t10, iran_mandatrophic_collapse, theater_ratio, 10, 0.20).

% Extraction over time (shows intensification as margins are depleted):
narrative_ontology:measurement(imc_ex_t0, iran_mandatrophic_collapse, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(imc_ex_t5, iran_mandatrophic_collapse, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(imc_ex_t10, iran_mandatrophic_collapse, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The "Resistance Economy" is a resource allocation mechanism, albeit a highly extractive one.
narrative_ontology:coordination_type(iran_mandatrophic_collapse, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(iran_mandatrophic_collapse, 0.0).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(iran_mandatrophic_collapse, some_other_constraint).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */