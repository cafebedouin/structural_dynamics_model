% ============================================================================
% CONSTRAINT STORY: lula_hemisphere_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_lula_hemisphere_2026, []).

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
 * * constraint_id: lula_hemisphere_2026
 * human_readable: The Monroe Doctrine Revival (Unilateral US Hegemony)
 * domain: political
 * * SUMMARY:
 * This constraint models the revival of unilateral hegemonic power, as described by
 * Brazilian President Lula in response to a hypothetical 2026 US military intervention
 * in Venezuela. The action is framed as a violation of the sovereign equality of nations,
 * where "zones of influence" and unilateral force replace the collectively agreed-upon
 * rules of the UN Charter.
 * * KEY AGENTS:
 * - Venezuelan Civilians: Subjects (Powerless) experiencing the intervention as an overwhelming, unchangeable event.
 * - United States Government: Beneficiary (Institutional/Powerful) acting unilaterally to achieve strategic goals.
 * - President Lula of Brazil: Observer (Institutional) advocating for multipolarity and international law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(lula_hemisphere_2026, 0.75). % Rationale: Military capture of a sovereign state's assets and political autonomy represents extreme extraction.
domain_priors:suppression_score(lula_hemisphere_2026, 0.85).   % Rationale: Unilateral military force explicitly suppresses all other alternatives, such as diplomacy or UN-led processes.
domain_priors:theater_ratio(lula_hemisphere_2026, 0.10).       % Rationale: The action is primarily functional (military force) rather than performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(lula_hemisphere_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(lula_hemisphere_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(lula_hemisphere_2026, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The hegemonic power claims its action is a necessary enforcement of justice.
narrative_ontology:constraint_claim(lula_hemisphere_2026, snare).

% Binary flags
domain_priors:requires_active_enforcement(lula_hemisphere_2026). % The hegemony must be actively maintained by military presence.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(lula_hemisphere_2026, hegemonic_powers).
narrative_ontology:constraint_victim(lula_hemisphere_2026, sovereign_nations).
narrative_ontology:constraint_victim(lula_hemisphere_2026, venezuelan_civilians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% For a Venezuelan civilian, the intervention is an overwhelming, inescapable force of nature.
% χ = 0.75 * 1.5 (powerless) * 1.0 (national) = 1.125. High extraction and suppression feel like a Mountain.
constraint_indexing:constraint_classification(lula_hemisphere_2026, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% From the perspective of the US strategists executing the plan, it is a functional tool (Rope)
% to achieve strategic goals where multilateral systems have failed.
% χ = 0.75 * 0.6 (powerful) * 1.2 (global) = 0.54. Extraction is felt as moderate cost for strategic gain.
constraint_indexing:constraint_classification(lula_hemisphere_2026, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE REGIONAL OBSERVER (SNARE)
% For President Lula, this is a coercive, extractive Snare that violates international law
% to serve hegemonic interests, trapping the region in a dependent relationship.
% χ = 0.75 * -0.2 (institutional) * 1.1 (continental) = -0.165. The negative extraction reflects
% the institutional cost and loss of stability for Brazil, classifying it as a predatory Snare.
constraint_indexing:constraint_classification(lula_hemisphere_2026, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% The analytical observer sees high base extraction (0.75) and high suppression (0.85),
% classifying it as a Snare by definition.
% χ = 0.75 * 1.15 (analytical) * 1.2 (global) = 1.035.
constraint_indexing:constraint_classification(lula_hemisphere_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lula_hemisphere_2026_tests).

test(perspectival_gap_subject_beneficiary) :-
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypePowerful,
        context(agent_power(powerful), _, _, _)),
    TypePowerless == mountain,
    TypePowerful == rope,
    TypePowerless \= TypePowerful.

test(analytical_classification_is_snare) :-
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeAnalytical == snare.

test(snare_threshold_validation) :-
    narrative_ontology:constraint_metric(lula_hemisphere_2026, extractiveness, E),
    narrative_ontology:constraint_metric(lula_hemisphere_2026, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(lula_hemisphere_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a clear case of hard power projection. Base extractiveness (0.75)
 * is high due to the seizure of national sovereignty and potential resources. Suppression
 * (0.85) is also high because unilateral military force is the ultimate suppressor of
 * alternative solutions like diplomacy.
 *
 * The Perspectival Gap is stark:
 * - The powerless civilian experiences this as a Mountain: an immutable, catastrophic event.
 * - The powerful hegemon sees it as a Rope: a tool for imposing order and achieving objectives.
 * - The neighboring institutional power (Lula) and the analytical observer see it for what
 *   the metrics compute: a predatory Snare that extracts value while coercively limiting options.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This case is a classic example of Mandatrophy resolution. A naive analysis might see the
 * hegemon's claim of "enforcing justice" and classify it as a broken Rope. However, the
 * extremely high base extraction and suppression scores, combined with the analytical
 * perspective, force the classification to Snare. This correctly identifies the action's
 * primary function as extractive and coercive, not coordinative, preventing mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_lula_hemisphere_2026_1,
    'Was the US intervention primarily for human rights justice or for strategic resource extraction?',
    'Audit of captured president trial evidence vs. US corporate contracts signed in Venezuela post-capture.',
    'If Justice: the constraint is a failing Rope. If Extraction: the constraint is a predatory Snare.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_lula_hemisphere_2026_2,
    'Will Latin American states overcome ideological differences to resist hegemonic pressure as Lula hopes?',
    'Track voting alignment in OAS/UN and joint infrastructure project funding through 2027.',
    'If Unified: Regional agency creates a counter-Rope. If Divided: States remain trapped in the Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. The narrative is a snapshot in 2026,
% but the model uses a standard 0-10 interval for drift analysis.
narrative_ontology:interval(lula_hemisphere_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (E > 0.46).
% Models the escalation of hegemonic pressure over the interval leading to the intervention.

% Theater ratio over time (remains low, as actions are functional):
narrative_ontology:measurement(lula_hemisphere_2026_tr_t0, lula_hemisphere_2026, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lula_hemisphere_2026_tr_t5, lula_hemisphere_2026, theater_ratio, 5, 0.08).
narrative_ontology:measurement(lula_hemisphere_2026_tr_t10, lula_hemisphere_2026, theater_ratio, 10, 0.10).

% Extraction over time (shows a sharp increase as diplomatic options fail and force is used):
narrative_ontology:measurement(lula_hemisphere_2026_ex_t0, lula_hemisphere_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(lula_hemisphere_2026_ex_t5, lula_hemisphere_2026, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(lula_hemisphere_2026_ex_t10, lula_hemisphere_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is an act of pure coercion and extraction, not coordination.
% Therefore, no coordination_type is declared. No network relationships are
% specified in the source narrative.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */