% ============================================================================
% CONSTRAINT STORY: france_cordon_sanitaire_2026
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_france_cordon_sanitaire_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: france_cordon_sanitaire_2026
 *   human_readable: The "Front Républicain" (Republican Front) Cordon Sanitaire
 *   domain: political/social
 *
 * SUMMARY:
 *   The "Front Républicain" (Republican Front) in France, once a functional
 *   mechanism to block the far-right, has transitioned into a Tangled Rope
 *   state by early 2026. While initially effective in preventing the
 *   far-right from gaining power, the cordon sanitaire has become a
 *   theatrical ritual, maintained through inertia and increasingly extracting
 *   from marginalized and moderate right voters. Mainstream parties continue
 *   to invoke it, but its impact on voter behavior has diminished, and it may
 *   even be counterproductive, fueling resentment and further empowering
 *   extremist forces.
 *
 * KEY AGENTS:
 *   - Marginalized Voters: Primary target (powerless/trapped) - feel unrepresented and excluded.
 *   - Mainstream Political Parties: Maintaining the cordon sanitaire (institutional/constrained) - clinging to a tradition, benefiting from the exclusion of the far-right.
 *   - Moderate Right Voters: Affected by the cordon sanitaire (moderate/constrained) - seeking alternatives.
 *   - Far-Right Parties: Beneficiary (powerful/arbitrage) - capitalizing on the exclusion.
 *   - Political Science Observer: Analytical perspective (analytical/analytical) - observing the historical trend.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(france_cordon_sanitaire_2026, 0.55).
domain_priors:suppression_score(france_cordon_sanitaire_2026, 0.6).
domain_priors:theater_ratio(france_cordon_sanitaire_2026, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(france_cordon_sanitaire_2026, tangled_rope).
narrative_ontology:human_readable(france_cordon_sanitaire_2026, "The \"Front Républicain\" (Republican Front) Cordon Sanitaire").
narrative_ontology:topic_domain(france_cordon_sanitaire_2026, "political/social").

domain_priors:requires_active_enforcement(france_cordon_sanitaire_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(france_cordon_sanitaire_2026, mainstream_political_parties).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, marginalized_voters).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, moderate_right_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of marginalized voters whose concerns are not addressed by mainstream parties, leading to further alienation and support for extremist alternatives. They are trapped within a system that does not represent them.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of mainstream political parties clinging to the cordon sanitaire despite its diminishing effectiveness and increasing irrelevance, constrained by tradition and fear of being perceived as legitimizing the far-right. They recognize the ineffectiveness but continue performative adherence.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of moderate right voters who may have been willing to vote for mainstream parties to block the far-right in the past, but now feel constrained by the cordon sanitaire and consider other options, including abstaining or voting for protest parties. They have some mobility but face social and political constraints.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of far-right parties who benefit from the cordon sanitaire by framing themselves as the only alternative to the status quo, and arbitraging on the political exclusion and the perceived failure of the mainstream parties.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of political science analysts who observe the cordon sanitaire as a historical artifact, a once-functional mechanism that has ossified into a theatrical ritual, maintained through inertia and lacking its original effectiveness. Analytical perspective highlighting the ineffectiveness of the cordon sanitaire.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(france_cordon_sanitaire_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(france_cordon_sanitaire_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(france_cordon_sanitaire_2026, TR),
    TR >= 0.70.

:- end_tests(france_cordon_sanitaire_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-High. The cordon sanitaire extracts votes from marginalized voters and moderate right voters, pushing them towards abstention or extremist alternatives. Suppression (0.60): High. Voters have limited alternatives and are increasingly unwilling to be constrained by the cordon sanitaire, but the political system actively suppresses alternative voices. Theater ratio (0.80): High. The cordon sanitaire is largely a performative ritual, with limited functional impact.
 *
 * PERSPECTIVAL GAP:
 *   Marginalized voters experience a snare because the cordon sanitaire reinforces their exclusion. Mainstream parties experience a piton because they are stuck with a ritual. Moderate right voters see it as a snare. Far right parties see it as a rope as it supports their position. The analytical observer sees a piton showing the degraded state.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the relationship to the cordon sanitaire. Marginalized voters and moderate right voters are targets (high d), mainstream parties benefit (low d), far-right parties benefit (low d). The analytical observer's d is based on the analysis of the structural function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cordon_effectiveness,
    'To what extent does the cordon sanitaire still influence voter behavior?',
    'Analysis of voting patterns, surveys, and focus groups.',
    'If effective, the cordon sanitaire is a rope. If ineffective, it is a piton or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cordon_effectiveness, empirical, 'The effectiveness of the cordon sanitaire in influencing voter behavior.').

omega_variable(
    alternative_representation,
    'Are there alternative mechanisms to address the concerns of marginalized voters?',
    'Comparative analysis of different electoral systems and political representation models.',
    'If alternatives exist, the cordon sanitaire is less justified. If not, it may be a necessary evil.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_representation, conceptual, 'Alternative mechanisms to address the concerns of marginalized voters.').

omega_variable(
    far_right_legitimacy,
    'Does the cordon sanitaire inadvertently legitimize the far-right by portraying them as victims of political exclusion?',
    'Analysis of media coverage, public discourse, and the far-right''s own rhetoric.',
    'If it does, the cordon sanitaire is counterproductive. If not, it may still serve a purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(far_right_legitimacy, conceptual, 'The extent to which the cordon sanitaire legitimizes the far-right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(france_cordon_sanitaire_2026, 2000, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fran_tr_t0, france_cordon_sanitaire_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fran_tr_t13, france_cordon_sanitaire_2026, theater_ratio, 13, 0.5).
narrative_ontology:measurement(fran_tr_t26, france_cordon_sanitaire_2026, theater_ratio, 26, 0.8).

% Extraction over time
narrative_ontology:measurement(fran_be_t0, france_cordon_sanitaire_2026, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(fran_be_t13, france_cordon_sanitaire_2026, base_extractiveness, 13, 0.4).
narrative_ontology:measurement(fran_be_t26, france_cordon_sanitaire_2026, base_extractiveness, 26, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
