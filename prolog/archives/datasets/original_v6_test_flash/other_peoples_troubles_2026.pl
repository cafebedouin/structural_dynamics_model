% ============================================================================
% CONSTRAINT STORY: other_peoples_troubles_2026
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [DRAFT]
% ============================================================================

:- module(constraint_other_peoples_troubles_2026, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: other_peoples_troubles_2026
 *   human_readable: The Asymmetry of Vicarious Resilience
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint describes how psychological distance allows observers to
 *   more easily 'bear' the suffering of others, potentially leading to
 *   reduced empathy and action. While awareness can be beneficial, the
 *   asymmetry arises when the burden of suffering is disproportionately borne
 *   by those directly affected, while distant observers experience vicarious
 *   resilience with minimal personal cost. Media amplification of suffering
 *   can exacerbate this asymmetry.
 *
 * KEY AGENTS:
 *   - Suffering Individuals: Primary target (powerless/trapped) - bears direct burden of suffering.
 *   - Local Support Networks: Secondary target (moderate/constrained) - constrained by proximity, yet can exit via compassion fatigue.
 *   - Distant Observers: Primary beneficiary (institutional/arbitrage) - benefits from awareness, with easy exit.
 *   - Media Outlets: Secondary beneficiary (powerful/mobile) - benefits from increased audience, can quickly shift focus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(other_peoples_troubles_2026, 0.55).
domain_priors:suppression_score(other_peoples_troubles_2026, 0.6).
domain_priors:theater_ratio(other_peoples_troubles_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(other_peoples_troubles_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(other_peoples_troubles_2026, tangled_rope).
narrative_ontology:human_readable(other_peoples_troubles_2026, "The Asymmetry of Vicarious Resilience").
narrative_ontology:topic_domain(other_peoples_troubles_2026, "social/psychological").

domain_priors:requires_active_enforcement(other_peoples_troubles_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(other_peoples_troubles_2026, distant_observers).
narrative_ontology:constraint_beneficiary(other_peoples_troubles_2026, media_outlets).
narrative_ontology:constraint_victim(other_peoples_troubles_2026, suffering_individuals).
narrative_ontology:constraint_victim(other_peoples_troubles_2026, local_support_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Those directly experiencing suffering have no exit and bear the full cost of the constraint. They are trapped in their situation, with no ability to escape the negative impacts.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% These networks are constrained by proximity and moral obligation, but also benefit from the resilience that comes from helping. They experience a mixed extraction and benefit.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Distant observers benefit from a sense of moral superiority or feeling good about awareness, with minimal personal cost. They can easily switch their attention to other issues.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Media outlets benefit from increased viewership/readership, but also bear some cost from criticism of sensationalism. They are mobile, able to shift focus as needed.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees the mixed coordination/extraction of the vicarious resilience phenomenon, recognizing both the benefits and the harms.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(other_peoples_troubles_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(other_peoples_troubles_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(other_peoples_troubles_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Moderate extraction due to the moral hazard of distance. Suppression: 0.6 - Limited alternative actions for distant observers due to the nature of distance. Theater_ratio: 0.3 - Some performative action, but genuine action is limited. The victims are those directly suffering, with limited ability to escape the negative impacts of their situation.
 *
 * PERSPECTIVAL GAP:
 *   The suffering individual experiences the full weight of the constraint, while distant observers experience vicarious resilience, which has low personal cost. Media outlets benefit from capturing attention, potentially amplifying the original suffering for profit. Local support networks operate in a space between the two.
 *
 * DIRECTIONALITY LOGIC:
 *   Distant observers benefit from a sense of moral satisfaction with negligible impact, while the directly affected bear all costs. Media outlets leverage suffering for profit, creating an asymmetry. Local networks are constrained to help, but can experience compassion fatigue.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not pure exploitation. The mechanism can be a form of coordination because it alerts people to needs and creates a loose sense of connection, but it also permits moral distance and a lack of responsibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_hazard_threshold,
    'At what point does the psychological distance create a moral hazard, where observers feel absolved of responsibility?',
    'Survey data correlating psychological distance with willingness to provide tangible assistance.',
    'If the threshold is low, distant observers are a major source of extraction. If high, the effect is minimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_threshold, empirical, 'Threshold for moral hazard effect').

omega_variable(
    compassion_fatigue_impact,
    'How does compassion fatigue among local support networks impact their ability to provide effective assistance?',
    'Longitudinal study of support network efficacy as compassion fatigue increases.',
    'If high impact, then local networks are significantly extracted. If low, then the resilience effect is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compassion_fatigue_impact, empirical, 'Impact of compassion fatigue on local networks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(other_peoples_troubles_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(othe_tr_t0, other_peoples_troubles_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(othe_tr_t5, other_peoples_troubles_2026, theater_ratio, 5, 0.2).
narrative_ontology:measurement(othe_tr_t10, other_peoples_troubles_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(othe_be_t0, other_peoples_troubles_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(othe_be_t5, other_peoples_troubles_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(othe_be_t10, other_peoples_troubles_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(other_peoples_troubles_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
