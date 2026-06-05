% ============================================================================
% CONSTRAINT STORY: oscar_campaign_spending
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oscar_campaign_spending, []).

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
 *   constraint_id: oscar_campaign_spending
 *   human_readable: Oscar Campaign Spending Limits
 *   domain: social
 *
 * SUMMARY:
 *   The informal limit on Oscar campaign spending reflects a balance between
 *   promoting a film and avoiding the perception of excessive influence. This
 *   constraint highlights the tension between fair competition and the
 *   financial advantages of larger studios. Films with smaller marketing
 *   budgets are at a disadvantage, while campaign consultants and media
 *   outlets benefit from increased spending. Academy voters are caught in the
 *   middle, influenced by campaigns but also wary of manipulation.
 *
 * KEY AGENTS:
 *   - Films with Modest Marketing Budgets: Primary target (powerless/trapped) - unable to compete with large campaigns.
 *   - Campaign Consultants: Primary beneficiary (institutional/arbitrage) - revenue increases with increased spending.
 *   - Academy Voters: Influenced but also resentful (moderate/constrained) - some agency, some constraints.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oscar_campaign_spending, 0.55).
domain_priors:suppression_score(oscar_campaign_spending, 0.4).
domain_priors:theater_ratio(oscar_campaign_spending, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oscar_campaign_spending, extractiveness, 0.55).
narrative_ontology:constraint_metric(oscar_campaign_spending, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(oscar_campaign_spending, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oscar_campaign_spending, tangled_rope).
narrative_ontology:human_readable(oscar_campaign_spending, "Oscar Campaign Spending Limits").
narrative_ontology:topic_domain(oscar_campaign_spending, "social").

domain_priors:requires_active_enforcement(oscar_campaign_spending).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oscar_campaign_spending, campaign_consultants).
narrative_ontology:constraint_beneficiary(oscar_campaign_spending, media_outlets).
narrative_ontology:constraint_victim(oscar_campaign_spending, films_with_modest_marketing_budgets).
narrative_ontology:constraint_victim(oscar_campaign_spending, academy_voter_perception).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Films with limited resources are trapped in a system where spending heavily influences visibility and perceived quality. They cannot compete with larger campaigns, effectively suppressing their chances regardless of artistic merit.
constraint_indexing:constraint_classification(oscar_campaign_spending, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Campaign consultants benefit directly from increased spending, as it generates revenue for their services. They see the spending as a coordination mechanism, facilitating the promotion of their client's film.
constraint_indexing:constraint_classification(oscar_campaign_spending, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Academy voters are targeted by campaigns and can be influenced by spending, but they also resent excessive displays of wealth and perceived manipulation. They are constrained by the information available and the pressure from campaigns, but they retain some agency in their voting decisions.
constraint_indexing:constraint_classification(oscar_campaign_spending, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From a broader perspective, excessive spending creates a tangled rope dynamic. It facilitates film promotion but also introduces inequities and potentially degrades the integrity of the awards process. There are coordination and extraction components.
constraint_indexing:constraint_classification(oscar_campaign_spending, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oscar_campaign_spending_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(oscar_campaign_spending, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(oscar_campaign_spending_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is estimated at 0.55 because significant resources are diverted to campaign spending, potentially at the expense of the creative process. Suppression is 0.40, as excessive spending can drown out films with more modest budgets and less aggressive campaigns. The theater ratio is 0.30, representing the portion of campaign activities that don't directly enhance the film's artistic value.
 *
 * PERSPECTIVAL GAP:
 *   Films with smaller budgets experience the constraint as a snare, as they are unable to compete effectively. Campaign consultants view it as a rope, enabling them to promote their clients' films. Academy voters see it as a tangled rope, both influenced by campaigns and wary of excessive displays of influence. The analytical observer captures a broader perspective of imbalance that impacts the awards process and perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Campaign consultants benefit, driving directionality towards 0. Films with limited marketing budgets are victims, driving directionality towards 1. Academy voters are in the middle, with moderate directionality. This dynamic creates the tangled rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impact_of_spending_on_voter_perception,
    'To what extent does spending directly influence voter perception versus simply increasing awareness?',
    'Analyze correlation between spending levels, critic reviews, and voter surveys.',
    'High influence: Snare is a more accurate depiction. Low influence: Rope is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_spending_on_voter_perception, empirical, 'The direct impact of campaign spending on voter perceptions of a film''s quality.').

omega_variable(
    definition_of_excessive_spending,
    'What constitutes ''excessive'' spending that triggers backlash?',
    'Track public sentiment and media coverage in response to campaigns.',
    'Clear threshold: Enforcement mechanisms are more effective. Vague threshold: The constraint relies more on social norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_excessive_spending, conceptual, 'Definition of ''excessive'' spending and what actions it includes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oscar_campaign_spending, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(osca_tr_t0, oscar_campaign_spending, theater_ratio, 0, 0.2).
narrative_ontology:measurement(osca_tr_t5, oscar_campaign_spending, theater_ratio, 5, 0.3).
narrative_ontology:measurement(osca_tr_t10, oscar_campaign_spending, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(osca_be_t0, oscar_campaign_spending, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(osca_be_t5, oscar_campaign_spending, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(osca_be_t10, oscar_campaign_spending, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oscar_campaign_spending, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
