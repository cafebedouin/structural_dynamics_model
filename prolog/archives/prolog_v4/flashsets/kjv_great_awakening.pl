% ============================================================================
% CONSTRAINT STORY: kjv_great_awakening
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_great_awakening, []).

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
 *   constraint_id: kjv_great_awakening
 *   human_readable: The Great Awakening's Reframing of Biblical Authority
 *   domain: religious/social
 *
 * SUMMARY:
 *   The First Great Awakening (c. 1730s-1740s) was a series of religious
 *   revivals that swept through the British colonies in North America. A key
 *   aspect of this movement was a reframing of biblical authority,
 *   emphasizing personal experience and emotional engagement over traditional
 *   interpretations and hierarchical structures. This led to the rise of
 *   itinerant preachers, the formation of new denominations, and a challenge
 *   to the established clergy and parishes.
 *
 * KEY AGENTS:
 *   - Itinerant Preachers: Beneficiaries of the reframing (institutional/arbitrage)
 *   - New Denominations: Beneficiaries, gained influence and membership (institutional/arbitrage)
 *   - Established Clergy: Victims, authority undermined (powerless/trapped)
 *   - Traditional Parishes: Victims, faced challenges to their membership and resources (powerless/trapped)
 *   - Parishioners: Both beneficiaries and victims (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_great_awakening, 0.55).
domain_priors:suppression_score(kjv_great_awakening, 0.45).
domain_priors:theater_ratio(kjv_great_awakening, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_great_awakening, extractiveness, 0.55).
narrative_ontology:constraint_metric(kjv_great_awakening, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(kjv_great_awakening, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_great_awakening, tangled_rope).
narrative_ontology:human_readable(kjv_great_awakening, "The Great Awakening's Reframing of Biblical Authority").
narrative_ontology:topic_domain(kjv_great_awakening, "religious/social").

domain_priors:requires_active_enforcement(kjv_great_awakening).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_great_awakening, itinerant_preachers).
narrative_ontology:constraint_beneficiary(kjv_great_awakening, new_denominations).
narrative_ontology:constraint_victim(kjv_great_awakening, established_clergy).
narrative_ontology:constraint_victim(kjv_great_awakening, traditional_parishes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Established clergy in traditional parishes experienced the Awakening as a snare. They were trapped within existing power structures and saw their authority undermined by itinerant preachers and new interpretations.
constraint_indexing:constraint_classification(kjv_great_awakening, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Itinerant preachers benefitted from the reframing of biblical authority, gaining influence and followers. They could arbitrage between different communities and denominations, leveraging their interpretations.
constraint_indexing:constraint_classification(kjv_great_awakening, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Parishioners experienced the Awakening as a tangled rope. They were both drawn to the emotional preaching and new interpretations, while also constrained by social expectations and established traditions. Their exit options were limited by community ties.
constraint_indexing:constraint_classification(kjv_great_awakening, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% From a civilizational perspective, the Awakening represents a tangled rope. It spurred religious fervor and social change, but also led to divisions and disruptions. The long-term effects are a mix of coordination and extraction.
constraint_indexing:constraint_classification(kjv_great_awakening, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_great_awakening_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_great_awakening, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_great_awakening, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_great_awakening, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_great_awakening_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.55) because the reframing of authority extracted power and influence from established clergy and traditional parishes, but also provided new avenues for religious expression and community for many colonists. The suppression is also moderate (0.45) because the established religious authorities attempted to suppress the new religious movements, but the revivals gained significant momentum and popular support.
 *
 * PERSPECTIVAL GAP:
 *   The established clergy viewed the Awakening as a snare because it undermined their authority and disrupted traditional parish structures. Itinerant preachers and new denominations viewed it as a rope because it provided them with new opportunities for influence and growth. Parishioners experienced the Awakening as a tangled rope, drawn to the emotional preaching but constrained by existing social and religious norms.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by who benefitted and who bore the costs of the reframing of biblical authority. Itinerant preachers and new denominations benefitted through the ability to arbitrage different audiences. Established clergy bore the costs as it undermined their authority. Parishioners were constrained in their ability to exit traditional structures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emotional_intensity_vs_lasting_impact,
    'To what extent was the Great Awakening''s impact due to genuine spiritual renewal versus emotional manipulation?',
    'Analyze sermons and writings for emotional appeals vs. reasoned arguments; compare long-term religious participation rates.',
    'If emotional manipulation was primary: the ''awakening'' is a snare for followers. If genuine renewal was primary: it''s a rope or scaffold for religious communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_intensity_vs_lasting_impact, empirical, 'Determine the extent to which emotional intensity or lasting impact are observed').

omega_variable(
    long_term_denominational_stability,
    'Did new denominations arising from the Great Awakening maintain long-term stability or fragment into further divisions?',
    'Track denominational splits and mergers over time; assess theological coherence.',
    'If stable: the Awakening led to positive coordination. If fragmented: it exacerbated existing religious tensions and extraction within religious communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_denominational_stability, empirical, 'Determine the effect of long-term denominational stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_great_awakening, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_great_awakening, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kjv__tr_t10, kjv_great_awakening, theater_ratio, 10, 0.3).
narrative_ontology:measurement(kjv__tr_t20, kjv_great_awakening, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_great_awakening, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(kjv__be_t10, kjv_great_awakening, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(kjv__be_t20, kjv_great_awakening, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_great_awakening, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
