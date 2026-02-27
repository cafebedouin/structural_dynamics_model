% ============================================================================
% CONSTRAINT STORY: nfl_superbowl_marketing_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nfl_superbowl_marketing_regulation, []).

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
 *   constraint_id: nfl_superbowl_marketing_regulation
 *   human_readable: NFL Super Bowl Advertising Regulations
 *   domain: economic
 *
 * SUMMARY:
 *   The NFL exerts strong control over advertising and marketing during the
 *   Super Bowl, limiting competitors and extracting rent from advertisers who
 *   wish to associate with the event. This creates a complex ecosystem where
 *   the NFL and its broadcast partners benefit, while smaller advertisers and
 *   competing brands face significant challenges.
 *
 * KEY AGENTS:
 *   - NFL: Institutional beneficiary (institutional/arbitrage) - benefits from advertising revenue and control
 *   - Broadcast Networks: Institutional beneficiary (institutional/arbitrage) - benefits from selling advertising slots
 *   - Competing Brands: Primary victim (powerless/trapped) - faces barriers to entry
 *   - Smaller Advertisers: Secondary victim (moderate/constrained) - faces high costs and stringent regulations
 *   - Consumers: Moderate agent (moderate/constrained) - subjected to advertisements with some entertainment value
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nfl_superbowl_marketing_regulation, 0.65).
domain_priors:suppression_score(nfl_superbowl_marketing_regulation, 0.7).
domain_priors:theater_ratio(nfl_superbowl_marketing_regulation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, extractiveness, 0.65).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nfl_superbowl_marketing_regulation, tangled_rope).
narrative_ontology:human_readable(nfl_superbowl_marketing_regulation, "NFL Super Bowl Advertising Regulations").
narrative_ontology:topic_domain(nfl_superbowl_marketing_regulation, "economic").

domain_priors:requires_active_enforcement(nfl_superbowl_marketing_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, nfl).
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, broadcast_networks).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, competing_brands).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, smaller_advertisers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Competing brands face significant barriers to entry during the Super Bowl due to NFL regulations, limiting their ability to reach a large audience. They are essentially trapped within the NFL's marketing ecosystem.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Smaller advertisers may be able to participate in Super Bowl advertising but face constraints due to high costs and stringent regulations, leading to a mixed experience of coordination and extraction. They are constrained by budget and access but gain exposure.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% The NFL benefits from the advertising regulations, which create a controlled marketplace and generate revenue. They can arbitrage their position by controlling access and setting prices.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Broadcast networks benefit from the advertising regulations by selling advertising slots at premium prices. They can arbitrage their position by controlling access and setting prices.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the NFL's advertising regulations as a tangled rope, exhibiting both coordination (creating a valuable marketing opportunity) and extraction (limiting competition and increasing costs).
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Consumers are subjected to the advertising regulations because they are forced to view the commercials but they derive some entertainment value from the advertisements.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nfl_superbowl_marketing_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nfl_superbowl_marketing_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nfl_superbowl_marketing_regulation, TR),
    TR >= 0.70.

:- end_tests(nfl_superbowl_marketing_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The NFL extracts significant value from advertisers who wish to reach the large Super Bowl audience. Suppression (0.70): High. The NFL actively suppresses competition through exclusive agreements and strict regulations. Theater ratio (0.30): Low. The NFL’s regulations are primarily functional, aimed at maximizing revenue and controlling the event's marketing landscape. The ratio reflects that the regulations are not primarily performative but are designed to directly extract economic value.
 *
 * PERSPECTIVAL GAP:
 *   Competing brands see the regulations as a snare, limiting their ability to reach a large audience. Smaller advertisers experience a mix of coordination and extraction. The NFL and broadcast networks see the regulations as a rope, facilitating a controlled marketplace and generating revenue. An analytical observer sees the situation as a tangled rope, characterized by both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The NFL and broadcast networks benefit from the advertising regulations, which create a controlled marketplace and generate revenue. Competing brands and smaller advertisers bear the costs of the regulations, which limit their ability to reach a large audience and increase their advertising expenses.
 *
 * MANDATROPHY ANALYSIS:
 *   The NFL's Super Bowl advertising regulations could be misconstrued as pure coordination, as the regulations do allow for a structured marketing experience. However, they also exhibit significant extraction by limiting competition and increasing costs for some advertisers. Therefore, it is categorized as a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_alternatives,
    'To what extent do alternative advertising channels (e.g., social media, streaming services) provide viable alternatives to Super Bowl advertising?',
    'Analysis of advertising spending across different channels and their relative effectiveness in reaching target audiences.',
    'If alternatives are viable, the NFL''s extraction power is limited. If not, the NFL maintains a significant degree of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_alternatives, empirical, 'The availability and effectiveness of alternative advertising channels.').

omega_variable(
    regulatory_scrutiny,
    'Could the NFL''s advertising regulations be subject to antitrust scrutiny or other forms of regulatory oversight?',
    'Legal analysis of the regulations and their potential impact on competition.',
    'If the regulations are deemed anticompetitive, they could be challenged in court or by regulatory agencies, potentially leading to changes in the advertising landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_scrutiny, conceptual, 'The potential for regulatory scrutiny of the NFL''s advertising regulations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nfl_superbowl_marketing_regulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nfl__tr_t0, nfl_superbowl_marketing_regulation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nfl__tr_t10, nfl_superbowl_marketing_regulation, theater_ratio, 10, 0.3).
narrative_ontology:measurement(nfl__tr_t20, nfl_superbowl_marketing_regulation, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(nfl__be_t0, nfl_superbowl_marketing_regulation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(nfl__be_t10, nfl_superbowl_marketing_regulation, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(nfl__be_t20, nfl_superbowl_marketing_regulation, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nfl_superbowl_marketing_regulation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
