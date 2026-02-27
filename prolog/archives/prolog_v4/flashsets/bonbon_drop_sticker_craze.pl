% ============================================================================
% CONSTRAINT STORY: bonbon_drop_sticker_craze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bonbon_drop_sticker_craze, []).

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
 *   constraint_id: bonbon_drop_sticker_craze
 *   human_readable: UHA Mikakuto's 'Bonbon Drop' Collectible Sticker Promotion
 *   domain: economic
 *
 * SUMMARY:
 *   UHA Mikakuto's "Bonbon Drop" candy packages include collectible stickers,
 *   creating a craze among children and potential pressure for parents. This
 *   creates a feedback loop between extraction and coordination: the more
 *   children collect, the more valuable the missing pieces become, which
 *   increases the willingness to pay for the candy. The base extractiveness
 *   reflects a combined emotional value (child appeal) and a structural value
 *   (network effect).
 *
 * KEY AGENTS:
 *   - UHA Mikakuto Company: Institutional Beneficiary (institutional/arbitrage) - Benefits financially from the increased sales and brand loyalty
 *   - Sticker Designers: Beneficiary (powerful/arbitrage)
 *   - Children Collectors: Primary Victim (powerless/trapped) - Trapped by the desire to complete the set, leading to more purchases of the candy
 *   - Parents of Collectors: Secondary Victim (moderate/constrained) - Constrained by their child's desires but also have opportunities to limit the candy purchases.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bonbon_drop_sticker_craze, 0.55).
domain_priors:suppression_score(bonbon_drop_sticker_craze, 0.7).
domain_priors:theater_ratio(bonbon_drop_sticker_craze, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, extractiveness, 0.55).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bonbon_drop_sticker_craze, tangled_rope).
narrative_ontology:human_readable(bonbon_drop_sticker_craze, "UHA Mikakuto's 'Bonbon Drop' Collectible Sticker Promotion").
narrative_ontology:topic_domain(bonbon_drop_sticker_craze, "economic").

domain_priors:requires_active_enforcement(bonbon_drop_sticker_craze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, uha_mikakuto_company).
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, sticker_designers).
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, children_collectors).
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, parents_of_collectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The child collector is trapped in the desire to complete the set, leading to excessive consumption and potential financial strain. Limited exit options due to the emotional investment in collecting.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Parents are constrained by their child's desires and the social pressure of providing these candies, but also benefit from their child's happiness. Some have exit options, such as limiting candy purchases or finding alternative treats.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% The company benefits from increased sales and brand loyalty due to the sticker promotion. They have arbitrage opportunities by adjusting the rarity of stickers and marketing strategies.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The sticker designers benefit from a contract with UHA Mikakuto
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The craze exhibits a mixed coordination and extraction - children coordinate to trade stickers which benefits the candy company, at the expense of the children and their parents.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bonbon_drop_sticker_craze_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bonbon_drop_sticker_craze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bonbon_drop_sticker_craze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) score reflects how the sticker promotion leads to excessive consumption of candy by children and financial costs by parents. Suppression (0.70) indicates the difficulty for children and parents to resist the urge to continue purchasing the candy to complete the sticker set. The theater_ratio (0.30) is low because the value is not primarily for show, but about an emotional bond with characters, and a compulsion to 'catch them all'.
 *
 * PERSPECTIVAL GAP:
 *   The classification varies based on the stakeholder. UHA Mikakuto perceives the promotion as a successful campaign (Rope), sticker designers as beneficial (Rope), while children become victims of the addiction to completing the sticker collection (Snare). Parents are in a state of constrained support, providing the candy for their children to collect the sticker (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Children collectors are trapped because it is emotionally hard to stop buying candy once they started collecting. Parents are constrained, since stopping the candy purchase may have social repercussions for the child. UHA Mikakuto is a beneficiary as it can arbitrage the rarity of the stickers. Sticker designers also benefit from their connection with UHA Mikakuto. As a result the chi value will reflect the level of experienced extraction for different agents.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emotional_value_vs_nutritional_value,
    'To what extent does the emotional value of collecting stickers outweigh the limited nutritional value of the candy?',
    'Studies on child psychology and consumer behavior, surveys on parental attitudes towards candy consumption.',
    'If emotional value is high: justifies high extractiveness rating. If nutritional value is considered: might raise the extractiveness even higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_value_vs_nutritional_value, empirical, 'Evaluate the ratio between the emotional value of collecting and the nutritional value of the candy.').

omega_variable(
    rarity_effect_on_consumption,
    'How does the artificially induced rarity of certain stickers influence consumption patterns and the overall success of the promotion?',
    'Market analysis, sales data, consumer surveys analyzing the effect of rarity in consumption patterns.',
    'If rarity drives sales considerably: shows greater extraction strength. If demand is independent of rarity, then the constraint is mostly a rope',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rarity_effect_on_consumption, empirical, 'Analyze the role of rarity in increasing sales for the candy company.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bonbon_drop_sticker_craze, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bonb_tr_t0, bonbon_drop_sticker_craze, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bonb_tr_t3, bonbon_drop_sticker_craze, theater_ratio, 3, 0.25).
narrative_ontology:measurement(bonb_tr_t6, bonbon_drop_sticker_craze, theater_ratio, 6, 0.3).

% Extraction over time
narrative_ontology:measurement(bonb_be_t0, bonbon_drop_sticker_craze, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bonb_be_t3, bonbon_drop_sticker_craze, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(bonb_be_t6, bonbon_drop_sticker_craze, base_extractiveness, 6, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bonbon_drop_sticker_craze, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
