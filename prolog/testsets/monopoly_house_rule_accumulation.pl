% ============================================================================
% CONSTRAINT STORY: monopoly_house_rule_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_house_rule_accumulation, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: monopoly_house_rule_accumulation
 *   human_readable: Monopoly House Rule Accumulation and Game Dynamics Distortion
 *   domain: social/recreational_games
 *
 * SUMMARY:
 *   Monopoly house rule accumulation creates a structural constraint on
 *   gameplay equity and accessibility that exhibits classic tangled-rope
 *   dynamics: genuine coordination functions (shared rule understanding,
 *   accelerated gameplay setup) coexist with asymmetric extraction
 *   (accumulated rules favor experienced players, exclude newcomers, lock in
 *   early advantage). The constraint emerges through incremental rule
 *   additions over time, with each addition justified as clarification or
 *   enhancement. The accumulated effect is a rule corpus that bears little
 *   resemblance to published rules, functions as an in-group barrier, and
 *   systematically disadvantages players unfamiliar with the group's specific
 *   history. Theater ratio rises significantly over time as groups spend
 *   increasing effort enforcing consistency with prior games rather than
 *   playing the game itself. The constraint demonstrates how coordination
 *   mechanisms can degrade into pure extraction through institutional drift.
 *
 * KEY AGENTS:
 *   - New Players: Primary victims (powerless/trapped) — face accumulated rules with no prior context; cannot exit without social cost
 *   - Experienced Players: Primary beneficiaries (organized/arbitrage) — benefit from early advantage locks; can navigate rule complexity without cognitive load
 *   - Rule Innovators: Secondary beneficiaries (moderate/constrained) — propose new rules; often benefit from the rule they introduce even if subsequent players do not
 *   - Casual/Occasional Players: Mixed position (moderate/constrained) — participate regularly enough to enforce rules but not frequently enough to optimize play; experience both coordination benefit and extraction cost
 *   - Tournament Organizers: Exit architects (organized/mobile) — create standardized rule venues; demonstrate that rule standardization does not eliminate coordination
 *   - Game Balance System: Primary victim (powerless/trapped) — abstract property bearing extractive load; the game's designed dynamics are progressively obscured by accumulated modifications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_house_rule_accumulation, 0.52).
domain_priors:suppression_score(monopoly_house_rule_accumulation, 0.58).
domain_priors:theater_ratio(monopoly_house_rule_accumulation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_house_rule_accumulation, extractiveness, 0.52).
narrative_ontology:constraint_metric(monopoly_house_rule_accumulation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(monopoly_house_rule_accumulation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_house_rule_accumulation, tangled_rope).
narrative_ontology:human_readable(monopoly_house_rule_accumulation, "Monopoly House Rule Accumulation and Game Dynamics Distortion").
narrative_ontology:topic_domain(monopoly_house_rule_accumulation, "social/recreational_games").

domain_priors:requires_active_enforcement(monopoly_house_rule_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_house_rule_accumulation, rule_innovators).
narrative_ontology:constraint_beneficiary(monopoly_house_rule_accumulation, experienced_players).
narrative_ontology:constraint_victim(monopoly_house_rule_accumulation, new_players).
narrative_ontology:constraint_victim(monopoly_house_rule_accumulation, game_balance).
narrative_ontology:constraint_victim(monopoly_house_rule_accumulation, universal_gameplay).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW PLAYER (SNARE) — Enters the game with standard published rules but faces accumulated house rules enforced by experienced players. Cannot exit without social cost (refusing to play, being labeled a poor sport). Suppression is high: deviation from house rules faces immediate correction and social pressure. No coordination benefit perceived — the new player sees only constraint extraction.
constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: OCCASIONAL PLAYER (TANGLED ROPE) — Participates irregularly enough to question some house rules but committed enough to the social group to enforce them. Constrained by desire to maintain group cohesion. Experiences genuine coordination (shared understanding accelerates play) alongside extraction (house rules often favor established players). Mixed experience — benefits from rule clarity when playing with the group but bears cost when rules disadvantage them.
constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: TOURNAMENT ORGANIZER (ROPE) — Standardized tournaments use only published rules, explicitly rejecting house rules. Sees house rule accumulation as a coordination problem that threatens inter-group comparability. Can exit the unregulated play context and create new venues with standard rules. Organized agents can collectively enforce rule standardization. Classification reflects genuine coordination function without asymmetric extraction.
constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: RULE STANDARDIZER (SCAFFOLD) — Individual player introduces a written house rule sheet at game start with sunset clause: 'We'll use these house rules for tonight; next week we can revise or return to published rules.' Temporary enforcement framework. Low theater because the rule constraints are explicit and revisable. Suppression is moderate because the sunset is agreed-upon in advance. Coordination benefit is clear (everyone knows what rules apply). Sunset clause is the diagnostic feature.
constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: FAMILY GAME NIGHT TRADITION (PITON) — Multi-generational family game night has accumulated layers of house rules ('house money doesn't earn interest', 'Free Parking is a card draw', 'you can trade properties mid-mortgage'). Original function (family bonding and skill development) is now subordinated to enforcing the rule tradition itself. Theater ratio is high: much of the interaction is maintaining consistency with past games rather than optimizing gameplay. Rules persist through institutional inertia — family members enforce them because 'that's how we've always played,' not because the rules serve a current coordination need. Piton classification derives from theater_ratio ≥ 0.70 despite low extractiveness.
constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GAME MECHANICS VIEW (MOUNTAIN) — From a civilizational perspective analyzing game design principles, Monopoly's extraction dynamics are inherent to the ruleset itself: the game is explicitly designed for early leader advantage (rapid capital accumulation) and late-game elimination (players dropping out). This is not a house rule accumulation problem but a fundamental property of the game system. The constraint is unchangeable without redesigning the game entirely. However, this perspective risks naturalizing what is actually a contingent design choice — the engine will flag this as a false summit.
constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_house_rule_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_house_rule_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_house_rule_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monopoly_house_rule_accumulation, TR),
    TR >= 0.70.

:- end_tests(monopoly_house_rule_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The house rules systematically favor players with prior game experience. New players begin from disadvantage through rule unfamiliarity alone; the asymmetric information creates effective extraction that persists across games until the new player internalizes the group's specific rules. The extractiveness is not as high as a pure snare (0.66+) because rules are eventually learnable and some rules may genuinely improve gameplay clarity. Suppression (0.58): Moderate-high. Barriers to exit include social pressure (refusing to play is seen as obstruction), normalization of the rules (they are presented as 'how we play' rather than 'a version of Monopoly'), and the cognitive burden of simultaneously learning published rules and the group's modifications. Newcomers cannot easily distinguish core gameplay from local variants. Theater ratio (0.68): High. Significant portions of game interaction are devoted to rule clarification, dispute resolution, and consistency maintenance rather than strategic play. Experienced players spend cognitive effort tracking rule state; new players spend cognitive effort learning rules rather than playing strategically. As rule complexity increases, theater ratio rises because the overhead of tracking divergence from standard rules increases.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is substantial. Experienced players see coordination (Rope) — they perceive house rules as clarifications that make games run smoothly. New players see extraction (Snare) — they perceive rules as barriers. Occasional players see mixed dynamics (Tangled Rope) — they experience both coordination (rule stability across sessions) and extraction (rules sometimes disadvantage them). The tournament organizer sees the accumulation as a coordination failure solvable by standardization (Rope or Scaffold). The family tradition sees the rules as identity markers (Piton) — maintaining consistency with past games is more important than optimizing gameplay. The analytical observer risks seeing immutable game design (Mountain) but this false summit misses the contingent human choices that produced the rule divergence. The gap reveals that what appears as coordination from one position appears as exclusion from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (experienced players, rule innovators) derive clear benefit from house rule accumulation: they play from a position of advantage and relative information symmetry. Their d value is low (~0.15-0.25) reflecting beneficiary status and arbitrage exit options (they can always play under standard rules elsewhere or create new groups). Victims (new players, game balance) derive extraction: they bear the cost of asymmetric information and locked-in disadvantage. Their d value is high (~0.85-0.95) reflecting trapped exit options and no ability to reverse accumulated rules mid-session. Occasional players occupy the middle: they enforce rules (beneficiary behavior, low d) but are also subject to them (victim behavior, high d). The Tangled Rope classification emerges from the coexistence of genuine coordination (rules being known collectively accelerates setup and play) with asymmetric extraction (the rules chosen favor established players). The engine derives d from beneficiary/victim declarations: experienced players as beneficiaries with arbitrage exit produce negative χ (they benefit); new players as victims with trapped exit produce high χ (they extract costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that house rule accumulation contains a genuine coordination function (players knowing the rules in advance accelerates play, reduces mid-game disputes) which is authentic and valuable. However, the rule accumulation mechanism systematically biases the rule corpus toward complexity and toward rules that favor early/experienced players. The constraint avoids mislabeling by preserving both dimensions: it is Tangled Rope not Snare because coordination genuinely occurs; it is not pure Rope because the extraction asymmetry is structural and systematic. The theater ratio rising over time (0.35 → 0.68) indicates that the coordination function is degrading — as rules accumulate, groups spend more effort maintaining rule consistency and less effort playing strategically. This lifecycle suggests the constraint could be addressed through periodic rule resets (Scaffold logic) or explicit rule standardization (returning to published rules as the coordinating mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_accidental_accumulation,
    'Are house rules accumulated deliberately to create strategic advantage or incidentally through tolerance of player suggestions?',
    'Historical analysis of rule proposal origins; tracking which player groups introduced which rules; comparison of rule benefit distribution to original proposers vs established players',
    'If intentional: constraint is Snare (deliberate extraction mechanism). If accidental: constraint is Piton (degradation through drift). Different mechanisms require different interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_accidental_accumulation, empirical, 'Whether house rule accumulation is deliberate strategy or institutional drift').

omega_variable(
    coordination_benefit_reality,
    'Do accumulated house rules actually improve gameplay coordination and enjoyment or do they primarily serve to exclude new players and lock in established player advantages?',
    'Survey of new vs experienced players on rule comprehension and gameplay satisfaction; analysis of game duration and elimination timing under house rules vs standard rules; measurement of new player retention across sessions',
    'If genuine coordination: classification should weight Rope more heavily. If primarily exclusionary: classification should weight Snare more heavily. Determines whether the constraint is justified or exploitative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_benefit_reality, empirical, 'Whether house rules provide genuine coordination benefits or serve primarily as exclusion mechanism').

omega_variable(
    published_rule_accessibility,
    'Do new players have easy access to and understanding of published Monopoly rules before joining, and is this information offered proactively by experienced players?',
    'Observation of rule explanation protocols; tracking whether published rules are referenced or mentioned; measurement of time and complexity difference between explaining published rules vs accumulated house rules',
    'If published rules are accessible and explained: suppression metric should be lower (players could theoretically reference standard rules). If deliberately obscured: suppression metric should be higher (players are systematically denied the baseline). Affects the scale of the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(published_rule_accessibility, empirical, 'Accessibility and proactive disclosure of published rules to new players').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_house_rule_accumulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_house_rule_accumulation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mono_tr_t3, monopoly_house_rule_accumulation, theater_ratio, 3, 0.52).
narrative_ontology:measurement(mono_tr_t6, monopoly_house_rule_accumulation, theater_ratio, 6, 0.62).
narrative_ontology:measurement(mono_tr_t10, monopoly_house_rule_accumulation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_house_rule_accumulation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mono_be_t3, monopoly_house_rule_accumulation, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(mono_be_t6, monopoly_house_rule_accumulation, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(mono_be_t10, monopoly_house_rule_accumulation, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_house_rule_accumulation, information_standard).
narrative_ontology:affects_constraint(monopoly_house_rule_accumulation, game_mechanic_power_scaling).
narrative_ontology:affects_constraint(monopoly_house_rule_accumulation, social_norm_enforcement_dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
