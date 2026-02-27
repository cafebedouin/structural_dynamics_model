% ============================================================================
% CONSTRAINT STORY: monopoly_fp_house_rule
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_fp_house_rule, []).

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
 *   constraint_id: monopoly_fp_house_rule
 *   human_readable: Monopoly 'Free Parking' House Rule
 *   domain: social/game_mechanics
 *
 * SUMMARY:
 *   The Free Parking house rule in Monopoly exemplifies how a
 *   well-intentioned modification to game mechanics creates a snare by
 *   masking extraction as luck-based relief. The official rules deliberately
 *   create tension through property monopolization and tax collection;
 *   players who land on properties they cannot afford face bankruptcy and
 *   elimination. The Free Parking rule adds a cash pool at the board's center
 *   that any player can access by landing on the Free Parking space. This
 *   modification has multiple structural effects: it extends game duration
 *   (preventing early eliminations), it introduces unpredictable wealth
 *   redistribution (creating narrative drama), but critically, it shifts
 *   incentive structures in ways that advantage early accumulators and lock
 *   in systematic disadvantage for those who fall behind early. The rule's
 *   attraction lies partly in its performative fairness — it appears to help
 *   struggling players — while its actual function is to redistribute
 *   unpredictably while keeping the game in the hands of those who
 *   established early leads. The constraint is a snare because escape
 *   requires coordinated group decision-making (switching to official rules)
 *   that carries social friction, and because the rule extracts strategic
 *   depth and game-ending conditions from the original design while
 *   maintaining the appearance of friendly accommodation.
 *
 * KEY AGENTS:
 *   - Systematically Impoverished Player: Primary victim (powerless/trapped) — caught in cascading financial collapse with only unpredictable Free Parking relief
 *   - Lucky Early Accumulator: Secondary beneficiary (moderate/constrained) — captures wealth concentration; exit constrained by need to protect advantage
 *   - Gaming Community (House Rule Defenders): Organized beneficiary (organized/mobile) — sees genuine coordination benefits (engagement, extended play, narrative drama) but exhibits extraction (disadvantages weaker players)
 *   - Casual Host: Beneficiary (institutional/arbitrage) — adult social coordinator who adopts rule for group enjoyment and engagement
 *   - Original Game Design (Hasbro/Rules as Published): Institutional observer (institutional/arbitrage) — sees house rule as degradation of design intent (piton perspective)
 *   - Analytical Observer: Civilizational analytical view (analytical/analytical) — risks naturalizing contingent rule choice as inevitable game property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_fp_house_rule, 0.52).
domain_priors:suppression_score(monopoly_fp_house_rule, 0.68).
domain_priors:theater_ratio(monopoly_fp_house_rule, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_fp_house_rule, extractiveness, 0.52).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_fp_house_rule, snare).
narrative_ontology:human_readable(monopoly_fp_house_rule, "Monopoly 'Free Parking' House Rule").
narrative_ontology:topic_domain(monopoly_fp_house_rule, "social/game_mechanics").

% --- Structural relationships ---
narrative_ontology:constraint_victim(monopoly_fp_house_rule, systematic_loser).
narrative_ontology:constraint_victim(monopoly_fp_house_rule, game_duration_extension).
narrative_ontology:constraint_victim(monopoly_fp_house_rule, original_game_design).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEMATICALLY IMPOVERISHED PLAYER (SNARE) — Caught in cascading financial collapse. Early bad luck (landing on expensive properties, paying taxes) depletes cash reserves. Free Parking rule provides unpredictable, insufficient relief. Cannot exit the game without social friction or abandonment. High suppression (no alternative income sources in-game). d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LUCKY EARLY ACCUMULATOR (SNARE) — Benefits from early property acquisition and tax collection from opponents. Free Parking rule provides surprise windfalls that accelerate wealth concentration. Their exit option is constrained (leaving forfeits advantage) but superior to the impoverished player. d≈0.35, f(d)≈0.28, σ=0.8 → χ≈0.12. Net positive but still experiences the constraint's structural coercion (must stay to protect position).
constraint_indexing:constraint_classification(monopoly_fp_house_rule, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: GAMING COMMUNITY — HOUSE RULE DEFENDERS (TANGLED ROPE) — Organized players who adopt and propagate Free Parking rule see genuine coordination benefits: the rule makes game outcomes less deterministic, extends game length, provides dramatic swings that increase engagement and narrative tension. Also exhibits extraction: newer or weaker players are systematically disadvantaged. Mobile exit (switch rules, play original rules, play other games) available but involves social coordination cost. d≈0.48, f(d)≈0.60, σ=0.8 → χ≈0.31.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: CASUAL HOST (ROPE) — Adult organizing family game night adopts Free Parking rule to make game more fun and exciting for children. Sees it as pure coordination: keeps underperforming players engaged, extends social time, creates narrative interest. No extraction perceived — it's about managing group enjoyment. d≈0.08, f(d)≈-0.08, σ=0.8 → χ≈-0.04. Net negative extraction = beneficiary.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: ORIGINAL GAME DESIGN (PITON) — Hasbro's official rules deliberately exclude Free Parking income to create strategic tension and game-ending conditions. Free Parking house rule is a degradation of the original design intent. Theater ratio (0.65) reflects the rule's performative nature: Free Parking appears to be a 'fair' neutral space but actually functions as wealth redistribution. The design sees itself as corrupted through inertia (players don't read official rules, host variations persist). d≈0.10, f(d)≈-0.01, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — FALSE MOUNTAIN VIEW — From a civilizational perspective, one might argue that wealth concentration and player elimination are 'inevitable' in any competitive resource game, and house rules are 'natural' adaptations. However, base extractiveness (0.52) and suppression (0.68) contradict mountain classification. The constraint is contingent on rule choice, not inevitable law. Engine detects false summit: the 'inevitability' narrative naturalizes what are actually designer choices.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_fp_house_rule_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_fp_house_rule, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monopoly_fp_house_rule, TR),
    TR >= 0.70.

:- end_tests(monopoly_fp_house_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Free Parking rule increases extractiveness compared to official rules by shifting the game from deterministic (early advantage = inevitable win) to luck-dependent (early advantage + favorable die rolls = continued advantage, but disrupted by random windfalls). This appears to reduce extractiveness (more unpredictability), but actually increases it because the unpredictability is not uniform: players with cash reserves benefit more from Free Parking windfalls (they have capital to reinvest the windfall); players with depleted reserves benefit less (they may go bankrupt before landing on Free Parking again). The extraction is the *asymmetric relief effect*. Suppression (0.68): High. Once a player is significantly behind, alternatives are few: (a) continue playing in a losing position, (b) propose switching to official rules (social friction), (c) abandon the game (social friction and sore-loser perception). The rule structure suppresses all three alternatives. Theater ratio (0.65): Moderate-high. Free Parking appears to be a 'lucky break' or 'fair neutral space' but functions as selective wealth redistribution. The performative narrative is 'this helps everyone equally'; the actual function is 'this helps cash-rich players disproportionately.' Theater increased from 0.55 to 0.65 over the measurement interval as awareness of the rule's effect spread among game communities — players now explicitly invoke it as a 'fun house rule' (theater label) while acknowledging its luck-based nature (cognitive dissonance).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the casual host's rope perspective and the systematically impoverished player's snare perspective is the core diagnostic. The host genuinely experiences the rule as coordination: keeping less skilled players engaged, extending social time, creating narrative interest. The impoverished player genuinely experiences it as extraction: unpredictable relief that may arrive too late, reinforcing early advantage, requiring them to stay in a losing game. Neither perspective is false — they measure different structural positions relative to the constraint. The piton perspective (original design) sees the rule as degradation of elegant design (bankruptcy as a game-ending condition is an intentional design feature, not a bug). The analytics view risks naturalizing the rule choice as inevitable 'game nature' when it is actually a contingent modification made by specific communities. The gaming community's organized perspective reveals tangled rope structure: the rule does provide coordination benefits (extended engagement) but also exhibits extraction (systematic disadvantage for weaker players, delayed eliminations that extend their suffering).
 *
 * DIRECTIONALITY LOGIC:
 *   Systematically impoverished player: Victim + trapped → d≈0.92, f(d)≈1.40. Nearly maximum extraction; exit requires game abandonment (high social cost). Lucky early accumulator: Beneficiary + constrained → d≈0.35, f(d)≈0.28. Moderate extraction because their exit is constrained (must stay to protect advantage) but their position is superior. Gaming community: Mixed (organized + mobile) → d≈0.48, f(d)≈0.60. Tangled rope requires both coordination function (engagement, extended play) and extraction (systematic disadvantage). Mobile exit (switch rules) is available but requires group coordination. Casual host: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; full control over rule adoption. Original design: Institutional beneficiary of baseline (no rule) → d≈0.10, f(d)≈-0.01. The piton classification comes from theater_ratio ≥ 0.70 (barely below at 0.65), indicating institutional recognition that the rule performs a function it was not designed for.
 *
 * MANDATROPHY ANALYSIS:
 *   The Free Parking house rule resolves the mandatrophy by showing how a rule that appears to address a coordination problem (players getting eliminated) actually functions as an extraction mechanism (asymmetric relief that preserves early advantage while extending suffering). The rule is neither 'pure coordination' (Rope) nor 'pure extraction' (Snare) from every perspective: the casual host genuinely sees rope (group engagement), the gaming community sees tangled rope (coordination + extraction), the impoverished player sees snare (extraction + suppression). The mandatrophy is resolved by recognizing that these are not contradictory classifications — they measure different structural positions. The rule persists because it solves a real problem (player elimination reduces engagement) while creating a hidden problem (systematic disadvantage becomes less visible through luck-based relief). The piton perspective (original design) clarifies that the rule is degraded design — it replaces the elegant bankruptcy-as-ending with an ambiguous luck-based mess — but persists through inertia because players prefer to avoid eliminating friends (social friction mitigation). The theater_ratio (0.65) captures the rule's performative nature: it performs fairness while functioning as selective advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cash_injection_sufficiency,
    'Is the Free Parking cash injection sufficient to prevent cascading player elimination, or merely to delay it?',
    'Statistical analysis of game duration and player elimination rate with vs without Free Parking rule across game parameter spaces (number of players, starting cash, property distribution); survival curve analysis',
    'If sufficient: rule solves a real coordination problem (Rope from community perspective). If merely delays: rule is extraction mechanism masquerading as help (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cash_injection_sufficiency, empirical, 'Whether Free Parking provides sufficient economic relief or merely delays elimination').

omega_variable(
    player_agency_perception,
    'Do players who benefit from Free Parking windfalls perceive their wealth as earned through skill or as luck-dependent?',
    'Qualitative post-game interviews and survey data on player attribution (skill vs luck); correlation between Free Parking benefit and self-reported sense of control; comparison of player satisfaction metrics with/without rule',
    'If perceived as luck-dependent: rule increases game narrative but undermines skill-based engagement (snare extraction clarified). If perceived as skill-dependent: rule obscures luck and enables overconfidence (different type of snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(player_agency_perception, empirical, 'Player attribution of Free Parking windfalls to skill vs luck').

omega_variable(
    exit_option_feasibility,
    'Is the social cost of refusing Free Parking rule (proposing standard rules) equivalent across player groups?',
    'Social network analysis of game groups; interviews about veto power and norm-setting authority; observation of rule negotiation dynamics',
    'If exit cost is uniform: suppression is shared (less snare). If exit cost is asymmetric (children cannot easily propose alternatives): suppression is asymmetric (confirmed snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Social cost symmetry of refusing Free Parking rule').

omega_variable(
    original_design_intent_recovery,
    'Can players who switch to official rules adjust strategic play within one or two games?',
    'Longitudinal play study comparing strategy adaptation rates; skill-based player performance before/after rule switch; coaching or instruction impact on transition',
    'If adaptive: piton is confirmed (skill can overcome design switch). If maladaptive: original design intent is lost (house rule is now structural default, not degradation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_design_intent_recovery, empirical, 'Player adaptive capacity when switching from Free Parking to official rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_fp_house_rule, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfp_tr_t0, monopoly_fp_house_rule, theater_ratio, 0, 0.55).
narrative_ontology:measurement(mfp_tr_t3, monopoly_fp_house_rule, theater_ratio, 3, 0.6).
narrative_ontology:measurement(mfp_tr_t6, monopoly_fp_house_rule, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(mfp_be_t0, monopoly_fp_house_rule, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mfp_be_t3, monopoly_fp_house_rule, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mfp_be_t6, monopoly_fp_house_rule, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_fp_house_rule, resource_allocation).

% DUAL FORMULATION NOTE:
% The Free Parking house rule is a standalone constraint in informal game mechanics. It could be decomposed into separate constraints (elimination suppression mechanism vs. luck-based extraction), but the natural-language concept is sufficiently coherent to treat as one story. No upstream empirical claims are required to establish the rule's existence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
