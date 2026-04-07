% ============================================================================
% CONSTRAINT STORY: monopoly_fp_house_rule
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: monopoly_fp_house_rule
 *   human_readable: Monopoly 'Free Parking' House Rule
 *   domain: social/economic
 *
 * SUMMARY:
 *   The 'Free Parking' house rule in Monopoly represents a widespread
 *   deviation from official rules justified by intuitions about fairness and
 *   gameplay pacing. The rule creates a pooled redistribution mechanism where
 *   all taxes, fines, and penalties paid during the game accumulate on the
 *   Free Parking space, awarding the entire fund to whichever player lands
 *   there. This constraint exhibits multiple structural classifications
 *   depending on perspective: from the systematically unlucky player's view,
 *   it is a snare extracting wealth by random chance; from the rule-enforcer
 *   host's view, it is coordination mechanism accelerating play; from the
 *   game-designer's view, it is a temporary scaffold addressing perceived
 *   official-rule flaws; from the household tradition's view, it is a
 *   degraded piton maintained by nostalgia. The constraint is a diagnostic
 *   case for how indexical classification disambiguates 'fairness' from
 *   asymmetric extraction.
 *
 * KEY AGENTS:
 *   - Systematically Unlucky Players: Primary victims (powerless/trapped) — bear maximum extraction through penalty-accumulation patterns; cannot exit mid-session
 *   - Early Lucky Players: Primary beneficiaries (moderate/constrained) — disproportionately land on Free Parking early in the game when the fund is still small; enjoy reduced extraction risk
 *   - Rule-Enforcer Host: Secondary beneficiary (organized/arbitrage) — controls house-rule interpretation; can adjust enforcement mid-game
 *   - Game-Designer Intent: Analytical reference (analytical/analytical) — original Parker Brothers rules specify Free Parking as penalty-free space; house rule violates design intent
 *   - Household Tradition: Institutional actor (institutional/constrained) — maintains rule through generational inertia; now self-perpetuating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_fp_house_rule, 0.38).
domain_priors:suppression_score(monopoly_fp_house_rule, 0.42).
domain_priors:theater_ratio(monopoly_fp_house_rule, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_fp_house_rule, extractiveness, 0.38).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_fp_house_rule, tangled_rope).
narrative_ontology:human_readable(monopoly_fp_house_rule, "Monopoly 'Free Parking' House Rule").
narrative_ontology:topic_domain(monopoly_fp_house_rule, "social/economic").

domain_priors:requires_active_enforcement(monopoly_fp_house_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_fp_house_rule, early_lucky_players).
narrative_ontology:constraint_beneficiary(monopoly_fp_house_rule, players_avoiding_penalties).
narrative_ontology:constraint_victim(monopoly_fp_house_rule, systematic_penalty_payers).
narrative_ontology:constraint_victim(monopoly_fp_house_rule, game_pacing_integrity).
narrative_ontology:constraint_victim(monopoly_fp_house_rule, rule_enforcement_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEMATICALLY UNLUCKY PLAYER (SNARE) — A player who lands on tax/fine spaces repeatedly with no countervailing Free Parking hits bears maximum extraction. Trapped within the game session, cannot exit without social friction. The house rule creates a random redistribution mechanism that systematically extracts from the already-disadvantaged. No alternatives exist within house-rule play.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AVERAGE PLAYER (TANGLED ROPE) — Experiences the Free Parking rule as both coordination benefit (the pooled tax/fine fund keeps money in active circulation, accelerating gameplay) and extraction risk (random chance determines who captures the fund). Constrained by social expectations to play by house rules; modest exit cost if refusing to play. Mixed experience — coordinating faster gameplay but bearing asymmetric penalty exposure based on luck.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: RULE-ENFORCER HOST (ROPE) — The player or host who initiates and enforces the house rule benefits from the perception of 'fairness' through redistribution and acceleration of play. Can arbitrage by using house-rule flexibility to adjust rules mid-game (official rules allow no such adjustment). Low effective extraction because the host has agency and perceived legitimacy.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: GAME DESIGNER PERSPECTIVE (SCAFFOLD) — The original Monopoly rules (Parker Brothers, 1930s) explicitly made Free Parking a neutral space generating no income. The house rule represents an unauthorized deviation justified by desires for 'fairness' and speedier play. From the designer's intent, the house rule has a natural sunset: the game ends, players revert to official rules for next session, or adoption of faster official variants (Speed Monopoly, 2-hour tournament rules) replaces the house rule entirely. The scaffolding is temporary workaround to perceived official-rule flaws.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, scaffold,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: HOUSEHOLD TRADITION (PITON) — Many households have played 'Free Parking has money' for 50+ years, treating it as the canonical Monopoly experience. Theater ratio high (0.65) — the rule is justified by nostalgia and family tradition rather than by game-theoretic benefit. The original function (attempting to balance perceived official-rule harshness) has atrophied; the rule persists through institutional inertia. Enforcement becomes ceremonial rather than functional.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: NATURAL LAW VIEW (FALSE SUMMIT) — An analytical observer might claim that 'house rules are inevitable in long gameplay' or 'players naturally want fairness mechanisms,' treating the Free Parking rule as an immutable law of game sociology. This perspective risks naturalizing what is actually a contingent community norm. The engine will identify this as a false summit — the rule is a bounded, revisable institutional practice, not a law of nature.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_fp_house_rule_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(monopoly_fp_house_rule, TR),
    TR >= 0.70.

:- end_tests(monopoly_fp_house_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Free Parking rule creates an unequal redistribution mechanism — penalties are pooled and randomly distributed based on movement luck rather than cause-and-effect. However, the extraction is not severe (not 0.70+) because the rule is transparently applied, affects all players equally in principle (though unequally in practice), and is temporary (games have defined endpoints). Suppression (0.42): Moderate. Players cannot easily exit the house rule without social friction ('but everyone plays this way'). The rule enforcement is active — the host must explicitly maintain it. Alternatives exist (official rules) but require breaking household tradition. Theater ratio (0.65): Moderate-high. Justifications for the rule often appeal to 'fairness' and 'speedier play,' but empirical validation is weak. The rule persists more due to tradition than to measurable gameplay improvement. Theater has increased over time as original game-design rationales (teaching economic principles through bankruptcy consequences) were forgotten, replaced by post-hoc justifications.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the unlucky player's snare and the host's rope reveals how the same rule mechanism generates opposite structural experiences. For early players or lucky players, the rule enables hope of a windfall and accelerates cash recovery — coordination benefit. For late-game players who have paid many penalties, the rule has become pure extraction — the pooled fund is now large and they have little chance of landing there. The organized host's perspective (rope) obscures the distributional asymmetry because they have agency (can adjust rules, can exit the rule-enforcement role). The piton perspective reveals that the rule persists despite uncertain coordination benefit — households maintain it through tradition alone. The mountain perspective risks naturalizing the rule as 'inevitable' human nature (we always want fairness mechanisms), when it is actually a contingent 20th-century North American household practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (early_lucky_players, players_avoiding_penalties): Low directionality value (d ≈ 0.15-0.25). These agents experience the rule as beneficial — the pooled fund represents a positive random event. Their exit options are moderate (they could demand official rules, but at low social cost if they're winning). Victims (systematic_penalty_payers, game_pacing_integrity, rule_clarity): High directionality value (d ≈ 0.75-0.95). Systematic penalty payers are trapped by the rule structure — their penalties subsidize the public fund. Game pacing integrity and rule clarity are abstract victims: the rule obscures the original design intent (economic consequence education) and creates rule-state uncertainty (how much is in the fund?). The host's power atom (organized/arbitrage) places them at low extraction experience (d ≈ 0.25) because they have agency and can revise the rule. Average players (moderate/constrained) experience moderate extraction (d ≈ 0.50), distributed through luck.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the Free Parking rule is NOT an immutable law but a contingent social arrangement masquerading as fairness. The mountain perspective (natural law of human game design) is a false summit because the rule violates the original designer's explicit intent — Parker Brothers defined Free Parking as tax-free. The snare perspective (systematic extraction from unlucky players) is real but underspecified — the extraction is random, not systematic in the game-theory sense. The tangled rope classification (coordinate faster play + extract via luck) is the accurate middle ground: the rule does accelerate circulation of capital, but it also creates asymmetric penalty exposure. The piton classification (degraded tradition) captures that the rule's original justifications (teaching economic consequence) have been forgotten, replaced by post-hoc fairness narratives. The mandatrophy resolution is: the rule is a bounded household norm, not a law; households that adopt official rules or tournament variants escape the constraint without loss of gameplay quality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fairness_vs_game_integrity,
    'Does the Free Parking rule actually increase perceived fairness or does it obscure the original design intent (penalty mechanism as strategic consequence)?',
    'Comparative analysis of player satisfaction in official-rules vs house-rule sessions; measurement of median game duration and win distribution; post-game surveys of fairness perception',
    'If perceived fairness increases: rule serves genuine coordination function (Rope classification). If perceived fairness is illusion: rule is pure extraction theater (Snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_vs_game_integrity, empirical, 'Whether Free Parking rule increases or obscures fairness perception').

omega_variable(
    cash_flow_dynamics,
    'Does pooling taxes/fines in the Free Parking fund actually accelerate gameplay or does it extend the endgame by keeping weak players solvent longer?',
    'Measurement of average game duration with official rules vs house rule; analysis of bankruptcy timing and win distributions; simulation of cash flow dynamics under both rulesets',
    'If acceleration is real: scaffolding is functional (Scaffold classification justified). If cash flow extends endgame: the ''speedier play'' justification fails, exposing the rule as pure extraction theater (Piton/Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cash_flow_dynamics, empirical, 'Whether Free Parking pooling accelerates or extends gameplay').

omega_variable(
    adoption_motivation,
    'Is the Free Parking rule adopted primarily to address perceived official-rule harshness or is it a pre-game coordination mechanism to avoid penalty-induced early-game demoralization?',
    'Historical tracing of house rule adoption patterns; interviews with households explaining their reasoning; analysis of whether official-rules violations cluster in early-game or late-game phases',
    'If early-game morale motivation: extraction is directed at new players (snare from their perspective). If late-game cash-flow motivation: extraction is directed at economically weak players (snare from their perspective). Either way, victim identification clarifies the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_motivation, conceptual, 'Primary motivation for adopting Free Parking rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_fp_house_rule, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfp_tr_t0, monopoly_fp_house_rule, theater_ratio, 0, 0.45).
narrative_ontology:measurement(mfp_tr_t50, monopoly_fp_house_rule, theater_ratio, 50, 0.6).
narrative_ontology:measurement(mfp_tr_t100, monopoly_fp_house_rule, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(mfp_be_t0, monopoly_fp_house_rule, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mfp_be_t50, monopoly_fp_house_rule, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(mfp_be_t100, monopoly_fp_house_rule, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_fp_house_rule, resource_allocation).
narrative_ontology:affects_constraint(monopoly_fp_house_rule, monopoly_house_rule_accumulation).
narrative_ontology:affects_constraint(monopoly_fp_house_rule, informal_game_rule_adoption).

% DUAL FORMULATION NOTE:
% The Free Parking house rule is a specific instance of broader constraint family: informal game rule deviations adopted to correct perceived official-rule flaws. Upstream constraint: the original Monopoly rules' aggressive bankruptcy mechanism. Downstream constraint: generalized tendency to adopt 'fairness' house rules without empirical validation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
