% ============================================================================
% CONSTRAINT STORY: informal_game_rule_adoption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informal_game_rule_adoption, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: informal_game_rule_adoption
 *   human_readable: Informal Game Rule Adoption and Coordination Drift
 *   domain: social_coordination/game_dynamics
 *
 * SUMMARY:
 *   Informal game rule adoption occurs when players in a multiplayer game
 *   introduce and enforce new rules outside the official ruleset, usually to
 *   address perceived gaps, unfairness, or novel strategic situations. The
 *   constraint exhibits a dual character: genuine coordination function
 *   (solving rule gaps, enabling new play styles) and asymmetric extraction
 *   (the rule innovators gain advantage, late joiners face enforced
 *   compliance to rules they didn't negotiate). As rules crystallize, the
 *   theater ratio increases — elaborate ritual explanations and
 *   justifications become necessary to enforce rules whose origins may have
 *   become obscure or contentious. The constraint exhibits all six types from
 *   different perspectives, revealing how a coordination mechanism can
 *   calcify into an extraction apparatus through power asymmetry and
 *   information lag.
 *
 * KEY AGENTS:
 *   - Rule Innovators: Primary beneficiaries (institutional/arbitrage) — gain strategic advantage during adoption window, can revise rules in their favor, experience coordination as low-friction governance
 *   - Game Coherence: Primary victim (powerless/trapped) — abstract collective good threatened by rule fragmentation; cannot organize or exit without abandoning all games
 *   - Late Joiners: Secondary victims (powerless/trapped) — must learn unstandardized rules, face enforced compliance to rules created without their input, cannot exit without leaving the game
 *   - Casual Player Community: Secondary actors (moderate/constrained) — experience genuine coordination benefit but also extraction as rules change in favor of original circle
 *   - Official Game Authority: Institutional observer (institutional/arbitrage) — benefits from informal rule ecosystem reducing governance burden; can selectively enforce rules
 *   - Rule Documentation Initiative: Organized agents (organized/mobile) — working toward sunset by standardizing and documenting rules, creating pathway to formalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informal_game_rule_adoption, 0.32).
domain_priors:suppression_score(informal_game_rule_adoption, 0.45).
domain_priors:theater_ratio(informal_game_rule_adoption, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informal_game_rule_adoption, extractiveness, 0.32).
narrative_ontology:constraint_metric(informal_game_rule_adoption, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(informal_game_rule_adoption, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informal_game_rule_adoption, tangled_rope).
narrative_ontology:human_readable(informal_game_rule_adoption, "Informal Game Rule Adoption and Coordination Drift").
narrative_ontology:topic_domain(informal_game_rule_adoption, "social_coordination/game_dynamics").

domain_priors:requires_active_enforcement(informal_game_rule_adoption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informal_game_rule_adoption, rule_innovators).
narrative_ontology:constraint_beneficiary(informal_game_rule_adoption, early_adopters).
narrative_ontology:constraint_victim(informal_game_rule_adoption, game_coherence).
narrative_ontology:constraint_victim(informal_game_rule_adoption, late_joiners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE JOINER (SNARE) — Arrives after informal rules have crystallized. Cannot exit without abandoning the game entirely; faces enforced compliance to unstandardized, evolving rules created without their input. Rules change in favor of established players. Full extraction with no coordination benefit experienced from their position.
constraint_indexing:constraint_classification(informal_game_rule_adoption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CASUAL PLAYER COMMUNITY (TANGLED ROPE) — Experiences genuine coordination benefit (shared rules enable play) alongside asymmetric extraction (rule changes favor original circle, require continuous renegotiation). Constrained by social bonds and investment in learning existing rules, but can organize collectively to propose counter-rules. Mixed extraction and coordination.
constraint_indexing:constraint_classification(informal_game_rule_adoption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: OFFICIAL GAME AUTHORITY (ROPE) — Benefits from informal rule ecosystem reducing official governance burden. Can arbitrage between house rules and official rules, choosing enforcement when convenient. Experiences constraint as pure coordination mechanism with asymmetric gain through reduced administrative overhead.
constraint_indexing:constraint_classification(informal_game_rule_adoption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RULE DOCUMENTATION INITIATIVE (SCAFFOLD) — Organized effort to formalize and document informal rules, creating sunset logic: as rules become explicit and standardized, the informal adoption constraint dissolves. Documentation replaces oral tradition; transparency replaces favor-based rule interpretation. Has clear exit pathway and sunset timeline.
constraint_indexing:constraint_classification(informal_game_rule_adoption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: NOSTALGIC HOUSE RULES COMMITTEE (PITON) — Original rule innovators whose influence has degraded as the game's social base shifted. Rules persist through nostalgia and group identity ('this is how we've always played') rather than functional necessity. Theater ratio high: elaborate rule rituals performed to maintain coherence despite the rules having been superseded by official rulesets. Low extraction because the original innovators have lost institutional power.
constraint_indexing:constraint_classification(informal_game_rule_adoption, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long-term perspective, informal rule adoption emerges naturally from coordination problems: any multiplayer game with gaps in official rules will generate local conventions. This perspective sees the constraint as a universal structural feature of decentralized game evolution. However, structural data contradicts this naturalization — the extractive elements (rule favoritism, suppression of alternatives) are not inherent to coordination but contingent on power asymmetries.
constraint_indexing:constraint_classification(informal_game_rule_adoption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informal_game_rule_adoption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informal_game_rule_adoption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informal_game_rule_adoption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(informal_game_rule_adoption, TR),
    TR >= 0.70.

:- end_tests(informal_game_rule_adoption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. Informal rule adoption provides genuine coordination benefit (solves rule gaps, enables expanded play) alongside measurable extraction (first movers gain strategic advantage, late joiners face enforced compliance). The extraction is not maximal because the coordination function is real and significant — rules genuinely solve problems. However, the extraction is not negligible because rule innovators systematically benefit from rule design and can revise rules when they lose advantage. Suppression (0.45): Moderate. Barriers to alternative rules include social cohesion (deviating is disruptive), sunk learning costs (must relearn official rules or negotiate new informal rules), and collective action problems (proposing rule changes is costly). But suppression is not total — organized groups can successfully propose and adopt alternative rules; exit via official ruleset is always available at cost of game experience. Theater ratio (0.38): Low-moderate, increasing over time. Early informal rules are justified by pragmatic necessity ('we need a rule for this situation'). As rules crystallize and memory of origins fades, theater increases — elaborate ritual justifications ('this is how we've always done it') substitute for functional necessity. Documentation initiatives work to reduce theater by making rules explicit and grounded in function rather than tradition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. Rule innovators see coordination (Rope) — they are solving genuine problems. Late joiners see extraction (Snare) — they face enforced compliance to rules they didn't choose. Casual communities see hybrid coordination-extraction (Tangled Rope) — benefits exist but asymmetric. The official authority sees institutional benefit (Rope) — reduced governance overhead. Documentation efforts see a temporary problem with exit pathway (Scaffold) — formalization will sunset the constraint. Original innovators whose influence has degraded see a nostalgic ritual (Piton) — rules persist through tradition rather than function. The analytical observer risks seeing natural law (Mountain) — informal coordination naturally emerges — but structural data reveals this as false naturalization: the extraction elements are contingent on power asymmetries, not inherent to coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value d is determined by their structural relationship to the extraction flow. Rule innovators (beneficiaries with arbitrage exit) experience low d — they benefit from the constraint and can exit to official rules if needed, so the constraint extracts toward them. Late joiners (trapped with no exit option) experience high d — they must comply with rules they didn't negotiate. Casual players (moderate power, constrained exit) experience mid-range d — they benefit from coordination but face costs of rule renegotiation. The official authority (institutional, arbitrage) experiences negative d — the constraint reduces their governance burden. The documentation initiative (organized, mobile) experiences declining d over time as the constraint's extraction mechanism weakens through formalization.
 *
 * MANDATROPHY ANALYSIS:
 *   COORDINATION-EXTRACTION BOUNDARY CASE: This constraint tests whether a genuine coordination mechanism can simultaneously be an extraction apparatus. The mandatrophy resolves by recognizing that all six types are structurally valid from different observer positions. The constraint is Rope from the beneficiary perspective (pure coordination gain). It is Snare from the powerless perspective (pure extraction). It is Tangled Rope from the moderate perspective (mixed). The analytical observer's 'natural law' framing that informal rules always emerge naturally is diagnostically important because it naturalizes what is actually a contingent power asymmetry: yes, informal rules emerge, but their distribution of benefit is not naturally balanced — it concentrates advantage on first movers. The scaffold view (documentation will sunset) is empirically testable: if documented rules reduce extraction and theater, the sunset is real. If extraction merely shifts (documented rules now favor whoever controls documentation), the constraint has not dissolved but transformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explicit_vs_emergent_rules,
    'What proportion of adopted informal rules are consciously explicit innovations versus emergent behavioral conventions?',
    'Historical reconstruction of rule origins; interviewing original adopters about intentionality; tracing rule diffusion patterns through community networks',
    'If mostly explicit: constraint is a mechanism for favoring known innovators (higher extraction). If mostly emergent: constraint reflects coordination discovery process (lower extraction, higher coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explicit_vs_emergent_rules, empirical, 'Explicit innovation vs emergent convention in rule adoption').

omega_variable(
    rule_reversal_barriers,
    'What structural barriers prevent reverting to official rules or adopting alternative informal rules once a set crystallizes?',
    'Case studies of attempted rule reversals; measurement of social cost to proposing deviation; analysis of voting or consensus mechanisms for rule changes',
    'If barriers are low: constraint is primarily coordination with low suppression (Rope). If barriers are high and asymmetric: constraint is extraction mechanism (Snare or Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rule_reversal_barriers, empirical, 'Barriers to rule reversion or alternative rule adoption').

omega_variable(
    documentation_displacement_timeline,
    'When informal rules are documented and standardized, do they remain functionally equivalent or does formalization alter them?',
    'Comparison of informal rule practice before and after documentation; measurement of continued informal deviation from documented versions; assessment of whether documentation sunset is real or merely shifts extraction mechanism',
    'If documentation preserves function: scaffold sunset is real, constraint dissolves. If documentation changes meaning or enforcement: constraint may degrade to piton rather than sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_displacement_timeline, empirical, 'Whether documentation preserves functional equivalence of informal rules').

omega_variable(
    power_concentration_mechanism,
    'Do rule innovators systematically engineer rules favoring themselves, or does power concentration emerge passively from first-mover advantage?',
    'Comparative rule analysis: measure symmetry of rule benefits across players; interview rule innovators about intentionality; analyze whether rules change when innovators change player pool vs remain fixed',
    'If systematic engineering: constraint is extraction mechanism (Snare). If passive first-mover: constraint is coordination with asymmetric gain (Rope/Tangled Rope). Affects whether extraction is feature or artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_concentration_mechanism, conceptual, 'Intentional vs passive power concentration in rule innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informal_game_rule_adoption, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igra_tr_t0, informal_game_rule_adoption, theater_ratio, 0, 0.15).
narrative_ontology:measurement(igra_tr_t2, informal_game_rule_adoption, theater_ratio, 2, 0.28).
narrative_ontology:measurement(igra_tr_t4, informal_game_rule_adoption, theater_ratio, 4, 0.38).

% Extraction over time
narrative_ontology:measurement(igra_be_t0, informal_game_rule_adoption, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(igra_be_t2, informal_game_rule_adoption, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(igra_be_t4, informal_game_rule_adoption, base_extractiveness, 4, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informal_game_rule_adoption, resource_allocation).
narrative_ontology:affects_constraint(informal_game_rule_adoption, house_rule_legitimacy).
narrative_ontology:affects_constraint(informal_game_rule_adoption, game_coherence_fragmentation).

% DUAL FORMULATION NOTE:
% Informal rule adoption constrains two distinct structural problems: (1) Solving rule gaps in incomplete official rulesets (coordination function, low extraction). (2) Establishing power asymmetry through rule design (extraction function). These could be decomposed as separate constraint stories (informal_rule_coordination vs informal_rule_extraction) with different epsilon values, but they are presented as tangled_rope because they co-occur structurally — solving the coordination problem necessarily creates the extraction opportunity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(informal_game_rule_adoption, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
