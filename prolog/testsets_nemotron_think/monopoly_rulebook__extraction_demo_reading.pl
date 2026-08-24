% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__extraction_demo_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook as Demonstration of Inevitable Wealth Concentration
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint story models the extraction_demo_reading of the
 *   monopoly_rulebook kernel: the claim that Monopoly's unmodified rules
 *   demonstrate the inevitable concentration of wealth through rent
 *   extraction, presenting this as capitalism's pedagogical truth. The
 *   rulebook is treated as a mountain constraint — a natural law of economic
 *   physics — but its high extractiveness (0.62), active enforcement
 *   (elimination mechanics), and identifiable beneficiaries/victims mark it
 *   as a false summit. The measurement series tracks a single game's
 *   progression: early low extraction (property acquisition phase), rising
 *   extraction as monopolies form and houses are built, peaking in the
 *   endgame where eliminated players transfer their remaining assets. Theater
 *   ratio rises as the 'fair competition' framing becomes increasingly
 *   performative relative to the mathematical certainty of the leader's
 *   victory. Suppression requirement increases as the rulebook must actively
 *   enforce elimination against players' desire to continue participating.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.62).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.78).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Demonstration of Inevitable Wealth Concentration").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).
domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '415be9b1-b8e4-4ac8-b0e4-7760730523e1').
narrative_ontology:cs_kernel_codification('415be9b1-b8e4-4ac8-b0e4-7760730523e1', fixed_text).
narrative_ontology:cs_authority_grounding('415be9b1-b8e4-4ac8-b0e4-7760730523e1', lineage).
narrative_ontology:cs_interpretation_layer_present('415be9b1-b8e4-4ac8-b0e4-7760730523e1').
narrative_ontology:cs_reading_relation('415be9b1-b8e4-4ac8-b0e4-7760730523e1', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('415be9b1-b8e4-4ac8-b0e4-7760730523e1', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('415be9b1-b8e4-4ac8-b0e4-7760730523e1', foundational, capital_concentration_inevitable).
narrative_ontology:cs_axiom_status(capital_concentration_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('415be9b1-b8e4-4ac8-b0e4-7760730523e1', capital_concentration_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('415be9b1-b8e4-4ac8-b0e4-7760730523e1', foundational, elimination_pedagogically_necessary).
narrative_ontology:cs_axiom_status(elimination_pedagogically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('415be9b1-b8e4-4ac8-b0e4-7760730523e1', elimination_pedagogically_necessary, deontological).
narrative_ontology:cs_reference_frame('415be9b1-b8e4-4ac8-b0e4-7760730523e1', original_1935_rulebook_as_capitalism_natural_law).
narrative_ontology:cs_drift_state('415be9b1-b8e4-4ac8-b0e4-7760730523e1', contemporary_play_practices, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('415be9b1-b8e4-4ac8-b0e4-7760730523e1', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, ultimate_winner).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, capitalist_class_analogue).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, midgame_contenders).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, midgame_contenders).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, capital_concentration_inevitability).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, rent_extraction_as_natural_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulates all assets through rent collection on owned properties. The game structure guarantees their victory once a critical mass of properties is acquired. Can exit at any time by declaring victory, converting game capital to social capital (winning).
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, ultimate_winner, beneficiary,
    powerful, biographical, arbitrage, global).

% Progressively lose liquidity through rent payments to property owners. Elimination is mandatory when assets are exhausted — no bankruptcy protection, no redistribution, no re-entry. The rulebook provides no mechanism to recover; exit means total loss of position and sitting out the remainder of the 60-90 minute game.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Administers the rulebook: distributes starting capital, enforces rent schedules, auctions properties, manages the housing supply constraint (32 houses, 12 hotels), and certifies eliminations. The banker never loses — they are the rule apparatus itself. In household play this role is often rotated; in tournament play it is a designated official.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, banker, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, banker, observer).

% Hold some properties and collect rent from weaker players while paying rent to stronger ones. Their position is transient — the positive feedback loop of rent extraction ensures they will either become the ultimate winner or be eliminated. Exit is constrained: selling assets to the leader accelerates concentration; mortgaging delays elimination at punitive interest.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, midgame_contenders, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, midgame_contenders, beneficiary).

% Analyze the game as a model of capitalist accumulation. Include economists (Marx, Piketty), game designers, educators using Monopoly to teach inequality. They do not play; they study the constraint's operation and its ideological function. Their exit is analytical — they can reject the model without material cost.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, critical_observers, observer,
    analytical, civilizational, analytical, universal).

% Players who inject liquidity (Free Parking jackpot, interest-free loans, property gifts) to prevent elimination and prolong play. They are excluded from the rulebook's authoritative reading — the text explicitly forbids these modifications. Their presence demonstrates the rulebook's harshness is not self-sustaining; social coordination requires overriding the text.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_practitioners, excluded,
    organized, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook coordinates a demonstration: it structures play so that rent extraction inevitably concentrates all wealth in one player, presenting this outcome as the natural result of property rights and free exchange rather than a designed feature.
% TRANSFER_FUNCTION: Moves liquidity from eliminated players to property owners through mandatory rent payments on landed spaces. The transfer is asymmetric: owners collect without labor; payers pay without consent. No mechanism returns assets to eliminated players. The banker facilitates but does not benefit.
% ABSENT_VOICES: Players who would prefer cooperative or non-elimination play (house_rule_practitioners) are structurally excluded by the rulebook's text. Critics who would frame the game as a critique of capitalism rather than its celebration are absent from the rulebook's authority structure — the rulebook presents itself as neutral description, not ideological argument.
% DISAPPEARANCE_RATIONALE: If the rulebook vanished, the specific demonstration of inevitable concentration through rent extraction would disappear. House rules would become the default (as they already are in most households), transforming the game into a social scaffold. The pedagogical claim — that monopoly capitalism's truth is revealed by unmodified play — would lose its authoritative text.
% FOUNDING_PROBLEM: Elizabeth Magie's 1904 Landlord's Game was designed to demonstrate the injustice of rent extraction under private property monopoly. Parker Brothers' 1935 Monopoly rulebook inverted this purpose: it stripped the single-tax alternative rules and presented the concentration outcome as the game's entire point — proving capitalism's natural law rather than critiquing it.
% FOUNDING_PROBLEM_CORROBORATION: Magie's original patent and the Landlord's Game rules (archived at the Strong National Museum of Play) corroborate the anti-monopoly founding purpose. Parker Brothers' acquisition and suppression of the alternative rules is documented in corporate correspondence. The extraction_demo_reading's claim that the rulebook *instantiates* capitalism's truth is corroborated by Piketty's empirical work on r > g — but this is an external theoretical overlay, not internal to the rulebook.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__extraction_demo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, ExtMetricName, E),
    domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monopoly_rulebook__extraction_demo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is mountain (the reading presents the outcome as natural law). Authored metrics describe a constraint that actively extracts from eliminated players (high ε), requires enforcement to maintain elimination (high suppression), and becomes increasingly performative (rising theater) as the outcome becomes predetermined. The divergence between claim and metrics is the measurement: a false summit mountain that presents constructed extraction as natural law. Beneficiaries (ultimate_winner, capitalist_class_analogue) and victims (eliminated_players) are declared, triggering FSM evaluation.
 *
 * PERSPECTIVAL GAP:
 *   From the winner/banker seat, the constraint appears as genuine coordination: a fair game where skill is rewarded. From the eliminated_players seat, the same structure operates as pure extraction with no exit. The engine computes this divergence from the structural data — the authored claim (mountain) does not adjudicate it. The midgame_contenders' shifting directionality (beneficiary→target) models the false consciousness of temporary advantage in a rigged structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The ultimate_winner sits at d ≈ 0.05 (full beneficiary: collects all rents, controls the game's conclusion). Eliminated_players sit at d ≈ 0.95 (full targets: pay all rents, forced exit with total loss). Midgame_contenders sit near d ≈ 0.5 early (symmetric: collecting and paying) but drift toward target as concentration accelerates. Banker sits at d ≈ 0.1 (structural beneficiary: administers the extraction apparatus). Critical_observers sit at d ≈ 0.0 (analytical seat). House_rule_practitioners are excluded — their exclusion is what the constraint's enforcement machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The rulebook's original mandate (Magie's anti-monopoly demonstration) is dead — the current rulebook inverts it. The mandate has atrophied into its opposite: the rulebook now *performs* the very concentration it was designed to critique. This is not a piton (inertial persistence) but an active ideological apparatus — the extraction is the point. Mandatrophy is resolved in the sense that the founding problem is acknowledged as dead, but the constraint persists because its extraction function serves the beneficiary (capitalist_class_analogue as ideological reinforcement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalness,
    'Is the monopoly_rulebook a mountain of economic physics (natural law of capital concentration) or a snare of game design (constructed extraction mechanism)?',
    'Compare the rulebook''s extraction profile against: (a) mathematical models of rent-seeking in closed systems; (b) the Landlord''s Game''s alternative rules which prevent concentration; (c) empirical data on house rule prevalence. If concentration persists across all variants, mountain; if concentration is rulebook-specific, constructed.',
    'If mountain: the extraction_demo_reading is vindicated — Monopoly reveals capitalism''s truth. If constructed: the rulebook is a false summit (FSM candidate) whose ''natural law'' framing is ideological cover for a designed extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalness, conceptual, 'Whether the rulebook''s concentration outcome is a discovered natural law or an authored feature.').

omega_variable(
    extraction_invariance_across_player_count,
    'Does the high epsilon (0.62) hold across player counts (2-8), or does it depend on the standard 4-player configuration?',
    'Simulate or observe games at each player count. Two players: faster concentration, higher peak extraction. Eight players: slower, more chaotic, possible stalemates. Measure epsilon at each count.',
    'If epsilon varies significantly with player count, the ''inevitable concentration'' claim is parameter-dependent — the constraint is not ε-invariant. This would require decomposing into player-count-specific constraint stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_invariance_across_player_count, empirical, 'Whether the extraction_demo_reading''s epsilon is stable across the rulebook''s valid configurations.').

omega_variable(
    committer_structure_omega,
    'How does the extraction_demo_reading''s structural relationship to the monopoly_rulebook kernel differ from its siblings?',
    'Map the three readings'' victim sets, epsilon values, and claimed types. Extraction_demo: victims=eliminated_players, ε=0.62, claimed=mountain. Social_scaffold: victims=none (house rules prevent elimination), ε≈0.15, claimed=scaffold. Tournament_orthodoxy: victims=none (elimination is fair competition), ε≈0.25, claimed=rope. The disagreement is located in the elimination mechanic''s normative valence.',
    'Clarifies that the kernel contest is not about the rulebook''s mechanics (all readings agree on the text) but about the classification of those mechanics: mountain (natural law), scaffold (transitional coordination), or rope (competitive coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_omega, conceptual, 'Committer-frame structural delta between this reading and its siblings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression (elimination) structural (rulebook mechanics) or internalized (players accept elimination as fair)?',
    'Observe eliminated players'' behavior: do they protest the rules, propose house rules, or accept elimination as ''just how the game works''? Track post-elimination attitudes. If players internalize the elimination as fair, suppression is partially internalized — the constraint carries its own legitimacy.',
    'If internalized, effective suppression exceeds the structural measure — the constraint reproduces its own legitimacy through the elimination experience itself. This strengthens the mountain claim (natural law feels inevitable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the elimination mechanic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.42).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.53).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.76).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__extraction_demo_reading, 0.12).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This is the extraction_demo_reading of the monopoly_rulebook kernel. It claims the rulebook is a mountain (natural law) with high epsilon. The social_scaffold_reading claims it is a scaffold requiring house rules (coordination with sunset). The tournament_orthodoxy_reading claims it is a rope (competitive coordination) with immutable text authority. All three share the same rulebook text but decompose into distinct constraints with different ε, victims, and types — per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, powerful, 0.05).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, powerless, 0.95).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, moderate, 0.55).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
