% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Monopoly Rulebook as Extraction Pedagogy (Reading: Extraction Demo)
 *   domain: game_theory/institutional_design/pedagogy
 *
 * SUMMARY:
 *   The Monopoly rulebook, read as extraction pedagogy, instantiates wealth
 *   concentration through its mechanics (property ownership, geometric rent
 *   escalation, elimination of bankrupt players) and presents this structure
 *   as a natural mathematical necessity—demonstrating how capitalism produces
 *   inevitable monopoly. Players begin with equal capital and opportunities;
 *   the rulebook's rules guarantee that within 60-90 minutes, one player has
 *   accumulated all remaining wealth and all other players are eliminated.
 *   This reading claims the rulebook proves monopoly capitalism is
 *   structurally inevitable, not a contingent policy choice. The constraint
 *   is claimed as a mountain (natural law of property accumulation) while the
 *   metrics describe a highly extractive, actively enforced structure—the
 *   divergence is the reading's signature: it asserts mathematical necessity
 *   while the game mechanics accomplish active suppression of alternatives
 *   (house rules, redistribution) to demonstrate the point.
 *
 * KEY AGENTS:
 *   - game_winner: accumulates all wealth by rulebook operation; benefits from asymmetric compounding
 *   - eliminated_players: lose all capital through rent extraction; trapped in geometric escalation
 *   - late_accumulators: fall behind early and face identity-locked exit (competitive shame prevents quitting)
 *   - rulebook_authority: sets the frame that this text is legitimate competitive format; excludes house-rule alternatives
 *   - pedagogical_observer: reads the game as demonstration of mathematical necessity of monopoly formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.68).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.71).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Extraction Pedagogy (Reading: Extraction Demo)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/institutional_design/pedagogy").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'd0adfc68-c578-42c7-a4f0-d3f595d69088').
narrative_ontology:cs_kernel_codification('d0adfc68-c578-42c7-a4f0-d3f595d69088', fixed_text).
narrative_ontology:cs_authority_grounding('d0adfc68-c578-42c7-a4f0-d3f595d69088', extraction).
narrative_ontology:cs_interpretation_layer_present('d0adfc68-c578-42c7-a4f0-d3f595d69088').
narrative_ontology:cs_reading_relation('d0adfc68-c578-42c7-a4f0-d3f595d69088', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0adfc68-c578-42c7-a4f0-d3f595d69088', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('d0adfc68-c578-42c7-a4f0-d3f595d69088', foundational, property_accumulation_mathematical_necessity).
narrative_ontology:cs_axiom_status(property_accumulation_mathematical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d0adfc68-c578-42c7-a4f0-d3f595d69088', property_accumulation_mathematical_necessity, empirically_contingent).
narrative_ontology:cs_axiom('d0adfc68-c578-42c7-a4f0-d3f595d69088', foundational, elimination_inevitable_from_equal_start).
narrative_ontology:cs_axiom_status(elimination_inevitable_from_equal_start, holdable).
narrative_ontology:cs_axiom_grounding('d0adfc68-c578-42c7-a4f0-d3f595d69088', elimination_inevitable_from_equal_start, empirically_contingent).
narrative_ontology:cs_reference_frame('d0adfc68-c578-42c7-a4f0-d3f595d69088', rulebook_as_mathematical_truth).
narrative_ontology:cs_drift_state('d0adfc68-c578-42c7-a4f0-d3f595d69088', contemporary_house_rules_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d0adfc68-c578-42c7-a4f0-d3f595d69088', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, game_winner).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, early_accumulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, late_accumulator).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulates all remaining liquid capital and property titles by game end. Emerges from the rulebook's operation as the sole surviving player. The rulebook guarantees their victory through asymmetric wealth compounding once early advantage forms.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_winner, beneficiary,
    powerful, biographical, arbitrage, local).

% Players with early favorable rolls who acquire properties faster than competitors. Enjoy temporary advantage that compounds through the rulebook's rent-doubling mechanics. Their position is preserved through the game's escalating extraction phase until elimination begins.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, early_accumulators, beneficiary,
    moderate, immediate, constrained, local).

% Lose all capital to rent payments and are forced to exit the game. Once property concentration reaches critical mass (typically 40-60 minutes into play), elimination accelerates and becomes inevitable for most players. No redistribution mechanisms exist to slow this trajectory.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Players who fall behind in early acquisition and cannot accumulate capital fast enough to offset rent payments to ahead players. Trapped in an asymmetric competitive structure where their position worsens predictably as the game progresses. Identity-locked because competitive players see quitting as admission of defeat.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, late_accumulator, payer,
    moderate, biographical, identity_locked, local).

% The official text and its interpretive community (Parker Brothers/Hasbro, tournament officials, game scholars). Sets the frame that this rulebook is the legitimate competitive format and that house rules represent corruption of the design intent.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, rulebook_authority, agenda_setter,
    analytical, civilizational, analytical, universal).

% Analyzes the game as a teaching device about wealth concentration and capitalism. Observes the rulebook's mathematics as instantiating and demonstrating inevitable monopoly formation.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, pedagogical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, game_winner).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared game frame where players' relative economic decisions and fortune (dice rolls) translate into capital accumulation, creating a measurable demonstration of wealth inequality emergence from initially equal starting conditions.
% TRANSFER_FUNCTION: Moves accumulated capital from players with fewer properties to players with more properties via rent payments. As properties concentrate, the transfer accelerates geometrically (rent doubles when a player owns a complete color set) until players are eliminated.
% ABSENT_VOICES: Players who quit before elimination, players who enforce house rules (free parking money, income redistribution, graduated rent caps) — these players experience the rulebook differently but are excluded from the text-authority framing that defines this reading. Alternative readings' proponents would attest the founding problem is 'playable social experience,' not 'demonstrate mathematical necessity.'
% DISAPPEARANCE_RATIONALE: If the Monopoly rulebook (this specific text) disappeared, players would continue playing games involving property accumulation and capital transfer — but without this rulebook's specific structure, they would implement alternatives (house rules, different mechanics) that distribute capital differently. The pedagogical claim — that the rulebook *demonstrates* inevitable wealth concentration — would collapse; the mathematical necessity would be unprovable without the text.
% FOUNDING_PROBLEM: How can a competitive game model the emergence of wealth concentration and monopoly capitalism in a form that players can observe and understand through play?
% FOUNDING_PROBLEM_CORROBORATION: The reading's pedagogical proponents (game scholars studying economic inequality through gameplay, educators using Monopoly to teach about capitalism) attest the founding problem is live and the rulebook solves it. The alternative-reading community (social-coordination advocates, players who enforce house rules) attest the founding problem was solved differently or is misdefined entirely.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises sharply from 0.15 (game start, equal capital, low rent) to 0.68 (late game, concentrated ownership, exponential rent) and plateaus near the end as extraction becomes complete and dominant. Suppression tracks extractiveness: low early (players can adapt strategy, exit is voluntary) and high late (rent payments become non-negotiable, elimination is mechanical). Theater_ratio rises to 0.45 as the game progresses: early gameplay involves genuine strategic decisions (which properties to buy, when to save cash); late gameplay is increasingly theatrical—eliminated players watch, surviving players execute mechanical rent collection, the outcome is predetermined by wealth distribution. The reading frames this temporal signature as proof: the rulebook *demonstrates* that inequality emerges inevitably from initial equality + property rules. Suppression is NOT scaled (raw structural property); extraction is scaled by directionality (winners extract from losers through the rules' operation). Measurements are authored on a shared time grid (all metrics sampled at 0, 15, 30, 45, 60, 75, 90 minutes) so temporal analysis has complete data across the game's lifecycle.
 *
 * PERSPECTIVAL GAP:
 *   From the pedagogical observer's seat: the rulebook is a pure-knowledge instrument demonstrating mathematical necessity. From the eliminated player's seat: the same rulebook is a mechanism of elimination and extraction with no escape. From the rulebook-authority's seat: the text is legitimate and unchangeable, and house rules corrupt it. From the social-scaffold reading's seat: the text is unplayable without community correction. These are DIFFERENT CONSTRAINTS (different ε, different beneficiary/victim sets, different narratives)—they are not perspectives on one constraint, but readings of a contested kernel. This file instantiates ONLY the extraction-demo reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The game_winner seat has d ≈ 0.0 (full beneficiary): the rulebook subsidizes them through asymmetric compounding. Early accumulators have d ≈ 0.3 (mostly beneficiaries early, become targets if they fall behind late). Eliminated players have d ≈ 1.0 (full targets): the rulebook extracts from them via geometric rent escalation until they have zero capital and exit. Late accumulators are identity-locked (d ≈ 0.85): they bear extraction costs and competitive shame prevents exit even when rational choice would quit. The rulebook_authority is analytical (d not applicable; sets the frame). This seat divergence is computed by the engine from beneficiary status (winners/early accumulators), victim status (eliminated/late players), and exit options (trapped/identity_locked). The reading asserts that from the pedagogy seat, this structure is NATURAL—the rulebook reveals how capitalism works—while from the eliminated-player seat, it is EXTRACTIVE and SUPPRESSIVE. The engine measures this divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The rulebook's founding problem (how to model capitalism's inequality) is stated as LIVE by this reading, but DEAD by the social-scaffold reading (the problem is playable social experience, not mathematical demonstration). The disappearance_verdict is world_rearranges (if the rulebook vanishes, players implement alternatives), which creates a mismatch with the mountain claim—mountains have world_unchanged verdicts. This mismatch is exactly the FSM candidate: the reading claims the rulebook instantiates natural law, but declares beneficiaries (game_winner, early_accumulators) and an active founding problem. The engine's false_summit_mountain signature will evaluate this. The mandate has not outlived its function (the reading claims the function is live), so mandatrophy_resolved is false. However, the social-scaffold reading (a sibling) DOES declare mandatrophy (the founding problem is solved; the rulebook persists as theater). This is the inter-reading dispute the kernel structure is designed to model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_designed_extraction,
    'Is the wealth concentration that emerges from the rulebook a feature of the game''s mathematical structure (natural law of competitive accumulation), or is it an intentional design choice that benefits early winners and was coded into the rules by conscious authorial decision?',
    'Historical analysis of the rulebook''s evolution (Darrow to Parker Brothers to modern editions), designer commentary, and comparison to alternative property-accumulation mechanics that do NOT produce rapid elimination. If the doubling-rent mechanic was added specifically to accelerate wealth concentration, then the ''natural law'' claim collapses.',
    'If the structure is designed (not natural), the constraint reclassifies from mountain to tangled_rope (coordination function + intentional extraction). This reading''s pedagogical truth-claim depends on the ''natural'' reading; if designed, the reading shifts to ''demonstrating intentional design of inequality,'' which is a different thesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_designed_extraction, empirical, 'Whether wealth concentration in Monopoly is mathematical necessity or intentional design.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'What makes this a ''mountain'' reading of the kernel rather than a ''snare'' reading? Is the rulebook a discovered mathematical truth about property accumulation, or is it a designed extraction mechanism that disguises itself as truth?',
    'The distinction rests on whether the rulebook''s authority derives from ''this is how accumulation necessarily works'' (mountain framing) or ''this is how the designer chose to encode inequality for profit/pedagogy'' (snare framing). The two readings are distinct commitments to the kernel''s legitimacy.',
    'A mountain reading claims the rulebook''s harsh dynamics are inevitable and educational; a snare reading claims they are designed for engagement or pedagogical shock value. The engine will compute this from the structural data (beneficiary concentration, active enforcement of rules, victim elimination). This omega documents that the reading-framing choice is author-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the rulebook instantiates mathematical necessity or designed inequality.').

omega_variable(
    pedagogical_intent_vs_unintended_consequence,
    'Is the wealth concentration an intended pedagogical demonstration (the reading''s framing), or an unintended side effect of a game designed for entertainment that happened to model inequality well?',
    'Designer interviews, contemporaneous commentary on the game''s purpose (was it designed to teach about capitalism or to entertain?), and adoption patterns (is it used in classrooms as pedagogy or at tables as recreation?). The reading assumes pedagogical intent; alternative readings may attribute the same structure to entertainment design.',
    'If the structure was unintended or entertainment-focused, the ''pedagogical truth'' claim becomes post-hoc interpretation rather than encoded purpose. This shifts the reading''s epistemic status from ''the rulebook proves monopoly is inevitable'' to ''the rulebook happens to model monopoly well, and we read it as pedagogy.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pedagogical_intent_vs_unintended_consequence, conceptual, 'Whether the rulebook''s inequality structure was pedagogically intended or entertainment-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.43).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.42).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.61).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.69).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__extraction_demo_reading, 0.12).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The monopoly_rulebook kernel decomposes into three constraint stories, each instantiating one reading: extraction_demo_reading (this file, ε ≈ 0.68, mountain claim), social_scaffold_reading (ε ≈ 0.45, scaffold claim, community correction required), and tournament_orthodoxy_reading (ε ≈ 0.35, rope claim, skill-determined outcomes). All three read the same text but with different ε values, beneficiary/victim sets, and interpretations of the founding problem. The extraction_demo reading affects its siblings by providing the 'harsh baseline' case: the other readings define themselves against this reading's mathematics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
