% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Monopoly Rulebook: Extraction Demonstration Reading
 *   domain: game_theory/institutional_design/social_coordination
 *
 * SUMMARY:
 *   This constraint story instantiates the 'extraction_demo_reading' of the
 *   Monopoly rulebook kernel. From this perspective, the rulebook is not
 *   merely a game, but a pedagogical tool that demonstrates the inevitable
 *   wealth concentration and rent extraction inherent in monopoly capitalism.
 *   Player elimination is seen as a structurally necessary outcome, serving
 *   as a 'truth' about economic systems. The constraint is claimed as a
 *   'mountain' because its 'truth' is presented as an irreducible, natural
 *   law of the simulated economic system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.75).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.65).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook: Extraction Demonstration Reading").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/institutional_design/social_coordination").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).
domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'dcc6987d-dfd6-47d5-8772-ada95214c6ec').
narrative_ontology:cs_kernel_codification('dcc6987d-dfd6-47d5-8772-ada95214c6ec', fixed_text).
narrative_ontology:cs_authority_grounding('dcc6987d-dfd6-47d5-8772-ada95214c6ec', practice).
narrative_ontology:cs_reading_relation('dcc6987d-dfd6-47d5-8772-ada95214c6ec', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('dcc6987d-dfd6-47d5-8772-ada95214c6ec', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('dcc6987d-dfd6-47d5-8772-ada95214c6ec', foundational, wealth_concentration_is_inevitable).
narrative_ontology:cs_axiom_status(wealth_concentration_is_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('dcc6987d-dfd6-47d5-8772-ada95214c6ec', wealth_concentration_is_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('dcc6987d-dfd6-47d5-8772-ada95214c6ec', foundational, elimination_is_pedagogical).
narrative_ontology:cs_axiom_status(elimination_is_pedagogical, holdable).
narrative_ontology:cs_axiom_grounding('dcc6987d-dfd6-47d5-8772-ada95214c6ec', elimination_is_pedagogical, instrumental).
narrative_ontology:cs_reference_frame('dcc6987d-dfd6-47d5-8772-ada95214c6ec', unfettered_capitalism_simulation).
narrative_ontology:cs_drift_state('dcc6987d-dfd6-47d5-8772-ada95214c6ec', contemporary_economic_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('dcc6987d-dfd6-47d5-8772-ada95214c6ec', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, winning_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_analysts).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, losing_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, game_participants).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, game_participants).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, inevitable_wealth_concentration).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, rent_extraction_pedagogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The player who successfully accumulates all wealth and eliminates opponents, demonstrating the 'inevitable' outcome of the game's rules. They enforce the rules to their advantage.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, winning_player, beneficiary,
    powerful, immediate, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, winning_player, agenda_setter).

% Players who gradually lose their assets and liquidity, paying rents and taxes until they can no longer participate. They are structurally bound by the rules of the game.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, losing_players, payer,
    powerless, biographical, trapped, local).

% Players who have lost all their assets and are forced out of the game, serving as a clear demonstration of the 'elimination' aspect of the game's pedagogical truth.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Academics and commentators who interpret the game's outcomes as a direct, unvarnished demonstration of the inherent dynamics of monopoly capitalism, validating their theoretical frameworks.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_analysts, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_analysts, beneficiary).

% All players who engage with the game, accepting its rules and participating in its dynamics. They pay through their participation and potential losses, but also 'benefit' from the pedagogical lesson it offers.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_participants, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, game_participants, beneficiary).

% The designated banker and other players who collectively ensure the rules are followed, including rent collection, property auctions, and player elimination. Their role is to uphold the integrity of the game's 'demonstration'.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, rulebook_enforcers, agenda_setter,
    moderate, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, winning_player).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally understood framework for competitive play, resource acquisition, and wealth transfer within the game's defined boundaries.
% TRANSFER_FUNCTION: Systematically transfers wealth (money, property, and ultimately, player agency) from losing players to winning players, culminating in the elimination of all but one participant.
% ABSENT_VOICES: Players or designers who would advocate for alternative rule sets that promote redistribution, cooperation, or prevent player elimination. Their perspectives are excluded by the fixed, 'pedagogical' nature of the rulebook.
% DISAPPEARANCE_RATIONALE: If the rulebook and its enforcement vanished, the game of Monopoly as a structured activity and a pedagogical tool would cease to exist. The simulation of wealth concentration and elimination would no longer occur, fundamentally altering the social and analytical landscape it currently occupies.
% FOUNDING_PROBLEM: To create a game that explicitly simulates and demonstrates the inherent dynamics of monopolistic capitalism, including wealth concentration, rent extraction, and the eventual elimination of competitors.
% FOUNDING_PROBLEM_CORROBORATION: Game theorists, economists, and social critics, independent of the players' immediate experience, corroborate that the game continues to serve as a potent model for understanding real-world economic systems, supporting the claim that its founding problem remains relevant.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the systematic transfer of wealth from many to one, leading to elimination. Suppression (0.65) is inherent in the fixed rules that prevent players from opting out of rent payments or challenging property ownership. The theater ratio is low (0.10) because the game's function is seen as a direct, unvarnished demonstration, with minimal performative overhead. Accessibility collapse is high (0.85) as the 'inevitability' of the outcome is a core tenet of this reading, leaving few perceived alternatives within the game's framework. Resistance is moderate (0.40) as players may express frustration but are bound by the rules.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the winning player and the analysts, the game's structure is a 'mountain' demonstrating an economic truth. From the perspective of losing or eliminated players, it is a harsh, extractive system. The engine's computation of per-seat classification will highlight this divergence, showing how the same 'mountain' is experienced as a 'snare' by those trapped within its 'inevitable' dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   The winning player is the primary beneficiary, accumulating all assets. Monopoly capitalism analysts also benefit by having a clear, consistent model to validate their theories. Losing and eliminated players are the clear targets, bearing the costs of wealth transfer and eventual exclusion. Game participants generally pay through their engagement but are also seen as 'benefiting' from the pedagogical lesson. Rulebook enforcers (banker, other players) act as agenda-setters, upholding the system's integrity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, to demonstrate wealth concentration and elimination, is considered 'live' by this reading. The persistence of the game and its consistent outcomes are seen as ongoing validation of this mandate, preventing any perception of mandatrophy. The 'pedagogical truth' is considered timeless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_pedagogy,
    'Is the ''inevitable wealth concentration'' a genuine natural law demonstrated by the game, or a constructed pedagogical outcome designed by specific rules?',
    'Comparative analysis with alternative game designs (e.g., anti-monopoly games) that use different rule sets to produce different economic outcomes. If alternative rules yield different ''inevitabilities'', the claim of natural law is weakened.',
    'If constructed, the ''mountain'' claim would be reclassified, likely to a ''snare'' or ''tangled_rope'', as the ''naturalness'' cover story for extraction would be exposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_pedagogy, conceptual, 'Ambiguity between inherent economic truth and rule-based design.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.65) primarily structural (the fixed rules of the game) or internalized (players'' acceptance of the game''s premise as ''just how it works'')?',
    'Post-game debriefing and player surveys: if players express a strong belief in the ''fairness'' or ''inevitability'' of the outcome despite personal losses, it suggests a higher degree of internalized suppression.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as players carry the ''lesson'' with them, influencing their real-world economic perceptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in game play.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 1935, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t1935, monopoly_rulebook__extraction_demo_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(mono_tr_t1955, monopoly_rulebook__extraction_demo_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(mono_tr_t1975, monopoly_rulebook__extraction_demo_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(mono_tr_t1995, monopoly_rulebook__extraction_demo_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(mono_tr_t2010, monopoly_rulebook__extraction_demo_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(mono_tr_t2024, monopoly_rulebook__extraction_demo_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t1935, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(mono_be_t1955, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 1955, 0.7).
narrative_ontology:measurement(mono_be_t1975, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 1975, 0.72).
narrative_ontology:measurement(mono_be_t1995, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 1995, 0.73).
narrative_ontology:measurement(mono_be_t2010, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(mono_be_t2024, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t1935, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 1935, 0.6).
narrative_ontology:measurement(mono_su_t1955, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 1955, 0.61).
narrative_ontology:measurement(mono_su_t1975, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(mono_su_t1995, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 1995, 0.63).
narrative_ontology:measurement(mono_su_t2010, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(mono_su_t2024, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
