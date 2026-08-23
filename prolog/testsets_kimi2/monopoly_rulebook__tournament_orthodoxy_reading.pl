% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__tournament_orthodoxy_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Monopoly Rulebook â Tournament Orthodoxy Reading
 *   domain: game_theory/social_coordination
 *
 * SUMMARY:
 *   This constraint story captures the tournament-orthodoxy reading of the
 *   Monopoly rulebook kernel: the rulebook is treated as an immutable,
 *   authoritative competitive framework within which strategic skill
 *   determines outcomes. House rules are defined as noise that obscures
 *   competitive depth, and text authority is fixed for ranking and comparison
 *   purposes. The reading rejects both the extraction-demo framing (the game
 *   as pedagogical demonstration of capitalism) and the social-scaffold
 *   framing (the game as requiring community correction to be playable). It
 *   is authored as a rope: a voluntary coordination standard with very low
 *   extraction, where participation is opt-in and the primary beneficiary is
 *   the competitive community.
 *
 * KEY AGENTS:
 *   - competitive_players: Primary beneficiary (moderate/mobile) â gains cross-context comparability and skill-ranking legitimacy from the fixed standard
 *   - tournament_arbiters: Agenda setter (organized/mobile) â administers and interprets the rulebook without extracting surplus
 *   - house_rule_community: Excluded voice (moderate/mobile) â structurally excluded from tournament legitimacy because their preferred variants are defined as non-competitive noise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.12).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook â Tournament Orthodoxy Reading").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '1292d72a-ede8-41dd-8b67-82b00f392ced').
narrative_ontology:cs_kernel_codification('1292d72a-ede8-41dd-8b67-82b00f392ced', fixed_text).
narrative_ontology:cs_authority_grounding('1292d72a-ede8-41dd-8b67-82b00f392ced', lineage).
narrative_ontology:cs_interpretation_layer_present('1292d72a-ede8-41dd-8b67-82b00f392ced').
narrative_ontology:cs_reading_relation('1292d72a-ede8-41dd-8b67-82b00f392ced', monopoly_rulebook__extraction_demo_reading, forecloses).
narrative_ontology:cs_reading_relation('1292d72a-ede8-41dd-8b67-82b00f392ced', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('1292d72a-ede8-41dd-8b67-82b00f392ced', foundational, immutable_text_authority).
narrative_ontology:cs_axiom_status(immutable_text_authority, holdable).
narrative_ontology:cs_axiom_grounding('1292d72a-ede8-41dd-8b67-82b00f392ced', immutable_text_authority, conventional).
narrative_ontology:cs_axiom('1292d72a-ede8-41dd-8b67-82b00f392ced', foundational, competitive_skill_primacy).
narrative_ontology:cs_axiom_status(competitive_skill_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1292d72a-ede8-41dd-8b67-82b00f392ced', competitive_skill_primacy, instrumental).
narrative_ontology:cs_reference_frame('1292d72a-ede8-41dd-8b67-82b00f392ced', classical_competitive_framework).
narrative_ontology:cs_drift_state('1292d72a-ede8-41dd-8b67-82b00f392ced', contemporary_casual_gaming_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1292d72a-ede8-41dd-8b67-82b00f392ced', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in ranked tournaments where the immutable rulebook guarantees cross-event comparability. They benefit from a level playing field that allows skill to determine outcomes and rankings, and they can exit to casual play at any time.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_players, beneficiary,
    moderate, biographical, mobile, global).

% Administer competitive events by enforcing the official rulebook, issuing clarifications for edge cases, and disqualifying house-rule variants. They maintain the coordination standard without extracting surplus from participants.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_arbiters, agenda_setter,
    organized, biographical, mobile, global).

% Prefers localized rule variants that extend liquidity, prevent elimination, or customize play for social enjoyment. They are structurally excluded from tournament legitimacy because their preferred modes are defined as noise rather than valid competitive alternatives, though they remain free to play casually outside the ranked framework.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, house_rule_community, excluded,
    moderate, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, fixed rule set for competitive play so that strategic skill can be compared across tournaments and players without variation introduced by localized modifications.
% TRANSFER_FUNCTION: Moves authority over legitimate play from local table-level negotiation to centralized text interpretation; transfers ranking legitimacy and cross-context comparability to competitive players who demonstrate skill under standardized conditions.
% ABSENT_VOICES: House-rule advocates who treat gameplay as primarily social rather than competitive; extraction-demo readers who see the rulebook as a predetermined demonstration of structural inequality; and social-scaffold advocates who believe community correction is necessary for playable outcomes. They are absent from the tournament legitimization conversation because the orthodox framework defines their framings as illegitimate noise.
% DISAPPEARANCE_RATIONALE: If the immutable rulebook constraint disappeared, tournament rankings would become incomparable across events, competitive integrity would dissolve into local variant regimes, and the coordination function enabling cross-context skill comparison would collapse. The competitive community would need to renegotiate a new standard or fragment into incompatible circuits.
% FOUNDING_PROBLEM: Board-game competition under variable local rules produces incomparable outcomes and irresolvable legitimacy disputes; without a fixed, authoritative standard, claims of skill superiority are unverifiable and rankings cannot generalize beyond a single table.
% FOUNDING_PROBLEM_CORROBORATION: Competitive players and tournament organizers attest to the ongoing need for fixed rules. No external corroborating body outside the benefiting competitive community independently verifies that the specific Monopoly rulebook (as opposed to another standard or house-rule set) is the necessary solution; corroboration from outside the beneficiary set is absent.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).
:- end_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the constraint is a voluntary coordination standard: players opt into tournament play and can exit to casual contexts. Suppression is low (0.12) because the only exclusion is definitional (house rules are not recognized for ranking), not coercive. Theater ratio is low (0.08) because most rule enforcement is functional, though formalization has generated some interpretive overhead. Accessibility collapse is moderate (0.40): within the tournament frame, house rules collapse as legitimate alternatives, but outside the frame they remain widely practiced. Resistance is low (0.20) because the constraint meets only mild friction from house-rule advocates who do not seek tournament entry. The metrics and claimed type are authored independently: the engine may compute a slightly different per-seat classification, and that divergence is the intended signal.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (tournament arbiters) and beneficiary seat (competitive players) largely converge in experiencing this constraint as functional coordination. The excluded seat (house-rule community) experiences it as a definitional boundary that renders their preferred play invisible, but because their exit is mobile and the frame is voluntary, the engine should not compute them as high-extraction targets. The divergence is between inclusion and exclusion, not between beneficiary and victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Competitive players are declared beneficiaries, yielding a low directionality value and thus negligible effective extraction. Tournament arbiters are agenda setters with mobile exit; their structural relationship is administrative rather than extractive. The house-rule community is not declared a victim because participation is voluntary and exit is unobstructed; their exclusion from legitimacy is a categorical boundary, not an asymmetric extraction. No directionality overrides are needed because the derivation chain produces accurate d values from the beneficiary declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâvariable rules preventing comparable competitive outcomesâremains live. Tournament circuits still require fixed standards to maintain ranking integrity. There is no evidence that the mandate has outlived its function, so mandatrophy is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tournament_reading_kernel_position,
    'Does treating the rulebook as immutable competitive text foreclose legitimate alternative framings (pedagogical extraction, social scaffold) by definitional fiat rather than structural refutation?',
    'Cross-reading empirical comparison: test whether tournament-orthodox play produces measurably different social and economic outcomes than the sibling readings predict, and whether those outcomes are better explained by skill variance or structural inevitability.',
    'If extraction-demo or social-scaffold predictions hold under orthodox conditions, the orthodox reading''s epsilon is understated and its rope classification may be a definitional boundary rather than a structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tournament_reading_kernel_position, conceptual, 'Whether the orthodox reading''s low epsilon is achieved by excluding alternative frames').

omega_variable(
    house_rule_noise_or_coordination,
    'Are house rules genuinely noise obscuring competitive depth, or do they represent an alternative coordination mechanism that the tournament orthodoxy suppresses by standardization?',
    'Comparative analysis of game enjoyment, engagement duration, and social cohesion under strict rulebook versus popular house rules.',
    'If house rules solve genuine coordination problems (e.g., preventing player elimination, maintaining engagement), the orthodox reading''s suppression metric is understated and the constraint may function as a tangled rope for casual players pressured into tournament norms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(house_rule_noise_or_coordination, empirical, 'Whether house rules are noise or alternative coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_orthodoxy_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(monopoly_orthodoxy_tr_t5, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 5, 0.03).
narrative_ontology:measurement(monopoly_orthodoxy_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement(monopoly_orthodoxy_tr_t15, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 15, 0.04).
narrative_ontology:measurement(monopoly_orthodoxy_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(monopoly_orthodoxy_tr_t25, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 25, 0.06).
narrative_ontology:measurement(monopoly_orthodoxy_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.08).

% Extraction over time
narrative_ontology:measurement(monopoly_orthodoxy_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(monopoly_orthodoxy_be_t5, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(monopoly_orthodoxy_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(monopoly_orthodoxy_be_t15, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(monopoly_orthodoxy_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(monopoly_orthodoxy_be_t25, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(monopoly_orthodoxy_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__tournament_orthodoxy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, social_scaffold_reading).

% DUAL FORMULATION NOTE:
% The monopoly_rulebook kernel decomposes into three structurally distinct constraints per the Îµ-invariance principle: tournament_orthodoxy_reading (coordination standard, low epsilon), extraction_demo_reading (pedagogical extraction frame, high epsilon), and social_scaffold_reading (corrective coordination, moderate epsilon). Each reading assigns a different structural role to house rules, text mutability, and the function of elimination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
