% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Monopoly Rulebook (Tournament Orthodoxy Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'tournament orthodoxy' reading of the
 *   Monopoly rulebook kernel. It asserts the rulebook's text as the
 *   immutable, legitimate framework for competitive play, where strategic
 *   skill determines outcomes. House rules are considered 'noise' that
 *   obscure competitive depth and undermine the integrity of ranking and
 *   comparison. This reading emphasizes coordination around a shared, fixed
 *   standard, with very low extraction and no identifiable victims,
 *   consistent with a Rope constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.15).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook (Tournament Orthodoxy Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '6f4909ad-9db6-4a65-86f9-e2900a14a3db').
narrative_ontology:cs_kernel_codification('6f4909ad-9db6-4a65-86f9-e2900a14a3db', fixed_text).
narrative_ontology:cs_authority_grounding('6f4909ad-9db6-4a65-86f9-e2900a14a3db', practice).
narrative_ontology:cs_reading_relation('6f4909ad-9db6-4a65-86f9-e2900a14a3db', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f4909ad-9db6-4a65-86f9-e2900a14a3db', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('6f4909ad-9db6-4a65-86f9-e2900a14a3db', foundational, rulebook_text_is_immutable).
narrative_ontology:cs_axiom_status(rulebook_text_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('6f4909ad-9db6-4a65-86f9-e2900a14a3db', rulebook_text_is_immutable, conventional).
narrative_ontology:cs_axiom('6f4909ad-9db6-4a65-86f9-e2900a14a3db', foundational, strategic_skill_is_sole_determinant).
narrative_ontology:cs_axiom_status(strategic_skill_is_sole_determinant, holdable).
narrative_ontology:cs_axiom_grounding('6f4909ad-9db6-4a65-86f9-e2900a14a3db', strategic_skill_is_sole_determinant, instrumental).
narrative_ontology:cs_reference_frame('6f4909ad-9db6-4a65-86f9-e2900a14a3db', pure_competitive_framework).
narrative_ontology:cs_drift_state('6f4909ad-9db6-4a65-86f9-e2900a14a3db', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6f4909ad-9db6-4a65-86f9-e2900a14a3db', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a standardized, immutable rule set that allows for fair comparison of strategic skill and competitive ranking. Voluntary participation in tournaments is predicated on this shared understanding of the rules. They see house rules as diluting the competitive integrity.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community, beneficiary,
    organized, biographical, mobile, global).

% Administer tournaments based on the official rulebook, ensuring strict adherence to maintain competitive legitimacy. Their authority derives from upholding the 'pure' game. Deviations (house rules) are seen as undermining their role and the event's integrity.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, biographical, constrained, regional).

% Often prefer house rules for a more forgiving, social experience. They are largely outside the competitive community's discourse and their preferences for rule modifications are dismissed as irrelevant to 'true' competitive play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, excluded,
    powerless, immediate, mobile, local).

% Analyze the game's mechanics and social dynamics, observing how different rule interpretations (including house rules) affect player engagement and strategic depth. They can offer insights into the game's design intent versus emergent play patterns.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_designers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, unambiguous framework for competitive play, allowing players to develop and compare strategic skill on a level playing field, and enabling consistent tournament organization and ranking.
% TRANSFER_FUNCTION: Transfers competitive legitimacy and status to players who master the official rule set, and to tournaments that strictly adhere to it. It transfers the burden of adaptation to players, rather than allowing rule modifications.
% ABSENT_VOICES: Casual players and proponents of house rules are excluded from the discourse on competitive legitimacy; they would argue for rules that prioritize social engagement and accessibility over strict competitive purity.
% DISAPPEARANCE_RATIONALE: If the authority of the official rulebook vanished, competitive play would fragment into countless local variations, making universal ranking and comparison impossible. The competitive community as it exists would dissolve, replaced by disparate local scenes.
% FOUNDING_PROBLEM: The need for a standardized, fair, and universally understood framework to enable competitive play and skill comparison in a complex board game.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers and the competitive community consistently attest to the ongoing need for a stable rulebook to maintain competitive integrity. Game designers also corroborate the foundational need for clear rules, though they may differ on the rigidity of interpretation.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.08, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very low (0.08) because participation is voluntary and the 'cost' is adherence to rules, which is also the 'benefit' of fair competition. Suppression is low (0.15) as it primarily involves social pressure within the competitive community to conform to the official rules, rather than coercive enforcement. Theater ratio is minimal (0.05) as the stated function (fair competition) is genuinely pursued. Accessibility collapse is high (0.85) because once one commits to competitive play, the alternatives to the official rulebook are largely irrelevant for comparison purposes. Resistance is low (0.05) within the competitive community itself, as adherence to the rulebook is a core tenet.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the competitive community, this is a pure Rope, enabling fair competition. From the perspective of casual players or those advocating for social play, the rigidity of the rulebook might be seen as a Snare, extracting fun or social cohesion. However, this reading explicitly rejects those alternative framings, focusing solely on competitive integrity.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive_monopoly_community is the primary beneficiary, gaining a stable framework for skill comparison. Tournament_organizers are agenda-setters, upholding the rules for the community's benefit. Casual_players are excluded, as their preferences for modified rules are outside the scope of this competitive framing. No direct victims are identified, as participation is voluntary and the constraint's function is seen as purely coordinative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rule_immutability_vs_design_intent,
    'Is the rulebook''s text truly immutable for competitive purposes, or does its original design intent (e.g., as a pedagogical tool) imply a different ''correct'' interpretation?',
    'Historical analysis of the game''s original design documents and creator''s statements, combined with a survey of early competitive play practices.',
    'If the original intent contradicts strict immutability, it could weaken the ''tournament orthodoxy'' claim, potentially shifting it towards a more ''tangled_rope'' if the competitive community actively suppresses alternative interpretations despite historical evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rule_immutability_vs_design_intent, conceptual, 'Ambiguity regarding the rulebook''s intended interpretive flexibility.').

omega_variable(
    competitive_purity_vs_player_base_growth,
    'Does strict adherence to the rulebook, by excluding casual players who prefer house rules, ultimately limit the growth and long-term viability of the competitive community?',
    'Longitudinal study comparing growth rates of competitive communities with strict rule adherence versus those that embrace more flexible rule sets or ''house rules''.',
    'If strict adherence demonstrably hinders growth, the ''beneficiary'' status of the competitive community might be re-evaluated, suggesting a hidden cost or a ''piton'' element where the constraint''s rigidity harms its own base over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competitive_purity_vs_player_base_growth, empirical, 'Trade-off between competitive purity and broader community engagement.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine ''tournament orthodoxy'' reading, or is it a ''snare'' that uses the rhetoric of competitive purity to maintain a specific power structure within the community?',
    'Analysis of resource allocation within the competitive community: do those who most strongly advocate for ''orthodoxy'' also disproportionately control resources or status, and do they actively suppress dissent beyond mere disagreement?',
    'If evidence points to active suppression and disproportionate benefit, the classification would shift from ''rope'' to ''snare'', and the ''beneficiary'' status would be re-evaluated as ''agenda_setter'' with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, empirical, 'Distinguishing genuine coordination from disguised extraction within the competitive community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t5, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mono_be_t5, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 15, 0.08).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(mono_su_t5, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
