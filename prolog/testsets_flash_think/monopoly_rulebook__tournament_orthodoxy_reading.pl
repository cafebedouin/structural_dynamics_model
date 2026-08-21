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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Monopoly Rulebook: Tournament Orthodoxy Reading
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'tournament orthodoxy' reading of the
 *   Monopoly rulebook, where the rulebook is seen as the immutable,
 *   legitimate framework for competitive play. Strategic skill is believed to
 *   determine outcomes, and any 'house rules' are considered noise that
 *   obscures competitive depth. This reading emphasizes the rulebook's text
 *   authority for ranking and comparison purposes. It is one reading of the
 *   'monopoly_rulebook' kernel, distinct from readings focused on extraction
 *   or social scaffolding.
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
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook: Tournament Orthodoxy Reading").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__tournament_orthodoxy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '21b9de46-ee46-470a-a8e7-46be60bba1da').
narrative_ontology:cs_kernel_codification('21b9de46-ee46-470a-a8e7-46be60bba1da', fixed_text).
narrative_ontology:cs_authority_grounding('21b9de46-ee46-470a-a8e7-46be60bba1da', practice).
narrative_ontology:cs_reading_relation('21b9de46-ee46-470a-a8e7-46be60bba1da', monopoly_rulebook__extraction_demo_reading, forecloses).
narrative_ontology:cs_reading_relation('21b9de46-ee46-470a-a8e7-46be60bba1da', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('21b9de46-ee46-470a-a8e7-46be60bba1da', foundational, rulebook_as_sole_arbiter_of_skill).
narrative_ontology:cs_axiom_status(rulebook_as_sole_arbiter_of_skill, holdable).
narrative_ontology:cs_axiom_grounding('21b9de46-ee46-470a-a8e7-46be60bba1da', rulebook_as_sole_arbiter_of_skill, conventional).
narrative_ontology:cs_axiom('21b9de46-ee46-470a-a8e7-46be60bba1da', foundational, competitive_integrity_is_paramount).
narrative_ontology:cs_axiom_status(competitive_integrity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('21b9de46-ee46-470a-a8e7-46be60bba1da', competitive_integrity_is_paramount, deontological).
narrative_ontology:cs_reference_frame('21b9de46-ee46-470a-a8e7-46be60bba1da', pure_competitive_meritocracy).
narrative_ontology:cs_drift_state('21b9de46-ee46-470a-a8e7-46be60bba1da', contemporary_gaming_culture, gap(stable, minor, true)).
narrative_ontology:cs_created_at('21b9de46-ee46-470a-a8e7-46be60bba1da', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__tournament_orthodoxy_reading, casual_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, meritocracy_of_skill).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, fair_competition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are players who value the game's competitive integrity and believe that strict adherence to the official rulebook is essential for fair ranking and skill-based outcomes. They benefit from a clear, stable competitive framework.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_monopoly_community, beneficiary,
    organized, biographical, mobile, global).

% Responsible for enforcing the official rulebook in competitive play. They invest resources in maintaining the integrity of tournaments and rankings, which relies on consistent rule application. Their legitimacy depends on upholding the orthodoxy.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, biographical, constrained, global).

% Participate in the game but may find the strict ruleset less forgiving or enjoyable than modified 'house rules'. They pay in terms of adherence to a rigid framework that might not optimize for their casual enjoyment, but they can easily opt out of competitive play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, payer,
    moderate, immediate, mobile, local).

% Believe that 'house rules' improve the game's playability, social dynamics, or pedagogical value. Their proposals are actively rejected by the tournament orthodoxy, which views such modifications as undermining competitive depth and integrity. They are excluded from the formal competitive discourse.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, house_rule_advocates, excluded,
    moderate, biographical, constrained, local).

% Academics or game theorists who study the structural properties of Monopoly and its rulebook, analyzing its competitive dynamics, social effects, and the various interpretations of its rules. They are outside the direct competitive or social stakes.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__tournament_orthodoxy_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__tournament_orthodoxy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universally accepted, immutable framework for competitive play, allowing players to compare skill and strategy across different tournaments and contexts without ambiguity.
% TRANSFER_FUNCTION: Transfers the authority for defining 'fair play' and 'skill' from individual groups or 'house rules' to the official, immutable text, ensuring consistent competitive standards.
% ABSENT_VOICES: Advocates for 'house rules' and alternative pedagogical or social framings of the game are excluded from the discourse of competitive integrity. They would argue for rule flexibility to enhance social play or learning, but their views are dismissed as irrelevant to competitive depth.
% DISAPPEARANCE_RATIONALE: If the rulebook's authority as the immutable competitive framework vanished, competitive Monopoly as a recognized skill-based endeavor would collapse. Rankings would become meaningless, tournaments would lose legitimacy, and the community built around competitive play would fragment, as there would be no shared standard for evaluating skill.
% FOUNDING_PROBLEM: The problem of establishing a clear, unambiguous, and universally accepted standard for competitive play and skill assessment in Monopoly, preventing arbitrary rule changes from undermining competitive integrity.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers and the competitive community consistently attest that the problem of maintaining competitive integrity and a level playing field is ongoing. Independent game analysts and sports ethicists also corroborate the necessity of a stable rule framework for legitimate competition, supporting the 'live' status of the founding problem.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is very low (0.08) because this reading views the rulebook as a pure coordination mechanism for fair competition, not a source of rent. Suppression is low (0.15) as participation in competitive play is voluntary, though 'house rule' advocates are actively excluded from the competitive discourse. Theater ratio is negligible (0.05) because the enforcement of rules is seen as directly functional to competitive integrity, not performative. Accessibility collapse is high (0.85) because the official rules define the very possibility of 'competitive Monopoly'; alternatives (like house rules) are seen as fundamentally different games. Resistance is low (0.10) from within the competitive community, as they largely accept the rules.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the competitive community and tournament organizers, the rulebook is a pure Rope, enabling fair competition. From the perspective of 'house rule' advocates, the orthodoxy's rigid enforcement suppresses alternative, potentially more socially beneficial, ways of playing the game. The engine will compute this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'competitive_monopoly_community' and 'tournament_organizers' are beneficiaries, as they gain from the stable, clear competitive framework. 'Casual_players' are payers in terms of adhering to a strict framework, but their exit options are mobile. 'House_rule_advocates' are excluded, as their alternative framings are actively rejected by the orthodoxy, making them targets of the constraint's enforcement of its own legitimacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a pure coordination mechanism for competitive integrity, or does its rigid enforcement inadvertently serve other functions (e.g., maintaining a specific power dynamic within the gaming community)?',
    'Analysis of community power structures and resource allocation within competitive Monopoly, particularly how adherence to orthodoxy impacts access and influence for different player groups.',
    'If other functions are identified, the constraint''s extractiveness or suppression might be higher than currently assessed, potentially shifting its classification from Rope towards Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the constraint''s primary function beyond pure competitive coordination.').

omega_variable(
    house_rules_as_noise_or_adaptation,
    'Are ''house rules'' genuinely ''noise obscuring competitive depth'' as this reading claims, or are they legitimate adaptations that address flaws in the original rulebook for different play contexts?',
    'Empirical studies comparing player engagement, social cohesion, and perceived fairness in games played under official rules versus common ''house rules''.',
    'If ''house rules'' are found to be legitimate adaptations, the orthodoxy''s suppression of them would be re-evaluated, potentially increasing the constraint''s suppression metric and challenging its ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rules_as_noise_or_adaptation, empirical, 'The nature of ''house rules'' and their impact on game experience and competitive integrity.').

omega_variable(
    immutability_of_text_authority,
    'Is the rulebook''s text authority truly immutable for ranking/comparison purposes, or is its perceived immutability a social construct maintained by the competitive community?',
    'Historical analysis of rule changes or interpretations in other competitive games, and sociological study of how ''immutability'' claims are sustained within communities.',
    'If immutability is primarily a social construct, the constraint''s persistence would be seen as more dependent on active social enforcement rather than inherent textual authority, potentially increasing its suppression metric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immutability_of_text_authority, conceptual, 'The source and nature of the rulebook''s claimed immutability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t4, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 4, 0.05).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 8, 0.05).
narrative_ontology:measurement(mono_tr_t12, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 12, 0.05).
narrative_ontology:measurement(mono_tr_t16, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(mono_be_t4, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 4, 0.07).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 8, 0.08).
narrative_ontology:measurement(mono_be_t12, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 12, 0.08).
narrative_ontology:measurement(mono_be_t16, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 16, 0.08).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(mono_su_t4, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 4, 0.13).
narrative_ontology:measurement(mono_su_t8, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 8, 0.14).
narrative_ontology:measurement(mono_su_t12, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 12, 0.14).
narrative_ontology:measurement(mono_su_t16, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 16, 0.15).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'tournament_orthodoxy_reading' of the 'monopoly_rulebook' kernel. It is structurally distinct from the 'extraction_demo_reading' and 'social_scaffold_reading', each representing different interpretations of the same core rulebook.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
