% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Monopoly Rulebook Tournament Orthodoxy
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the tournament_orthodoxy_reading of
 *   the monopoly_rulebook kernel. The reading holds that the official rule
 *   text is the legitimate competitive framework where strategic skill
 *   determines outcomes; house rules are noise that obscures competitive
 *   depth; text authority is immutable for ranking and comparison. The
 *   reading rejects both the extraction_demo_reading (which frames the game
 *   as a necessary lesson in monopoly capitalism) and the
 *   social_scaffold_reading (which frames house rules as necessary
 *   corrections for social playability). This is a rope constraint: a
 *   voluntary coordination standard around a fixed text, with very low
 *   extractiveness (ε=0.05), negligible suppression, and a beneficiary set
 *   (the competitive community) that gains portable skill measurement without
 *   extracting from non-participants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook Tournament Orthodoxy").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '374c8e97-408a-4c21-a189-7a1b84349574').
narrative_ontology:cs_kernel_codification('374c8e97-408a-4c21-a189-7a1b84349574', fixed_text).
narrative_ontology:cs_authority_grounding('374c8e97-408a-4c21-a189-7a1b84349574', lineage).
narrative_ontology:cs_interpretation_layer_present('374c8e97-408a-4c21-a189-7a1b84349574').
narrative_ontology:cs_reading_relation('374c8e97-408a-4c21-a189-7a1b84349574', monopoly_rulebook__extraction_demo_reading, forecloses).
narrative_ontology:cs_reading_relation('374c8e97-408a-4c21-a189-7a1b84349574', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('374c8e97-408a-4c21-a189-7a1b84349574', foundational, textual_immutability_for_ranking).
narrative_ontology:cs_axiom_status(textual_immutability_for_ranking, holdable).
narrative_ontology:cs_axiom_grounding('374c8e97-408a-4c21-a189-7a1b84349574', textual_immutability_for_ranking, conventional).
narrative_ontology:cs_axiom('374c8e97-408a-4c21-a189-7a1b84349574', foundational, strategic_skill_determines_outcome).
narrative_ontology:cs_axiom_status(strategic_skill_determines_outcome, holdable).
narrative_ontology:cs_axiom_grounding('374c8e97-408a-4c21-a189-7a1b84349574', strategic_skill_determines_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('374c8e97-408a-4c21-a189-7a1b84349574', official_tournament_text_as_immutable_standard).
narrative_ontology:cs_drift_state('374c8e97-408a-4c21-a189-7a1b84349574', contemporary_competitive_circuit, gap(stable, minor, true)).
narrative_ontology:cs_created_at('374c8e97-408a-4c21-a189-7a1b84349574', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, textual_immutability_principle).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, strategic_skill_determines_outcome).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, comparative_ranking_requires_fixed_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tournament players, organizers, and rankers who coordinate around the official rulebook as a shared standard. They gain a portable skill credential, interoperable rankings, and a stable competitive ladder. Participation is voluntary; exit means playing a different game or variant without penalty.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community, beneficiary,
    organized, biographical, arbitrage, global).

% Players and community organizers who prefer variants with liquidity injections (Free Parking jackpots, no-auction properties, mercy rules). They are excluded from the tournament orthodoxy's ranking system but remain free to run their own events. Their objection is to the orthodoxy's claim that only the text produces valid competition.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, house_rule_advocates, excluded,
    moderate, immediate, mobile, local).

% Educators and commentators who read the rulebook as a demonstration of monopoly capitalism's inevitability (extraction_demo_reading). They are excluded from the competitive community's definition of legitimate play because they reject the premise that the game is a skill contest rather than a structural lesson.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, pedagogical_critics, excluded,
    moderate, biographical, mobile, global).

% Groups who treat Monopoly as a social scaffold (social_scaffold_reading), using house rules to prevent elimination and preserve the session. They are excluded from tournament rankings but lose nothing they value — their goal is sustained social coordination, not comparative measurement.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, social_play_organizers, excluded,
    organized, immediate, mobile, local).

% The formal authority (Hasbro, WPN, or tournament circuit) that publishes and maintains the official rule text. They administer the standard but do not extract rents from competitive play; their interest is brand coherence and IP control. They could change the text but treat it as fixed for ranking integrity.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rules_body, agenda_setter,
    institutional, generational, analytical, global).

% The indexical classification seat. Sees the rulebook as a coordination standard with near-zero extraction, voluntary participation, and a beneficiary set that gains portable skill measurement. Notes that the orthodoxy reading forecloses the extraction-demonstration reading logically (one cannot be both a pure skill contest and a necessary structural lesson), but coexists with the social-scaffold reading as a different party's live practice.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, immutable rule set that enables portable skill credentials, interoperable rankings, and stable comparative competition across time and geography. Solves the problem of 'what counts as a valid game of Monopoly' for competitive purposes.
% TRANSFER_FUNCTION: Moves no resources between parties. The constraint coordinates by fixing the standard; participants opt in voluntarily for the benefit of comparable measurement. No extraction flow exists.
% ABSENT_VOICES: House-rule advocates (excluded by the orthodoxy's definition of valid play), pedagogical critics who read the game as structural demonstration, and social-play organizers who prioritize session continuity over ranking. All three groups are present in the broader Monopoly ecosystem but are not seated in the competitive community's framework.
% DISAPPEARANCE_RATIONALE: If the tournament orthodoxy vanished, competitive Monopoly would lose its portable ranking system. Players would fragment into variant-specific ladders or abandon competitive play. The rules body would lose its coordination function. The competitive community's skill credential would become non-portable.
% FOUNDING_PROBLEM: Early competitive Monopoly suffered from variant chaos: every venue used different house rules, making rankings non-comparable and skill non-portable. The orthodoxy fixed the text as the single legitimate standard.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers and competitive players attest the problem remains live — without a fixed text, rankings fracture. House-rule advocates and social-play organizers corroborate from outside the beneficiary set that the orthodoxy solves a real coordination problem for the competitive community, even though they do not participate in it.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero because the constraint operates by voluntary opt-in — no one is forced to play tournament Monopoly, and the rules body collects no rents from competitive play. Suppression is negligible because alternatives (house rules, other games, not playing) remain fully accessible; the orthodoxy only governs what counts as 'tournament Monopoly.' Theater ratio is low (0.1) because the competitive function is genuine and the rules body's maintenance is functional, not performative. Accessibility collapse is low (0.15) because the constraint only collapses alternatives within the narrow domain of 'valid tournament play' — outside that domain, house rules flourish. Resistance is low (0.2) because the orthodoxy faces only rhetorical contestation from excluded voices, not structural resistance from trapped parties.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive community is the beneficiary (d ≈ 0.1) — they gain portable rankings and skill credentials. The rules body is the agenda setter (d ≈ 0.3) — they administer the standard but do not extract from it. House-rule advocates, pedagogical critics, and social-play organizers are excluded (d ≈ 0.5 symmetric) — they are not subject to the constraint because they do not participate in the competitive framework; their objection is to the orthodoxy's claim of exclusive legitimacy, not to being coerced by it. The analytical observer sees the full structure (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The orthodoxy reading has not undergone mandatrophy — its founding problem (variant chaos preventing portable rankings) remains live and the constraint continues to solve it without accumulating extractive layers. The mandate has not outlived its function. The constraint would only become a piton if the competitive community dissolved but the rules body continued maintaining the standard theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the monopoly_rulebook a single kernel with three contested readings, or are these three distinct constraints that merely share a label?',
    'Test ε-invariance: if the extraction_demo_reading and social_scaffold_reading require different ε values (high vs. moderate) and different beneficiary/victim structures to describe their claims, they are distinct constraints per the ε-invariance principle. This reading''s ε=0.05 is stable only if the referent is the tournament orthodoxy arrangement specifically.',
    'If the kernel decomposes into three separate constraints, each gets its own ε and classification. The tournament_orthodoxy_reading remains a rope. The extraction_demo_reading would likely be a snare or tangled_rope (high extraction, structural victims). The social_scaffold_reading would likely be a scaffold (transitional coordination with sunset). The network.affects_constraints links would map the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the monopoly_rulebook label covers one kernel with multiple readings or three structurally distinct constraints.').

omega_variable(
    coordination_vs_ideology_boundary,
    'Does the competitive community''s commitment to textual immutability serve pure coordination, or does it encode an ideological preference for fixed rules over adaptive play?',
    'Compare tournament Monopoly to other competitive games with living rule sets (chess, Go, fighting games with patches). If portable rankings persist under rule evolution, the immutability claim is ideological, not coordinative.',
    'If immutability is ideological, the constraint carries latent extractiveness — it excludes adaptive variants not because they break comparability but because they violate a normative commitment. This would raise ε and potentially shift classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_ideology_boundary, conceptual, 'Whether textual immutability is a coordination necessity or an ideological commitment masquerading as one.').

omega_variable(
    voluntary_participation_assumption,
    'Is participation in the tournament orthodoxy truly voluntary, or do network effects and institutional recognition create de facto coercion for aspiring competitive players?',
    'Survey competitive Monopoly players: would they play a variant if it offered equal recognition, prize support, and ranking portability? If no such variant exists, test whether the orthodoxy''s dominance is maintained by merit or by incumbent control of the recognition pipeline.',
    'If participation is de facto coerced by monopoly control of the competitive pipeline, the constraint acquires victims (aspiring players with no exit) and suppression rises. Classification could shift from rope to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_assumption, empirical, 'Whether the orthodoxy''s voluntary character holds under institutional network effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t6, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(mono_tr_t12, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(mono_tr_t18, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(mono_tr_t24, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mono_be_t6, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 6, 0.05).
narrative_ontology:measurement(mono_be_t12, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 12, 0.05).
narrative_ontology:measurement(mono_be_t18, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 18, 0.05).
narrative_ontology:measurement(mono_be_t24, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(mono_su_t6, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 6, 0.05).
narrative_ontology:measurement(mono_su_t12, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 12, 0.05).
narrative_ontology:measurement(mono_su_t18, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 18, 0.05).
narrative_ontology:measurement(mono_su_t24, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 24, 0.05).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% This reading (tournament_orthodoxy_reading) and the extraction_demo_reading are mutually foreclosing within a single framework: one cannot simultaneously hold that Monopoly is a pure skill contest with an immutable text AND that it is a necessary demonstration of monopoly capitalism's inevitable concentration. They coexist across different parties' commitments. The social_scaffold_reading coexists with both — it occupies a different practice domain (social play vs. competitive ranking vs. pedagogical demonstration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
