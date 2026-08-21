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
 *   Monopoly rulebook, where the rulebook is seen as the immutable,
 *   legitimate framework for competitive play. Strategic skill within these
 *   fixed rules determines outcomes, and any deviation (e.g., 'house rules')
 *   is considered noise that obscures competitive depth. This reading
 *   emphasizes text authority for ranking and comparison purposes. This is
 *   one reading of the 'monopoly_rulebook' kernel.
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
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '577979ab-1703-4d07-9e4a-7e4b8bb8d8a9').
narrative_ontology:cs_kernel_codification('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', fixed_text).
narrative_ontology:cs_authority_grounding('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', lineage).
narrative_ontology:cs_reading_relation('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', monopoly_rulebook__extraction_demo_reading, forecloses).
narrative_ontology:cs_reading_relation('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', foundational, rulebook_immutability_for_comparison).
narrative_ontology:cs_axiom_status(rulebook_immutability_for_comparison, holdable).
narrative_ontology:cs_axiom_grounding('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', rulebook_immutability_for_comparison, conventional).
narrative_ontology:cs_axiom('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', foundational, strategic_skill_determines_outcome).
narrative_ontology:cs_axiom_status(strategic_skill_determines_outcome, holdable).
narrative_ontology:cs_axiom_grounding('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', strategic_skill_determines_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', original_rulebook_as_competitive_standard).
narrative_ontology:cs_drift_state('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', contemporary_tournament_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('577979ab-1703-4d07-9e4a-7e4b8bb8d8a9', '').
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

% Administer tournaments based on the official rulebook, ensuring strict adherence to maintain competitive integrity. Their legitimacy derives from upholding the 'true' rules, which attracts serious players. Deviations are seen as undermining their authority.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, biographical, constrained, regional).

% Often prefer house rules for a more relaxed, less punitive game experience. They are not part of the competitive community that upholds the orthodoxy and their preferences are largely ignored in tournament settings.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, excluded,
    powerless, immediate, mobile, local).

% Analyze the game's mechanics and competitive meta-game, often documenting the evolution of strategies under the official rules. They are interested in the game's 'solved' states and optimal play, which requires a stable rule set.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_designers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal standard for competitive play, allowing players to compare skill and strategy across different tournaments and regions without ambiguity.
% TRANSFER_FUNCTION: Transfers competitive legitimacy and ranking authority to players who master the official rulebook, from those who prefer or rely on house rules.
% ABSENT_VOICES: Casual players and those who advocate for house rules are largely absent from the discourse of competitive play; they would argue for flexibility and social engagement over strict adherence to text.
% DISAPPEARANCE_RATIONALE: If the authority of the official rulebook vanished, competitive play would fragment into countless local variations, making universal ranking and comparison of skill impossible. The competitive community would dissolve or reorganize around new, potentially unstable, standards.
% FOUNDING_PROBLEM: The need for a consistent, unambiguous framework to determine winners and losers in competitive Monopoly, allowing for skill-based comparison and fair play.
% FOUNDING_PROBLEM_CORROBORATION: The competitive community and tournament organizers consistently attest to the ongoing need for a stable rulebook to maintain fair competition. Game designers also corroborate the importance of a fixed rule set for analytical purposes.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.08) because participation is voluntary and the constraint primarily serves a coordination function for competitive play, not rent extraction. Suppression is low (0.15) as it's maintained by social norms within the competitive community rather than active coercion. Accessibility collapse is high (0.85) because once one commits to competitive play, the official rulebook is the only viable option for meaningful comparison. Resistance is low (0.05) as the competitive community largely self-selects for adherence to these rules. Theater ratio is low (0.05) as the rules are genuinely applied for their stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the competitive community, this is a pure Rope, enabling fair competition. From the perspective of casual players (who are excluded from this reading's scope), the strict adherence to rules might be seen as overly rigid or even extractive of fun, but this reading does not account for their perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive Monopoly community is the primary beneficiary, gaining a stable framework for skill comparison. Tournament organizers are agenda-setters, enforcing the rules to maintain the integrity of their events. Casual players are excluded, as their preference for house rules is outside the scope of this competitive framework. No identifiable victims exist, as participation is voluntary and the constraint's primary function is coordination.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent coordination mechanism, or is its ''coordination'' function a cover for a deeper, more extractive dynamic (as suggested by the ''extraction_demo_reading'' sibling)?',
    'Analysis of player behavior and economic outcomes in long-term, high-stakes competitive play: if the game consistently produces extreme wealth concentration and player elimination despite ''fair'' rules, it supports the extraction reading.',
    'If the extraction reading is validated, this constraint would be reclassified from Rope to Snare, with the competitive community re-evaluated as either complicit beneficiaries or unwitting victims of a larger structural dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between coordination and extraction as the primary function of the rulebook.').

omega_variable(
    house_rules_as_social_correction,
    'Are ''house rules'' merely ''noise'' obscuring competitive depth, or are they a necessary social correction mechanism that prevents the game from becoming unplayable in casual settings (as suggested by the ''social_scaffold_reading'' sibling)?',
    'Sociological study of casual play groups: if groups consistently adopt house rules to maintain social cohesion and prevent player elimination, it supports the social scaffold reading.',
    'If the social scaffold reading is validated, the ''tournament orthodoxy'' reading''s dismissal of house rules would be seen as ignoring a vital social function, potentially reclassifying the orthodoxy as a Tangled Rope for casual players (coordinating competition while extracting social playability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rules_as_social_correction, empirical, 'Role of house rules in social play vs. competitive integrity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(mono_tr_t50, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(mono_be_t50, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 50, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(mono_su_t40, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(mono_su_t50, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 50, 0.15).


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
