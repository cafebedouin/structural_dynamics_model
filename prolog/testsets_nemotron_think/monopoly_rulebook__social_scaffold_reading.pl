% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__social_scaffold_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: monopoly_rulebook__social_scaffold_reading
 *   human_readable: Monopoly House-Rule Scaffold for Social Playability
 *   domain: social_coordination/game_theory
 *
 * SUMMARY:
 *   Monopoly's printed rules mandate a ruthless elimination game: auctions,
 *   no Free Parking payout, no loans, bankruptcy removes you. In social play
 *   (families, friend groups), this is experienced as a coordination failure
 *   — the game outlasts the social frame. House rules (Free Parking jackpot,
 *   rent forgiveness, loans, no auctions) emerge universally as a community
 *   correction. They redistribute from leaders to laggards, inject liquidity,
 *   and prevent elimination, extending the game to 3+ hours of shared
 *   activity. The scaffold is the house-rule layer; the kernel is the
 *   rulebook text. This reading claims the rulebook *requires* this
 *   correction to be socially playable — the raw text is structurally
 *   incomplete for social coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.42).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.25).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly House-Rule Scaffold for Social Playability").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "social_coordination/game_theory").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4').
narrative_ontology:cs_kernel_codification('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', fixed_text).
narrative_ontology:cs_authority_grounding('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', practice).
narrative_ontology:cs_interpretation_layer_present('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4').
narrative_ontology:cs_reading_relation('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', foundational, social_playability_requires_correction).
narrative_ontology:cs_axiom_status(social_playability_requires_correction, holdable).
narrative_ontology:cs_axiom_grounding('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', social_playability_requires_correction, conventional).
narrative_ontology:cs_axiom('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', foundational, prolonged_coordination_over_fidelity).
narrative_ontology:cs_axiom_status(prolonged_coordination_over_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', prolonged_coordination_over_fidelity, instrumental).
narrative_ontology:cs_reference_frame('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', raw_rulebook_text).
narrative_ontology:cs_drift_state('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', house_rule_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('276f2be4-e6e1-4e2f-ace7-15ba4db0f1a4', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, dominant_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, vulnerable_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, social_playability_requires_correction).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, prolonged_coordination_over_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The group of friends or family who gather to play. They collectively maintain house rules (Free Parking jackpot, no auctions, loans between players, etc.) because the raw elimination game breaks the social evening — someone goes bankrupt at hour two and sits out for hours. The house rules keep everyone at the table. The group decides which rules to adopt; the constraint is their shared practice.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group, agenda_setter,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, social_group, beneficiary).

% Players who build a commanding lead under raw rules. House rules (redistribution via Free Parking, rent forgiveness, loans) systematically transfer assets away from them, slowing their victory. They could insist on raw rules, but doing so risks breaking the social frame — the group stops playing. Their exit is constrained by the social cost of being 'that player'.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, dominant_players, payer,
    moderate, immediate, constrained, local).

% Players who would be eliminated early under raw rules. House rules give them liquidity injections (Free Parking windfalls, interest-free loans, rent immunity deals) that keep them in the game. They benefit from the scaffold but do not control it — the group consensus controls it. Their continued participation is the scaffold's output.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, vulnerable_players, beneficiary,
    moderate, immediate, constrained, local).

% Players who want the game played by the printed rules for strategic depth and comparability. In a social group committed to house rules, they are structurally excluded — insisting on raw rules marks them as antisocial. They either conform, stop playing with this group, or form a separate tournament table. Their voice is not in the house-rule negotiation.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_purists, excluded,
    moderate, biographical, trapped, local).

% The printed Monopoly rules (Hasbro/ Parker Brothers). The text mandates auctions, no Free Parking payout, no loans, elimination on bankruptcy. It does not enforce itself in social play; it is the kernel that the house-rule scaffold corrects. The text's authority is cited by competitive_purists and ignored by the social_group when it conflicts with cohesion.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_text, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, rulebook_text).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The house-rule scaffold coordinates a social evening: it keeps all players engaged for 3+ hours, prevents early elimination from fracturing the group, and transforms a zero-sum elimination game into a prolonged positive-sum social activity. The raw rulebook fails at this; the scaffold succeeds.
% TRANSFER_FUNCTION: Moves assets (cash, properties, immunity) from dominant_players to vulnerable_players via house rules: Free Parking jackpots funded by fees/taxes, rent forgiveness agreements, interest-free loans, property trades at non-market prices. The transfer is the mechanism of coordination — it buys continued participation.
% ABSENT_VOICES: Competitive_purists are structurally excluded from the house-rule negotiation; their preference for raw rules is treated as a threat to the social frame. Would-be players who avoid Monopoly entirely because of its reputation for brutal elimination are also absent — the scaffold does not reach them because they never sit down.
% DISAPPEARANCE_RATIONALE: If the house-rule scaffold vanished overnight, the social_group would either (a) stop playing Monopoly because the raw game destroys the evening, (b) fracture into competitive_purists who play raw and vulnerable_players who quit, or (c) invent a new scaffold. The 3+ hour social coordination collapses without the redistribution mechanism.
% FOUNDING_PROBLEM: The printed Monopoly rules produce a 30-60 minute strategic game followed by 2-3 hours of one player dominating and others sitting out eliminated — socially unacceptable for a group activity. The scaffold was built to stretch the game into a 3+ hour shared experience where everyone stays at the table.
% FOUNDING_PROBLEM_CORROBORATION: Board-game design literature (e.g., Engelstein & Shalev, 'Building Blocks of Tabletop Game Design') identifies player elimination as a known social-coordination failure mode for long games. Independent game-design commentary (BoardGameGeek designer forums, 2010s-present) consistently cites Monopoly's elimination mechanic as the primary reason house rules exist — not strategic preference. The rulebook publisher (Hasbro) has never endorsed house rules as necessary; the corroboration comes from design analysis outside the benefiting social groups.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__social_scaffold_reading_tests).
:- end_tests(monopoly_rulebook__social_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because house rules systematically transfer value from dominant_players to vulnerable_players — but the transfer is voluntary, consensual, and serves the coordination function. Suppression is low (0.25) because no one is forced to play; the constraint is the group's shared practice, not an external mandate. Theater ratio (0.35) reflects that some house-rule rituals (ceremonial Free Parking collection, performative loan negotiations) are performative maintenance of the social frame. Accessibility collapse (0.45) is moderate: raw rules are always available, but the social cost of insisting on them is high. Resistance (0.3) is low: dominant_players occasionally push for raw rules but usually conform.
 *
 * PERSPECTIVAL GAP:
 *   From the social_group's seat, the scaffold is a rope (pure coordination — everyone wins by staying together). From dominant_players' seat, it is a tangled_rope (coordination + extraction — they pay to keep the game social). From competitive_purists' seat, it is a snare (pure extraction — their preferred game is suppressed). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The social_group (agenda_setter + beneficiary) sits at d ≈ 0.2 — they author and benefit from the scaffold. Dominant_players (payer) sit at d ≈ 0.7 — they bear the redistribution but stay for the social frame. Vulnerable_players (beneficiary) sit at d ≈ 0.1 — they receive transfers and continued play. Competitive_purists (excluded) sit at d ≈ 0.9 — the scaffold actively excludes their preference. The rulebook_text (observer) is analytical, d ≈ 0.5 — it neither benefits nor pays.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (raw elimination breaks social play) remains live — Monopoly's raw rules still eliminate players in 30-60 minutes. The scaffold has not atrophied into a piton because it still performs its coordination function; theater_ratio has risen but not crossed 0.5. The sunset clause is the game's end or the group's dissolution — each session is a scaffold instance. No single agent captures the extraction; it circulates within the group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the social_scaffold_reading a distinct constraint from the raw rulebook, or a descriptive overlay on the same constraint?',
    'Test ε-invariance: measure extraction/suppression of raw rulebook alone vs. rulebook+house-rules in social play. If ε differs substantially, they are distinct constraints. The engine''s ε-invariance principle requires separate stories for distinct ε.',
    'If distinct, this story correctly models the scaffold as a separate constraint with its own ε (0.42). If not distinct, the extraction measured here belongs to the rulebook kernel and this story double-counts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the house-rule scaffold is a separate constraint or a measurement variant of the rulebook.').

omega_variable(
    scaffold_sunset_ambiguity,
    'Does the scaffold have a genuine sunset clause, or has it become a permanent fixture (piton drift)?',
    'Track whether social groups ever *stop* using house rules and return to raw rules, or whether house rules persist across generations of play. If no group ever sunsets the scaffold, has_sunset_clause is nominal only.',
    'If sunset is nominal, the constraint drifts from scaffold toward piton (theatrical maintenance of a coordination function that never transitions). The engine''s Piton gate would reclassify.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_sunset_ambiguity, empirical, 'Whether the scaffold''s transitional justification is real or ceremonial.').

omega_variable(
    extraction_beneficiary_boundary,
    'Does the redistribution via house rules extract from dominant_players, or does it purchase their continued participation in a game they would otherwise quit?',
    'Observe dominant_player behavior when house rules are suspended: do they play more aggressively (suggesting they prefer raw rules) or do they disengage (suggesting the scaffold serves them too)?',
    'If dominant_players also benefit (the scaffold prevents *their* boredom from a trivial win), the victim/beneficiary line blurs and extraction is lower than measured. The engine''s directionality derivation would shift d for dominant_players toward symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_boundary, empirical, 'Whether dominant_players are net payers or net beneficiaries of the scaffold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_social_scaffold_tr_t1935, monopoly_rulebook__social_scaffold_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t1960, monopoly_rulebook__social_scaffold_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t1985, monopoly_rulebook__social_scaffold_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t2000, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t2015, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t2025, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(monopoly_social_scaffold_be_t1935, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 1935, 0.15).
narrative_ontology:measurement(monopoly_social_scaffold_be_t1960, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(monopoly_social_scaffold_be_t1985, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(monopoly_social_scaffold_be_t2000, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(monopoly_social_scaffold_be_t2015, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(monopoly_social_scaffold_be_t2025, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(monopoly_social_scaffold_su_t1935, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 1935, 0.05).
narrative_ontology:measurement(monopoly_social_scaffold_su_t1960, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(monopoly_social_scaffold_su_t1985, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement(monopoly_social_scaffold_su_t2000, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(monopoly_social_scaffold_su_t2015, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 2015, 0.23).
narrative_ontology:measurement(monopoly_social_scaffold_su_t2025, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.08).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the monopoly_rulebook kernel alongside extraction_demo_reading and tournament_orthodoxy_reading. The kernel is the printed rulebook text; each reading instantiates a different constraint with different ε, beneficiaries, and type. This reading (social_scaffold) has moderate ε (0.42) and scaffold type; extraction_demo has higher ε and snare/tangled_rope type; tournament_orthodoxy has near-zero ε and mountain/rope type. The ε-invariance principle requires separate stories because the same text measured under different framings yields different ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
