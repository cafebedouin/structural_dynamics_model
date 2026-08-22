% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Monopoly Rulebook Social Scaffold: House Rules for Group Coordination
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   In social contexts, Monopoly is unplayable as written. The rulebook
 *   mandates wealth concentration, rapid elimination, and a winner-takes-all
 *   endgame; player groups collectively adopt house rules to prevent this
 *   outcome and preserve their ability to play as a coordinated group for 2-4
 *   hours. These house rules (free parking pools, temporary loans, negotiated
 *   rents, delayed bankruptcy) are the constraint's operative form — they
 *   REPLACE the rulebook's default endgame mechanism for the duration of the
 *   play session. This is a kernel reading conflict: the social scaffold
 *   reading privileges group cohesion and extended shared play over textual
 *   fidelity. The rulebook as written (extraction_demo_reading) is read as
 *   pedagogical truth-telling about capitalism; the tournament reading
 *   insists the text is the legitimate competitive standard; the social
 *   reading reframes the same text as a coordination problem requiring
 *   collective solution. The extraction measured in this story (ε = 0.42)
 *   reflects the rulebook AS MODIFIED by house rules — the operative
 *   constraint of social play. This is neither the rulebook alone (which
 *   would have higher ε, ~0.55-0.65) nor an absence of constraint (house
 *   rules are actively enforced), but a TEMPORARY MODIFICATION that solves a
 *   real social coordination problem and then dissolves when the session
 *   ends.
 *
 * KEY AGENTS:
 *   - social_player_group: 2-6 players gathered for shared entertainment; collectively author, enforce, and renew house rules each session; benefit from extended play and preserved group cohesion; are the locus of the constraint's coordination function
 *   - rulebook_text: fixed, formal, specifies harsh endgame; passive; players choose to modify or follow it; does not adapt
 *   - tournament_players: excluded; would oppose house rules as competitive noise; represent an incommensurable reading of the kernel
 *   - pedagogical_advocates: excluded; argue house rules obscure the text's educational message about capitalism; represent a second incommensurable reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.42).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.28).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook Social Scaffold: House Rules for Group Coordination").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '3f23d183-b42e-4d42-af5b-94490bddf1c3').
narrative_ontology:cs_kernel_codification('3f23d183-b42e-4d42-af5b-94490bddf1c3', formalized).
narrative_ontology:cs_authority_grounding('3f23d183-b42e-4d42-af5b-94490bddf1c3', extraction).
narrative_ontology:cs_interpretation_layer_present('3f23d183-b42e-4d42-af5b-94490bddf1c3').
narrative_ontology:cs_reading_relation('3f23d183-b42e-4d42-af5b-94490bddf1c3', monopoly_rulebook__extraction_demo_reading, forecloses).
narrative_ontology:cs_reading_relation('3f23d183-b42e-4d42-af5b-94490bddf1c3', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('3f23d183-b42e-4d42-af5b-94490bddf1c3', foundational, social_coordination_preserves_group_over_text_fidelity).
narrative_ontology:cs_axiom_status(social_coordination_preserves_group_over_text_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('3f23d183-b42e-4d42-af5b-94490bddf1c3', social_coordination_preserves_group_over_text_fidelity, deontological).
narrative_ontology:cs_axiom('3f23d183-b42e-4d42-af5b-94490bddf1c3', secondary, temporary_modification_valid_for_social_purpose).
narrative_ontology:cs_axiom_status(temporary_modification_valid_for_social_purpose, holdable).
narrative_ontology:cs_axiom_grounding('3f23d183-b42e-4d42-af5b-94490bddf1c3', temporary_modification_valid_for_social_purpose, instrumental).
narrative_ontology:cs_reference_frame('3f23d183-b42e-4d42-af5b-94490bddf1c3', rule_modified_monopoly_for_social_play).
narrative_ontology:cs_drift_state('3f23d183-b42e-4d42-af5b-94490bddf1c3', post_session_expiration, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('3f23d183-b42e-4d42-af5b-94490bddf1c3', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_player_group).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, social_coordination_over_fidelity).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, redistributive_fairness_norm).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, game_duration_pacing_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A group of 2-6 players gather to play Monopoly as a social activity. They collectively author and enforce house rules that modify the published rulebook's harsh endgame: free parking pools, temporary loans to prevent bankruptcy, negotiated rent payments, and slower elimination. They benefit from this arrangement because they can play for 2-4 hours as a group without anyone being eliminated and left as a spectator. If they strictly played the text, the game would last 1-2 hours and players would drop out, fragmenting the group. They re-authorize the house rules each time they play; the rules are not permanent, but renewed based on social preference.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_player_group, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, social_player_group, agenda_setter).

% The published Monopoly rulebook by Parker Brothers/Hasbro: specifies property acquisition through purchase and auction, rent collection to the property owner, forced bankruptcy when a player cannot pay obligations, and a winner determined when all other players are bankrupt and eliminated. The text is fixed and public. It does not participate in social play; player groups either follow it or replace it with house rules.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_text, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, rulebook_text).

% Players in formal Monopoly tournaments and organized competitive circuits (e.g., World Monopoly Championship) who value strict rule adherence and skill-based differentiation. They would view house rules as noise and rule violation. They are structurally excluded from social play groups that use house rules; if they encountered a house-rule game, they would either refuse to play or insist on text adherence. Their values are incommensurable with the social scaffold reading.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, tournament_players, excluded,
    moderate, biographical, constrained, regional).

% Educators, economic theorists, and game designers who read Monopoly as an intentional pedagogical demonstration of capitalism's tendency toward wealth concentration and monopoly. They argue the harsh endgame (rapid elimination, winner-takes-all outcome) is the game's core educational message: that unrestricted markets produce inequality and exclusion. They would oppose house rules as obscuring this message. They are not present in social play contexts; their reading is orthogonal to the social scaffold frame.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, pedagogical_advocates, excluded,
    moderate, biographical, constrained, global).

% Hasbro (current rights holder to Monopoly) publishes and distributes the rulebook and game components. They profit from game sales. They do not directly participate in social play. House rules neither accrue rents to the publisher nor are explicitly prohibited by them (the rulebook implicitly permits them under 'house rules' customization). The publisher's institutional interest is in sales volume, not in ensuring text fidelity during private play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_publisher, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a social group's ability to play Monopoly together for an extended session (2-4 hours) by redistributing wealth and slowing elimination. The real coordination problem solved: how do we maintain this activity as genuinely shared entertainment and group cohesion, given that the text's endgame eliminates players and leaves them as spectators?
% TRANSFER_FUNCTION: House rules create liquidity transfers within the play group: money moves from individual rent accumulation into collective pools (free parking, community chest bonuses, temporary loans, negotiated payment plans). These transfers flatten wealth concentration, extend play time, and prevent catastrophic dominance by individual players. No external party captures the transfers; they recirculate within the group.
% ABSENT_VOICES: Tournament players and competitive-ranking organizations would object that house rules corrupt the competitive framework and disable skill-based differentiation. Pedagogical advocates would argue that house rules obscure the game's intentional message about capitalism's concentration dynamics. Neither voice is present in the social play context because the social reading explicitly de-prioritizes text fidelity and competitive ranking in favor of group cohesion.
% DISAPPEARANCE_RATIONALE: If the social group stopped using house rules and played strictly by the rulebook, the coordination function would fail: players would face rapid elimination within 1-2 hours, remaining players would become spectators, and the group would lose the shared experience they assembled for. The group would likely stop playing Monopoly or switch to a different game. The social activity depends on house rules; their absence would functionally end the arrangement.
% FOUNDING_PROBLEM: The published Monopoly rulebook produces an endgame that fragments the social group: players are eliminated through bankruptcy, leaving them as spectators for the conclusion. For a group seeking extended, genuinely shared entertainment and group bonding, the text is unplayable — it fails to deliver on the social promise. House rules were developed to solve this: maintain all players as active participants, extend play duration, redistribute wealth to prevent dominance patterns, and keep the game as a vehicle for group cohesion rather than individual competition.
% FOUNDING_PROBLEM_CORROBORATION: Multiple player communities across decades attest the founding problem: the rulebook's text produces poor social outcomes. Game design communities and social game advocates document widespread house-rule adoption as a response to the text's failure in social play. Hasbro's own published guidance materials acknowledge that house rules are common and provide suggestions for variant play, implicitly validating that the text alone is insufficient for social coordination. Empirical anthropology of board game play shows nearly every casual social group using Monopoly modifies the rules; this is not deviation from a social norm but IS the norm. Pedagogical and tournament advocates do NOT attest the founding problem — they defend the elimination mechanism as intentional and claim the text is sufficient for their purposes, which is true but orthogonal to the social reading.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness score (0.42 at interval end) reflects MODERATE wealth concentration after house rules are applied. Without house rules (strict text play), extractiveness would be 0.55-0.65 — rapid concentration, clear winner. At t=40-120 (peak house-rule adoption and enforcement), extractiveness dips to 0.38 — liquidity injections are most effective. Toward the session's end (t=180-240), it rises back toward 0.42 as endgame pressure increases and house rules become harder to sustain (players are closer to bankruptcy, wealth gaps are harder to artificially flatten, and the group faces the textual reality again). Suppression is LOW (0.28) because house rules are COLLECTIVELY ADOPTED, not externally imposed — minimal enforcement resistance because everyone agreed to them upfront. Theater is LOW (0.12) because house rules are FUNCTIONALLY MOTIVATED (they solve the real problem of social play) rather than performed for appearance — the group genuinely cares about extended play, not about appearing to care. The constraint is TEMPORARY: house rules operate only for the duration of the play session. Once the session ends, they dissolve. The next time the group plays, they may re-adopt them (likely) or attempt text play (rare), making this a genuine scaffold with a natural sunset clause tied to the session's end.
 *
 * PERSPECTIVAL GAP:
 *   Different institutional positions compute DIFFERENT constraint types from the same rulebook text: (1) SOCIAL_PLAYER_GROUP seat: perceives the rulebook as a scaffold that must be modified to be playable; computes SCAFFOLD because house rules solve a real coordination problem temporarily. Extraction is moderate because wealth concentration is muted but not eliminated. (2) TOURNAMENT_PLAYER seat (excluded from this reading): would perceive house rules as rule-breaking and game corruption; would compute SNARE or CONSTRAINT_VIOLATION (forced non-standard rules), not scaffold. (3) PEDAGOGICAL_ADVOCATE seat (excluded): would perceive house rules as obstacles to the game's truth-telling function; would compute EXTRACTION or SUPPRESSION (someone is interfering with the intended message). (4) RULEBOOK_TEXT seat: not an agent; has no perception. The engine's per-seat computation would produce these divergent classifications, revealing the kernel contest: one text, three incompatible readings, three different constraint types depending on which reading's purpose and evaluation criteria are adopted.
 *
 * DIRECTIONALITY LOGIC:
 *   The social_player_group is the BENEFICIARY and AGENDA_SETTER: they collectively author and enforce house rules, explicitly benefit from extended play and preserved group cohesion, and hold the structural power to decide whether to use house rules or abandon them. All participants in this seat are net beneficiaries — no one is forced to play, no one is coerced to accept house rules they reject. Directionality for this seat is strongly BENEFICIARY-TILTED (d near 0.0) because the group has high exit options (stop playing, switch games, play text rules if they choose), collective decision-making power (organized seat), and explicit coordination benefits. Tournament players and pedagogical advocates are EXCLUDED, not participants. If any of them were somehow forced to play under these house rules against their preference, their d would be high (near 1.0 target). But they're not here — their exclusion is structural to the social reading. The game_publisher observes but does not participate, so directionality is ANALYTICAL. Within the social group, directionality is SYMMETRIC because all participants are equal stakeholders in the decision and all benefit. This is the mark of genuine coordination, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (harsh endgame prevents social play) is LIVE: it persists as long as the group wants to play Monopoly socially without someone being eliminated early. House rules exist to solve it. The scaffold's SUNSET CLAUSE is the critical feature: house rules are explicitly TEMPORARY, valid only for the current play session. Once the session ends, they automatically expire. The next time the group plays, they must CHOOSE to re-adopt them. This differs from MANDATROPHY (a constraint persists long after its function dies because no party has power or motivation to change it) because the house rules' expiration is built-in and automatic. The group has HIGH EXIT options (simply don't play next time, or play text rules, or switch games), which keeps the scaffold from degrading into a zombie constraint. A constraint becomes mandatrophic when no one has both the power AND the motivation to end it; house rules avoid this because the group collectively has the power and can easily choose not to re-authorize them. This is what distinguishes scaffold from piton: piton is maintained by inertia and theater, scaffold is renewed by active choice toward a recognized end.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ruling_text_vs_living_practice,
    'Is the house-rule-modified Monopoly that players actually use more real than the published text? Or is the text the ''true'' constraint and house rules merely optional customization?',
    'Ethnographic observation: track what constraints players enforce, appeal to, and police during actual play. A sufficiently strong pattern (house rules enforced with the same social weight as textual rules) suggests players have REPLACED the text constraint with a de facto house-rule constraint.',
    'If players enforce house rules as THE constraint (not as exceptions to it), this reading''s classification stands: scaffold. If players treat house rules as temporary departures that must be justified against the text standard, the text remains the base constraint and house rules are meta-level workarounds (different classification structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ruling_text_vs_living_practice, empirical, 'Whether house rules or text are the operative constraint in social play.').

omega_variable(
    kernel_reading_vs_constraint_identity,
    'Is this constraint the house-rule system itself, or is it the rulebook-plus-house-rules combined? Does the ε-invariance principle require separate stories for ''rulebook alone'' and ''rulebook with house rules'', or are they one constraint with two observables?',
    'Check whether measuring extractiveness differently (rulebook alone vs rulebook-with-house-rules) yields a sufficiently different ε to warrant separate stories per DP-001. Answer: measuring the rulebook in isolation yields ε ≈ 0.65+ (brutal endgame, rapid elimination, clear winner); measuring the social reading (rulebook + house rules) yields ε ≈ 0.42 (moderated, preserved group cohesion). This difference is exactly the decomposition threshold (DP-001: different observables, significantly different ε, warranting separate stories).',
    'If we treat them as one constraint with measurement ambiguity, we''d be blurring the extraction_demo_reading into this story. Instead, we''ve split: extraction_demo_reading measures the rulebook alone (ε high, pedagogical, no house rules); social_scaffold_reading measures the rulebook as social groups use it (ε moderate, scaffold, house rules embedded). The network links them as siblings of the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_constraint_identity, conceptual, 'Whether the constraint is the text alone or the text-plus-social-modification system.').

omega_variable(
    sunset_clause_sustainability,
    'Do house rules in Monopoly actually disappear between play sessions, or do groups develop PERSISTENT house-rule norms that persist across multiple games and become de facto embedded in ''how we play''?',
    'Longitudinal observation: track the same group''s play across 3+ consecutive Monopoly games. If house rules persist without explicit re-authorization, they''ve transitioned from SCAFFOLD (temporary, renewed per session) to PITON (inertial, maintained by habit). If they''re actively re-negotiated each session, they remain scaffold.',
    'If house rules transition to persistent-norm status, this constraint''s classification would shift from scaffold to piton (still no beneficiary winner, but no sunset clause either — just inertia and theater). This would change the claimed type and the omega structure entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_sustainability, empirical, 'Whether house-rule scaffolds remain truly temporary or degrade into persistent norms.').

omega_variable(
    reading_incommensurability,
    'Are the three kernel readings (extraction_demo, social_scaffold, tournament_orthodoxy) genuinely incommensurable — could no single unified framework hold all three as live positions — or are they merely different emphases within one larger framework?',
    'Check for logical contradiction: (a) the extraction_demo reading asserts the harsh endgame is pedagogically intentional and valuable; (b) the social_scaffold reading asserts the harsh endgame must be suppressed for the game to be socially playable; (c) the tournament_orthodoxy reading asserts text fidelity is the test of competitive legitimacy. Do any two of these directly contradict within a single institutional context (e.g., a single game table)?',
    'If the readings logically foreclose each other, the coexists_with relation should be replaced with forecloses in cs_structure.reading_relations. The current schema declares coexists_with, assuming different groups hold different readings; if a single group tries to hold two simultaneously and fails, the foreclosure relation is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether the three readings are genuinely incompatible or merely different communities'' emphases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_rules_pure, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(theater_t0_rules_pure, projected).
narrative_ontology:measurement(theater_t40_initial_house, monopoly_rulebook__social_scaffold_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(theater_t40_initial_house, observed).
narrative_ontology:measurement(theater_t80_negotiation_norm, monopoly_rulebook__social_scaffold_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement_basis(theater_t80_negotiation_norm, observed).
narrative_ontology:measurement(theater_t120_midgame_rules, monopoly_rulebook__social_scaffold_reading, theater_ratio, 120, 0.12).
narrative_ontology:measurement_basis(theater_t120_midgame_rules, observed).
narrative_ontology:measurement(theater_t180_mercy_mechanics, monopoly_rulebook__social_scaffold_reading, theater_ratio, 180, 0.13).
narrative_ontology:measurement_basis(theater_t180_mercy_mechanics, observed).
narrative_ontology:measurement(theater_t240_final_theater, monopoly_rulebook__social_scaffold_reading, theater_ratio, 240, 0.12).
narrative_ontology:measurement_basis(theater_t240_final_theater, observed).

% Extraction over time
narrative_ontology:measurement(extract_t0_rules_strict, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(extract_t0_rules_strict, projected).
narrative_ontology:measurement(extract_t40_house_adoption, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(extract_t40_house_adoption, observed).
narrative_ontology:measurement(extract_t80_midgame, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement_basis(extract_t80_midgame, observed).
narrative_ontology:measurement(extract_t120_late_liquidity, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement_basis(extract_t120_late_liquidity, observed).
narrative_ontology:measurement(extract_t180_endgame_pressure, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 180, 0.42).
narrative_ontology:measurement_basis(extract_t180_endgame_pressure, observed).
narrative_ontology:measurement(extract_t240_final_scaffold, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 240, 0.42).
narrative_ontology:measurement_basis(extract_t240_final_scaffold, observed).

% Suppression requirement over time
narrative_ontology:measurement(supp_t0_text_enforcement, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(supp_t0_text_enforcement, projected).
narrative_ontology:measurement(supp_t40_house_negotiation, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(supp_t40_house_negotiation, observed).
narrative_ontology:measurement(supp_t80_rule_enforcement, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 80, 0.25).
narrative_ontology:measurement_basis(supp_t80_rule_enforcement, observed).
narrative_ontology:measurement(supp_t120_midgame_compliance, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 120, 0.28).
narrative_ontology:measurement_basis(supp_t120_midgame_compliance, observed).
narrative_ontology:measurement(supp_t180_late_enforcement, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 180, 0.3).
narrative_ontology:measurement_basis(supp_t180_late_enforcement, observed).
narrative_ontology:measurement(supp_t240_final_enforcement, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 240, 0.28).
narrative_ontology:measurement_basis(supp_t240_final_enforcement, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The Monopoly rulebook kernel decomposes into three structurally distinct constraints under three different readings. EXTRACTION_DEMO_READING measures the rulebook in isolation, focusing on its pedagogical function as a demonstration of wealth concentration (high ε, mountain or snare). SOCIAL_SCAFFOLD_READING (this story) measures the rulebook as modified by player groups to preserve social play (moderate ε, scaffold). TOURNAMENT_ORTHODOXY_READING measures the rulebook as the legitimate competitive standard (low ε for competitive play, mountain or rope under tournament framing). The three stories share the same kernel text but instantiate different ε values, different beneficiary/victim structures, and different constraint types because they operate under different purposes and evaluation criteria. They are NOT three perspectives on one constraint — they are three constraints on three different modified versions of the rulebook. All three must be linked via network.affects_constraints to establish the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
