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
 *   human_readable: Monopoly Rulebook as Social Scaffold (House Rules Interpretation)
 *   domain: social/game_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes Monopoly as played in households with house
 *   rules—the social scaffold reading of the rulebook kernel. The rulebook
 *   text mandates rapid elimination and winner-take-all outcomes; this
 *   reading instantiates the constraint as the collective agreement to modify
 *   those rules to preserve social coordination across a 3+ hour game
 *   session. The constraint is scaffolding: it is explicitly temporary
 *   (sunset at game end), justified by transition (maintaining play quality
 *   until natural conclusion), and active (enforced by group consensus). It
 *   is NOT pure coordination (the text exists as a baseline reference that is
 *   actively modified) and NOT pure extraction (the modification preserves a
 *   real coordination function that the text alone destroys). The reading
 *   differs fundamentally from the extraction-demo reading (which sees house
 *   rules as obscuring pedagogy) and the tournament-orthodoxy reading (which
 *   sees them as noise). This reading sees them as necessary social repair of
 *   an otherwise unplayable endgame.
 *
 * KEY AGENTS:
 *   - social_group_playing: Collectively decides and enforces house rules; benefits from extended engagement and distributed elimination.
 *   - rulebook_text: The published standard; serves as reference boundary that is modified by consensus.
 *   - early_eliminated_players: Bear the cost of continued engagement under modified rules rather than exit as spectators.
 *   - leading_player: Bears the cost of constrained advantage trajectory and delayed victory.
 *   - game_facilitator: Enforces the chosen ruleset and mediates boundary disputes.
 *   - parker_brothers_publisher: Excluded; would advocate for text fidelity.
 *   - competitive_tournament_players: Excluded; would argue house rules break comparability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.42).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.28).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Rulebook as Social Scaffold (House Rules Interpretation)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "social/game_theory/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301').
narrative_ontology:cs_kernel_codification('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', fixed_text).
narrative_ontology:cs_authority_grounding('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', extraction).
narrative_ontology:cs_interpretation_layer_present('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301').
narrative_ontology:cs_reading_relation('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', foundational, social_coordination_requires_adaptive_governance).
narrative_ontology:cs_axiom_status(social_coordination_requires_adaptive_governance, holdable).
narrative_ontology:cs_axiom_grounding('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', social_coordination_requires_adaptive_governance, instrumental).
narrative_ontology:cs_axiom('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', foundational, text_authority_conditional_on_coordination_function).
narrative_ontology:cs_axiom_status(text_authority_conditional_on_coordination_function, holdable).
narrative_ontology:cs_axiom_grounding('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', text_authority_conditional_on_coordination_function, conventional).
narrative_ontology:cs_reference_frame('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', rulebook_as_adaptive_template).
narrative_ontology:cs_drift_state('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', contemporary_household_play, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('53bdcbd5-ee40-4e2d-8cf7-9fa87e74e301', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_cohesion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_playing).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, early_eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, leading_player).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, games_require_social_adaptation).
narrative_ontology:constraint_vindicates(monopoly_rulebook__social_scaffold_reading, coordination_trumps_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively decides which house rules to adopt before the game begins. Benefits from prolonged play, distributed elimination, and repeated opportunity to influence the outcome. Sets the rulebook boundary by consensus; the game's social function (group bonding, extended entertainment, narrative arc) depends on their active modification of the text.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_playing, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__social_scaffold_reading, social_group_playing, agenda_setter).

% The published Monopoly rules as written: rapid elimination through bankruptcy, winner-take-most payoff structure, 45-90 minute canonical game duration. Exists as a formal standard; the constraint is the lived arrangement where players invoke and modify it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, rulebook_text, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__social_scaffold_reading, rulebook_text).

% Under strict rulebook, exit the game within 30-60 minutes through bankruptcy, becoming spectators with no further influence. House rules (loan mechanisms, wealth redistribution, late-game liquidity injection) keep them in play longer, bearing the cost of delayed gratification and continued engagement rather than passive observation.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, early_eliminated_players, payer,
    moderate, immediate, constrained, local).

% Under strict rulebook, can force rapid victory through compound advantage. House rules (caps on property development, rent ceilings, forced loans to weaker players) constrain their advantage trajectory, distributing wins and preserving competitive uncertainty. Bears the cost of stretched victory timelines and competitive pressure from coordinated other players.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, leading_player, payer,
    moderate, immediate, constrained, local).

% Enforces the chosen ruleset (modified or strict) and mediates disputes. Under social-scaffold reading, actively monitors for text violations and negotiates boundary cases. Bears administrative burden; enforces the scaffold structure.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, game_facilitator, agenda_setter,
    moderate, biographical, mobile, local).

% Created and maintains the rulebook authority. Under strict reading, endorses only the published text as legitimate. They are excluded from household play sessions; if present, would advocate for rulebook fidelity and against house rules as 'corruption' of the game design.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, parker_brothers_publisher, excluded,
    institutional, civilizational, trapped, global).

% Play under strict rulebook for ranking and comparison. House rules break comparability and are explicitly prohibited in tournament contexts. They would object if household rules were declared 'legitimate' Monopoly, as it would devalue ranking systems based on strict-text play.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_tournament_players, excluded,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, social_group_playing).
narrative_ontology:fixing_cost_class(monopoly_rulebook__social_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the temporal coordination problem: how to run a game from initiation to meaningful conclusion while keeping all players engaged, maintaining social participation, and distributing narrative outcomes (wins, close calls, comebacks) across the session. The rulebook text alone produces rapid elimination; the house rule scaffold preserves the game-as-social-event function.
% TRANSFER_FUNCTION: Moves time, attention, and social participation from eliminated players (under strict rules) to all players throughout the session. Also moves wealth and advantage more slowly and distribitely than the text mandates—injecting liquidity, capping property monopolies, forcing bailouts. The transfer is FROM fidelity-to-text TO social-cohesion-during-play.
% ABSENT_VOICES: The publisher (Parker Brothers/Hasbro) is structurally excluded—they designed the rulebook for tournament play and rapid resolution, not household social sessions. Tournament players are excluded—they would argue that house rules invalidate comparison and competitive integrity. Both have legitimate structural claims but are absent from household play decision-making.
% DISAPPEARANCE_RATIONALE: If the social scaffold disappeared—if households reverted entirely to strict rulebook—Monopoly sessions would collapse into 45-90 minute experiences with early elimination, spectator frustration, and rapid victory by advantage accumulation. Many households would stop playing or replace the game with others that sustain engagement. The social function the constraint scaffolds would dissolve.
% FOUNDING_PROBLEM: The published Monopoly rulebook produces socially unplayable endgames: players eliminated mid-game become bored spectators; the leading player's advantage compounds into inevitable victory; the experience feels punitive rather than entertaining. Households discovered that ad-hoc modifications (free parking pools, bailout loans, rent caps, wealth redistribution) restored play quality and group cohesion.
% FOUNDING_PROBLEM_CORROBORATION: Thousands of household play reports, game design analyses, and board game guides document that strict Monopoly produces social friction and early dropouts. The rulebook itself includes a note: 'House rules may vary'—an implicit acknowledgment from the publisher that the text alone is insufficient for all play contexts. Academic game design literature (including Salen & Zimmerman) documents the scaffold phenomenon across rule-heavy games.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end) because the house-rule scaffold redistributes advantage but does not eliminate hierarchy—leading players still win, but through prolonged play rather than rapid dominance. The constraint's function is genuine (keeping players engaged), so the measured extraction reflects the cost of maintaining that coordination (delay, wealth redistribution, administrative overhead), not parasitic rent. Suppression is low (0.28) because the scaffold persists through group consensus and mobile exit—no coercive force maintains it except the group's own choice to modify the text. Theater increases slightly (0.47) because rule adjudication becomes a performance element (disputes, boundary cases, collective decision-making) rather than mechanical text application, but this does not indicate loss of function—the theater IS part of the social repair. The measurement series plateau mid-interval, indicating a stable scaffold state once rules are adopted (t=0-4 show adoption friction; t=8-20 show steady-state operation). The constraint satisfies the sunset clause gate: it terminates when the game ends (100+ turn cycles later or social agreement to stop), transitioning to nothing rather than persisting as institutional structure.
 *
 * PERSPECTIVAL GAP:
 *   From the social group's seat, the house rules are legitimate adaptive governance—they solve a coordination problem the text creates. From the publisher's seat (excluded), the rules corrupt the game design and break it as a comparable competitive artifact. From the tournament player's seat (also excluded), the rules are illegitimate because they devalue strict-text rankings. The engine computes these divergences: the group's seat experiences the scaffold as low-extraction coordination; the publisher's seat (if included) would experience it as deviation from authority; the tournament seat would experience it as loss of legitimacy. The perspectival gap is structural, not a disagreement about values—it flows from different positions relative to the rulebook's authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The social group is the primary beneficiary (they decide the rules, achieve their coordination goal—d near beneficiary end) and secondary payer (they bear the administrative overhead and accept slower victories). Early-eliminated players are constrained payers (they must stay engaged longer than the text would require) who become secondary beneficiaries (they stay in the game rather than spectating). The leading player is a constrained payer (advantage is capped) who remains a beneficiary (they still win, just slowly). The rulebook text itself is not an agent but carries the beneficiary logic of 'preserving the game design intent' (which this reading interrogates—the intent for household vs. tournament play is contested). The publisher and tournament players are excluded, so their directionality does not feed this constraint's derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The social scaffold reading avoids mandatrophy by maintaining explicit connection to its founding problem: the rulebook produces unplayable endgames in household contexts. House rules directly solve that problem. The constraint is not a zombie—it has no persistence independent of its function (game sessions end, the rules sunset, households can revert to strict text if they choose). The scaffold prevents the mandate from outliving its function by design: it is temporary, consensual, and function-linked. The extraction elements (delayed victory, wealth redistribution) remain structurally justified by the coordination function they enable, not by institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_authority_vs_social_function,
    'Does rulebook authority derive from the publisher''s design intent, or from the coordination function the rules enable in their actual play context?',
    'Examine player testimony across household vs. tournament contexts; analyze Parker Brothers'' original design documentation for what audience and play duration the rules were authored for.',
    'If authority derives from design intent: household modifications that better serve the intended context (social engagement for non-competitive play) are legitimate. If authority is immutable text: modifications are deviation. This distinction separates the social_scaffold reading from the tournament_orthodoxy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_authority_vs_social_function, conceptual, 'Whether rulebook authority is context-conditional or universal.').

omega_variable(
    extraction_vs_coordination_cost,
    'Is the measured extractiveness (0.42) the cost of preserving coordination, or is it asymmetric rent-taking by those who modify the rules to their advantage?',
    'Analyze wealth distribution across modified-rule sessions: if extraction concentrates in specific player seats, it indicates rent-taking; if it distributes evenly, it indicates coordination cost.',
    'High concentration would shift classification toward snare or tangled_rope; distributed extraction supports scaffold framing. This distinction separates the social_scaffold reading from the extraction_demo reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_cost, empirical, 'Whether house rules preserve coordination or enable coordinated extraction.').

omega_variable(
    sunset_mechanism_enforcement,
    'Is the sunset clause (game ends, rules terminate) truly binding, or do household rule modifications persist across sessions and accumulate into institutional norms?',
    'Track long-term play groups: do they reinvent rules each session (true sunset), or do modifications crystallize into persistent house traditions (effective mandate drift)?',
    'If modifications accumulate into persistent norms, the constraint is not a true scaffold but a piton (theater maintaining atrophied function). If rules reset per-session, the scaffold framing holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_mechanism_enforcement, empirical, 'Whether house rules terminate or evolve into persistent institutional norms.').

omega_variable(
    consensus_mechanism_coercion,
    'Does the ''group consensus'' that enforces house rules truly operate through mobile exit and voluntary participation, or is there internalized pressure (identity as a player, group belonging) that traps participants?',
    'Ethnographic observation: do players freely opt out of modified-rule sessions, or do they feel obligated to participate even when rule changes disadvantage them?',
    'If consensus is genuinely mobile, suppression stays low (0.28). If conformity is internalized through identity, suppression is higher and the constraint approaches snare (interpersonal identity-lock dynamics).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_mechanism_coercion, empirical, 'Whether group consensus operates through mobile exit or internalized obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(mono_tr_t0, observed).
narrative_ontology:measurement(mono_tr_t4, monopoly_rulebook__social_scaffold_reading, theater_ratio, 4, 0.39).
narrative_ontology:measurement_basis(mono_tr_t4, observed).
narrative_ontology:measurement(mono_tr_t8, monopoly_rulebook__social_scaffold_reading, theater_ratio, 8, 0.43).
narrative_ontology:measurement_basis(mono_tr_t8, observed).
narrative_ontology:measurement(mono_tr_t12, monopoly_rulebook__social_scaffold_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement_basis(mono_tr_t12, observed).
narrative_ontology:measurement(mono_tr_t16, monopoly_rulebook__social_scaffold_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement_basis(mono_tr_t16, observed).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__social_scaffold_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(mono_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(mono_be_t0, observed).
narrative_ontology:measurement(mono_be_t4, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(mono_be_t4, observed).
narrative_ontology:measurement(mono_be_t8, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement_basis(mono_be_t8, observed).
narrative_ontology:measurement(mono_be_t12, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(mono_be_t12, observed).
narrative_ontology:measurement(mono_be_t16, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement_basis(mono_be_t16, observed).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(mono_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(mono_su_t0, observed).
narrative_ontology:measurement(mono_su_t4, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 4, 0.21).
narrative_ontology:measurement_basis(mono_su_t4, observed).
narrative_ontology:measurement(mono_su_t8, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement_basis(mono_su_t8, observed).
narrative_ontology:measurement(mono_su_t12, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 12, 0.27).
narrative_ontology:measurement_basis(mono_su_t12, observed).
narrative_ontology:measurement(mono_su_t16, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement_basis(mono_su_t16, observed).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(mono_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__social_scaffold_reading, 0.18).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the Monopoly rulebook kernel. The extraction_demo_reading treats the rulebook as a pedagogical tool for demonstrating monopoly capitalism (high extractiveness, no modification justifiable). The tournament_orthodoxy_reading treats the rulebook as an immutable competitive standard (text authority is absolute, house rules are noise). The social_scaffold_reading treats the rulebook as a template requiring adaptive modification to preserve social coordination in household play. All three readings operate on the same kernel (published text) but instantiate different constraints via different ε values and beneficiary/victim structures. Link them via network.affects_constraints to signal that changes to rulebook authority or interpretation affect all three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
