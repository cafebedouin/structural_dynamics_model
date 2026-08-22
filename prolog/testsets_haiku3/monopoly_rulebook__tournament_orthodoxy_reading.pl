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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Monopoly Rulebook as Tournament-Orthodox Competitive Standard
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   The Monopoly rulebook, as read through a tournament-orthodoxy lens, is a
 *   purely coordinative rope constraint: it establishes a single immutable
 *   text as the legitimate reference frame for competitive play. This reading
 *   treats the rulebook as a solved institutional problem (coordination
 *   around shared standards) and rejects alternative framings that treat the
 *   rulebook as (a) an inevitable engine of wealth concentration warranting
 *   abolition (extraction_demo_reading) or (b) a text requiring
 *   community-level house-rule corrections to remain socially playable
 *   (social_scaffold_reading). The tournament-orthodoxy reading is ONE
 *   instantiation of the contested kernel 'monopoly_rulebook'; the other
 *   readings are separate constraint stories with different ε values and
 *   different structural analyses. This JSON generates only the
 *   tournament-orthodoxy reading.
 *
 * KEY AGENTS:
 *   - competitive_community: the beneficiary set that coordinates on rule-text immutability for fair comparison and strategic accumulation
 *   - tournament_organizers: the agenda-setters who enforce the rulebook and recognize only standard-text play
 *   - rules_scholars: custodians of authoritative interpretation, benefit from immutable text for analysis
 *   - casual_players: observe the constraint but remain outside the competitive frame; play locally with variants
 *   - players_seeking_social_playability: excluded from the competitive frame; their objection to harsh endgame mechanics is not recognized as valid within tournament-orthodoxy
 *   - pedagogical_reformers: excluded; their desire to modify rules for teaching purposes has no standing in competitive legitimacy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook as Tournament-Orthodox Competitive Standard").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination/institutional_design").

domain_priors:requires_active_enforcement(monopoly_rulebook__tournament_orthodoxy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '8a446a80-3d5e-4ced-a3b9-d48226e92ef8').
narrative_ontology:cs_kernel_codification('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', fixed_text).
narrative_ontology:cs_authority_grounding('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', practice).
narrative_ontology:cs_interpretation_layer_present('8a446a80-3d5e-4ced-a3b9-d48226e92ef8').
narrative_ontology:cs_reading_relation('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', foundational, rule_text_authority_is_immutable_for_competitive_purposes).
narrative_ontology:cs_axiom_status(rule_text_authority_is_immutable_for_competitive_purposes, holdable).
narrative_ontology:cs_axiom_grounding('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', rule_text_authority_is_immutable_for_competitive_purposes, conventional).
narrative_ontology:cs_axiom('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', foundational, competitive_skill_determines_outcomes_under_standard_rulebook).
narrative_ontology:cs_axiom_status(competitive_skill_determines_outcomes_under_standard_rulebook, holdable).
narrative_ontology:cs_axiom_grounding('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', competitive_skill_determines_outcomes_under_standard_rulebook, empirically_contingent).
narrative_ontology:cs_reference_frame('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', standard_rulebook_competitive_legitimacy).
narrative_ontology:cs_drift_state('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', contemporary_rules_challenge_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8a446a80-3d5e-4ced-a3b9-d48226e92ef8', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, rules_scholars).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, competitive_skill_determines_outcomes).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, rule_text_authority_immutability).
narrative_ontology:constraint_vindicates(monopoly_rulebook__tournament_orthodoxy_reading, strategic_depth_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Players who value the standardized rulebook as the legitimate competitive framework. They benefit from a fixed, immutable reference text that enables fair comparison across tournaments, rankings, and competitive contexts. The rulebook provides a coordination point: adherence to text authority allows competitive results to be comparable and skill differences to be meaningfully measured without ambiguity introduced by local house-rule variation.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community, beneficiary,
    organized, biographical, mobile, global).

% Enforce rulebook orthodoxy as the standard for recognized competitive play. They maintain the immutability of text authority by refusing to recognize results from games played under house rules, excluding non-standard variants from ranking systems, and publishing canonical rules interpretations. They benefit from standardization through reduced arbitration overhead and preserved competitive legitimacy across venues.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, generational, mobile, global).

% Play within their own groups and modify rules for social fit and game length. From the tournament-orthodoxy perspective, they are outside the competitive frame entirely; their house rules do not affect the legitimacy of the standardized text, but their alternative frameworks are not recognized as valid competitive contexts.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, observer,
    moderate, immediate, mobile, local).

% Study the rulebook's game-theoretic properties and strategic depth. They benefit from text immutability: it enables rigorous analysis of strategy without moving targets, and permits accumulation of strategic knowledge (opening theory, endgame techniques) that remains valid across time and venues. They act as custodians of the rulebook's authoritative interpretation.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rules_scholars, beneficiary,
    analytical, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__tournament_orthodoxy_reading, rules_scholars, observer).

% Want to modify the rulebook to slow the endgame, prevent harsh elimination dynamics, or adjust pacing for social contexts. From the tournament-orthodoxy frame, they are excluded: their objection is that the text creates undesirable social outcomes (long player elimination, wealth concentration), but the tournament frame treats these as features—evidence of competitive depth—rather than problems to solve. They have no recognized voice in competitive legitimacy.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, players_seeking_social_playability, excluded,
    moderate, immediate, constrained, local).

% Use games to teach economic principles and want rule modifications to highlight or obscure particular lessons. From the tournament-orthodoxy frame, pedagogical intent does not warrant deviation from text authority; the rulebook's properties are what they are, and if they teach something about capitalism, that is incidental to the rules' competitive legitimacy, not a reason to alter them.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, pedagogical_reformers, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, immutable rulebook as the authoritative standard for competitive play, enabling tournament organization, skill comparison across venues and time, and strategic analysis that persists because the rules do not shift. Players coordinating on one canonical text rather than local variants can build cumulative strategic knowledge and compete fairly in standardized contexts.
% TRANSFER_FUNCTION: Transfers authority from local communities (whose house rules would prevail in isolation) to a global standardized text. No monetary or material transfer; the transfer is epistemic and jurisdictional: the rulebook becomes the reference frame for competitive legitimacy, rendering local variants as non-standard noise rather than alternative legitimate frameworks.
% ABSENT_VOICES: Casual players who prefer socially adjusted variants, pedagogical reformers who want the game to illustrate particular economic lessons, and players who experience the rulebook's endgame as unplayably harsh. They would argue the rulebook should be modifiable for social, pedagogical, or play-experience reasons, but the tournament-orthodoxy frame treats rule-text authority as immutable and non-negotiable for competitive contexts.
% DISAPPEARANCE_RATIONALE: If the rulebook-as-immutable-standard vanished, competitive play would fragment into local house-rule variants. Tournament organization would become chaotic (results from different venues incomparable), strategic knowledge would not accumulate (strategies valid under one rule-set invalid under another), and players seeking standardized competition would face coordination collapse. The competitive community depends on this constraint to maintain organized comparison and ranking.
% FOUNDING_PROBLEM: Early game play was fragmented: each group played with different house rules, making competitive comparison impossible and strategy development difficult because knowledge gained in one context did not transfer. Tournament organization required a canonical text that all competitors would accept as the legitimate frame, immutable to preserve consistency across venues and over time.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers, rules scholars, and competitive players attest the problem remains live: any deviation from text orthodoxy creates incomparability and strategic chaos. Independent accounts from competitive communities (tournament reports, rankings governance, competitive-community retrospectives) confirm that standardization on immutable text remains the necessary condition for organized play across venues.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very low (0.08 at interval end) because this reading treats the rulebook as pure coordination: all competitive players voluntarily adopt the standard text because it solves their mutual problem (comparable results, cumulative strategic knowledge). No party extracts rents; beneficiaries and agenda-setters both benefit from standardization. Suppression is minimal (0.05) because participation is voluntary—players seeking competitive play adopt the rulebook willingly, and those who prefer social variants simply play in non-competitive contexts. Theater is low (0.12) because the constraint's function is genuine: standardization on immutable text truly enables tournament organization. The small theater ratio reflects the minimal performative overhead needed to maintain rule-text authority. Accessibility collapse is very high (0.92) because once players adopt a standard rulebook for competitive purposes, alternatives (house rules) become incompatible with that purpose—not suppressed, but logically unavailable within the competitive frame. Resistance is low (0.18) because the constraint solves a real coordination problem that the competitive community wants solved; resistance comes only from those excluded from the competitive frame (social-correction seekers, pedagogical reformers) who are not the constraint's primary stakeholders.
 *
 * PERSPECTIVAL GAP:
 *   From the tournament-organizer and competitive-player perspective, the rulebook is unambiguously coordinative and necessary. From the excluded players' perspective (social-playability seekers, pedagogical reformers), the same rulebook appears unnecessarily rigid and socially costly. These are not different measurements of the same thing; they are different framings of the constraint's purpose. The tournament-orthodoxy reading adopts the competitive frame and treats alternatives as outside the scope of competitive legitimacy. The engine should compute different type classifications from different seats: beneficiaries and organizers compute rope (coordination around standard); excluded players might compute tangled_rope or snare (if forced to play by rules they did not consent to). This story licenses only the competitive-frame classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Competitive community and tournament organizers are structural beneficiaries (d near 0.0): they benefit from standardization, face no suppression (voluntary participation), and have mobile exit (they could abandon competitive play, but choose not to). Rules scholars also benefit (arbitrage exit: they can publish analyses, teach, consult). Casual players and excluded reformers are outside the frame entirely: they are not payers (they do not sustain the constraint through coercion or mandatory participation) and not beneficiaries of the competitive coordination (they opt out). The constraint has NO victim set in this reading—a key structural feature distinguishing it from the extraction_demo_reading and social_scaffold_reading, which do identify victims within their own frames.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmentation without a standard rulebook) remains live and unresolved. Tournament organizers and competitive communities continue to affirm the need for immutable text authority. No mandatrophy dynamic is present: the constraint's founding function persists and is actively maintained. The ruling is not a zombie maintaining theater after the problem solved itself; it is a genuine ongoing solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the rulebook legitimately read as a pure competitive-coordination problem (tournament-orthodoxy frame), or does it necessarily instantiate wealth-concentration dynamics that require abolition (extraction-demo frame) or social-playability constraints that require house-rule correction (social-scaffold frame)?',
    'The three readings coexist as live positions held by different communities (competitive players, pedagogical critics, casual-social players). No single empirical fact resolves the contest; the readings reflect different framings of what problem the rulebook solves. Resolution would require one reading to formally foreclose another—i.e., to establish that no coherent framework could hold both—which has not occurred.',
    'If the tournament-orthodoxy reading is the sole legitimate frame, the rulebook is a rope (pure coordination). If the extraction-demo reading forecloses tournament-orthodoxy (by establishing that competitive legitimacy cannot exist separate from wealth-concentration critique), the rulebook becomes snare or tangled_rope. If the social-scaffold reading wins, the rulebook is scaffold (requiring house-rule correction to remain socially playable). The contest is fundamental to constraint classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the rulebook is legitimately read through a single dominant frame or remains genuinely contested across multiple incommensurable frames.').

omega_variable(
    voluntary_participation_boundary,
    'Does treating the rulebook as pure coordination depend on the assumption that all participation is voluntary? If participation were mandatory (e.g., in schools or institutions), would the same rulebook constitute extraction or a snare?',
    'Empirical examination of institutional contexts where the rulebook is enforced on non-volunteers (mandatory game education, forced participation). If identical rules produce different type classifications depending on voluntariness of participation, the constraint boundary is actually the participation frame, not the rulebook itself.',
    'If participation-voluntariness is the true constraint classifier, the tournament-orthodoxy reading is contingent on a specific (voluntary) participation context. The rulebook''s ε would change with participation frame. This would decompose the constraint into separate stories per participation frame (voluntary-competitive vs. mandatory-institutional), following ε-invariance principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_boundary, conceptual, 'Whether the constraint''s classification depends on voluntariness of participation and whether participation frame should generate separate constraint stories.').

omega_variable(
    strategic_depth_as_vindicated_proposition,
    'Is the claim ''rulebook text authority enables meaningful strategic depth'' a feature of the rulebook or a property of the reading? Could the extraction-demo or social-scaffold readings vindicate alternative propositions (e.g., ''the rulebook''s strategic depth is a sophisticated gloss on wealth concentration'' or ''true strategic depth emerges only from socially-adjusted house rules'') without changing the rulebook itself?',
    'Comparative game-theoretic analysis of strategic properties under tournament-orthodoxy rules vs. under contested house rules. If strategic depth (measurable as branching factor, state-space complexity, Nash equilibrium properties) is empirically equivalent under different rule-sets, the proposition ''rulebook enables strategic depth'' is vindication_reading-dependent rather than objective.',
    'If vindicated propositions are reading-dependent, they do not appear in base_properties.vindicated_propositions (which should contain objective, non-reading-indexed claims). The current population of vindicated_propositions may need revision or relocation to omega reasoning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_depth_as_vindicated_proposition, empirical, 'Whether ''strategic depth'' is an objective property of the rulebook or a claim vindicated differently by different readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(mono_tr_t0, observed).
narrative_ontology:measurement(mono_tr_t5, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(mono_tr_t5, observed).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(mono_tr_t10, observed).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(mono_tr_t15, observed).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(mono_tr_t20, observed).
narrative_ontology:measurement(mono_tr_t25, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(mono_tr_t25, observed).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(mono_tr_t30, observed).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(mono_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(mono_be_t0, observed).
narrative_ontology:measurement(mono_be_t5, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement_basis(mono_be_t5, observed).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement_basis(mono_be_t10, observed).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 15, 0.08).
narrative_ontology:measurement_basis(mono_be_t15, observed).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement_basis(mono_be_t20, observed).
narrative_ontology:measurement(mono_be_t25, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 25, 0.08).
narrative_ontology:measurement_basis(mono_be_t25, observed).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement_basis(mono_be_t30, observed).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(mono_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement_basis(mono_su_t0, observed).
narrative_ontology:measurement(mono_su_t5, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 5, 0.04).
narrative_ontology:measurement_basis(mono_su_t5, observed).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement_basis(mono_su_t10, observed).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 15, 0.05).
narrative_ontology:measurement_basis(mono_su_t15, observed).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.06).
narrative_ontology:measurement_basis(mono_su_t20, observed).
narrative_ontology:measurement(mono_su_t25, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 25, 0.06).
narrative_ontology:measurement_basis(mono_su_t25, observed).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement_basis(mono_su_t30, observed).
narrative_ontology:measurement(mono_su_t40, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement_basis(mono_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% The Monopoly rulebook kernel has been decomposed into three distinct constraint stories per ε-invariance principles (OQ-DP-001). Each reading instantiates the kernel differently: tournament_orthodoxy_reading treats it as pure coordination (ε ≤ 0.10, rope), extraction_demo_reading treats it as demonstrating wealth concentration (ε high, snare), social_scaffold_reading treats it as requiring house-rule correction (ε moderate, tangled_rope or scaffold). The readings coexist as live positions held by different communities; none forecloses another at present. All three are valid constraint stories; they share a kernel but have independent ε values, beneficiary/victim structures, and type classifications. Network edges link all three bidirectionally to indicate mutual influence and contested interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
