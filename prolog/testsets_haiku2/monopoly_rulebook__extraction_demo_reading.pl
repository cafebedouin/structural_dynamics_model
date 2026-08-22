% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook: Inevitable Extraction Demonstration Reading
 *   domain: game_theory/institutional_design/social_coordination
 *
 * SUMMARY:
 *   The Monopoly rulebook, under this reading, is not a game system but a
 *   pedagogical apparatus demonstrating the mathematical inevitability of
 *   wealth concentration and player elimination under capitalist property
 *   rules. The claim is that the rulebook INSTANTIATES natural law: given
 *   unequal starting capital distributions, property acquisition dynamics,
 *   compound rent extraction, and player elimination, wealth concentration is
 *   structurally inevitable, not a contingent outcome. This is a MOUNTAIN
 *   reading — it asserts the rulebook proves a necessary truth about
 *   capitalism by embedding it in game mechanics. Sibling readings ('social
 *   scaffold' and 'tournament orthodoxy') reject this interpretation and
 *   treat the rulebook differently: as a social game requiring community
 *   adaptation, or as a competitive standard with immutable authority. This
 *   reading's pedagogical claim is that elimination and winner-take-all
 *   outcomes are the POINT, the living proof that capitalism concentrates
 *   wealth by structural necessity.
 *
 * KEY AGENTS:
 *   - rulebook_mathematics: The mathematical structure embedded in property rules (vindicated proposition, not a human agent)
 *   - early_eliminated_players: Those removed by bankrupt (victims bearing extraction, high d)
 *   - late_game_survivors: Players still in but under escalating rent pressure (mixed: beneficiaries of having survived, payers of rising extraction)
 *   - eventual_winner: The final player with all capital and property (beneficiary, d near 0.0)
 *   - house_rule_reformers: Community players who modify the rulebook (excluded, their readings rejected by this framework)
 *   - competitive_game_authorities: Tournament operators who enforce the rulebook as canonical (observers, analytical seat)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.68).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.71).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook: Inevitable Extraction Demonstration Reading").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/institutional_design/social_coordination").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'cf014ab5-ba21-45a0-a604-ac60964700e2').
narrative_ontology:cs_kernel_codification('cf014ab5-ba21-45a0-a604-ac60964700e2', fixed_text).
narrative_ontology:cs_authority_grounding('cf014ab5-ba21-45a0-a604-ac60964700e2', extraction).
narrative_ontology:cs_interpretation_layer_present('cf014ab5-ba21-45a0-a604-ac60964700e2').
narrative_ontology:cs_reading_relation('cf014ab5-ba21-45a0-a604-ac60964700e2', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf014ab5-ba21-45a0-a604-ac60964700e2', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('cf014ab5-ba21-45a0-a604-ac60964700e2', foundational, elimination_endgame_necessity).
narrative_ontology:cs_axiom_status(elimination_endgame_necessity, holdable).
narrative_ontology:cs_axiom_grounding('cf014ab5-ba21-45a0-a604-ac60964700e2', elimination_endgame_necessity, empirically_contingent).
narrative_ontology:cs_axiom('cf014ab5-ba21-45a0-a604-ac60964700e2', foundational, capitalism_structural_proof).
narrative_ontology:cs_axiom_status(capitalism_structural_proof, holdable).
narrative_ontology:cs_axiom_grounding('cf014ab5-ba21-45a0-a604-ac60964700e2', capitalism_structural_proof, instrumental).
narrative_ontology:cs_reference_frame('cf014ab5-ba21-45a0-a604-ac60964700e2', rulebook_as_pedagogical_proof).
narrative_ontology:cs_drift_state('cf014ab5-ba21-45a0-a604-ac60964700e2', contemporary_house_rule_proliferation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cf014ab5-ba21-45a0-a604-ac60964700e2', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, rulebook_mathematics).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, monopoly_capitalism_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, late_game_survivors).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, eventual_winner).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, early_eliminated_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, late_game_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The mathematical structure of property acquisition, rent extraction, and player elimination embedded in the official rules. This is not a human actor but a vindicated proposition — the rules themselves prove wealth concentration is structurally inevitable under these constraints.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, rulebook_mathematics, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(monopoly_rulebook__extraction_demo_reading, rulebook_mathematics).

% Players who lose all capital and are eliminated from the game according to the rulebook's mandate. Once eliminated, they cease participating in the game economy entirely. The rulebook offers no mechanism for re-entry, bankruptcy protection, or redistribution. Their elimination is the teaching point: the rulebook proves the inevitability of removing competitors from the wealth system.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, early_eliminated_players, payer,
    powerless, immediate, trapped, local).

% Players still solvent but operating under increasing pressure as cash becomes concentrated in fewer hands and property monopolies form. They continue playing under the rules' mandate, bearing rising extraction through rent payments. Their continued participation demonstrates the asymmetry the rulebook instantiates — survival requires ever-larger capital reserves while rent demands accelerate.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, late_game_survivors, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, late_game_survivors, beneficiary).

% The final remaining player who has accumulated all property and capital. The rulebook's structure guarantees this outcome through elimination mechanics. The winner takes all, validating the extraction trajectory that eliminated the other players.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eventual_winner, beneficiary,
    powerful, biographical, mobile, local).

% Players and communities who modify the rulebook via house rules (free parking money, softer bankruptcy, trading mechanisms, redistribution pools) to make the game more playable and socially sustainable. They are structurally excluded from this reading's framwork — their amendments are treated as violations of the rulebook's pedagogical truth, not legitimate adaptations.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_reformers, excluded,
    organized, biographical, constrained, local).

% Tournament operators, competitive league organizers, and game designers who enforce the official rulebook as the canonical competitive standard. They observe that the rulebook's extraction mechanics produce consistent outcomes and stable rankings, which validates the text authority in their framework.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, competitive_game_authorities, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, eventual_winner).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rulebook provides a shared game system: all players understand the same rules, property values follow predictable formulas, rent is calculated identically for all, and the game has a clear terminal condition (one player remaining with all capital). This solves the coordination problem of 'how do we play this game together' by offering a complete, unambiguous rule set.
% TRANSFER_FUNCTION: The rulebook moves capital from less-capitalized players (through rent payments and property transactions) to more-capitalized players and ultimately to the winner. The transfer mechanism is property monopoly — owning all properties in a color set doubles rent on unowned properties, then tripling it with houses — which accelerates wealth concentration geometrically. The final transfer is total: all remaining capital and property moves to the last surviving player.
% ABSENT_VOICES: Players who have been eliminated cannot argue for redistribution mechanisms, bankruptcy protection, or rule modification — they are no longer at the table. House-rule reformers and community game facilitators would argue for liquidity injection, slower elimination, and wealth redistribution, but this reading treats their amendments as corruptions of the rulebook's pedagogical truth, not legitimate readings.
% DISAPPEARANCE_RATIONALE: If the Monopoly rulebook and its elimination mandate disappeared, households and game groups would immediately adopt house rules permitting continued play, liquidity injection, and redistribution — the coordination problem would be solved differently by a different rule set. The particular extraction trajectory (inevitable wealth concentration leading to single-winner endgame) depends entirely on the rulebook's structure; absent that structure, players rearrange to achieve more balanced outcomes.
% FOUNDING_PROBLEM: How do we play a property-acquisition game with clear economic consequences and a definitive ending condition? The rulebook was designed to solve this by making property ownership, rent collection, and player elimination transparent, mathematical, and unambiguous.
% FOUNDING_PROBLEM_CORROBORATION: Game designers, competitive players, and economic theorists confirm the rulebook solves the coordination problem of providing a shared, unambiguous property system. However, educators and community game facilitators testify that the rulebook's elimination mandate creates a social problem (excluded players, prolonged endgames, relationship strain) that house rules were invented to address — the founding problem is 'live' as a technical coordination problem, but 'contested' as a social adequacy claim.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The measurement series tracks the extraction trajectory from game start to game end. Early extractiveness is low (0.12 at t=0) because players have equal starting capital and rent is not yet geometrically compounding; by mid-game (t=45, ε=0.58) extraction accelerates as monopolies form and rent doubles; late game (t=90, ε=0.68) extraction plateaus near its ceiling because only the winner remains to extract from the losers' elimination. Theater ratio (performative activity vs functional) stays low (0.05-0.28) because the rulebook's extraction mechanism is direct and explicit — there is minimal rhetorical overhead; the game does not need to hide what it is doing. Suppression requirement (the active force needed to sustain the constraint) rises steadily as late-game players face escalating rent and bankruptcy pressure — the rulebook requires enforcement (players must agree to elimination, must accept rent demands) rather than spontaneous compliance. This is declared as a MOUNTAIN because the reading asserts the extraction trajectory is not contingent on player cooperation but mathematically inevitable — a structural fact of the rules, not a constructed social arrangement. The beneficiaries are the rulebook's mathematical structure (vindicated by the outcome) and the monopoly capitalism doctrine (the reading's pedagogical point).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (the rulebook itself, or those enforcing its authority) and the payers (early-eliminated and late-game players) experience radically different types. From the rulebook's perspective, the extraction is not a constraint but a PROOF — the mathematical demonstration of capitalism's truth. From the eliminated player's perspective, the same structure is a SNARE — they are forced to play knowing elimination is inevitable, and the rulebook offers no exit. The engine computes this divergence: the rulebook's seat classifies as mountain (natural law, inevitable structure), while the eliminated player's seat classifies as snare (extractive, suppressive, no exit). This is the kernel reading's central contestation: what the extraction reading sees as pedagogical necessity, the social-scaffold reading sees as a social failure requiring correction.
 *
 * DIRECTIONALITY LOGIC:
 *   Early-eliminated players (powerless, trapped, immediate horizon) sit at d ≈ 0.95 — they are the targets of the extraction; their elimination IS the reading's pedagogical demonstration. Late-game survivors (moderate power, constrained exit) sit at d ≈ 0.70 — they continue paying escalating rent and bear extraction, but have some agency in property transactions and can slow their elimination through skillful play. The eventual winner sits at d ≈ 0.15 — they benefit from the extraction of all other players and have high exit options (they can stop playing anytime and keep their accumulated capital). House-rule reformers are excluded (role: excluded) — they would argue d should be lower for all players through redistribution, but the reading treats their amendments as corruptions of the rulebook's truth. The eventual winner and the reading's beneficiary (the vindicated proposition) align: both benefit from proving wealth concentration is inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'live' but the reading instantiates it as SOLVED BY PROOF rather than SOLVED BY FUNCTION. The rulebook was designed to coordinate a game (founding problem: how do we play property acquisition together?). Under the extraction reading, the rulebook's function is not to enable play but to demonstrate capitalism's truth. This is a mandatrophy candidate: the rulebook's original purpose (coordination of a playable game) has been superseded by a pedagogical reading (proof of inevitable extraction). The disappearance verdict confirms mandatrophy: if the rulebook vanished, players would immediately adopt house rules solving the original founding problem differently — the rulebook is not NECESSARY to coordinate property games, only necessary to instantiate this particular pedagogical reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_designed_system,
    'Is the rulebook''s wealth concentration trajectory a natural law (mathematical necessity of property systems) or a design choice (the rulebook COULD be different)? Does instantiating a pedagogical truth make it a mountain, or does the fact that humans wrote the rulebook make it a constructed snare?',
    'Compare elimination and wealth-concentration trajectories across variant rulesets: if all property systems converge on winner-take-all regardless of rules, the extraction is natural law; if alternative rules (progressive taxation, redistribution, slower elimination) produce different outcomes, the extraction is designed constraint.',
    'If natural law, the reading is correctly classified as mountain and the rulebook is merely the apparatus revealing an invariant truth. If designed constraint, the reading mis-categorizes an extractive system as inevitable, and the constraint should reclassify to snare or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_designed_system, conceptual, 'Whether the extraction is structural inevitability or artifact of the rulebook''s design choices.').

omega_variable(
    pedagogical_authority_grounding,
    'What grants the rulebook authority to function as a proof of capitalism''s truth? Is the authority grounded in the game''s mathematical structure, in tradition (Monopoly as canonical game), in institutional competitive use, or in the reading''s own interpretive frame?',
    'Examine whether non-Monopoly property games (real estate, stock markets, land reform simulations) converge on the same extraction proof, or whether the proof is specific to this rulebook''s design. Ask whether the rulebook''s competitive authority (tournament standard) is independent of its pedagogical authority (proof apparatus).',
    'If the proof is universal across property systems, the reading''s mountain classification holds. If the proof is specific to this rulebook, the reading is one interpretation layered on top of a designed system, and the constraint may be better classified as a kernel-reading of contested authority (the rulebook''s meaning) rather than as a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_authority_grounding, conceptual, 'Whether the rulebook''s authority as pedagogy is independent of its authority as competitive standard.').

omega_variable(
    house_rule_suppression_mechanism,
    'Is the suppression measurement (0.71 at game end) capturing active enforcement of the official rulebook, or is it measuring the social resistance to those who propose house-rule modifications? Are house rules a form of counter-suppression, or a failure to suppress?',
    'Measure the frequency of house-rule adoption in household games vs tournament games; track whether house rules emerge because players resist suppression or because the rulebook''s outcome is socially unacceptable. Examine whether house rules spread despite suppression or because suppression is absent in non-competitive contexts.',
    'If house rules emerge as counter-suppression (players resist the rulebook and modify it), the suppression metric should be higher and the constraint should reclassify toward snare. If house rules are merely playgroup preference (suppression is low in households), the suppression metric holds and the mountain classification is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_rule_suppression_mechanism, empirical, 'Whether suppression is enforcement of official rules or measurement of resistance to those rules.').

omega_variable(
    kernel_reading_boundary,
    'Is this reading a coherent interpretation of the rulebook kernel, or is it imposing a pedagogical framework the rulebook text does not explicitly claim? The rulebook says ''last player remaining wins'' and ''elimination is final'' — does that textual mandate instantiate a capitalism proof, or is the reading a creative interpretation added by theorists?',
    'Compare the reading''s claims to the rulebook''s stated purpose (Parker Brothers / Hasbro''s own design documentation); examine whether competitive players and casual players agree the rulebook''s PURPOSE is pedagogical proof or whether the pedagogy is an external interpretation. Ask whether the rulebook would be different if designed to teach a different lesson.',
    'If the rulebook''s text explicitly instantiates the pedagogy, the reading is a true kernel reading and the axioms stand. If the pedagogy is an external interpretation, the reading misattributes authority and may be better classified as one faction''s reading rather than a structural reading of the rulebook itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the pedagogical reading is embedded in the rulebook''s authority or imposed by interpreters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.27).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.28).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.67).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.54).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.67).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__extraction_demo_reading, 0.18).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The monopoly_rulebook kernel decomposes into three structurally distinct constraints, one per reading. Each instantiates a different type, beneficiary/victim structure, and ε from the same rulebook text. The extraction_demo_reading claims the rulebook IS a proof of capitalism's necessity (mountain, high ε, victims = eliminated players, pedagogical beneficiary = doctrine). The social_scaffold_reading claims the rulebook REQUIRES correction (scaffold, moderate ε, beneficiaries = continued-play enablement). The tournament_orthodoxy_reading claims the rulebook IS the legitimate competitive standard (rope, low ε, beneficiary = competitive ranking integrity). All three are live readings held by different communities; they coexist without logical foreclosure because they operate in different institutional contexts (pedagogical, household, competitive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, powerless, 0.95).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, moderate, 0.68).
constraint_indexing:directionality_override(monopoly_rulebook__extraction_demo_reading, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
