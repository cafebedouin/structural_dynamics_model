% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__social_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Monopoly Social Scaffold Reading (House Rules as Community Correction)
 *   domain: game_theory/social_coordination
 *
 * SUMMARY:
 *   This constraint story models the social_scaffold_reading of the
 *   monopoly_rulebook kernel: the printed Monopoly rulebook mandates a harsh
 *   zero-sum endgame with player elimination and wealth concentration, which
 *   proves socially unplayable in casual leisure contexts. To preserve the
 *   evening's coordination, players actively inject house rules (free parking
 *   jackpots, inter-player loans, elimination moratoria) that redistribute
 *   liquidity and extend duration. The constraint is the ensemble of these
 *   community corrections, which function as a temporary scaffold allowing
 *   the group to complete a 3+ hour social session. This reading coexists
 *   with sibling readings: tournament_orthodoxy_reading (the text is
 *   immutable for competitive ranking) and extraction_demo_reading (the text
 *   intentionally demonstrates capitalist extraction). The authored metrics
 *   and claimed type are independent: the claim is scaffold (transitional
 *   social support), while the metrics acknowledge moderate extraction from
 *   competitive strategists whose skill advantage is diluted by the
 *   redistribution.
 *
 * KEY AGENTS:
 *   - social_group_members: primary beneficiary (moderate/constrained) â receive extended coordination and inclusion
 *   - house_rule_advocate: agenda_setter (moderate/mobile) â administers the scaffold and proposes corrections
 *   - competitive_strategist: primary payer (moderate/mobile) â bears diluted competition and extended time cost
 *   - tournament_orthodox_player: excluded voice (moderate/mobile) â would demand rules-as-written integrity but is outside the leisure frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, 0.48).
domain_priors:suppression_score(monopoly_rulebook__social_scaffold_reading, 0.52).
domain_priors:theater_ratio(monopoly_rulebook__social_scaffold_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(monopoly_rulebook__social_scaffold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__social_scaffold_reading, scaffold).
narrative_ontology:human_readable(monopoly_rulebook__social_scaffold_reading, "Monopoly Social Scaffold Reading (House Rules as Community Correction)").
narrative_ontology:topic_domain(monopoly_rulebook__social_scaffold_reading, "game_theory/social_coordination").

domain_priors:requires_active_enforcement(monopoly_rulebook__social_scaffold_reading).
narrative_ontology:has_sunset_clause(monopoly_rulebook__social_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__social_scaffold_reading, '73c61e7d-c8c4-4518-8736-6043340ea2fd').
narrative_ontology:cs_kernel_codification('73c61e7d-c8c4-4518-8736-6043340ea2fd', fixed_text).
narrative_ontology:cs_authority_grounding('73c61e7d-c8c4-4518-8736-6043340ea2fd', practice).
narrative_ontology:cs_interpretation_layer_present('73c61e7d-c8c4-4518-8736-6043340ea2fd').
narrative_ontology:cs_reading_relation('73c61e7d-c8c4-4518-8736-6043340ea2fd', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('73c61e7d-c8c4-4518-8736-6043340ea2fd', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('73c61e7d-c8c4-4518-8736-6043340ea2fd', foundational, social_play_supersedes_text_elimination).
narrative_ontology:cs_axiom_status(social_play_supersedes_text_elimination, holdable).
narrative_ontology:cs_axiom_grounding('73c61e7d-c8c4-4518-8736-6043340ea2fd', social_play_supersedes_text_elimination, conventional).
narrative_ontology:cs_axiom('73c61e7d-c8c4-4518-8736-6043340ea2fd', foundational, leisure_mandates_inclusive_duration).
narrative_ontology:cs_axiom_status(leisure_mandates_inclusive_duration, holdable).
narrative_ontology:cs_axiom_grounding('73c61e7d-c8c4-4518-8736-6043340ea2fd', leisure_mandates_inclusive_duration, conventional).
narrative_ontology:cs_reference_frame('73c61e7d-c8c4-4518-8736-6043340ea2fd', corrected_social_play).
narrative_ontology:cs_drift_state('73c61e7d-c8c4-4518-8736-6043340ea2fd', literal_endgame_phase, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('73c61e7d-c8c4-4518-8736-6043340ea2fd', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__social_scaffold_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__social_scaffold_reading, social_group_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__social_scaffold_reading, competitive_strategist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A group of friends or family gathered for leisure play. They benefit from house rules that keep all players engaged and prevent early elimination, preserving the social atmosphere of the evening. They experience extended game duration and artificial liquidity as the price of inclusion.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, social_group_members, beneficiary,
    moderate, immediate, constrained, local).

% The player who proposes and enforces house rules such as free parking jackpots, no auctions, or inter-player loans. They set the corrective agenda to keep the game socially viable and prevent the text's elimination mechanics from ending the evening early.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, house_rule_advocate, agenda_setter,
    moderate, immediate, mobile, local).

% A player who understands the rulebook's competitive depth and would prefer rules-as-written play. They bear the cost of diluted strategy, slower gameplay, and reduced skill premium as house rules redistribute wealth and prevent elimination. They can leave but risk social friction.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, competitive_strategist, payer,
    moderate, immediate, mobile, local).

% Players committed to rules-as-written competitive play who are not present in the casual living-room context. They would object that house rules destroy the game's strategic integrity and pedagogical function, but their voice is absent from the social leisure frame.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__social_scaffold_reading, tournament_orthodox_player, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__social_scaffold_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a multi-hour board-game session to remain socially playable by preventing early player elimination and maintaining engagement across mixed skill levels, compensating for a printed rulebook that otherwise produces a rapid zero-sum endgame.
% TRANSFER_FUNCTION: Moves time, attention, and competitive advantage from skilled players toward prolonged group participation; redistributes liquidity and survival probability to sustain coordination until the session's natural social conclusion.
% ABSENT_VOICES: Competitive tournament players and rules-as-written purists are absent from the casual living-room context; they would object that house rules destroy strategic integrity and the game's designed pedagogical arc, but their seats are not represented.
% DISAPPEARANCE_RATIONALE: If the house-rule scaffold vanished mid-session, the text's elimination mechanics would knock out weaker players early, the social dynamic would fracture, and the group would likely abandon the game or shift to another activityâthe evening's coordination depends on the correction.
% FOUNDING_PROBLEM: The Monopoly rulebook's literal mechanics produce early elimination, wealth concentration, and player downtime, making the game dysfunctional as a leisure activity among friends with divergent skill and risk profiles.
% FOUNDING_PROBLEM_CORROBORATION: Casual players and family groups attest that literal-rules games end in conflict or dropout; game-design critics outside the benefiting leisure set corroborate that the rulebook's elimination mechanics create poor engagement for casual contexts, though tournament organizers dispute this is a flaw.
narrative_ontology:disappearance_verdict(monopoly_rulebook__social_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__social_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__social_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__social_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__social_scaffold_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__social_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_rulebook__social_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monopoly_rulebook__social_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the house rules do transfer valueâskilled players' time and strategic edge are partly expropriated to subsidize continued participation for weaker playersâbut the transfer is bounded by the leisure context and the group's tolerance. Suppression is moderate (0.52) because the correction requires active social enforcement: players must agree to ignore the text's elimination triggers and stigmatize rules-lawyering. Theater ratio rises from 0.15 to 0.33 as the game progresses: early house rules are functional, but later they become increasingly performative as the underlying economic dynamics make the 'correction' obviously artificial. Accessibility collapse is moderate (0.58): once the group commits to house rules, reverting to text mid-game is socially inaccessible. Resistance is moderate (0.40): competitive players occasionally push back but are outvoted or socially pressured.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as benign coordination that rescues the evening; the competitive_strategist seat experiences it as active redistribution that punishes competence. The engine will compute divergent per-seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The social_group_members are structural beneficiaries (low d): the scaffold subsidizes their participation and extends their evening. The house_rule_advocate sits near the beneficiary end, gaining social status rather than material extraction. The competitive_strategist is the structural target (high d): their skill premium is the resource redistributed to preserve coordination. The excluded tournament_orthodox_player would experience maximal extraction if forced into the frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is gated by has_sunset_clause: trueâthe house rules expire at the end of the session. Without that sunset, the same structure would drift toward tangled_rope (persistent redistribution enforced by social pressure) or piton (unquestioned house rules that outlived their original social context). The mandatrophy check ensures we do not mislabel a temporary, mutually-beneficial correction as a permanent extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the social_scaffold reading of the Monopoly rulebook represent a genuine temporary coordination support, or does it obscure the rulebook''s extractive pedagogical function by reframing redistribution as benign social maintenance?',
    'Comparative corpus analysis against extraction_demo_reading: if the same house rules appear in contexts where players explicitly acknowledge the extraction narrative, the scaffold reading is context-dependent framing rather than structural independence.',
    'If the extraction narrative is inescapable even in leisure contexts, this constraint links more strongly to extraction_demo_reading as a dual formulation and epsilon should rise; if separable, the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between scaffold coordination and extraction cover story within kernel readings.').

omega_variable(
    scaffold_sunset_or_piton,
    'Do house rules persist across multiple game sessions as an unwritten tradition, or do they genuinely reset to the text at each session''s end?',
    'Longitudinal observation of the same social group across repeated Monopoly sessions; persistence indicates piton or tangled_rope drift.',
    'If persistent, the scaffold classification fails and the constraint reclassifies toward piton (theater without function) or tangled_rope (asymmetric extraction via social pressure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_or_piton, empirical, 'Whether the scaffold''s sunset is real or theoretical.').

omega_variable(
    extraction_beneficiary_or_diffuse,
    'Is the social cohesion produced by house rules a diffuse public good among players, or does it accrue disproportionately to a subset (e.g., the advocate or weaker players)?',
    'Seat-resolved payoff mapping: track who proposes house rules and who gains material or status advantage from their application.',
    'If concentrated, gain_flow should name a specific seat rather than ''diffuse'', shifting directionality and potentially raising extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_or_diffuse, empirical, 'Distribution of scaffold benefits across player seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__social_scaffold_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_social_scaffold_tr_t0, monopoly_rulebook__social_scaffold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t2, monopoly_rulebook__social_scaffold_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t4, monopoly_rulebook__social_scaffold_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(monopoly_social_scaffold_tr_t6, monopoly_rulebook__social_scaffold_reading, theater_ratio, 6, 0.33).

% Extraction over time
narrative_ontology:measurement(monopoly_social_scaffold_be_t0, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(monopoly_social_scaffold_be_t2, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(monopoly_social_scaffold_be_t4, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(monopoly_social_scaffold_be_t6, monopoly_rulebook__social_scaffold_reading, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(monopoly_social_scaffold_su_t0, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(monopoly_social_scaffold_su_t2, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 2, 0.35).
narrative_ontology:measurement(monopoly_social_scaffold_su_t4, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(monopoly_social_scaffold_su_t6, monopoly_rulebook__social_scaffold_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__social_scaffold_reading, attachment_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
