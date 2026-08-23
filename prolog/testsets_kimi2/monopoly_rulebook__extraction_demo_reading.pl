% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monopoly_rulebook__extraction_demo_reading
 *   human_readable: Monopoly Rulebook as Pedagogical Extraction Demonstration
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This constraint story captures the extraction_demo_reading of the
 *   monopoly_rulebook kernel. In this reading, the official rulebook â with
 *   its fixed rent schedules, monopoly escalation, and bankruptcy elimination
 *   â is not merely a recreational framework but a pedagogical
 *   demonstration of monopoly capitalism's structural inevitability. The
 *   constraint is the rulebook's arrangement of rent extraction and
 *   winner-takes-all elimination. The reading treats this arrangement as
 *   revealing a natural law of wealth concentration, yet the authored metrics
 *   describe a highly extractive, constructed rule set that systematically
 *   eliminates players. This divergence between the mountain claim and the
 *   extractive metrics is the core signal the corpus is designed to detect.
 *
 * KEY AGENTS:
 *   - monopoly_holder: structural beneficiary (powerful/mobile) â extracts rents and eliminates opponents
 *   - eliminated_players: structural targets (moderate/constrained) â progressively bankrupted by the rule set
 *   - game_publisher: agenda-setter (institutional/arbitrage) â owns and enforces the official text
 *   - house_rule_proponents: excluded voices (moderate/constrained) â advocate for modifications that slow extraction
 *   - critical_pedagogue: analytical observer (analytical/analytical) â reads the game as ideological demonstration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.68).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.45).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Pedagogical Extraction Demonstration").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'ccffa753-c6e8-41cb-a9f6-66996eb2eb55').
narrative_ontology:cs_kernel_codification('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', fixed_text).
narrative_ontology:cs_authority_grounding('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', lineage).
narrative_ontology:cs_reading_relation('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', foundational, rent_extraction_inevitable).
narrative_ontology:cs_axiom_status(rent_extraction_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', rent_extraction_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', foundational, elimination_pedagogically_required).
narrative_ontology:cs_axiom_status(elimination_pedagogically_required, holdable).
narrative_ontology:cs_axiom_grounding('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', elimination_pedagogically_required, instrumental).
narrative_ontology:cs_reference_frame('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', textual_rent_extraction_pure).
narrative_ontology:cs_drift_state('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', contemporary_family_play, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ccffa753-c6e8-41cb-a9f6-66996eb2eb55', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, monopoly_holder).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Player who acquires color monopolies, builds houses and hotels, and extracts escalating rents from opponents. The rulebook structurally rewards this seat with the entire bank as opponents are eliminated.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, monopoly_holder, beneficiary,
    powerful, immediate, mobile, local).

% Players who land on monopolized properties, pay rents, mortgage assets, and are progressively bankrupted until they are removed from play. Social pressure to finish the game and respect the rulebook limits their exit.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    moderate, immediate, constrained, local).

% Owns the intellectual property, prints the official rulebook, and enforces text authority in tournament settings. Can revise rules but benefits from the cultural persistence of the classic rule set.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_publisher, agenda_setter,
    institutional, generational, arbitrage, global).

% Players and families who modify rules such as Free Parking jackpots, no auctions, or liquidity injections to prolong play and soften elimination. Their modifications are delegitimized by official readings and tournament standards.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_proponents, excluded,
    moderate, immediate, constrained, local).

% Analyzes the game as a simulation of capitalist extraction. Treats the rulebook not as a neutral recreational frame but as a pedagogical device that demonstrates the structural inevitability of wealth concentration.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, critical_pedagogue, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, monopoly_holder).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized competitive framework for a multiplayer board game, resolving coordination problems about turn order, property acquisition, rent calculation, and bankruptcy procedure.
% TRANSFER_FUNCTION: Moves accumulated wealth from players who land on monopolized properties to the monopoly holder through fixed rent schedules, progressively concentrating the bank until all opposing players are eliminated.
% ABSENT_VOICES: House rule proponents who would soften the elimination mechanic; cooperative-game advocates who reject zero-sum structures; players eliminated early who have no continued voice in the session.
% DISAPPEARANCE_RATIONALE: If the rulebook's specific rent schedules, monopoly escalation mechanics, and bankruptcy elimination rules vanished, the game would dissolve into unstructured play or house-rule variants; the wealth concentration outcome this reading focuses on would not reproduce.
% FOUNDING_PROBLEM: How to create a standardized board game simulating property trading and competitive accumulation with a clear victory condition.
% FOUNDING_PROBLEM_CORROBORATION: Game historians outside the benefiting parties attest that Lizzie Magie's original design intended a Georgist critique of land monopoly; the extraction-demo reading is a later reframing. Commercial publishers attest the game is entertainment. No unanimous corroboration exists from outside all contesting parties.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.68) because the rulebook structurally mandates escalating rent extraction with no redistribution or liquidity injection, guaranteeing wealth concentration. Suppression is moderate (0.45): enforcement depends on social contract and player buy-in rather than external coercion, but social pressure to 'play by the rules' and finish the game constrains exit. Theater ratio is low (0.20) because the extraction is mechanically functional â rents are paid, properties are lost â though some performative friendliness may overlay the zero-sum endgame. Accessibility collapse is very high (0.90) because once the rulebook is accepted as the legitimate frame, alternative play modes are cognitively marginalized. Resistance is low (0.15) because players typically accept the rulebook as 'just how the game works.' The measurement series run on a single shared time grid spanning the 90-minute session.
 *
 * PERSPECTIVAL GAP:
 *   The monopoly_holder and game_publisher seats experience the constraint as legitimate, naturalized coordination â 'this is how Monopoly works.' The eliminated_players experience it as progressively extractive and inescapable within the social frame of the game session. The engine computes this divergence from the structural asymmetry in exit options (mobile vs constrained) and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The monopoly_holder is the declared beneficiary (collects rents, can end the game â d near 0.0). Eliminated_players are declared payers (bear extraction, socially constrained to continue â d near 1.0). The game_publisher, as agenda-setter with arbitrage-grade exit (can revise the rulebook), sits near the beneficiary end though it does not directly collect in-game rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â creating a standardized property-trading board game â is long solved. The rulebook persists not because the coordination problem is live, but because the text has become culturally fixed. The extraction_demo reading naturalizes this persistence as pedagogical truth, masking that the specific elimination mechanic is a design choice, not an inevitability. A genuine mountain (natural law) would not have a victim set; the presence of eliminated_players as structural payers signals that this is a constructed constraint performing mountainhood.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is the wealth concentration produced by the Monopoly rulebook a demonstration of an inevitable natural law, or a contingent feature of this specific rule set designed by particular agents?',
    'Comparison with alternative board games that simulate property without elimination mechanics (e.g., cooperative variants). If those produce different wealth distributions, the outcome is constructed, not natural.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies as snare or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Whether the rulebook reveals natural law or is a constructed extraction mechanism').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is player compliance with the elimination mechanic driven by structural enforcement (the rulebook as external authority) or internalized acceptance (players believing ''that''s just the game'')?',
    'Observe behavior when house rules are proposed. If players resist modifications that would save eliminated players despite no external penalty, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in voluntary game contexts').

omega_variable(
    founding_problem_origins,
    'Does the original design intent (Lizzie Magie''s Georgist critique) or the later commercialization constitute the authentic founding problem of the constraint?',
    'Historical archival research into design documents, patents, and early marketing materials.',
    'If the original intent was critical pedagogy opposing land monopoly, the extraction_demo reading is an ironic inversion rather than a fulfillment, altering the mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_origins, empirical, 'Original design intent vs later commercial framing ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.16).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.2).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.48).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__extraction_demo_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% The monopoly_rulebook kernel decomposes into three structurally distinct constraints: extraction_demo_reading (high epsilon, mountain claim, victim set), social_scaffold_reading (coordination through house rules), and tournament_orthodoxy_reading (immutability for competitive ranking). Each reading instantiates a different constraint with a different beneficiary/victim structure and epsilon profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
