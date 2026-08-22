% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Monopoly Rulebook as Extraction Demonstration (Extraction-Demo Reading)
 *   domain: game_theory/social_coordination/institutional_design
 *
 * SUMMARY:
 *   This file instantiates the extraction_demo_reading of the
 *   monopoly_rulebook kernel: the official text, played uncorrected, as a
 *   self-operating demonstration in which scheduled rent transfers compound,
 *   wealth concentrates, and players are eliminated — with the harshness read
 *   as the demonstration's payload rather than a defect. The epsilon referent
 *   is the standing arrangement under contest: the rulebook as written and
 *   played without community correction, assessed by this reading's own
 *   lights. Sibling readings (social_scaffold_reading,
 *   tournament_orthodoxy_reading) are separate constraints in separate files
 *   and are not averaged into this one. Claim/metric independence is
 *   deliberate: the reading claims mountain — the outcome framed as
 *   structurally necessary — while the metrics describe a substantially
 *   extractive, fully self-executing mechanism with named beneficiaries; the
 *   engine measures that divergence. KEY AGENTS (by structural relationship):
 *   - winning_player: primary beneficiary (moderate/constrained) — collects
 *   scheduled rents and the assets of every eliminated player; bound by the
 *   same text it profits under - eliminated_players: primary target
 *   (powerless/trapped) — bear compounding rents until bankruptcy removes
 *   them from the session - official_rulebook_publisher: agenda setter
 *   (institutional/arbitrage) — owns and prints the canonical text, collects
 *   on every copy, could amend the rules at will -
 *   political_economy_educator: analytical observer — runs the text as a
 *   demonstration and reads the outcome as the lesson
 *
 * KEY AGENTS:
 *   - winning_player: primary beneficiary (moderate/constrained) — collects scheduled rents and the assets of every eliminated player; bound by the same text it profits under
 *   - eliminated_players: primary target (powerless/trapped) — bear compounding rents until bankruptcy removes them from the session
 *   - official_rulebook_publisher: agenda setter (institutional/arbitrage) — owns and prints the canonical text, collects on every copy, could amend the rules at will
 *   - political_economy_educator: analytical observer — runs the text as a demonstration and reads the outcome as the lesson
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.68).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.48).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Extraction Demonstration (Extraction-Demo Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game_theory/social_coordination/institutional_design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, '44eba57a-2f1d-4d06-b038-884fde64aa3a').
narrative_ontology:cs_kernel_codification('44eba57a-2f1d-4d06-b038-884fde64aa3a', fixed_text).
narrative_ontology:cs_authority_grounding('44eba57a-2f1d-4d06-b038-884fde64aa3a', self_enforcing).
narrative_ontology:cs_reading_relation('44eba57a-2f1d-4d06-b038-884fde64aa3a', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('44eba57a-2f1d-4d06-b038-884fde64aa3a', monopoly_rulebook__tournament_orthodoxy_reading, influences).
narrative_ontology:cs_axiom('44eba57a-2f1d-4d06-b038-884fde64aa3a', foundational, wealth_concentration_structurally_inevitable).
narrative_ontology:cs_axiom_status(wealth_concentration_structurally_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('44eba57a-2f1d-4d06-b038-884fde64aa3a', wealth_concentration_structurally_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('44eba57a-2f1d-4d06-b038-884fde64aa3a', foundational, elimination_carries_pedagogical_truth).
narrative_ontology:cs_axiom_status(elimination_carries_pedagogical_truth, holdable).
narrative_ontology:cs_axiom_grounding('44eba57a-2f1d-4d06-b038-884fde64aa3a', elimination_carries_pedagogical_truth, instrumental).
narrative_ontology:cs_reference_frame('44eba57a-2f1d-4d06-b038-884fde64aa3a', rulebook_as_capitalist_microcosm).
narrative_ontology:cs_drift_state('44eba57a-2f1d-4d06-b038-884fde64aa3a', contemporary_tabletop_play, gap(stable, minor, true)).
narrative_ontology:cs_created_at('44eba57a-2f1d-4d06-b038-884fde64aa3a', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, winning_player).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, official_rulebook_publisher).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, inevitable_wealth_concentration_thesis).
narrative_ontology:constraint_vindicates(monopoly_rulebook__extraction_demo_reading, rent_extraction_compounding_dynamic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits down with the same starting capital as everyone else. Buys properties, charges scheduled rents to whoever lands there, and trades from a position that improves as rivals' cash drains. As opponents mortgage and drop out, acquires their remaining holdings. Cannot change any rule mid-session; ends the session holding nearly all assets when every rival is bankrupt or concedes.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, winning_player, beneficiary,
    moderate, immediate, constrained, local).

% Pay rent on every landing on owned property; income arrives only from passing the salary space and from drawn cards. When cash runs short, mortgage properties at half value and sell buildings back at a loss. Once liabilities exceed assets, declare bankruptcy, hand remaining holdings to the creditor, and leave the table while the session continues without them. Quitting earlier forfeits outright, so staying is the only path that preserves any standing.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Owns the trademark and prints the canonical rulebook shipped in every box. Collects revenue on each copy sold and on licensed editions worldwide. Periodically reissues the text with cosmetic updates; the core rent schedule, bankruptcy procedure, and single-winner condition have remained stable for decades. Could print amended rules at any time and bears no cost from any single session's outcome.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, official_rulebook_publisher, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, official_rulebook_publisher, beneficiary).

% Uses the game in seminars on political economy: runs sessions under the unmodified text, tracks who bankrupts whom, and presents the resulting concentration pattern as a small-scale illustration of rent-based accumulation. Takes no side in any single session's outcome and exits the arrangement freely when the demonstration concludes.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, political_economy_educator, observer,
    analytical, biographical, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, winning_player).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a bounded multiplayer session: fixes turn order, defines purchasable assets and their prices, schedules rents, and specifies bankruptcy so that two to eight players can play a complete competitive session to a definite end without negotiating procedure.
% TRANSFER_FUNCTION: Moves in-game capital from whichever player lands on an owned space to that property's owner, on a schedule set by the rent table; over a session it consolidates the losing players' cash and deeds into one player's hands, ending with a single holder of essentially all assets.
% ABSENT_VOICES: Bankrupted players exit the table and have no further voice in the session they helped populate. The original designer's companion rule set — which paired the monopolist rules with prosperity rules producing circulation instead of elimination — is absent from the canonical text. Players who prefer cooperative or redistributive play are served by no official mechanism and must improvise outside it.
% DISAPPEARANCE_RATIONALE: Game nights, classroom demonstrations, and a large market of themed and licensed editions are organized around this exact text; overnight removal would scatter sessions to substitute games or improvised variants, strand licensed merchandise, and erase a widely used teaching demonstration — the arrangements built on it would visibly reorganize.
% FOUNDING_PROBLEM: The design descends from a 1904 pedagogical game built to show, by direct experience, how land monopolization concentrates wealth and immiserates tenants — with a second, prosperity-oriented rule set offered as the contrast case. The commercial edition retained the monopolist rules as a mass-market entertainment proposition: an evening-long competitive game with a definite winner.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the designer's 1904 patent filing and her contemporaneous publications state the instructional purpose in her own words, and later game-history scholarship documents the dual-rule design and its commercial truncation. Neither the current rights-holder nor session winners attest the founding problem — the attestation comes from the historical record and independent scholarship.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.68: the rent schedule transfers capital from mobile players to property owners automatically and compounds; by design most seats end bankrupt and one holds essentially everything. Suppression 0.48 is authored as a raw structural property (unscaled by power or scope): entry is voluntary, but once a session begins the text binds completely — quitting forfeits, and no in-text mechanism relents. Theater 0.11: the mechanism performs exactly its written function; the mild late-session rise reflects procedurally determined endgames playing out after the outcome is settled. Accessibility_collapse 0.78: past the concentration threshold no legal move restores parity for a trapped seat, though alternatives persist outside the text (other games, house-ruled sessions), keeping the value below natural-law range. Resistance 0.35: rage-quits and the widespread cultural drift to house-ruled play evidence real friction, none of which alters the canonical text's operation. Coalition note: the victim seats cannot coordinate effectively — elimination is sequential and asymmetric, so each bankrupt seat exits before the next is pressed, denying the class a joint bargaining moment. Measurement grid: one shared seven-point grid (minutes 0-90 of a representative session) carries both tracked series; suppression_requirement is intentionally untracked because the text needs no enforcement machinery — its static character lives in base_properties.suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the winner's seat the text is an opportunity structure: early acquisitions compound into dominance, and every rule that ruins a rival enriches this seat. From the eliminated seats the identical text is a ratchet: every landing on owned ground transfers capital upward and every circuit of the board narrows their option set until bankruptcy ends their participation. The publisher's seat registers neither dynamic — it registers unit sales and text stability. The engine computes these divergent classifications from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   winning_player is declared a beneficiary: the transfer schedule subsidizes whoever holds property, and the seat ends the session holding nearly everything (d near the beneficiary end, tempered by constrained exit — the winner is also bound by the text it profits under). eliminated_players are declared victims with trapped exit: once capital falls below the viability threshold the rules offer no recovery path, placing the seat near the full-target end. official_rulebook_publisher is declared a beneficiary of the arrangement's persistence (revenue on every copy sold) but is not the recipient of in-game transfers — the receipt surface names the winning seat, keeping receipt distinct from benefit. political_economy_educator holds the analytical seat and feeds no directionality. Spatial scope is local (a tabletop), so scope amplification of effective extraction is minimal; the extraction signal rides on directionality, not scale.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — demonstrating how rent-based accumulation concentrates wealth and eliminates the propertyless — is performed anew every session; the arrangement's function and its operation coincide, so the mandate has not outlived its function. The sharper risk is the reverse mislabel: the reading claims mountain (structural necessity, no enforcement, naturality) while the structural data names beneficiaries and authored parameters. The false-summit signature exists precisely to test such claims; if the engine reclassifies on beneficiary presence, the 'pedagogical truth' framing is exposed as a designed artifact wearing natural-law dress. Either way the classification prevents conflating a working demonstration with an immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    designed_parameters_vs_economic_law,
    'Is the concentration-and-elimination dynamic a discovered regularity the text faithfully reproduces, or the product of design parameters (rent multipliers, scarce properties, no liquidity injection) chosen to guarantee that outcome?',
    'Comparative rule-set analysis: run the historical companion rule set and parameter-varied variants under identical play; if modest parameter changes flip the outcome from universal elimination to a circulating equilibrium, the dynamic is designed rather than discovered.',
    'If designed, the mountain framing fails and the arrangement computes as a constructed mechanism with identifiable beneficiaries — the ''pedagogical truth'' becomes a demonstration of design choices, not of economic necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(designed_parameters_vs_economic_law, conceptual, 'Whether the text''s dynamics reflect natural economic law or authored parameters.').

omega_variable(
    one_reading_of_monopoly_kernel,
    'This constraint is the extraction_demo_reading of kernel monopoly_rulebook; what would the sibling readings change structurally, and where exactly does the disagreement bite?',
    'Adopting the social_scaffold_reading adds community-correction mechanisms to the operative arrangement (liquidity injections, slowed elimination), lowering measured extraction; adopting tournament_orthodoxy_reading keeps the text intact but reframes outcomes as skill-ranked competition, changing the legitimacy structure while leaving the transfer pattern untouched.',
    'Classification is reading-indexed: the same printed text yields a high-extraction demonstration under this reading, a softened coordination arrangement under the scaffold reading, and a legitimated contest under the orthodoxy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_monopoly_kernel, conceptual, 'Committer structure: this file is one of three readings of the monopoly_rulebook kernel.').

omega_variable(
    elimination_equilibrium_uniqueness,
    'Is elimination truly the unique absorbing outcome under the written rules, or do the rules admit non-eliminating equilibria (perpetual trading stalemates, deliberate capital circulation among surviving players)?',
    'Formal state-space analysis or large-scale computational play of the exact text, searching for recurrent non-absorbing trajectories under optimal and heuristic play.',
    'If non-elimination paths exist with nonzero probability, the ''structurally necessary outcome'' premise weakens and the determinism supporting the mountain claim drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elimination_equilibrium_uniqueness, empirical, 'Whether the written rules make elimination the unique long-run outcome.').

omega_variable(
    microcosm_scale_validity,
    'Does the tabletop demonstration carry to the economies it is used to illustrate, or do disanalogies (closed board, no production sector, no state, finite horizon) break the inferential bridge?',
    'Comparative review in political economy of game-based instruction: identify which concentration mechanisms in the game have real-world counterparts (rent escalation, distress sales, liquidity cascades) and which are artifacts of the board.',
    'If the bridge breaks, the reading''s pedagogical claim collapses and the arrangement reduces to entertainment whose dynamics are incidental rather than instructive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(microcosm_scale_validity, empirical, 'Whether the game''s dynamics validly illustrate large-scale rent-based accumulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(extraction_demo_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(extraction_demo_tr_t0, observed).
narrative_ontology:measurement(extraction_demo_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement_basis(extraction_demo_tr_t15, observed).
narrative_ontology:measurement(extraction_demo_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement_basis(extraction_demo_tr_t30, observed).
narrative_ontology:measurement(extraction_demo_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.07).
narrative_ontology:measurement_basis(extraction_demo_tr_t45, observed).
narrative_ontology:measurement(extraction_demo_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(extraction_demo_tr_t60, observed).
narrative_ontology:measurement(extraction_demo_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement_basis(extraction_demo_tr_t75, observed).
narrative_ontology:measurement(extraction_demo_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.11).
narrative_ontology:measurement_basis(extraction_demo_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(extraction_demo_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(extraction_demo_be_t0, observed).
narrative_ontology:measurement(extraction_demo_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(extraction_demo_be_t15, observed).
narrative_ontology:measurement(extraction_demo_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement_basis(extraction_demo_be_t30, observed).
narrative_ontology:measurement(extraction_demo_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.47).
narrative_ontology:measurement_basis(extraction_demo_be_t45, observed).
narrative_ontology:measurement(extraction_demo_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(extraction_demo_be_t60, observed).
narrative_ontology:measurement(extraction_demo_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement_basis(extraction_demo_be_t75, observed).
narrative_ontology:measurement(extraction_demo_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.68).
narrative_ontology:measurement_basis(extraction_demo_be_t90, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monopoly_rulebook__extraction_demo_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the monopoly_rulebook kernel per the epsilon-invariance principle: one printed text, three structurally distinct constraints. This story (extraction_demo_reading) authors high epsilon over the uncorrected text with eliminated players as victims; social_scaffold_reading authors the corrected, house-ruled arrangement with lower epsilon and a coordination function; tournament_orthodoxy_reading authors the same text as legitimated competition with a legitimacy structure instead of a victim set. The upstream member is the canonical text itself; this reading depends on text immutability (shared with the orthodoxy reading) for the purity of its demonstration, and is cited against the scaffold reading as evidence that correction destroys the lesson. Each file links the other two via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
