% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__extraction_demo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Monopoly Rulebook as Rent-Concentration Demonstration (Extraction-Demo Reading)
 *   domain: economic/game-theory/institutional-design
 *
 * SUMMARY:
 *   A printed rulebook mandates a property-trading game in which rent flows
 *   to whoever completes an uncontested monopoly, no mechanism returns
 *   capital to the paying side, and insolvency ends a player's participation
 *   permanently. This story instantiates ONE reading of that rulebook — the
 *   extraction-demonstration reading — which holds that the mandated
 *   arrangement faithfully exhibits how private ownership of land and
 *   utilities concentrates all wealth in few hands, and that the elimination
 *   terminus is not a defect but the demonstration's payload. Per the
 *   epsilon-invariance principle, the sibling readings (social correction,
 *   competitive orthodoxy) are separate constraints in separate files; they
 *   are linked, not folded in. KEY AGENTS (by structural relationship): the
 *   winner seat (powerful/constrained) collects the escalating transfers; the
 *   bankrupted (powerless/trapped) bear them until removal; midgame players
 *   (moderate/constrained) pay while still holding assets; the demonstration
 *   host (moderate/mobile) administers by-the-book protocol without
 *   collecting in-game money; house-rule advocates (organized/mobile/global)
 *   stand excluded by protocol and answer with exit; game-history scholars
 *   (analytical) observe the full genealogy.
 *
 * KEY AGENTS:
 *   - - monopoly_accumulating_winners: Primary beneficiary seat ([powerful]/[constrained]) — receives every rent stream; the rules reward accumulation and penalize restraint, so even a reluctant occupant of this seat is carried to the same terminus
 *   - - bankrupt_eliminated_players: Primary payer seat ([powerless]/[trapped]) — pays until insolvent, then exits permanently with no in-game voice
 *   - - struggling_midgame_players: Dual-positioned seat ([moderate]/[constrained]) — collects scattered small rents while paying larger ones; coalition counter-play is available but unstable under individual defection incentives
 *   - - demonstration_host_facilitators: Agenda-setting seat ([moderate]/[mobile]) — administers the by-the-book protocol, adjudicates procedure, collects nothing in-game
 *   - - house_rule_advocates: Excluded seat ([organized]/[mobile]/global) — the liquidity-injecting majority, barred from the conversation and answering with exit to modified tables
 *   - - game_history_scholars: Analytical observer ([analytical]/[analytical]) — documents the pedagogical origin and the commercial pivot
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.72).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.66).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Monopoly Rulebook as Rent-Concentration Demonstration (Extraction-Demo Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "economic/game-theory/institutional-design").

domain_priors:requires_active_enforcement(monopoly_rulebook__extraction_demo_reading).
domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'a47b5316-504f-45f4-b8bc-3310173f1ee7').
narrative_ontology:cs_kernel_codification('a47b5316-504f-45f4-b8bc-3310173f1ee7', fixed_text).
narrative_ontology:cs_authority_grounding('a47b5316-504f-45f4-b8bc-3310173f1ee7', lineage).
narrative_ontology:cs_interpretation_layer_present('a47b5316-504f-45f4-b8bc-3310173f1ee7').
narrative_ontology:cs_reading_relation('a47b5316-504f-45f4-b8bc-3310173f1ee7', monopoly_rulebook__social_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('a47b5316-504f-45f4-b8bc-3310173f1ee7', monopoly_rulebook__tournament_orthodoxy_playing_standard, influences).
narrative_ontology:cs_axiom('a47b5316-504f-45f4-b8bc-3310173f1ee7', foundational, rent_concentration_is_structurally_inevitable_under_the_text).
narrative_ontology:cs_axiom_status(rent_concentration_is_structurally_inevitable_under_the_text, holdable).
narrative_ontology:cs_axiom_grounding('a47b5316-504f-45f4-b8bc-3310173f1ee7', rent_concentration_is_structurally_inevitable_under_the_text, empirically_contingent).
narrative_ontology:cs_axiom('a47b5316-504f-45f4-b8bc-3310173f1ee7', secondary, mandated_elimination_carries_the_demonstration_payload).
narrative_ontology:cs_axiom_status(mandated_elimination_carries_the_demonstration_payload, holdable).
narrative_ontology:cs_axiom_grounding('a47b5316-504f-45f4-b8bc-3310173f1ee7', mandated_elimination_carries_the_demonstration_payload, instrumental).
narrative_ontology:cs_reference_frame('a47b5316-504f-45f4-b8bc-3310173f1ee7', founder_georgist_demonstration_design).
narrative_ontology:cs_drift_state('a47b5316-504f-45f4-b8bc-3310173f1ee7', commercial_mass_market_play, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a47b5316-504f-45f4-b8bc-3310173f1ee7', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, monopoly_accumulating_winners).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, bankrupt_eliminated_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, struggling_midgame_players).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, struggling_midgame_players).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assembles uncontested color-group monopolies, builds houses and hotels, and collects escalating payments from every opponent who lands. The rulebook gives them no way to decline the position: refusing to develop or collect forfeits the competitive race, and the same rules that reward accumulation punish restraint. Exit from the arrangement means winning, which ends the game.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, monopoly_accumulating_winners, beneficiary,
    powerful, immediate, constrained, local).

% Pay rent on every landing, mortgage and sell holdings at distressed prices, and eventually owe more than their liquid assets cover. Bankruptcy removes them from the table permanently; the rules provide no re-entry, no relief payment, and no appeal. Once eliminated, they have no standing left to object inside the game — their objection can only take the form of leaving it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, bankrupt_eliminated_players, payer,
    powerless, immediate, trapped, local).

% Hold scattered properties and collect occasional small rents while paying far larger ones to whoever completed a monopoly first. Every legal move — mortgaging, selling, trading, auction bidding — is open to them, but each converges on the same destination unless they win the monopoly race outright. They may form temporary trading coalitions to blockade a leader, but such cartels are unstable under the rules' individual incentive to defect.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, struggling_midgame_players, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, struggling_midgame_players, beneficiary).

% Sets the table's protocol: the printed rules govern, no house amendments, play runs to the rulebook's conclusion. They adjudicate disputes over auctions, bankruptcy timing, and loans, and they hold the line against the recurring proposal to soften the endgame. Nothing binds them to this role — they can announce house rules at any session — but within a session they administer the arrangement as written and collect no in-game money for doing so.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, demonstration_host_facilitators, agenda_setter,
    moderate, biographical, mobile, local).

% The large population of players who favor free-parking jackpots, doubled starting salaries, mercy loans, and slowed elimination. Under the by-the-book protocol they are ruled out of the conversation before the first roll: their proposals are classified as corruption of the demonstration rather than contributions to it. Their practical response has been exit — most household tables worldwide play modified versions — which is why their voice survives everywhere except inside the pure-rule session.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_advocates, excluded,
    organized, biographical, mobile, global).

% Study the rulebook's genealogy: the original patented Landlord's Game designed to teach a critique of land rent, its commercial successor that kept the mechanics and changed the purpose, and the century of play those two intents produced. They publish the documentation that lets anyone check which parts of the current text are inherited pedagogy and which are product design.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, game_history_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, monopoly_accumulating_winners).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem for two to eight strangers: turn order, property pricing, auction procedure, building rules, and bankruptcy procedure are specified once and centrally, so a property-trading game can run without the players negotiating meta-rules before every session.
% TRANSFER_FUNCTION: Moves in-game capital from every player, via rent on developed monopolies, toward whoever first assembles an uncontrollable position; the terminal transfers strip the last solvent opponents of everything and leave one holder of the entire board.
% ABSENT_VOICES: House-rule advocates would object that the mandated endgame makes the game socially unplayable and would inject liquidity and slow elimination; they are excluded by the protocol's by-the-book rule. Bankrupted players would object loudest of all, but elimination strips them of in-game standing — their objection arrives only as departure. Both voices live outside this reading's session, in the sibling readings and at modified tables.
% DISAPPEARANCE_RATIONALE: If the rulebook's mandated arrangement vanished overnight, tables would reorganize around modified play within a generation — which is roughly what already happened wherever house rules spread: liquidity returns, eliminations slow, and the winner-takes-all terminus becomes rare. The seats that depend on the arrangement as written (the demonstration host's protocol, the winner's claim to a rulebook-legitimate conquest) would lose their object.
% FOUNDING_PROBLEM: To compress the dynamics of private land ownership into a playable hour: whoever lands on another's improved property pays whatever the owner asks, and concentration of everything in few hands follows — a playable proof built to teach a critique of rent (the patented Landlord's Game lineage), later carried into a commercial product whose drama is conquest and ruin.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the historical record: patent filings, the founder's published rules and accompanying political statement, and subsequent game-history scholarship document the pedagogical origin independently of any current winner or host. The commercial successor's own trade history corroborates the second genealogy — an entertainment product — and the two lineages' partisans dispute which founding problem governs the modern text. No party inside the game's winner seat attests anything.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.72 against the fixed referent — the standing by-the-book arrangement, assessed by this reading's own lights — not against any alternative the reading prefers. Transfers are one-directional once a monopoly forms, no clause returns capital downward, and the terminal event confiscates all remaining positions from all but one seat; the score is discounted slightly from saturation because stakes are bounded and participation voluntary. Suppression (0.66, unscaled by design) is structural first — the text offers no relief mechanism and makes insolvency procedurally mandatory — with a normative overlay maintained by the host; it is capped below saturation because the trade and auction rules admit coalition counter-play, which occasionally rescues a doomed seat. Theater (0.45) is honest about the arc's back half: once concentration becomes irreversible around minute 60, continued play is largely enactment of a foregone conclusion — real mechanism early, performance late. Accessibility_collapse is 0.40 because alternatives do NOT collapse: modified play remains trivially available (that is precisely why a sibling reading exists), and this reading suppresses alternatives rhetorically, not practically. Resistance is 0.55 — the mass adoption of house rules worldwide is documented, persistent resistance to the mandated endgame. The three measurement series share one grid (minutes 0-90) so every metric is authored at every examined point; the trajectory is a monotonic positive-feedback ratchet, not a cycle — each rent payment tightens the next squeeze — so no oscillation machinery is invoked, and the oscillation-is-extraction warning does not apply. Endpoint values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the winner seat the arrangement narrates as merit: skill identified undervalued assets, timing converted them into a moat, and the payout is the earned return. From the bankrupted seat the same sequence is compulsory ruin — the rules gave them obligations with no corresponding rights. The host seat experiences fiduciary fidelity to the text; the excluded advocates experience a protocol that disqualified their contribution before it was voiced; the analytical seat sees an authored artifact marketed for a century as a timeless classic. None of these perceptions is authored as fact — the engine derives each seat's classification from power, horizon, exit, and scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The winner sits near the subsidy end (declared beneficiary, powerful, exit only through victory). Bankrupted players sit at the full-target end (declared victims, powerless, trapped — no exit preserves their position, and elimination removes them entirely). Midgame players derive high d from their payer role despite their secondary beneficiary role, because their net flow is strongly negative. House-rule advocates derive low d through mobility: having exited, the arrangement barely reaches them. No directionality_overrides are authored: the one candidate — marking the host symmetric at 0.5 — shares the moderate power atom with the midgame payers, so a power-atom-keyed override would flatten the payers' correctly derived high d; the derivation from the host's absence from both beneficiary and victim lists is the better approximation of custodial neutrality. Scope effects are modest here: most seats are local (table-scale verification is easy), while the globally distributed advocate seat is the least exposed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabel in both directions. Reading the arrangement as pure mountain would launder a designed artifact's parameter choices as natural law — the declared beneficiaries make that laundering visible rather than silent. Reading it as pure extraction would erase the genuine coordination function (strangers can play a complex trading game at all only because the text specifies procedure) and would misread the pedagogical intent that distinguishes this reading from a mere predation story. Mandatrophy is deliberately NOT declared resolved: this reading holds the founding mandate live, and the R5 interview records the contest instead. The watch-item is the theater trajectory — if the demonstration's audience fully evaporates and by-the-book enforcement persists as ritual (sessions run to a foregone conclusion for no one's instruction), the arrangement slides toward the degraded-inertial signature; the rising theater series is the leading indicator of that slide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authored_rules_vs_natural_law_ambiguity,
    'Is the rulebook''s concentration dynamic a genuine instantiation of structural economic law, or a deliberately authored artifact whose apparent inevitability is a set of design parameters that identifiable parties (winners, the demonstration''s custodians) benefit from presenting as natural?',
    'Parameter-sensitivity testing across matched play sessions and simulations: vary starting capital, salary schedules, and property pricing while holding everything else fixed. If small parameter changes reliably dissolve or restore the concentration terminus, the ''law'' is authored and tunable rather than discovered.',
    'If the dynamic is authored, the mountain claim fails and the arrangement computes as an enforced hybrid — real coordination shell carrying asymmetric transfer with a beneficiary seat — rather than a demonstration of necessity; if it is robustly parameter-insensitive, the reading''s lawlikeness claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authored_rules_vs_natural_law_ambiguity, empirical, 'Whether the rulebook''s concentration outcome reflects discoverable structure or designable parameters.').

omega_variable(
    kernel_reading_commitment_scope,
    'This constraint is one reading (extraction_demo_reading) of the monopoly_rulebook kernel; how would instantiating the sibling readings change the structural picture?',
    'Author the siblings as separate stories and compare: social_scaffold_reading (house rules as required community correction) and tournament_orthodoxy_reading (the text as immutable competitive standard). Compare epsilon, victim sets, and computed types across the family.',
    'Under the scaffold reading the victim set shrinks and epsilon drops (liquidity slows elimination); under the orthodoxy reading the referent shifts to ranked competitive play where elimination is a consented entry condition and epsilon re-prices accordingly. The disagreement is located in the status of the mandated elimination: truth-bearing necessity versus correctable defect versus legitimate ranking condition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_scope, conceptual, 'Committer-frame scoping: which kernel, which reading, what siblings would change.').

omega_variable(
    bounded_stakes_moral_weight,
    'Does the arrangement''s bounded, voluntary, zero-material-stakes character (pieces reset at the next session) discount the weight of its transfers, or does this reading hold the mechanism load-bearing regardless of stakes?',
    'Contrast assessments of identical play arcs with and without material side-bets; test whether the reading''s own adherents price the demonstration differently when real money rides.',
    'If bounded-stakes discounting applies, effective pressure on the payer seats drops materially and the arrangement reads closer to a costly lesson than a predatory structure; if the reading weighs mechanism over stakes, the high extraction assessment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_stakes_moral_weight, preference, 'Whether simulated stakes carry the same evaluative weight as lived ones.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (the rules contain no relief mechanism, bankruptcy is procedurally mandatory) or internalized/social (tables keep playing past the point of fun because quitting violates the by-the-book norm the host maintains)?',
    'Observe session abandonment rates where the host explicitly releases players from the finish-the-game norm versus where the norm stands; if abandonment spikes when permission is granted, much of the held-in-place behavior was normative rather than procedural.',
    'If internalized, the arrangement''s effective grip exceeds what the rule text alone implies — enforcement lives in the table''s manners and would survive edits to the text; if structural, removing the no-relief clauses would collapse suppression immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus normative sources of the arrangement''s hold on players.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.45).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.47).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial object 'Monopoly's official rules' covers three structurally distinct claims and is split into three stories sharing the monopoly_rulebook kernel. This file (extraction_demo_reading) authors the historical-origin claim: the mandated arrangement demonstrates rent-driven concentration, epsilon high (0.72), victim set = bankrupted players. The social_scaffold_reading authors the correction claim: community amendment is required for playability, epsilon lower because injected liquidity slows the elimination cascade and shrinks the victim set. The tournament_orthodoxy_reading authors the competition claim: the text is the legitimate ranked-play framework, with the referent shifted to consented competitive entry so the same mechanics price differently. The extraction reading sits upstream: it is the genealogical indictment that pressures the orthodoxy's legitimacy conditions (its 'timeless neutral classic' framing) and the baseline the scaffold reading defines itself against. All three files carry cross-links in network.affects_constraints; no story in the family is an orphan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
