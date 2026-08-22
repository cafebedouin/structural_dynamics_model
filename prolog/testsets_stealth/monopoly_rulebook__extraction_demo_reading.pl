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
 *   human_readable: Unamended Monopoly Rulebook as Rent-Extraction Demonstration
 *   domain: game theory / social coordination / institutional design
 *
 * SUMMARY:
 *   The standing arrangement under contest is play conducted by the unamended
 *   Monopoly rule text: scheduled rents scaling with development, no
 *   redistribution mechanisms, bankruptcy as permanent removal, and
 *   consolidation of all assets in one survivor within 60 to 90 minutes. This
 *   file instantiates the extraction_demo_reading of the monopoly_rulebook
 *   kernel, for which that arc is not a defect but the payload: the text is a
 *   faithful, deliberately harsh working model of rent-extraction dynamics,
 *   and elimination is the structurally necessary outcome that carries the
 *   lesson. The epsilon referent is the unamended arrangement itself,
 *   assessed by this reading's own lights — never the house-ruled alternative
 *   the scaffold sibling endorses. Claim and metrics are authored
 *   independently: the claim is mountain because the reading presents the
 *   outcome as structurally necessary rather than chosen; the metrics
 *   describe the session arc as it actually runs. The eventual_winner is
 *   declared as beneficiary intentionally — a collector seat demonstrably
 *   exists — which routes this story through false-summit evaluation; the
 *   designed_vs_emergent_necessity omega documents the required
 *   natural-law-versus-constructed ambiguity, sharpened by the historical
 *   fact that the text descends from a game explicitly designed to produce
 *   this demonstration.
 *
 * KEY AGENTS:
 *   - eventual_winner: primary beneficiary (moderate/constrained) — converts mid-game rent exposure into the sole collecting position; gain expires at box-up
 *   - eliminated_players: primary victims (powerless/trapped) — drained by the rent schedule, removed at bankruptcy, no re-entry or rescue
 *   - struggling_holders: secondary payers (moderate/constrained) — solvent but net-negative; their choices select the winner, not whether one exists
 *   - house_rule_advocates: excluded (moderate/mobile) — liquidity-injection amendments ruled out of order at strict tables
 *   - publisher_trademark_holder: agenda_setter (institutional/arbitrage) — publishes and maintains the canonical text; its gain is orthogonal to the session transfer
 *   - critical_game_scholars: analytical observers — assess the demonstration's fidelity to actual market dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__extraction_demo_reading, 0.78).
domain_priors:suppression_score(monopoly_rulebook__extraction_demo_reading, 0.65).
domain_priors:theater_ratio(monopoly_rulebook__extraction_demo_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(monopoly_rulebook__extraction_demo_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__extraction_demo_reading, mountain).
narrative_ontology:human_readable(monopoly_rulebook__extraction_demo_reading, "Unamended Monopoly Rulebook as Rent-Extraction Demonstration").
narrative_ontology:topic_domain(monopoly_rulebook__extraction_demo_reading, "game theory / social coordination / institutional design").

domain_priors:emerges_naturally(monopoly_rulebook__extraction_demo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__extraction_demo_reading, 'a331c913-06c3-40d3-9a1d-6a6a587fa7d6').
narrative_ontology:cs_kernel_codification('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', fixed_text).
narrative_ontology:cs_authority_grounding('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', lineage).
narrative_ontology:cs_reading_relation('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', monopoly_rulebook__tournament_orthodoxy_reading, coexists_with).
narrative_ontology:cs_axiom('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', foundational, concentration_structurally_inevitable_under_text).
narrative_ontology:cs_axiom_status(concentration_structurally_inevitable_under_text, holdable).
narrative_ontology:cs_axiom_grounding('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', concentration_structurally_inevitable_under_text, empirically_contingent).
narrative_ontology:cs_axiom('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', foundational, text_fidelity_required_for_pedagogical_validity).
narrative_ontology:cs_axiom_status(text_fidelity_required_for_pedagogical_validity, holdable).
narrative_ontology:cs_axiom_grounding('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', text_fidelity_required_for_pedagogical_validity, instrumental).
narrative_ontology:cs_reference_frame('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', unamended_rent_extraction_text).
narrative_ontology:cs_drift_state('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', contemporary_home_play, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a331c913-06c3-40d3-9a1d-6a6a587fa7d6', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__extraction_demo_reading, eventual_winner).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eliminated_players).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, eventual_winner).
narrative_ontology:constraint_victim(monopoly_rulebook__extraction_demo_reading, struggling_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Started with the same capital as everyone else and survived the mid-game rent drain by assembling monopolies early. Through most of the session they were among the largest rent-payers at the table; the late-game position converts that accumulated exposure into the only collecting seat. Their gain lasts until the board is boxed up and resets.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eventual_winner, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(monopoly_rulebook__extraction_demo_reading, eventual_winner, payer).

% Entered with equal capital; the rent schedule drained their liquidity faster than acquisition could offset it. Bankruptcy removes them from the table entirely — no assets, no further turns, and the text provides no re-entry, subsidy, or consolation mechanism. Their remaining role in the session is watching it conclude without them.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, eliminated_players, payer,
    powerless, immediate, trapped, local).

% Still solvent but paying more rent each circuit than they collect. The rules leave them a choice set that determines only who wins, not whether someone wins: acquire aggressively and hope, trade from weakness, or ride toward bankruptcy. Committed to the session once it began.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, struggling_holders, payer,
    moderate, immediate, constrained, local).

% Propose injecting cash on Free Parking, raising starting capital, or capping rents — amendments that would slow or prevent elimination. At a strict-text table their proposals are ruled out of order before the first turn; they are physically present but absent from the governing framework. Their alternative lives in other households' rule sheets.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, house_rule_advocates, excluded,
    moderate, biographical, mobile, local).

% Holds the trademark and publishes the canonical rulebook across editions. Profits from the game's continued sale in whatever form tables will buy, and tolerates widespread informal amendment at home tables so long as the standard text remains the reference product. Its revenue tracks the activity's popularity, not the outcome of any session.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, publisher_trademark_holder, agenda_setter,
    institutional, generational, arbitrage, global).

% Study the rulebook as a materialized model of rent dynamics and trace its lineage to Elizabeth Magie's 1904 Landlord's Game and the Georgist movement. They assess what the game's dynamics do and do not show about actual markets. They neither collect nor pay within any session.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__extraction_demo_reading, critical_game_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monopoly_rulebook__extraction_demo_reading, eventual_winner).
narrative_ontology:fixing_cost_class(monopoly_rulebook__extraction_demo_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common procedural framework — turn order, transaction and auction rules, bankruptcy adjudication — that lets four to six players run a determinate property-acquisition contest to a definite conclusion without negotiating process. It solves the problem of conducting a multi-player economic simulation with settled rules for dispute and termination.
% TRANSFER_FUNCTION: Moves paper capital from every player toward whichever player first assembles irreplaceable property monopolies, via scheduled rent payments that scale with development; terminal bankruptcy consolidates all remaining assets to the last solvent player. Net flow is many-to-one, terminating in winner-takes-all.
% ABSENT_VOICES: House-rule advocates are ruled out of order at strict tables before play begins; eliminated players lose their seat — literally — at bankruptcy and cannot speak to the endgame their losses funded; the Georgist counter-tradition (Magie's second, Prosperity ruleset) that would insist the demonstration argues for the single-tax remedy rather than fatalism is absent from the commercial text.
% DISAPPEARANCE_RATIONALE: Game nights organized around this text would reorganize around something else: tables already running amended versions show the activity continues comfortably under liquidity-injecting rules, while strict tables would either adopt another winner-takes-all game or schism into house-rule factions. The characteristic arc — 60 to 90 minutes, serial elimination, sole survivor — is constituted by the text and vanishes with it.
% FOUNDING_PROBLEM: Elizabeth Magie's 1904 Landlord's Game was built to make the Georgist critique of land rent playable: to let ordinary people feel how rent enrichment of landlords impoverishes tenants, and — in its second, Prosperity ruleset — how the single-tax remedy reverses it. The commercialized descendant retained the exploitation ruleset and dropped the remedy.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: Magie's patents and Georgist writings establish the design intent; game historians documenting the Magie-to-Darrow lineage and economists who assign the game to teach rent-seeking both attest that the pedagogical function persists. Hasbro's marketing, which frames the product as family entertainment, corroborates nothing about the founding problem.
narrative_ontology:disappearance_verdict(monopoly_rulebook__extraction_demo_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__extraction_demo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__extraction_demo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monopoly_rulebook__extraction_demo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__extraction_demo_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.78) because the rent schedule drains net-losers monotonically and the terminal consolidation leaves one holder of everything; the temporal series shows the accumulation ramp the reading treats as the point rather than a malfunction. Suppression (0.65) reflects the text's closure once play begins — no rescue, re-entry, or secession — tightening as liquidity drains; the suppression_requirement series tracks this within-session ratchet, which is why that series is authored despite a stable rulebook. Theater ratio is low (0.18): the mechanics do real work every session, with only mild ceremonial continuation in foregone endgames. Accessibility collapse is 0.82 — under the unamended text every playthrough converges to concentration, and alternatives collapse once the rules are understood; the residual is the meta-level exit of switching to house rules, which belongs to the sibling reading's world. Resistance (0.28) covers collusion attempts, trade refusals, and rage-quits, all futile under strict play. All three metric series share one seven-point grid (0/15/30/45/60/75/90 minutes) so no row substitutes an end-state value into earlier times.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same text and should compute differently. From the winner's seat the rules are a meritocratic ascent they financed fairly through the same rent schedule that ruins others; from the eliminated seat the identical schedule is unrecoverable loss with zero recourse; from the scholar's seat the whole arc is evidence about markets; from the publisher's seat it is product maintenance indifferent to who wins. The engine derives these per-seat classifications from the structural data (role, power, exit); the divergence between the winner's and the eliminated player's computed experience of the same rules is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map directly onto the transfer structure. eventual_winner is declared beneficiary (with payer as secondary role — they reached the collecting seat by passing through the paying one), placing them near the beneficiary end of directionality but not at 0.0. eliminated_players are declared victims with trapped exit, placing them near the full-target end; struggling_holders are victims-in-progress with slightly more agency. house_rule_advocates sit outside the transfer loop entirely — excluded rather than coordinated. publisher_trademark_holder administers the text but is deliberately NOT listed as a beneficiary: its revenue tracks the activity's existence, not the extraction transfer, so no override is needed to keep it out of the beneficiary derivation. critical_game_scholars carry the analytical atom. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already yield the correct directionalities, and the override surface keys on power atoms, which would smear corrections across unrelated seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved here because the founding function is live: the arrangement still performs the demonstration every session, so nothing has outlived its mandate — for this reading the mandate IS the extraction arc. The classification guards against two symmetric mislabels. Calling the arrangement a pure coordination device (an evening's entertainment that happens to have winners) erases the asymmetric transfer and the eliminated victim set; calling it pure predation erases consent at entry, bounded play-money stakes, and the full reset after each session. The piton failure mode is likewise distant: theater is low and the function is executed, not merely performed at. The live risk runs the opposite direction from atrophy — the constraint works exactly as designed, which is precisely this reading's claim and why the designed_vs_emergent_necessity omega matters more than any decay question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    designed_vs_emergent_necessity,
    'Is the concentration-and-elimination dynamic a natural consequence of any property-trading structure with positional rents, or an authored artifact tuned by designers to produce it?',
    'Parameter-space simulation over rent schedules, starting capital, and board topology; historical analysis of the Magie-to-Darrow-to-Hasbro design record; comparative study of variant property games.',
    'If the dynamic is robust across parameter space, the structural-necessity claim strengthens. If it depends on tuned parameters (rent levels calibrated to drain slower players on schedule), the inevitability is a design choice and the constraint is constructed — supporting reclassification away from the mountain claim despite the declared beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designed_vs_emergent_necessity, empirical, 'Whether the rulebook''s extraction arc is emergent law or tuned design.').

omega_variable(
    magic_circle_transfer_question,
    'Does the extraction experienced at the table transfer beyond the magic circle — shaping players'' beliefs about how markets work — or is it confined to play-money stakes?',
    'Controlled studies of gameplay effects on economic attitudes; longitudinal comparison of heavily exposed cohorts; discourse analysis of how the game is invoked in political argument.',
    'If transfer is real, the constraint''s effective reach exceeds the table and the pedagogical claim gains force in both directions. If nil, the measured extraction is bounded by the session and the reading reduces to metaphor — the structural-necessity claim would describe a toy world only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magic_circle_transfer_question, empirical, 'Whether in-game rent dynamics map to extra-game economic cognition.').

omega_variable(
    consent_vs_compulsion_boundary,
    'Is the suppression that binds players once the session begins structural (the text permits no rescue, re-entry, or secession) or consensual (players freely accepted the arc at shuffle)?',
    'Compare mid-game quitters'' post-exit assessments with rule-bound completers; test whether felt compulsion persists after leaving the table (post-exit suppression trajectory).',
    'If suppression is mostly consensual, effective suppression falls and the arrangement resembles an agreed harsh procedure at the individual seat. If structural within-session, the eliminated seat computes close to a fully targeted position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_compulsion_boundary, empirical, 'Structural versus consensual suppression mechanism in bounded play.').

omega_variable(
    kernel_reading_allocation,
    'Which reading of the monopoly_rulebook kernel correctly identifies the operative constraint — this extraction-demonstration reading, the social-scaffold reading (community correction is the real rule), or the tournament-orthodoxy reading (the text as legitimate ranking instrument)?',
    'Cross-table observational study correlating rule-fidelity with elimination rates, session welfare, and what regulars say the game is for; treat revealed house-rule adoption as revealed preference over readings.',
    'Scaffold adoption dissolves the victim set (elimination becomes optional) and drops epsilon; orthodoxy adoption recodes the transfer as earned outcome and shifts the constraint toward coordination-at-the-ranking-seat; this reading''s structure survives only where tables actually play the unamended text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer-frame omega: this story is one reading of the monopoly_rulebook kernel; sibling readings restructure the victim set and epsilon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__extraction_demo_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__extraction_demo_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__extraction_demo_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__extraction_demo_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(mono_tr_t45, monopoly_rulebook__extraction_demo_reading, theater_ratio, 45, 0.13).
narrative_ontology:measurement(mono_tr_t60, monopoly_rulebook__extraction_demo_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(mono_tr_t75, monopoly_rulebook__extraction_demo_reading, theater_ratio, 75, 0.17).
narrative_ontology:measurement(mono_tr_t90, monopoly_rulebook__extraction_demo_reading, theater_ratio, 90, 0.18).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(mono_be_t45, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(mono_be_t60, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(mono_be_t75, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 75, 0.74).
narrative_ontology:measurement(mono_be_t90, monopoly_rulebook__extraction_demo_reading, base_extractiveness, 90, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(mono_su_t45, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 45, 0.53).
narrative_ontology:measurement(mono_su_t60, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(mono_su_t75, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 75, 0.63).
narrative_ontology:measurement(mono_su_t90, monopoly_rulebook__extraction_demo_reading, suppression_requirement, 90, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__extraction_demo_reading, resource_allocation).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__social_scaffold_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__extraction_demo_reading, monopoly_rulebook__tournament_orthodoxy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one rulebook text, three structurally distinct constraints. This file instantiates the extraction-demonstration reading (epsilon ~0.78, victim set = eliminated players, winner-takes-all terminus). The social-scaffold sibling authors the same text as a coordination frame kept playable by community correction (low epsilon, no fixed victim set); the tournament-orthodoxy sibling authors it as a legitimate competitive ranking instrument (losses recoded as meritocratic prices, near-zero epsilon at the ranking seat). The epsilon differences are reading-indexed over the same referent text. Edges run from this reading to both siblings because the extraction dynamic is what each sibling responds to — by correcting it or by defending the text against correction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
